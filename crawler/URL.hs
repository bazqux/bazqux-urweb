{-# LANGUAGE RecordWildCards, OverloadedStrings, FlexibleContexts,
             ViewPatterns #-}
-- | Различные утилиты для работы с URL-ами. Некоторые достаточно
-- специфичны для кравлера, так что это не совсем библиотечный модуль,
-- скорее часть логики.
module URL
    ( URL, TURL, HostName, IP, UrlParam(..), renderUrlParam
    , normalizeURL, normalizeTURL, relUri, relUriNoNormalize, correctUrl
      -- * Работа с хостами для Downloader-а
    , hostNameFromUrl -- , changeUrlHostName
    , changeUrlQuery, isToplevelUrl
    , topLevelHostName
    , escapeQueryValue, escapeQueryValueT
    , parseQueryString, parseQueryStringUtf8Only
    , urlQueryString, urlQueryStringUtf8Only
    , urlPathAndQuery, decodeURIComponentT
    , ignoredUrlQuery, rmIgnoredUrlQuery, maybeHostNameFromUrl
    , urlAddParam
    ) where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import Network.URI
import Data.List
import Data.Maybe
import Data.Char
import Data.String
import qualified Network.HTTP.Types as N
import qualified Network.HTTP.Conduit as C
import qualified Control.Exception as E
-- import qualified Data.Encoding as Enc
-- import qualified Data.Encoding.BootString as Enc

-- import qualified Data.Text.Punycode as Punycode

-- punycode = T.intercalate "." . map puny . T.split (== '.')
--     where puny d
--               | T.all ((< 128) . ord) d = d
--               | otherwise = T.append "xn--" (T.decodeUtf8 $ Punycode.encode d)
-- работает, но parseURI[Reference] не разгребают http://президент.рф

-- import qualified Data.Text.IDN.Punycode as Punycode
-- что-то не работает
-- Punycode.encode (T.pack "п") Nothing
-- выдает *** Exception: Output would exceed the buffer space provided
-- (т.е. даже для одной буквы)

-- Библиотека encoding (с BootString) даже не собирается

type IP = String
type URL = String
type TURL = T.Text
type HostName = URL

-- | Делает абсолютный адрес из относительного или
-- оставляет абсолютным исходный, а также нормализует результат
relUri :: URL -> URL -> URL
relUri r url = normalizeURL $ relUriNoNormalize r url

relUriNoNormalize :: URL -> URL -> URL
relUriNoNormalize r url = fromMaybe r $ relUri' r url

relUri' :: URL -> URL -> Maybe URL
relUri' r to = fmap (($ "") . uriToString id) $ -- httpOnly $
               liftM2 relativeTo
                       (parseURIReference $ fixNonAscii $ trim r)
                       (parseURI to)

normalizeTURL = T.pack . normalizeURL . T.unpack
-- TODO: может сделать работу с unicode char, а не байтами?
-- все равно уже давным давно работаем с Text, а не ByteString

-- Стоит убрать unescape из normalize
-- Тогда не нужен будет escapeJunkURIString
-- http://en.wikipedia.org/wiki/URL_normalization
-- Сделать только Normalizations that Preserve Semantics
-- и убирание utm_source, phpsessid, ...
--
-- Плюс percent encode utf-8 и punicode hostname дабы parseURI работал
-- это можно вынести отдельно, чтобы parseURIReference работал.
-- fixInternationalURI

-- описание формата url
-- http://hackage.haskell.org/packages/archive/URLb/0.0.1/doc/html/src/Network-URLb.html

trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

fixURI uri@(URI {..}) =
     uri { uriScheme = scheme
         , uriPath =
               -- asdf.com?q=1  -> asdf.com/?q=1
               if uriPath == "" && q /= ""
               then "/"
               else uriPath
         , uriAuthority =
             fmap (\ a -> a { uriPort = rmPort (uriPort a) }) uriAuthority
         , uriQuery = q
         }
    where rmPort ":80"  | scheme == "http:"  = ""
          rmPort ":443" | scheme == "https:" = ""
          rmPort p = p
          scheme = if uriScheme == "feed:" then "http:" else uriScheme
          q | ignoredUrlQuery (T.pack uriQuery) = ""
            | otherwise = uriQuery
          plusQ "" = ""
          plusQ x  = '?' : x

ignoredUrlQuery (parseQueryString -> Just params) =
    all ignoredQueryParam params && not (null params)
    -- убираем только если все параметры не нужны,
    -- в противном случае может испортиться форматирование
    -- ?http://  -> asdf%3A%2F%2F
ignoredUrlQuery _ = False

ignoredQueryParam (p,_) =
    elem p $ map UUtf8
             [ "utm_source", "utm_medium", "utm_campaign"
             , "utm_term", "utm_content", "utm_cid", "utm_reader"
             , "PHPSESSID", "SID"]

rmIgnoredUrlQuery url
    | [before, after] <- T.split (== '?') url
    , (after:frag) <- T.split (== '#') after
    , ignoredUrlQuery after = T.intercalate "#" (before:frag)
    | otherwise = url

fixURL u = case parseURI u of
    Nothing -> u
    Just uri -> uriToString id (fixURI uri) ""

decodeURIComponentT = T.pack . unEscapeString . T.unpack

fixNonAscii = escapeURIString (\ c -> ord c <= 0x7f && c `notElem` " []{}|\"")

normalizeURL url' =
    fixURL $
    -- normalizePathSegments $
    normalizeCase $ -- escapeJunkURIString isUnesc $
    addScheme url
    where url = fixNonAscii $ trim url'
          -- isUnesc c = isUnescapedInURI c && not (elem c "[]")
          defaultScheme
              | url /= "" = "http://" ++ url -- по-умолчанию http
              | otherwise = "" -- пустой урл оставляем пустым?
          addScheme [] = defaultScheme
          addScheme (':':'/':'/':_) = url -- есть схема
          addScheme (c:cs)
              | isSchemeChar c = addScheme cs
              | otherwise = defaultScheme

isAlphaChar c    = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
isDigitChar c    = (c >= '0' && c <= '9')
isAlphaNumChar c = isAlphaChar c || isDigitChar c
isSchemeChar c   = (isAlphaNumChar c) || (c `elem` "+-.")

-- httpOnly (Just u) | "http" `isPrefixOf` uriScheme u = Just u
-- httpOnly _ = Nothing

-- -- | Вертает IP (в виде строки) и hostName для заданного URL.
-- addrFromUrl :: String -> IO (Either E.SomeException (HostAddressString, HostName))
-- addrFromUrl url = E.try $ do
-- --    print ("getHostByName", hostNameFromUrl url)
--     he <- getHostByName $ hostNameFromUrl url
--     let ha = hostAddress he
--     haa <- inet_ntoa ha
-- --    bprint (haa, he)
-- --    he' <- getHostByAddr AF_INET ha
-- --    ^ не всегда тут выдается нормальный адрес
-- --    bprint he'
--     return (haa, hostName he)

changeUrlQuery :: URL -> String -> URL
changeUrlQuery url query =
    case parseURIReference url of
        Just uri -> uriToString id (uri { uriQuery = query
                                        , uriFragment = ""
                                        }) ""
        Nothing -> url ++ query

changeUrlHostName :: URL -> HostName -> URL
changeUrlHostName url host =
    case parseURIReference url of
        Just uri@(URI {..}) ->
            case uriAuthority of
                Just auth ->
                    uriToString id (fixURI $
                                    uri { uriAuthority =
                                              Just auth { uriRegName = host }
                                        }) ""
                Nothing   -> url
        Nothing -> url -- непонятно что, оставим как есть

-- | a.b.c.d.com => d.com
topLevelHostName :: HostName -> HostName
topLevelHostName hn
    | isIPAddr hn = hn
    | otherwise = intercalate "." $ reverse $ take 2 $ reverse $ splitPoints hn

isIPAddr hn = length groups == 4 && all ip groups
    where groups = splitPoints hn
          ip x = length x <= 3 && all (\ e -> e >= '0' && e <= '9') x &&
                 read x <= 255
splitPoints = filter (/= ".") . groupBy (\ a b -> a /= '.' && b /= '.')

hostNameFromUrl :: URL -> HostName
hostNameFromUrl url =
    case parseURIReference url of
        Just uri ->
            case uriAuthority uri of
                Just auth -> uriRegName auth
                Nothing   -> uriPath uri
        Nothing -> url -- непонятно что, попробуем как есть (хотя,
                       -- сокрее всего, это говно какое-нить)

maybeHostNameFromUrl :: URL -> Maybe HostName
maybeHostNameFromUrl url =
    return . uriRegName =<< uriAuthority =<< parseURIReference url

isToplevelUrl :: URL -> Bool
isToplevelUrl url =
    case parseURI url of
        Just u -> uriPath u `elem` ["", "/"] &&
                  uriQuery u == "" && uriFragment u == ""
        Nothing -> False

correctUrl :: URL -> Bool
correctUrl = maybe False (const True) . parseURI

escapeQueryValue = escapeURIString isUnreserved

escapeQueryValueT = T.pack . escapeQueryValue . T.unpack

renderQuery :: [(UrlParam, UrlParam)] -> T.Text
renderQuery = T.intercalate "&" . map q
    where q (n, UUtf8 "") = e n
          q (n,v)  = T.concat [e n, "=", e v]
          e = renderUrlParam

renderUrlParam (UUtf8 t) = escapeQueryValueT t
renderUrlParam (UJunk j) = T.pack $ escapeQueryValue $ B.unpack j

-- escapeJunkURIString p s = concatMap (escapeJunkURIChar p) s
-- escapeJunkQueryValue = escapeJunkURIString isUnreserved

-- -- | Как escapeURIChar, но без обработки Utf8
-- escapeJunkURIChar :: (Char->Bool) -> Char -> String
-- escapeJunkURIChar p c
--     | p c       = [c]
--     | otherwise = ['%', toChrHex (ord c `div` 16), toChrHex (ord c `rem` 16)]
--     where
--         toChrHex d
--             | d < 10    = chr (ord '0' + fromIntegral d)
--             | otherwise = chr (ord 'A' + fromIntegral (d - 10))

utf8Only q = [(n, p) | (UUtf8 n, UUtf8 p) <- q]

parseQueryStringUtf8Only :: Monad m => T.Text -> m [(T.Text, T.Text)]
parseQueryStringUtf8Only = liftM utf8Only . parseQueryString

parseQueryString :: Monad m => T.Text -> m [(UrlParam, UrlParam)]
parseQueryString qs = do
    q <- return $ N.parseQuery $ T.encodeUtf8 qs
--    q <- liftM (N.parseQuery . C.queryString) $ C.parseUrl $ "http://example.com/asdf?" ++ T.unpack qs
--  TODO: ^ в скомпилированном режиме закомментированный код вызывает sigsegv на "url=%s"
    return [(urlParam n, urlParam $ fromMaybe "" v) | (n,v) <- q]

-- | Иногда попадаются параметры URL, которые не являются нормальным UTF-8
-- Например:
-- http://ntc.duma.gov.ru/duma_na/asozd/asozd_text.php?nm=436-%D4%C7&dt=2010
-- %D4%C7 -- не является корректной utf-8 последовательностью
data UrlParam
    = UUtf8 T.Text
    | UJunk B.ByteString
    deriving (Show, Eq)

instance IsString UrlParam where
    fromString = UUtf8 . T.pack

urlQueryStringUtf8Only :: T.Text -> Either String [(T.Text, T.Text)]
urlQueryStringUtf8Only = fmap utf8Only . urlQueryString

urlQueryString :: T.Text -> Either String [(UrlParam, UrlParam)]
urlQueryString u = case C.parseUrl $ T.unpack u of
    Left (E.fromException -> Just (C.InvalidUrlException _ e)) -> Left e
    Left e -> Left $ show e
    Right pu ->
--        trace (show $ HTTPE.queryString pu) $
        Right [ (urlParam n, urlParam $ fromMaybe "" v)
              | (n,v) <- N.parseQuery $ C.queryString pu ]
urlParam s = case T.decodeUtf8' s of
    Right t -> UUtf8 t
    Left _ ->  UJunk s

urlPathAndQuery = fmap (\ u -> uriPath u ++ uriQuery u) . parseURI

-- test = do
--     t <- fmap (map read . lines) $ readFile "resp.txt" :: IO [Double]
--     let s = sort t
--     print (sum s / fromIntegral (length s), s !! (length s `div` 3 * 2), last s)

urlAddParam u p =
    u ++ (if '?' `elem` u then "&" else "?") ++ p
