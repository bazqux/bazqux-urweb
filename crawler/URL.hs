{-# LANGUAGE RecordWildCards, OverloadedStrings, FlexibleContexts,
             ViewPatterns, TupleSections #-}
-- | Различные утилиты для работы с URL-ами. Некоторые достаточно
-- специфичны для кравлера, так что это не совсем библиотечный модуль,
-- скорее часть логики.
module URL
    ( URL, TURL, HostName, THostName, IP, UrlParam(..), renderUrlParam
    , normalizeURL, normalizeTURL, validateEmail
    , relUri, relUriNoNormalize, relUriNoNormalizeT, correctUrl
      -- * Работа с хостами для Downloader-а
    , hostNameFromUrl, hostNameFromUrlT -- , changeUrlHostName
    , fastHostNameFromUrl
    , humanReadableURL, humanReadableHostName
    , changeUrlQuery, isToplevelUrl, isIPAddr
    , topLevelHostName
    , encodeURIComponent, encodeURIComponentT
    , parseQueryString, parseQueryStringUtf8Only
    , urlQueryString, urlQueryStringUtf8Only
    , urlPathAndQuery, decodeURIComponentT
    , ignoredUrlQuery, rmIgnoredUrlQuery, maybeHostNameFromUrl
    , urlAddParam, urlAddParamT, urlAddParamsT
    , maskUrlPasswordT, maskUrlPasswordT', urlHasAuthT
    , urlAddOrReplaceParam
    , httpToHttps, rmHttpPrefix, rmHttpPrefix', isHttp, rmWwwPrefix
    , testDomains, matchDomains', pathGet, pathGet'
    , metaRedirectUrl
    , decodeDataURI
    ) where

import Control.Monad
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Internal as T (Text(..))
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Array as A
import Network.URI
import Data.List
import Data.Maybe
import Data.Char
import Data.String
import qualified Network.HTTP.Types as N
import qualified Network.HTTP.Client as C
import qualified Control.Exception as E
import Data.Array.Unboxed
import qualified Text.Email.Validate
import Lib.StringConversion
import Lib.Regex
-- import qualified Data.Encoding as Enc
-- import qualified Data.Encoding.BootString as Enc

-- import qualified Data.Text.IDN.Punycode as Punycode
-- что-то не работает
-- Punycode.encode (T.pack "п") Nothing
-- выдает *** Exception: Output would exceed the buffer space provided
-- (т.е. даже для одной буквы)

-- Библиотека encoding (с BootString) даже не собирается

import qualified Data.Text.Punycode as Punycode

punycode = T.intercalate "." . map puny . T.split (== '.')
    where puny (T.toLower . decodeURIComponentT -> d)
              | T.all allowed d = d
              | p <- T.decodeUtf8 $ Punycode.encode d
              , T.all allowed p
                -- допускаем только валидные punycode-домены
                -- (никаких "xn--\"http-"),
                -- чтобы следующий punycode/normalizeURL
                -- не добавлял новых "xn--"
              = T.append "xn--" p
              | otherwise = d
          allowed x =
              (x >= '0' && x <= '9') || (x >= 'a' && x <= 'z') || x == '-'
-- работает, но parseURI[Reference] не разгребают http://президент.рф

humanReadableHostName = T.intercalate "." . map unpuny . T.split (== '.')
    where unpuny (T.toLower . decodeURIComponentT -> d)
              | T.isPrefixOf "xn--" d
              , Right r <- Punycode.decode $ T.encodeUtf8 $ T.drop 4 d
              = r
              | otherwise = d

testHumanReadableURL =
    test "http://минобрнауки.рф/новости?ячсм=фыва&qw?%3Dф%26er=?%3D#йцук"
    where test x = humanReadableURL (normalizeTURL x) == x

-- нужен http, normalizeURL не делается
humanReadableURL u =
    maybe u (T.pack . showURI . fixPath) $
    parseNonAsciiURIReference' humanReadableHostName $ T.unpack u
    where fixPath uri =
              uri { uriPath = concat ['/':unesc "/?#" s | s <- pathSegments uri]
                    -- заодно убирает trailing slash, если есть
                  , uriFragment = unEscapeString $ uriFragment uri
                  , uriQuery =
                      if uriQuery uri /= "" then
                          '?' :
                          (T.unpack $ renderQuery' renderParam $
                           parseQueryString $ T.pack $ uriQuery uri)
                      else
                          ""
                  }
          unesc :: String -> String -> String
          unesc s =
              escapeURIString (\ c -> c `notElem` s) .
              unEscapeString
          renderParam (UUtf8 t) = T.pack $ unesc "&=#" $ T.unpack t
          renderParam (UJunk j) = T.pack $ encodeURIComponent $ B.unpack j

-- | Нормализация и проверка корректности e-mail
-- Делает punicode домену и escape-ит non-latin имя пользователя
validateEmail e
    | T.length tld > 1 && T.any isAlpha tld && not (T.null sd) =
        bst <$> Text.Email.Validate.canonicalizeEmail (tbs e')
    | otherwise = Nothing
    where e' = pre' <> domain'
          (sd, tld) = T.breakOnEnd "." domain'
          domain' = punycode $ T.filter (not . isSpace) domain
          pre' = T.pack $ escapeURIString (<= '\x7f') $ filter (not . isSpace)
              $ T.unpack pre
          (pre, domain) = T.breakOnEnd "@" $ T.strip e

maskUrlAuth' mask u
    | nu <- normalizeURL u
    , Just uri <- parseURI nu
    , nu' <- uriToString ((++ "@") . mask . init) uri ""
    , nu' /= nu
    = Just nu'
    | otherwise = Nothing

maskPassword t
    | p /= "" = T.append u ":***"
    | otherwise = t
    where (u, p) = T.break (== ':') t

maskUrlPasswordT' f u
    | T.any (== '@') u =
        maybe u T.pack $ maskUrlAuth' (T.unpack . f . T.pack) $ T.unpack u
    | otherwise = u
maskUrlPasswordT u = maskUrlPasswordT' maskPassword u

urlHasAuthT u
    | T.any (== '@') u = isJust $ maskUrlAuth' (const "") $ T.unpack u
    | otherwise = False

type IP = String
type URL = String
type TURL = T.Text
type HostName = URL
type THostName = TURL

-- | Делает абсолютный адрес из относительного или
-- оставляет абсолютным исходный, а также нормализует результат
relUri :: URL -> URL -> URL
relUri r url = normalizeURL $ relUriNoNormalize r url

relUriNoNormalize :: URL -> URL -> URL
relUriNoNormalize r url = fromMaybe r $ relUri' r url

relUriNoNormalizeT :: TURL -> TURL -> TURL
relUriNoNormalizeT r url =
    T.pack $ relUriNoNormalize (T.unpack r) (T.unpack url)

showURI u = uriToString id u ""

relUri' :: URL -> URL -> Maybe URL
relUri' r to = fmap showURI $ -- httpOnly $
               liftM2 relativeTo
                       (parseNonAsciiURIReference r)
                       (parseURI to)

normalizeTURL = T.pack . normalizeURL . T.unpack
-- TODO: может сделать работу с unicode char, а не байтами?
-- все равно уже давным давно работаем с Text, а не ByteString

-- Стоит убрать unescape из normalize
-- Тогда не нужен будет escapeJunkURIString
-- http://en.wikipedia.org/wiki/URL_normalization
-- Сделать только Normalizations that Preserve Semantics
-- и убирание utm_source, phpsessid, …
--
-- Плюс percent encode utf-8 и punycode hostname дабы parseURI работал
-- это можно вынести отдельно, чтобы parseURIReference работал.
-- fixInternationalURI

-- описание формата url
-- http://hackage.haskell.org/packages/archive/URLb/0.0.1/doc/html/src/Network-URLb.html

fixURI uri@(URI {..}) =
     uri { uriScheme = scheme
         , uriPath =
               -- asdf.com?q=1  -> asdf.com/?q=1
               if uriPath == "" && q /= ""
                   && scheme `elem` ["http:", "https:", "ftp:"]
               then "/"
               else uriPath
         , uriAuthority =
             fmap (\ a -> a { uriPort = rmPort (uriPort a)
                            , uriRegName = fixRegName (uriRegName a) })
             uriAuthority
         , uriQuery = q
         }
    where rmPort ":80"  | scheme == "http:"  = ""
          rmPort ":443" | scheme == "https:" = ""
          rmPort p = p
          fixRegName = T.unpack . punycode . T.pack
          scheme = if uriScheme == "feed:" then "http:" else uriScheme
          q | ignoredUrlQuery (T.pack uriQuery) = ""
            | otherwise = uriQuery

ignoredUrlQuery (parseQueryString -> params) =
    all ignoredQueryParam params && not (null params)
    -- убираем только если все параметры не нужны,
    -- в противном случае может испортиться форматирование
    -- ?http://  -> asdf%3A%2F%2F

ignoredQueryParam (p, _) =
    elem p $ map UUtf8
             [ "utm_source", "utm_medium", "utm_campaign"
             , "utm_term", "utm_content", "utm_cid", "utm_reader"
             , "PHPSESSID", "SID", "ASPSESSIONID"
             , "ncid" -- только у techcrunch видел, стоит ли вырезать?
             ]

rmIgnoredUrlQuery url
    | [before, after] <- T.split (== '?') url
    , (after:frag) <- T.split (== '#') after
    , ignoredUrlQuery after = T.intercalate "#" (before:frag)
    | otherwise = url

decodeURIComponentT = T.pack . unEscapeString . T.unpack

allowedCharacters :: UArray Char Bool
allowedCharacters =
    listArray ('\0', '\x7f')
    [ isJust (parseURIReference [c]) || c == ':' || c == '%'
    | c <- ['\0'..'\x7f']]

fixBadPercentEncoding = go
    where go [] = []
          go ('%':a:b:xs)
              | hex a && hex b = '%' : a : b : go xs
          go ('%':xs) = '%':'2':'5': go xs
          go (x:xs) = x : go xs
          hex x =
              (x >= '0' && x <= '9') ||
              (x >= 'A' && x <= 'F') ||
              (x >= 'a' && x <= 'f')

preprocessForParsing = stripViewSource . fixNonAscii . rmQuotes . trim
    where -- isUnesc c = isUnescapedInURI c && not (elem c "[]")
          stripViewSource u = fromMaybe u $ stripPrefix "view-source:" u
          fixNonAscii =
              escapeURIString (\ c -> ord c <= 0x7f && allowedCharacters ! c) .
              fixBadPercentEncoding
          rmQuotes ('"':(reverse -> ('"':rs))) = trim $ reverse rs
          rmQuotes x = x
          trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

normalizeURL url' =
    fixURL $
    -- normalizePathSegments $
    normalizeCase $ -- escapeJunkURIString isUnesc $
    addScheme url
    where fixURL u = case parseURI u of
              Nothing -> u
              Just uri -> showURI $ fixURI uri
          url = preprocessForParsing url'
          defaultScheme
              | url /= "" = "http://" ++ url -- по-умолчанию http
              | otherwise = "" -- пустой урл оставляем пустым?
          addScheme [] = defaultScheme
          addScheme (':':_) = url -- есть схема
          addScheme (c:cs)
              | isSchemeChar c = addScheme cs
              | otherwise = defaultScheme

isAlphaChar c    = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
isDigitChar c    = (c >= '0' && c <= '9')
isAlphaNumChar c = isAlphaChar c || isDigitChar c
isSchemeChar c   = (isAlphaNumChar c) || (c `elem` ("+-." :: [Char]))

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
        Just uri -> showURI $ uri { uriQuery = query
                                  , uriFragment = ""
                                  }
        Nothing -> url ++ query

changeUrlHostName :: URL -> HostName -> URL
changeUrlHostName url host =
    case parseURIReference url of
        Just uri@(URI {..}) ->
            case uriAuthority of
                Just auth ->
                    showURI $ fixURI $
                    uri { uriAuthority =
                            Just auth { uriRegName = host }
                        }
                Nothing   -> url
        Nothing -> url -- непонятно что, оставим как есть

-- | a.b.c.d.com => d.com
topLevelHostName :: THostName -> THostName
topLevelHostName hn
    | isIPAddr hn = hn
    | otherwise =
        T.intercalate "." $ reverse $ take 2 $ reverse $ splitPoints hn

isIPAddr hn = length groups == 4 && all ip groups
    where groups = splitPoints hn
          ip x = T.length x <= 3 && T.all (\ e -> e >= '0' && e <= '9') x &&
                 read (T.unpack x) <= 255
splitPoints = T.split (== '.')

fastHostNameFromUrl u
    | Just p <- T.stripPrefix "https://" u = go p
    | Just p <- T.stripPrefix "http://" u = go p
    | otherwise = u
    where go = punycode . T.takeWhileEnd (/= '@') . T.takeWhile (/= '/')

hostNameFromUrl :: URL -> HostName
hostNameFromUrl = T.unpack . hostNameFromUrlT . T.pack

parseNonAsciiURIReference' hostNameFix =
    fmap fixA . parseURIReference . preprocessForParsing
    where fixA uri =
              uri { uriAuthority =
                    fmap (\ a -> a { uriRegName = fixRegName (uriRegName a) }) $
                    uriAuthority uri }
          fixRegName = T.unpack . hostNameFix . T.pack

parseNonAsciiURIReference = parseNonAsciiURIReference' punycode

hostNameFromUrlT (T.unpack -> url) =
    T.pack $
    case parseNonAsciiURIReference url of
        Just uri ->
            case uriAuthority uri of
                Just auth -> uriRegName auth
                Nothing   -> uriPath uri
        Nothing -> url -- непонятно что, попробуем как есть (хотя,
                       -- скорее всего, это говно какое-нить)

maybeHostNameFromUrl :: URL -> Maybe THostName
maybeHostNameFromUrl url =
    return . (punycode . T.pack . uriRegName)
    =<< uriAuthority =<< parseNonAsciiURIReference url

isToplevelUrl :: URL -> Bool
isToplevelUrl url =
    case parseURI url of
        Just u -> uriPath u `elem` ["", "/"] &&
                  uriQuery u == "" && uriFragment u == ""
        Nothing -> False

correctUrl :: URL -> Bool
correctUrl = maybe False (const True) . parseURI

encodeURIComponent = escapeURIString isUnreserved

encodeURIComponentT = T.pack . encodeURIComponent . T.unpack

renderQuery = renderQuery' renderUrlParam

renderQuery' :: (UrlParam -> T.Text) -> [(UrlParam, UrlParam)] -> T.Text
renderQuery' e = T.intercalate "&" . map q
    where q (n, UUtf8 "") = e n
          q (n,v)  = T.concat [e n, "=", e v]

renderUrlParam (UUtf8 t) = encodeURIComponentT t
renderUrlParam (UJunk j) = T.pack $ encodeURIComponent $ B.unpack j

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

parseQueryStringUtf8Only :: T.Text -> [(T.Text, T.Text)]
parseQueryStringUtf8Only = utf8Only . parseQueryString

parseQueryString :: T.Text -> [(UrlParam, UrlParam)]
parseQueryString qs =
--    q <- liftM (N.parseQuery . C.queryString) $ C.parseUrl $ "http://example.com/asdf?" ++ T.unpack qs
--  TODO: ^ в скомпилированном режиме закомментированный код вызывает sigsegv на "url=%s"
    [ (urlParam n, urlParam $ fromMaybe "" v)
    | (n,v) <- N.parseQuery $ T.encodeUtf8 qs ]

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
urlQueryString u = case C.parseRequest $ T.unpack u of
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
urlAddParamT u p =
    u <> (if T.any (== '?') u then "&" else "?") <> p

urlAddParamsT [] u = u
urlAddParamsT p u =
    T.concat [ u0
             , if "?" `T.isInfixOf` u0 then "&" else "?"
               -- если /asdf? то будет /asdf?& ну и ладно
             , renderQuery [(UUtf8 n, UUtf8 v) | (n,v) <- p]
             , f ]
    where (u0, f) = T.break (== '#') u

urlAddOrReplaceParam n v u =
    T.concat [u0, "?", renderQuery $ go $ parseQueryString q, f]
    where (uq, f) = T.break (== '#') u
          (u0, q) = T.break (`elem` ['?', '&']) uq
          nv = (UUtf8 n, UUtf8 v)
          go [] = [nv]
          go ((UUtf8 n0, _):xs)
             | n0 == n = nv : filter ((/= UUtf8 n) . fst) xs
          go (x:xs) = x : go xs


httpToHttps x
    | Just s <- T.stripPrefix "http://" x = T.append "https://" s
    | otherwise = x

rmHttpPrefix x = fromMaybe x $ rmHttpPrefix' x

rmHttpPrefix' x =
    T.stripPrefix "http://" x <|> T.stripPrefix "https://" x

isHttp = isJust . rmHttpPrefix'

rmWwwPrefix u = fromMaybe u $ T.stripPrefix "www." u


testDomains :: [THostName] -> T.Text -> Bool
testDomains domains = isJust . matchDomains domains (\ _ _ -> ())

pathGet :: THostName -> String -> TURL -> Maybe (TURL, [[T.Text]])
pathGet hn re = pathGet' [hn] re

pathGet' :: [THostName] -> String -> TURL -> Maybe (TURL, [[T.Text]])
pathGet' hn re = matchDomains hn $ \ prefix path -> (prefix, regexGet regex path)
    where regex = fromString ('^':re)

matchDomains :: [THostName] -> (T.Text -> T.Text -> a) -> T.Text -> Maybe a
matchDomains domains f = matchDomains' $ map (, f) domains

matchDomains' :: [(THostName, T.Text -> T.Text -> a)] -> T.Text -> Maybe a
matchDomains' domains = \ url@(T.Text a o0 l0) ->
    case rmHttpPrefix' url of
        Just d
            | domain@(T.Text _ od l) <-
                T.takeWhile (\ c -> c /= '/' && c /= '?' && c /= '#') d
            , l /= 0 && l < 255 ->
                let lp = l + od - o0
                    go [] = Nothing
                    go ((d@(T.Text _ _ ld), f):cs)
                        | d `T.isSuffixOf` domain
                        , l == ld
                          || A.unsafeIndex a (od + l - ld - 1)
                             == toEnum (ord '.')
                        =
                            Just $ f (T.Text a o0 lp) (T.Text a (o0+lp) (l0 - lp))
                        | otherwise = go cs
                in
                    go domains
        _ -> Nothing


metaRedirectUrl baseUrl atts
    | Just (T.toLower -> "refresh") <- lookup "http-equiv" atts
    , Just content <- lookup "content" atts
    , "0;" `T.isPrefixOf` content
      -- только прямые redirect-ы. refresh через 900/1800 сек
      -- это совсем не то
    , url <- T.dropWhile (/= '=') content
    , T.length url > 1
    = Just $ T.pack $ relUri (T.unpack $ T.tail url) (T.unpack baseUrl)
    | otherwise = Nothing


unEscapeBytes :: String -> String
unEscapeBytes [] = ""
unEscapeBytes s@(c:cs) = case unEscapeByte s of
    Just (byte, rest) -> byte : unEscapeBytes rest
    Nothing -> c : unEscapeBytes cs

unEscapeByte :: String -> Maybe (Char, String)
unEscapeByte ('%':x1:x2:s) | isHexDigit x1 && isHexDigit x2 =
    Just (toEnum $ digitToInt x1 * 16 + digitToInt x2, s)
unEscapeByte _ = Nothing

-- | Maybe (MimeType, Data)
-- data:[<MIME-type>][;charset=<encoding>][;base64],<data>
-- encoding пока не обрабатывается, т.к. эта ф-я используется для двоичных
-- данных.
decodeDataURI :: TURL -> Maybe (B.ByteString, B.ByteString)
decodeDataURI u
    | Just s <- T.stripPrefix "data:" u
    , (fmt, dat) <- T.span (/= ',') s
    , not $ T.null dat
    , b <- tbs $ T.tail dat
    , mime <- T.takeWhile (/= ';') fmt
    , r <- Just . (tbs mime,)
    = if T.isSuffixOf ";base64" fmt then
          either (const Nothing) r $ Base64.decode b
      else
          r $ B.pack $ unEscapeBytes $ B.unpack b
    | otherwise = Nothing
