{-# LANGUAGE BangPatterns, ViewPatterns, RecordWildCards, OverloadedStrings,
             ScopedTypeVariables, TupleSections
  #-}
-- | Извлечение информации о веб-страницах
-- (заголовок, описание, картинки, иконки).
--
-- Содержит код для скачивания с ограничением на частоту запросов
-- по IP-адресу сервера.
--
module PageInfo
    ( readOrDownloadPageInfo, getLinkInfo
    , stripHttpWww
    , getAvatarFromLinkLazy, getAvatarFromLink
    , getFavicon, defaultFeedIcon
    , runLimitedUser, imageExtensionByMagic
    , rateLimitedDownload, rateLimitedDownload', DlResult(..)
    , rateLimitedDownloadWithRedirects, dnsCache
    ) where

import Control.Monad
import Control.Applicative
import Control.Concurrent
import qualified Control.Concurrent.MSem as MSem
import Data.List
import Data.Ord
import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as SB
import Generated.DataTypes
import Lib.UrTime
import Lib.ReadUtils
import Data.Maybe
import URL
import Resolvables
import Generated.RiakIO
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Network.HTTP.Conduit.Downloader
import qualified Network.HTTP.Client as C

import qualified Network.HTTP.Types as N
import qualified Network.Socket as N
import Lib.DnsCache
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Fast
import Lib.Log
import Lib.Regex
import Lib.StringConversion
import System.IO.Unsafe
import qualified Data.Text.Encoding as T
import qualified Data.Text.ICU.Convert as ICU
import Control.Concurrent.Async
import Data.Hashable
import Data.Time (UTCTime(..), Day(..), diffTimeToPicoseconds)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Char
import System.Process
import System.IO
import System.Exit
import System.Info
import Text.Printf
import Config (mediaProxyDownloaderSettings, addProxy, proxiedDomain, proxiedFeedDomain, proxiedRedirectWhitelist)
import Preprocess (firstWordsX)

riakSem :: MSem.MSem Int
riakSem = unsafePerformIO $ MSem.new 4
{-# NOINLINE riakSem #-}
-- т.к. getLinkInfo запускается параллельно кучей async-ов,
-- может появиться большое число одновременных соединений к Riak
-- и connection reset by peer
-- (хотя это и лечится настройкой pb_backlog, но riakPool тоже не стоит
-- забивать hot link-ами)

ipLimits :: MVar (Map.Map N.HostAddress (MVar (), MSem.MSem Int))
ipLimits = unsafePerformIO $ newMVar Map.empty
{-# NOINLINE ipLimits #-}

-- для фоновых обновлений (crawler/hotlinks) отдельные замедленные лимиты
bgIpLimits :: MVar (Map.Map N.HostAddress (MVar (), MSem.MSem Int))
bgIpLimits = unsafePerformIO $ newMVar Map.empty
{-# NOINLINE bgIpLimits #-}
-- TODO: по хорошему, надо делать размер map-ки не более 1000
-- и очереди на скачивание не более, к примеру, 10 на домен.
-- с другой стороны, у нас нет такой нагрузки, пусть качает сколько влезет.
-- У парсеров более 40 секунд (на blogspot) задержек нет.

-- url, use proxy, options, cookies
type UrlLimitsKey = (TURL, Bool, DownloadOptions, C.CookieJar)

urlLimits :: MVar (HM.HashMap UrlLimitsKey (MVar DlResult))
urlLimits = unsafePerformIO $ newMVar HM.empty
{-# NOINLINE urlLimits #-}

instance Hashable C.Cookie where
    hashWithSalt s (C.Cookie {..}) =
        s `hashWithSalt` cookie_name
          `hashWithSalt` cookie_value
          `hashWithSalt` cookie_expiry_time
          `hashWithSalt` cookie_domain
          `hashWithSalt` cookie_path
          `hashWithSalt` cookie_creation_time
          `hashWithSalt` cookie_last_access_time
          `hashWithSalt` cookie_persistent
          `hashWithSalt` cookie_host_only
          `hashWithSalt` cookie_secure_only
          `hashWithSalt` cookie_http_only
instance Hashable UTCTime where
    hashWithSalt s (UTCTime {..}) =
        s `hashWithSalt` toModifiedJulianDay utctDay
          `hashWithSalt` diffTimeToPicoseconds utctDayTime
instance Hashable C.CookieJar where
    hashWithSalt s j = s `hashWithSalt` C.destroyCookieJar j

instance Eq C.CookieJar where
    (==) = C.equalCookieJar

dnsCache = unsafePerformIO $ do
    var <- newEmptyMVar
    forkIO $ withDnsCache $ \ c -> do
        putMVar var c
        forever $ threadDelay $ 10*1000*1000
    takeMVar var
{-# NOINLINE dnsCache #-}

cd :: Downloader
cd = unsafePerformIO $ newDownloader mediaProxyDownloaderSettings
{-# NOINLINE cd #-}

data DlResult
    = Ok B.ByteString DownloadOptions RawDownloadResult
    | Err T.Text (Maybe RawDownloadResult)
    | NotModified
    | Redirect C.CookieJar TURL RawDownloadResult
    deriving Show

err e = return $ Err e Nothing
english cookieJar r =
    return $ r { C.requestHeaders =
                     ("Accept-Language", "en-US, en;q=0.9, *;q=0.5")
                     : C.requestHeaders r
               , C.cookieJar = Just cookieJar
               }

rateLimitedDownload bg proxy url = do
    r <- rateLimitedDownload' bg proxy url []
    return $ case r of
        Ok d o _ -> Right d
        Err e _ -> Left e
        NotModified -> Left "Not modified?"
        Redirect _ r _ -> Left $ "Redirect? " <> r
rateLimitedDownload' bg proxy url opts =
    dedupeDL bg proxy (C.createCookieJar []) opts url

dedupeDL bg proxy cookieJar opts url
    | Just (mime, d) <- decodeDataURI url =
        return $ Ok d [] $
        RawDownloadResult
        { rdrStatus = N.ok200
        , rdrHttpVersion = N.http10
        , rdrHeaders = [("Content-Type", mime)]
        , rdrBody = d
        , rdrCookieJar = cookieJar
        }
    | otherwise =
        dedupeDownloads (url, proxy, opts, cookieJar)
            $ dl bg proxy cookieJar opts url

rateLimitedDownloadWithRedirects l bg maxRedirects = go emptyJar 0
    where emptyJar = C.createCookieJar []
          go cookieJar n url = do
              let p = proxiedDomain url
              logLT l $ T.concat
                  ["Downloading ", if p then "(proxied) " else "", url]
              r <- logTime l "dl" $ dedupeDL bg p cookieJar [] url
              -- не используем проксирование при первом скачивании
              case r of
                  Ok _ _ rdr -> return $ Right (url, rdr)
                  Err e _ -> err e
                  NotModified -> err "Not modified?"
                  Redirect cj r _
                      | n >= maxRedirects -> err "Too many redirects"
                      | otherwise -> go cj (n+1) r
          err = return . Left

-- | Скачивание с ограничением частоты запросов по IP сервера
dl :: Bool -> Bool -> C.CookieJar -> DownloadOptions -> TURL -> IO DlResult
dl background proxy cookieJar opts url = do
    let hn = hostNameFromUrl $ T.unpack url
    mbip <- resolveA dnsCache hn
    case mbip of
        Left e -> err $ T.concat
            ["Can’t resolve “", humanReadableHostName (T.pack hn), "”: ", e]
        Right ip -> do
            (mv, sem) <- modifyMVar limits $ \ l ->
                case Map.lookup ip l of
                    Just v -> return (l,v)
                    Nothing -> do
                        mvs <- (,) <$>
                            newMVar () <*> MSem.new maxParallelDownloads
                        return (Map.insert ip mvs l, mvs)
            r <- MSem.with sem $ do
                when (not $ any nonFirstRange opts) $ do
                    -- не делаем дополнительных задержек для следующих slice,
                    -- а то долго грузятся видео с твиттера
                    rateLimit <- newEmptyMVar
                    forkIO $ withMVar mv $ \ _ -> do
                        putMVar rateLimit ()
                        threadDelay delay
                    takeMVar rateLimit
                rawDownload
                    ((if proxy then addProxy Nothing else return)
                     <=< english cookieJar)
                    cd (T.unpack url) (Just ip) opts
            t <- getUrTime
            case r of
                (DROK d opts', Just rdr)
                    | Just r <- findMetaRedirect $ parseTagsT d ->
                        return $ Redirect (rdrCookieJar rdr) r
                            $ rdr { rdrStatus = N.found302
                                  , rdrHeaders =
                                      ("Location", tbs r)
                                      : filter ((/= "Location") . fst)
                                          (rdrHeaders rdr) }
                    | otherwise ->
                        return $ Ok d opts' rdr
                (DRRedirect url', Just rdr) ->
                    return $ Redirect (rdrCookieJar rdr) (T.pack url') rdr
                (DRNotModified, _)
                    | opts /= [] -> return NotModified
                    | otherwise -> err "Not modified?"
                (DRError e, rdr) -> return $ Err (T.pack e) rdr
                (_, Nothing) -> err "No RawDownloadResult?"
    where (limits, delay, maxParallelDownloads)
              | background = (bgIpLimits, 1500000, 1)
              | otherwise  = (ipLimits, 500000, 5)
          nonFirstRange o
              | Just b <- stripPrefix "range: bytes=" o
              = not $ isPrefixOf "0-" b
              | otherwise = False
          findMetaRedirect [] = Nothing
          findMetaRedirect (TagOpen "meta" (metaRedirectUrl url -> Just u) : _)
              = Just u
          findMetaRedirect (_ : ts) = findMetaRedirect ts

-- | Выполняет только одно одновременное скачивание для заданного url
dedupeDownloads :: (TURL, Bool, DownloadOptions, C.CookieJar)
    -> IO DlResult -> IO DlResult
dedupeDownloads key action =
    join $ modifyMVar urlLimits $ \ l ->
        case HM.lookup key l of
            Just v -> return (l, readMVar v)
            Nothing -> do
                mv <- newEmptyMVar
                let put r = do
                         modifyMVar_ urlLimits $ \ l -> do
                             putMVar mv r
                             return $ HM.delete key l
                return (HM.insert key mv l,
                        do r <- action
                           put r
                           return r
                        `E.catch` \ (e :: E.SomeException) -> do
                           put $ error $ show e
                           E.throwIO e
                       )

-- | Чтение закешированной pageInfo или скачивание не существующей
-- или просроченной
readOrDownloadPageInfo :: Bool -> TURL -> IO PageInfo
readOrDownloadPageInfo bg = force <=< readOrDownloadPageInfo' bg False

force (Read r) = return r
force (Download d) = d

data RD a
    = Read a
    | Download (IO a)

instance Show a => Show (RD a) where
    show (Read a) = "Read (" ++ show a ++ ")"
    show (Download d) = "Download <<IO a>>"

-- | Хак, чтобы можно было Download-ы склеивать через обычный монадный
-- синтаксис, добавив RDM к ф-ии, возвращающей RD.
-- Используется в getLinkInfo', который может получить детали сразу из кеша,
-- а может и вернуть ф-ю, скачивающую данные.
newtype RDM a = RDM { unRDM :: IO (RD a) }

instance Functor RDM where
    fmap = liftM
instance Applicative RDM where
    pure = return
    (<*>) = ap
instance Monad RDM where
    return = RDM . return . Read
    RDM x >>= f = RDM $ do
        rd <- x
        case rd of
            Read r -> unRDM $ f r
                -- сразу идем на следующий, возможно все будут read до конца
            Download d -> return $ Download $ do
                -- возвращаем вычисление результата
                x <- d
                force =<< unRDM (f x)

readOrDownloadPageInfo' :: Bool -> Bool -> TURL -> IO (RD PageInfo)
readOrDownloadPageInfo' bg =
    readOrDownloadPageInfo_go bg (C.createCookieJar []) []

piError' p
    | piError p == piError (defaultPageInfo "") = Nothing
      -- был баг, и piError по-умолчанию не заменялся на Nothing
    | otherwise = piError p

readOrDownloadPageInfo_go :: Bool -> C.CookieJar -> [TURL] -> Bool -> TURL -> IO (RD PageInfo)
readOrDownloadPageInfo_go _ _ parents _ url
    | any (`T.isPrefixOf` url)
      [ "https://www.facebook.com/login.php"
        -- страница с facebook может перенаправится
        -- на логин, ну ее нафиг
      , "https://www.tumblr.com/login"
      , "https://accounts.google.com/ServiceLogin" ]
      || not ("http://" `T.isPrefixOf` url || "https://" `T.isPrefixOf` url)
    = return $ Read $ (defaultPageInfo url)
      { piError = Just (UrTime 0 0, "url cancelled") }
readOrDownloadPageInfo_go bg cookieJar parents favicon url0 = do
    t <- getUrTime
    r <- if debug then return Nothing else
        MSem.with riakSem $ cachedNothingReadPageInfo url
    case r of
        Nothing -> download []
        Just pi
            | Just (et, _) <- piError' pi
            , not favicon && diffUrTime t et < 3600 -> do
                ret pi -- не пытаемся перевыкачивать свежую ошибку
            | not favicon && diffUrTime t (timeOrErrorTime pi) > week ->
                download (piRedownloadOptions pi)
            | diffUrTime t (timeOrErrorTime pi)
              > (if favicon then 3 else 1)*day -> do
                forkIO $ void $ force =<< download (piRedownloadOptions pi)
                ret pi -- возвращаем, но фоном перевыкачиваем
            | otherwise -> do
                -- logS "cached"
                ret pi
    where timeOrErrorTime pi
              | Just (et,_) <- piError' pi = et
              | otherwise = piFetchTime pi
          url = normURL url0
          -- нормализуем URL, чтобы фрагменты не качать
          fix pi = if url == url0 then pi else pi { piUrl = url0 }
          ret = return . Read . fix
          download opts = return $ Download $ fmap fix $ do
              r <- dedupeDL bg (proxiedFeedDomain url) cookieJar
                  (map T.unpack opts) url
              t <- getUrTime
              let modRet f =
                      modifyPageInfo' url $ \ pi0 -> return $
                          let pi' = f pi0 in (pi', pi')
                  err e =
                      modRet $ \ pi ->
                          let pi' | piErrorsCount pi > 10
                                  = defaultPageInfo url
                                  -- сбрасываем данные после большого
                                  -- кол-ва ошибок
                                  | otherwise = pi
                          in
                              pi' { piError = Just (t, e)
                                  , piErrorsCount = piErrorsCount pi' + 1 }
                  redirect cookieJar' (normURL -> r)
--                       | r `elem` (url:parents) ->
--                           err $ T.pack $
--                               "Redirect loop" ++ show (r, url:parents)
                        -- не надо проверять, т.к. иногда надо два раза скачать
                      | length parents > 4 =
                          err $ T.pack $ "Too many redirects " ++ show parents
                      | otherwise = do
                          pi <- force =<< readOrDownloadPageInfo_go bg
                              cookieJar' (url:parents) favicon r
                          modRet $ const $
                              pi
                              { piUrl = url
                              , piRedownloadOptions = []
                              , piRedirectUrl = piRedirectUrl pi <|> Just r
                              }
                  ok d opts rdr = do
--                      when debug $ print [t | t@(TagOpen {}) <- parseTagsT d]
                      case parsePageInfo t rdr url d opts of
                          Left redir -> redirect (rdrCookieJar rdr) redir
                          Right pi -> modRet $ const pi
              case r of
                  Ok d opts rdr -> ok d opts rdr
                  Err "Too much data" (Just rdr) ->
                      ok "" [] rdr
                      -- для Too much data сохраняем content-length/content-type
                  Err e _ ->
                      err e
                  NotModified ->
                      modRet $ \ pi -> pi { piFetchTime = t }
                  Redirect c u _ -> redirect c u

-- | Скачивание иконки, находящейся по заданному url и конвертация ее в PNG.
-- Определение URL-а, где находится иконка -- отдельная задача
readOrDownloadFavicon' :: Bool -> TURL -> IO (RD Favicon)
readOrDownloadFavicon' bg =
    readOrDownloadFavicon_go bg (C.createCookieJar []) []

-- код почти один в один с readOrDownloadPageInfo_go
readOrDownloadFavicon_go
    :: Bool -> C.CookieJar -> [TURL] -> TURL -> IO (RD Favicon)
readOrDownloadFavicon_go bg cookieJar parents url = do
    dbg $ print url
    t <- getUrTime
    r <- if debug then return Nothing else
        MSem.with riakSem $ cachedNothingReadFavicon url
    case r of
        Nothing -> download []
        Just pi
            | diffUrTime t (timeOrErrorTime (faviconFetchTime pi)
                                            (faviconFile pi)) > 3*day -> do
                forkIO $ void $ force =<< download (faviconRedownloadOptions pi)
                ret pi -- возвращаем, но фоном перевыкачиваем
            | otherwise -> do
                -- logS "cached"
                ret pi
    where timeOrErrorTime _ (Left (et,_)) = et
          timeOrErrorTime t _ = t
          ret = return . Read
          download (map T.unpack -> opts) = return $ Download $ do
              r <- dedupeDL bg (proxiedFeedDomain url) cookieJar opts url
              t <- getUrTime
              let modRet f =
                      modifyFavicon' url $ \ pi0 -> return $
                          let pi' = f pi0 in (pi', pi')
                  err e = do
                      dbg $ logT e
                      modRet $ \ fi ->
                          if faviconErrorsCount fi > 10 then
                              (defaultFavicon url)
                              { faviconFile = Left (t, e)
                              , faviconErrorsCount = 1 }
                              -- сбрасываем данные и ставим ошибку
                          else
                              fi { faviconFile =
                                       either (const $ Left (t,e)) Right $
                                       faviconFile fi
                                       -- обновляем только, если и так ошибка
                                       -- была, имеющуюся иконку не сносим
                                 , faviconErrorsCount =
                                       faviconErrorsCount fi + 1 }
                  redirect cookieJar' (normURL -> r)
--                       | r `elem` (url:parents) ->
--                           err $ T.pack $
--                               "Redirect loop" ++ show (r, url:parents)
                        -- не надо проверять, т.к. иногда надо два раза скачать
                      | length parents > 4 =
                          err $ T.pack $ "Too many redirects " ++ show parents
                      | otherwise = do
                          fi <- force =<< readOrDownloadFavicon_go bg
                              cookieJar' (url:parents) r
                          modRet $ const $
                              fi
                              { faviconSourceUrl = url
                              , faviconRedownloadOptions = []
                              , faviconRedirectUrl =
                                  faviconRedirectUrl fi <|> Just r
                              }
              case r of
                  Ok d opts rdr ->
                      either err (modRet . const)
                          =<< processFavicon url t d opts rdr
                  Err e _ ->
                      err e
                  NotModified ->
                      modRet $ \ pi -> pi { faviconFetchTime = t }
                  Redirect c u _ -> redirect c u

ctExt =
    [ ("image/x-icon", "ico") -- предпочитаем более короткий вариант
    , ("image/vnd.microsoft.icon", "ico")
    , ("image/png", "png")
    , ("image/gif", "gif")
    , ("image/jpeg", "jpg")
    , ("image/svg+xml", "svg")
    , ("image/x-bmp", "bmp")
    , ("image/bmp", "bmp")
    ]

extensionByContentType :: B.ByteString -> String
extensionByContentType ct = fromMaybe "ico" $ lookup (B.map toLower ct) ctExt

contentTypeByExtension :: String -> B.ByteString
contentTypeByExtension ext =
    maybe "image/x-icon" fst $ find ((== ext) . snd) ctExt

debug = False
dbg act
    | debug = act
    | otherwise = return ()

runLimitedUser cmd args d
    | os == "darwin" = run cmd args d
    | otherwise =
        run "sudo" ([ "-H", "-u", "imagemagick", cmd ] ++ args) d

imageExtensionByMagic d
    | B.isPrefixOf "\x89PNG\x0D\x0A\x1A\x0A" d
      && suffix "\x49\x45\x4E\x44\xAE\x42\x60\x82"
    = Just "png"
    | (B.isPrefixOf "GIF89a" d || B.isPrefixOf "GIF87a" d)
      && suffix "\x00\x3B"
    = Just "gif"
    | B.isPrefixOf "BM" d = Just "bmp"
    | B.isPrefixOf "\xFF\xD8" d && suffix "\xFF\xD9" = Just "jpg"
    | B.isPrefixOf "\x00\x00\x01\x00" d = Just "ico"
    | B.isPrefixOf "\x00\x00\x02\x00" d = Just "cur"
    | B.isPrefixOf "RIFF" d && B.isPrefixOf "WEBP" (B.drop 8 d) = Just "webp"
    | B.isPrefixOf "<svg" d = Just "svg"
    | B.isPrefixOf "<?xml" d && B.isInfixOf "<svg" d = Just "svg"
      -- после начала тега может быть tab/newline, так что пробел не ищем
    | otherwise = Nothing
    where suffix s = B.isSuffixOf s $ fst $ B.spanEnd asciiWhitespace d
    -- бывают gif с \n\n в конце, или jpg с пробелами в конце,
    -- поэтому удаляем их перед проверкой суффикса

processFavicon url t d opts (rdrHeaders -> headers)
    | B.length d == 0 = return $ Left "empty file"
    | otherwise = do
        let ext = imageExtensionByMagic d
            ct = B.takeWhile (/= ';') <$> lookup "content-type" headers
            ctExt = extensionByContentType <$> ct
            charOrHex c = case init $ tail $ show c of
                s@[_] -> s
                s@['\\',_] -> s  -- \n \r и т.д.
                otherwise -> printf "\\x%02X" (fromEnum c) -- \x89 а не \137
        dbg $ print (ct, ext, ctExt, if ctExt /= ext then "INEQUAL!" else "")
        dbg $ wr ("test-in." ++ fromMaybe "ico" ext) d
        case ext of
            Just s -> getSizeAndConvert t opts url s d
            Nothing ->
                return $ Left $ T.concat
                    [ "Unknown header: "
                    , T.pack $ concatMap charOrHex $ take 10 $ B.unpack d
                    , "… (", maybe "No Content-Type" bst ct, ")" ]

getSizeAndConvert t opts url ext d = do
    i <- if ext == "svg"
    then
        return $ Right ""
        -- identify зависает на некоторых svg и нам он не нужен, т.к. у svg
        -- не надо выбирать подходящий размер
    else
        runLimitedUser "identify"
        [ "-format", "%[scene] %[width] %[height] %k\n", ext ++ ":-" ] d
        -- scene number (0 for default ico)
        -- %k -- number of colors
    case i of
        Right i | Just wh <- parseWH i -> do
            dbg $ mapM_ print $ detailedFirst wh
            fmap (fmap mkFavicon) $ convert ext d wh
        Right i ->
            return $ Left $ T.append "Can’t parse identify output:\n" (bst i)
        Left e ->
            return $ Left e
    where parseWH = mapM parseLine . T.lines . bst
          parseLine (T.words -> [ ri -> Just n, ri -> Just w, ri -> Just h
                                , ri -> Just k]) =
              Just ((w,h),(k,n))
          parseLine _ = Nothing
          ri = tryReadUnsignedInt
          mkFavicon file =
              (defaultFavicon url)
              { faviconFetchTime         = t
              , faviconRedownloadOptions = map T.pack opts
              , faviconFile = Right file }

detailedFirst =
    sortBy $ comparing $ Down . \ ((w,h),(k,n)) -> (w*h,k,-n)
whConvert "svg" _ = ("rsvg-convert", "-w 32 -h 32")
-- флаг -a, --keep-aspect-ratio может увеличить ширину или высоту
whConvert ext scenes = ("convert",) $ other <> ext <> ":- " <> case scenes of
    [] -> png16
    [x] -> convertOne x
    (x:_) | ext == "gif" -> convertOne x -- первый кадр
    _ | Just (_,n32) <- lookup (32,32) df -> scene n32 png32
    (x:_) -> convertOne (head df) -- самый подробный, самый первый
    -- TODO: учитывать число цветов?
    -- иногда gif может быть меньше png (если 16 цветов),
    -- хотя 300 байт или 200 байт уже не так важно
    where other = "-define png:include-chunk=none "
          df = detailedFirst scenes
          convertOne ((w,h),(_,n))
              | w < 32 && h < 32 = scene n png16
              | otherwise = scene n png32
          png16 = resize "16x16" ++ " png:-"
          png32 = resize "32x32" ++ " png:-"
--           ico = "( -clone 0 " ++ resize "16x16" ++ " ) \
--                 \( -clone 0 " ++ resize "32x32" ++ " ) -delete 0 ico:-"
          -- ico в chrome и safari игнорирует 16х16 и использует иконку большего
          -- размера. в firefox используется иконка 16х16, а при увеличении
          -- уже 32х32, но выглядит это так себе (blogspot из угловатой рамки
          -- превращается в округлую).
          -- Т.е. ico в WebKit = 32х32, в FF 16х16 с рывком при масштабировании.
          -- Сервис google показывает не 16х16, а иконку максимального размере,
          -- переведенную в 16х16 (не совсем как у нас, но у нас на retina
          -- должно правильнее выглядеть)
          -- Главное -- нужно, чтобы все выглядело одинаково. А то у retina
          -- одно, у других другое.
          -- --> нафиг ico, оставляем png, если возможно 16х16
          -- -format png -compress zip  -- не помогает, в ico все равно bmp-шки
          resize :: String -> String
          resize x =
              "-resize " ++ x ++ "> " ++
              "-gravity center -background transparent -extent " ++ x
              -- расширяем, если иконка была не квадратная
          scene :: Int -> String -> String
          scene n xs
              | null d = xs
              | otherwise = "-delete " ++ intercalate "," d ++ " " ++ xs
--               = "[" ++ show n ++ "] " ++ xs
              -- на некоторых иконках, нормально обрабатываемых identify,
              -- convert ico:-[scene_number] выдает ошибку InvalidImageIndex.
              -- При этом, если сделать -delete остальным сценам, то всё
              -- работает.
              -- Баг исправлен в ImageMagick 6.9.11 (у нас 6.9.10),
              -- но, на всякий случай, оставляем вариант с -delete
              where d = [show s | (_,(_,s)) <- scenes, s /= n]


--                 convert выдает несколько png-шек для иконки
-- они все пишутся в output и получается большой размер файла и выбирается
-- первая иконка (и ее размер не ужимается до 32х32, т.к. она уж 16х16)
-- кажется, идеальный вариант -- оставить иконки с 16х16 и 32х32, как и есть
-- у microsoft если 32х32 уменьшить, то идет замыливание квадратиков
-- (более светлая рамка), а иконка 16х16 выглядит нормально
-- и, при увеличении масштаба, браузер выбирает более подробную иконку
-- convert ico:- -resize 32x32> -define png:include-chunk=none png:-

wr fn dat = do
    putStrLn $ "writing " ++ fn
    B.writeFile fn dat

convert ext d wh = do
    let (executable, words -> args) = whConvert ext wh
        outExt = "png"
    r <- runLimitedUser executable args d
    dbg $ putStrLn $ unwords $ executable : args
    case r of
        Right out -> do
            dbg $ wr ("test-in." ++ ext) d
            dbg $ wr ("test-out." ++ outExt) out
            let o = SB.toShort out
            if o `elem` emptyIcons then do
                dbg $ putStrLn "Empty icon after convert"
                return $ Left "Empty icon"
            else
                return $ Right (SB.toShort $ contentTypeByExtension outExt, o)
        Left e -> do
            dbg $ logT e
            return $ Left e

run cmd args input = do
    (r,o,e) <- run' cmd args input
       `E.catch` \ (e :: E.SomeException) ->
       return (ExitFailure 1, "", B.pack $ show e)
    return $ case r of
        ExitSuccess -> Right o
        f -> Left $ T.concat [T.pack (show f), ": ", bst e]

run' cmd args input = do
    let cp_opts = (proc cmd args) {
                    std_in  = CreatePipe,
                    std_out = CreatePipe,
                    std_err = CreatePipe
                  }
    (Just inh, Just outh, Just errh, ph) <- createProcess cp_opts
    withAsync (B.hPutStr inh input `E.finally` hClose inh) $ \ put ->
      withAsync (B.hGetContents outh) $ \ out ->
      withAsync (B.hGetContents errh) $ \ err -> do
        o <- wait out
        e <- wait err

        -- wait on the process
        ex <- waitForProcess ph

        return (ex, o, e)


getFavicon' bg (rootPage' . normalizeTURL -> Just u) = do
    pi <- RDM $ readOrDownloadPageInfo' bg True u
    let sizeUrl (typ,(w,ct,u))
            | not (badSize w)
              && typ `elem` [PIICSIcon, PIICSShortcutIcon, PIICSShortcut]
              && T.length u < 4096
              && correctUrl (T.unpack u)
              && T.takeWhile (/= ':') u `elem` ["http", "https", "data"]
            = Just (w, u)
            | otherwise = Nothing
        icons = mapMaybe sizeUrl $ -- sortBy (comparing fst) $
            reverse $ piIcon pi
        -- reverse, чтобы учитывать последнюю иконку заданного типа.
        -- Иногда <link rel="icon" ...> повторяется несколько раз
        fu  | Just x <- lookup [PIS 32 32] icons = x
            | ((_,x):_) <- icons = x
            | otherwise = defaultFu
        defaultFu =
            fromMaybe u (rootPage' =<< piRedirectUrl pi) <> "/favicon.ico"
            -- берем иконку из итогового url (с учетом HTTPS и
            -- прочих перенаправлений)
        badSize [] = False
        badSize x = all badSize' x
        badSize' PISAny = False -- svg обычно
        badSize' (PIS w h) = w > 32 && h > 32 -- apple icon-ки
    f <- RDM $ dbg (mapM_ print $ piIcon pi) >> readOrDownloadFavicon' bg fu
    case faviconFile f of
        -- Chrome не пытается качать иконку по-умолчанию, если та, что указана
        -- на странице битая или не существует (хотя, при обновлении страницы,
        -- Chrome может её на мгновение показать). Так что и мы не будем.
--         Left (_, e) | fu /= defaultFu ->
--             RDM $ do
--                dbg $ logTL ["Downloading ", fu, " failed:\n", e
--                            ,"\nTrying default icon ", defaultFu]
--                readOrDownloadFavicon' bg defaultFu
        _ -> return f
getFavicon' _ u = return
    (defaultFavicon u) { faviconFile = Left (UrTime 0 0, "Invalid url") }

getFavicon bg u = force =<< unRDM (getFavicon' bg u)

defaultFeedIcon :: (SB.ShortByteString, SB.ShortByteString)
defaultFeedIcon = ("image/png", "\137PNG\r\n\SUB\n\NUL\NUL\NUL\rIHDR\NUL\NUL\NUL \NUL\NUL\NUL \b\ETX\NUL\NUL\NULD\164\138\198\NUL\NUL\STX\FSPLTE\255\255\255\184\189\250\183\189\250\183\188\249\158\168\230\157\167\229\155\166\228\183\189\250\154\164\226\183\188\249\153\163\225\158\168\230\DEL\142\205\157\167\229~\142\204\154\164\226\153\163\225\DEL\142\205~\142\204}\141\204\182\187\249\181\187\248\181\186\247\180\186\247\179\185\246\178\184\245\177\183\245\176\183\244\175\182\243\174\181\242\173\180\241\172\179\240\171\178\240\169\177\239\168\176\238\167\175\237\166\174\236\165\173\235\163\172\234\162\171\233\161\170\232\159\169\231\183\188\249\158\168\230\157\167\229\155\166\228\154\164\226\208\212\250\231\233\252\217\220\250\216\219\250\196\201\247\180\187\244\153\163\225\151\162\224\217\220\251\255\255\255\240\241\252\214\217\248\182\188\242\150\161\223\228\230\250\189\195\242\149\160\222\221\224\247\169\177\235\147\159\221\186\192\245\200\205\247\215\218\249\219\222\249\239\241\252\249\250\254\185\191\238\146\158\220\174\182\240\206\211\245\244\245\253\195\201\239\145\157\219\199\204\243\243\245\252\217\221\245\143\156\218\183\189\243\193\198\244\192\197\244\168\176\234\214\218\245\198\203\238\142\155\217\238\240\252\211\215\246\176\183\238\183\190\233\141\154\216\213\217\248\209\213\244\167\175\233\174\183\233\248\249\253\161\171\225\140\153\215\212\216\247\243\244\252\176\184\235\191\198\236\221\225\244\139\152\214\190\196\242\227\230\249\238\240\251\249\249\253\209\214\241\171\181\227\137\151\213\172\180\231\235\237\249\227\230\245\136\150\213\165\174\233\218\222\246\157\167\225\173\182\229\162\172\223\135\149\212\215\219\243\227\230\246\211\216\240\134\148\211\177\185\236\217\220\244\160\170\224\185\193\231\248\248\252\133\147\210\186\194\236\158\168\226\242\243\251\214\218\241\158\169\220\132\146\209\196\202\241\159\169\227\189\196\234\219\223\242\187\195\230\131\146\208\207\212\243\177\185\232\162\173\223\196\203\234\201\208\235\130\145\208\176\185\231\183\191\230\166\176\224\224\228\243\129\144\207\169\178\232\199\206\237\197\204\235\165\176\223\128\143\206\173\181\231\223\226\246\229\232\247\215\220\243\156\166\224\170\180\227\198\205\236\198\204\235\167\177\224\149\161\217\194\201\233\194\201\232\193\201\232\177\186\226\DEL\143\206\DEL\142\205l;\159=\NUL\NUL\NUL\DC4tRNS\NUL\ETX{\222\222{\ETXxx\234\234\234\234\132\132{\225\225{\ETX\tn\237s\NUL\NUL\SOH\239IDAT8\203\173\147W[\DC3Q\DC4E\177P\236\DC4\177\130\ENQ\177\128\DLEL41B\DC4\tR$\152H\181\NULJ\DC3E\163D#DP\164\137\SOH4\160D\132X\130\128b/\224\USt\159=\DC3\130\no\220\151s\190Yk\230\238{f&$d\t\214\178\229+6\198n\218\188e\235\182\237q\241;v\238\218\157\176'q\239\190\253\a\146V\134\134Q\bO^\152\USLI\141\160\176jQ\174YM\SOH<\237\144Vw\248\200\DEL\\o\160\128\251\143\SUBe\GSK\207\248\135\155(\224\249Z\163\186\142\159\248\139gR\192\254:\227\220:\153\&5\143\155) _\246\169\156\220<\213\200?\GS\228\ENQ\DC4\STX\249-\133g\DC4\197:\199m\DC4\230\157\239l\DC1\141\226\NUL/\161\NUL^ZVna\254s\231i\\P\249E\n\184?\GS\ETB+*\171$\191\149\198%\133_\166\128\231W+\155\215\212\"_\157t\245W\200\ESC(`\255\171\234\t\174\&5\"\223u\233n\144\219) \223\205[M\138\225\168C\190\219\210\221\DC1\238\164\160\204?\171&\159\198]\228kF\211\"\220E!0\255{\173\220\189-\211|_\154\a\224\237\DC4\130\239\167X\192C\228\239@\237\EOT\239\162@^\219-\243\237\DC1\227\145\173\164\ETB\229\&1\184\155\130p\204\199\209\167\209\247\SI\128<A~\DC1\159v\185=\DC4\192\a\153o\200`z&)\144\255\&9\234\176\219\227\165\128\253_\240\140#\152o=\234\203\ACK\251(\202\152\199\235\163\128|\138\240\n\243\149x\175\237\206\&7(o\189>?\ENQ\228\USw\224B\179\204\255\GS\154\t\167kR\EOT\159\DEL\138\130|\US\239?L\DEL\252$\239\231\243\151\175\DC3\223\\\237\223\DEL\252\252\&5\227\159\154\165\176&\248}p\254r~\228\195\253\179k)\172[\148\255^O!lC\228\194<*:f)~\237?\146\205\209\&9f\137\169g\NUL\NUL\NUL\NULIEND\174B`\130")

emptyIcons :: [SB.ShortByteString]
emptyIcons =
    [-- Пустой прозрачный PNG 32x32, выдаваемый rsvg-convert,
     -- если ничего не удалось нарисовать
     "\137PNG\r\n\SUB\n\NUL\NUL\NUL\rIHDR\NUL\NUL\NUL \NUL\NUL\NUL \b\ACK\NUL\NUL\NULszz\244\NUL\NUL\NUL\ACKbKGD\NUL\255\NUL\255\NUL\255\160\189\167\147\NUL\NUL\NUL\SUBIDATX\133\237\193\SOH\SOH\NUL\NUL\NUL\130 \255\175nH@\SOH\NUL\NUL\NUL\239\ACK\DLE \NUL\SOHG\SOH\160\136\NUL\NUL\NUL\NULIEND\174B`\130"
     -- Белый PNG 32x32 от rsvg-convert
    ,"\137PNG\r\n\SUB\n\NUL\NUL\NUL\rIHDR\NUL\NUL\NUL \NUL\NUL\NUL \b\STX\NUL\NUL\NUL\252\CAN\237\163\NUL\NUL\NUL\ACKbKGD\NUL\255\NUL\255\NUL\255\160\189\167\147\NUL\NUL\NUL)IDATH\137\237\205\&1\SOH\NUL\NUL\b\195\&0\192\191\231a\STX\190T@\211I\234\179y\189\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\128\195\SYN\199\241\ETX=J)\153-\NUL\NUL\NUL\NULIEND\174B`\130"
     -- Прозрачный PNG 32x32 от convert
    ,"\137PNG\r\n\SUB\n\NUL\NUL\NUL\rIHDR\NUL\NUL\NUL \NUL\NUL\NUL \b\EOT\NUL\NUL\NUL\217s\178\DEL\NUL\NUL\NUL\CANIDATH\199c`\CAN\ENQ\163`\DC4\140\130Q0\nF\193(@\ENQ\NUL\b \NUL\SOHU\136\212\157\NUL\NUL\NUL\NULIEND\174B`\130"
     -- Белый PNG 32x32 от convert
    ,"\137PNG\r\n\SUB\n\NUL\NUL\NUL\rIHDR\NUL\NUL\NUL \NUL\NUL\NUL \SOH\NUL\NUL\NUL\NUL[\SOHGY\NUL\NUL\NUL\SIIDAT\b\215c\248\SI\EOT\f\131\151\NUL\NUL\218\233\DEL\129\151TP\148\NUL\NUL\NUL\NULIEND\174B`\130"
     -- Прозрачный PNG 16x16 от convert
    ,"\137PNG\r\n\SUB\n\NUL\NUL\NUL\rIHDR\NUL\NUL\NUL\DLE\NUL\NUL\NUL\DLE\b\EOT\NUL\NUL\NUL\181\250\&7\234\NUL\NUL\NUL\SOIDAT(\207c`\CAN\ENQ\163\NUL\SOH\NUL\STX\DLE\NUL\SOH\DC4\194\192\146\NUL\NUL\NUL\NULIEND\174B`\130"
     -- Белый PNG 16x16 от convert
    ,"\137PNG\r\n\SUB\n\NUL\NUL\NUL\rIHDR\NUL\NUL\NUL\DLE\NUL\NUL\NUL\DLE\SOH\NUL\NUL\NUL\NUL7\136\194\204\NUL\NUL\NUL\SOIDAT\b\215c\248\255\159\129\DC4\EOT\NUL\253N\US\225s\139\166\172\NUL\NUL\NUL\NULIEND\174B`\130"
    ]

parseIconSizes s
    | T.length s > 1024 = []
    | otherwise =
        Set.toList $ Set.fromList $ mapMaybe size $ T.words $ T.toLower s
    where size "any" = Just PISAny
          size (T.words . T.replace "x" " " ->
                    [ tryReadUnsignedInt -> Just w
                    , tryReadUnsignedInt -> Just h ])
              = Just $ PIS w h
          size _ = Nothing

testPageInfo url = do
    withDownloader $ \ d -> do
        (DROK d _, Just rdr) <- rawDownload return d url Nothing []
        return $ parsePageInfo (UrTime 0 0) rdr (T.pack url) d []

parsePageInfo t (rdrHeaders -> headers) url dat opts = handleHeaders
    where start enc = go enc defaulPi . parseTagsT
          defaulPi =
              (defaultPageInfo url)
              { piFetchTime = t
              , piError = Nothing
              , piRedownloadOptions = map T.pack opts
              , piContentType = fmap bst $ lookup "content-type" headers
              , piContentLength =
                  tryReadUnsignedInt . bst =<< lookup "content-length" headers
                  -- если не HEAD, то можно размер dat брать
              }
          handleHeaders
              | Just (bst -> c) <- lookup "content-type" headers
                -- text/html,application/xhtml+xml
              , not ("html" `T.isInfixOf` c) && T.strip c /= "" =
                  Right defaulPi
                  -- не html
              | Just (bst -> c) <- lookup "content-type" headers
              , [[_,enc]] <- regexGet "charset=([^ ;]+)" c =
                  handleCharset enc Nothing defaulPi (parseTagsT dat)
              | otherwise =
                  start Nothing dat
          image typ href e pi xs
              | T.length href < 1024 =
                  go e (pi { piImage = (typ, fromRelUrl href) : piImage pi }) xs
              | otherwise = go e pi xs
          imageBody typ href e pi xs
              | T.length href < 1024 =
                  body e (pi { piImage = (typ, fromRelUrl href) : piImage pi }) xs
              | otherwise = body e pi xs
          title typ t e pi xs =
              go e (pi { piTitle = (typ, T.take 1000 t) : piTitle pi }) xs
--           robots typ r e pi xs =
--               go e (pi { piRobots = (typ, r) : piRobots pi }) xs
          description typ d e pi xs =
              go e (pi { piDescription =
                             (typ, T.take 10000 d) : piDescription pi }) xs
          handleCharset enc enc0 pi xs
              | isJust enc0 = go enc0 pi xs
              | T.toLower enc `elem` ["utf-8", "utf8"] =
                  go (Just enc) pi xs
              | otherwise =
                  start (Just enc) (toUnicode enc dat)
          go _ pi [] =
              Right $ pi
              { piTitle = reverse $ piTitle pi
              , piDescription = reverse $ piDescription pi
              , piImage = reverse $ piImage pi
              , piIcon = reverse $ piIcon pi
--              , piRobots = reverse $ piRobots pi
              }
          go e pi (TagOpen "title" _ : TagText t : TagClose "title" : xs) =
              title PITSTag t e pi xs
          go e pi (TagOpen "link" a : xs)
              | Just href <- lookup "href" a
              , T.length href < 1024
              , Just (T.toLower -> rel) <- lookup "rel" a =
                  let iconTypes =
                          [ ("icon", PIICSIcon)
                          , ("shortcut icon", PIICSShortcutIcon)
                          , ("shortcut", PIICSShortcut)
                          , ("mask-icon", PIICSIconMask)
                          , ("apple-touch-icon", PIICSAppleTouchIcon)
                          , ("apple-touch-icon-precomposed"
                            , PIICSAppleTouchIconPrecomposed)]
                  in
                  case lookup rel iconTypes of
                      Just typ ->
                          go e (pi { piIcon =
                                         (if isJust $ lookup "mask" a then
                                              PIICSIconMask
                                              -- черно-белая svg маска
                                          else
                                              typ
                                         , ( maybe [] parseIconSizes $
                                                 lookup "sizes" a
                                               , lookup "type" a
                                               , fromRelUrl href))
                                         : piIcon pi }) xs
                      _ | rel == "image_src" ->
                            image PIISLinkRelImageSrc href e pi xs
                      _ ->
                          go e pi xs
          go e pi (TagOpen "meta" a@(lookup "charset" -> Just enc) : xs) =
              handleCharset enc e pi xs
              -- на http://bash.im <meta charset=..>
          go e pi (TagOpen "meta" a@(lookup "content" -> Just (T.strip -> c)) : xs)
              | Just (T.toLower -> "content-type") <- lookup "http-equiv" a
              , [[_,enc]] <- regexGet "charset=([^ ;]+)" c =
                  handleCharset enc e pi xs
              | Just (T.toLower -> "refresh") <- lookup "http-equiv" a
              , readUnsignedInt c < 10
              -- только прямые redirect-ы. refresh через 900/1800 сек
              -- это автообновление страницы
              , rurl <- T.dropWhile (/= '=') c
              , T.length rurl > 0
              , r <- fromRelUrl $ T.tail rurl
              , r /= url =
                  Left r
              | Just (T.toLower -> ip) <- lookup "itemprop" a = case ip of
                  "name" -> title PITSItempropName c e pi xs
                  "image" -> image PIISItemprop c e pi xs
                  "description" -> description PIDSItemprop c e pi xs
                  _ -> go e pi xs
              | Just (T.toLower -> name) <- lookup "name" a = case name of
                  "description" -> description PIDSName c e pi xs
                  "twitter:description" -> description PIDSTwitter c e pi xs
                  "twitter:title" -> title PITSTwitter c e pi xs
                  "twitter:image:src" -> image PIISTwitter c e pi xs
--                   "robots" -> robots name c pi xs
--                   "googlebot" -> robots name c pi xs
--                   "bingbot" -> robots name c pi xs
--                   "teoma" -> robots name c pi xs
                  -- https://support.google.com/webmasters/answer/79812?hl=en
                  -- https://developers.google.com/webmasters/control-crawl-index/docs/robots_meta_tag?csw=1
                  -- http://w3guy.com/noodp-noarchive-nosnippet-noydir-meta-tag/
                  -- X-Robots-Tag надо обрабатывать, чтобы noindex, nosnippet
                  -- В принципе, но snippet означает только то, чтобы
                  -- не отображались описания в поиске.
                  -- Если есть og:… то почему бы его не отобразить
                  -- Facebook отображает
--                  "twitter:url"
                  _ -> go e pi xs
              | Just property <- lookup "property" a = case property of
                  "og:title" -> title PITSOpenGraph c e pi xs
                  "og:image" -> image PIISOpenGraph c e pi xs
                  "og:description" -> description PIDSOpenGraph c e pi xs
                  _ -> go e pi xs
          go e pi (TagClose "head" : xs)
              | "livejournal.com" `T.isInfixOf` url = body e pi xs
              | otherwise = go e pi xs
              -- YouTube содержит <meta> и <title> в <body>, а не в <head>
              -- так что сканируем <body> тоже
          go e pi (_:xs) = go e pi xs
          body e pi (TagOpen "img" a : xs)
              | Just "user_pic" <- lookup "class" a
              , Just s <- lookup "src" a
              , "userpic.livejournal.com/" `T.isInfixOf` s
              = imageBody PIISUserPic s e pi xs
          -- <img src='http://l-userpic.livejournal.com/113213537/670251' width='' height='' border='0' class='user_pic' alt='Юзерпик'/>
          -- а бывает пустая картинка
          -- http://l-stat.livejournal.net/img/profile_icons/user.gif?v=14273
          body e pi (_:xs) = body e pi xs
          body e pi [] = go e pi []

          fromRelUrl r = T.pack $ relUri (T.unpack r) (T.unpack url)

toUnicode encoding s
    | Right _ <- T.decodeUtf8' s = s
      -- first of all we try decode utf-8.
      -- Some sites specify non utf-8 encoding, while the text is in utf.
    | otherwise =
        unsafePerformIO $
        (do -- print enc
            c <- ICU.open (T.unpack encoding) Nothing
            return $! T.encodeUtf8 $ ICU.toUnicode c s)
        `E.catch` c
        where c :: E.SomeException -> IO B.ByteString
              c e = do
                  return s

-- вырезаем фрагмент и последний /?
normURL = normalizeRoot . T.takeWhile (/= '#')
normalizeRoot u
    | [[_, root]] <- regexGet "^(https?://[^/?#]+)/?\\??$" u = root
    | otherwise = u
isIndexPage = regexTest "^https?://[^/?#]+(/|/index.[a-z0-9]+)?$"
rootPage' u
    | [[root]] <- regexGet "^https?://[^/?#]+" u = Just root
    | otherwise = Nothing
rootPage u
    | not (isIndexPage u) = rootPage' u
    | otherwise = Nothing
topRootPage u
    | [[_, prefix, host]] <- regexGet "^(https?://)([^/?#]+)" u
    = Just $ T.append prefix $
      if "www." `T.isPrefixOf` host then host else topLevelHostName host
    | otherwise = Nothing
-- insWWW u
--     | [[_, prefix, host]] <- regexGet "(https?://)([^/]+)" u
--     , not ("www." `T.isPrefixOf` host) = T.concat [prefix, "www.", host]
--     | otherwise = u

norm stopWords =
    filter (not . (`HS.member` stopWords)) .
    T.words . T.map (\ c -> if isAlpha c then toLower c else ' ')
normDescription = norm descriptionStopWords

ignoredDescription d = length (normDescription d) <= 3

ignoredTitle t = length (norm titleStopWords t) == 0

titleStopWords =
    HS.fromList
    [ "", "title", "metadata", "ogtitle" ]

descriptionStopWords =
    HS.fromList
    [ ""
    , "default", "metadata", "description", "ogdescription"
    , "visit", "the", "post", "for", "more", "read", "a", "on", "view"
    , "here", "home", "null"
    , "nbsp", "google", "facebook", "twitter" ]

stripHttpWww u
    | ftrPrefix `T.isPrefixOf` u =
        stripHttpWww $
        decodeURIComponentT $ T.drop (T.length ftrPrefix) u
    | otherwise =
        fromMaybe u $
        T.stripPrefix "https://www." u <|>
        T.stripPrefix "http://www." u <|>
        T.stripPrefix "https://" u <|>
        T.stripPrefix "http://" u
--     | otherwise = regexReplace "^https?://(www\\.)?" "" u
    where ftrPrefix = "http://ftr.fivefilters.org/makefulltextfeed.php?url="


getLinkInfo bg u = force =<< unRDM (getLinkInfo' bg u)

getLinkInfo' bg url0 = do
--    RDM $ print url0 >> return (Read ())
    let url = normURL url0
    pi <- RDM $ readOrDownloadPageInfo' bg False url

    let rurl = piRedirectUrl pi -- и так нормализован
        parents
            | null (piImage pi) && null (piDescription pi) = []
              -- нет смысла качать parent-ов,
              -- если нечего проверять на дублирование
            | otherwise =
                nub $ filter (/= url) $ catMaybes
                [ rootPage url, topRootPage url
                , rootPage =<< rurl, topRootPage =<< rurl ]

    ps <- mapM (RDM . readOrDownloadPageInfo' bg False) parents

    let tryLookup (x:xs) l = lookup x l <|> tryLookup xs l
        tryLookup [] [] = Nothing
        tryLookup [] ((_,v):_) = Just v
        checkIgnoredDescription (t, d)
            | any (`T.isInfixOf` url)
              [ "fotki.yandex.ru"
              , "reddit.com"
              -- 0 points and 0 comments so far on reddit
              , "flickr.com"
              -- Explore pilllpat (agence eureka)'s photos on Flickr. pilllpat (agence eureka) has uploaded 44352 photos to Flickr.
              , "maps.google.com"
              , "cheezburger.com", "imagetwist.com"
              ] = Nothing
            | ignoredDescription d = Nothing
            | otherwise = Just (t, d)
        checkIgnoredImage (t, i)
            | "/blank." `T.isInfixOf` i ||
              any (`T.isSuffixOf` i)
              [ "/null" -- bloomberg
              , "logo-meta.png"
                -- l-stat.livejournal.net/img/schemius/logo-meta.png
              , "text_200.png"
                -- assets.tumblr.com/images/og/text_200.png
              , "/icon-stub.png"
              ] = Nothing
        checkIgnoredImage ti = Just ti
        parentDescrs =
            concatMap (map (normDescription . snd) . piTitle) (pi:ps) ++
            concatMap (map (normDescription . snd) . piDescription) ps
        parentImages =
            concatMap (map snd . piImage) ps
        images =
            filter ((`notElem` parentImages) . snd) $ piImage pi
        parentIcons =
            concatMap (map iconUrl . piIcon) ps
        icons =
            filter ((`notElem` parentIcons) . iconUrl) $ piIcon pi
        title
            | Just t0 <-
                tryLookup [PITSOpenGraph, PITSTwitter] $
                filter (not . ignoredTitle . snd) (piTitle pi)
            , t <- T.unwords $ T.words t0
            , t /= "" = t
            | imageLink = fileNameFromUrl url
            | otherwise = stripHttpWww url
        image
            | imageLink = Just url
            | otherwise =
                tryLookup [PIISOpenGraph, PIISTwitter] $
                mapMaybe checkIgnoredImage images
        imageLink
            | Just ct <- piContentType pi = T.isPrefixOf "image/" ct
            | otherwise = False
        fileNameFromUrl =
            T.reverse . T.takeWhile (/= '/') . T.reverse .
            T.takeWhile (`notElem` ("?#" :: [Char]))
        avatar
            | Just i <- findAvatarUrl $ map iconUrl icons = Just i
            | Just i <- findAvatarUrl $ map snd images = Just i
            | topRootPage url == Just url
              -- apple-touch-icon используем только для ссылок на корень
              -- В конкретных блогах (например codedot.dreamwidth.com)
              -- может быть общая для всего сайта иконка.
            , is@(_:_) <-
                [ (reverse $ sort is, url)
                | (kind, (is,_,url)) <- icons
                , kind `elem`
                  [PIICSAppleTouchIcon, PIICSAppleTouchIconPrecomposed]
                ]
              = Just $ snd $ maximumBy (comparing fst) is
            | otherwise = Nothing
        findAvatarUrl = find (regexTest "https?://[^/]+tumblr.com/avatar_")
        iconUrl (_,(_,_,u)) = u

    return $
        LinkInfo
        { liUrl         = url0
        , liTitle       = firstWordsX 150 title
        , liDescription =
            firstWordsX (if isJust image then 220 else 300) $
            fromMaybe "" $
            tryLookup [PIDSOpenGraph, PIDSTwitter] $
            mapMaybe checkIgnoredDescription $
            filter ((`notElem` parentDescrs) . normDescription . snd) $
            piDescription pi
        , liImage       = image
        , liAvatar      = avatar
        }

-- curl 'localhost:8098/buckets/PageInfo/index/$bucket/_' >all_page_infos.js

-- checkPageInfos = do
--     l <- getDirectoryContents hotLinksCacheDirectory
--     pis <- forM (filter (`notElem` [".", ".."]) l) $ \ fn -> do
--         f <- BL.readFile $ hotLinksCacheDirectory ++ "/" ++ fn
--         let !pi = decode f :: Either String PageInfo
--         return pi
--     let urls = [piURL | Right (PageInfo {..}) <- pis]
--         rootUrls = mapMaybe rootPage urls ++ mapMaybe topRootPage urls
--         nonExistentRootUrls =
--             Set.fromList rootUrls `Set.difference` Set.fromList urls
--         robots =
--             [ piURL
--             | Right (PageInfo {..}) <- pis
--             , ((/= "bq-non-html-content-type") -> True, T.toLower -> r)
--                 <- piRobots
--             , "nosnippet" `T.isInfixOf` r
--             ]
--     print ("robots", length robots)
-- --    mapM_ (B.putStrLn . tbs) robots
--     print ("urls", length urls)
--     -- print ("rootUrls", length rootUrls)
--     print ("uniqRootUrls", Set.size $ Set.fromList rootUrls)
--     print ("uniqNonExistentRootUrls", Set.size $ nonExistentRootUrls)
-- --    fail "stop"
-- --    print nonExistentRootUrls
--     rootPis <- withLogger $ \ l -> logTime l "rootPis" $ mapM wait =<< mapM (async . readOrDownloadPageInfo . T.unpack)
--         (Set.toList nonExistentRootUrls)
-- --    print rootPis
--     let piMap = Map.fromList [(piURL pi, pi) | Right pi <- pis ++ rootPis]
--         cts =
--             Map.toList $ Map.fromListWith Set.union
--             [ (T.takeWhile (/= ';') ct, Set.singleton piURL)
--             | Right (PageInfo {..}) <- pis
--             , ("bq-non-html-content-type", ct) <- piRobots
--             ]
--         top f =
--             mapM_ (\ (x,s) -> do
--                        B.putStrLn $ B.concat
--                             [ B.pack $ show $ Set.size s, "\t", tbs x ]
--                        forM_ (take 5 $ Set.toList s) $ \ u ->
--                            putStrLn $ "  " ++ T.unpack u
--                   ) $
--             take 100 $
--             sortBy (comparing $ Down . Set.size . snd) $
--             Map.toList $ Map.fromListWith Set.union
--             [ (x, Set.singleton $ piURL pi)
--             | Right pi <- pis
--             , not $ any (`T.isInfixOf` piURL pi)
--               [ "fotki.yandex.ru"
--               , "reddit.com"
--               -- 0 points and 0 comments so far on reddit
--               , "flickr.com"
--               -- Explore pilllpat (agence eureka)'s photos on Flickr. pilllpat (agence eureka) has uploaded 44352 photos to Flickr.
--               , "maps.google.com"
--               , "cheezburger.com", "imagetwist.com"
--               ]
--             , let rootX =
--                       [ norm d
--                       | r <- catMaybes [ rootPage (piURL pi)
--                                        , topRootPage (piURL pi)]
--                       , Just rpi <- [ Map.lookup x piMap
--                                     | x0 <- [r, T.append r "/"]
--                                     , x <- [x0, insWWW x0]
--                                     ]
--                       , (_, d) <- piDescription rpi ++ piTitle rpi
--                       ]
--                   parents = rootX ++ map (norm . snd) (piTitle pi)
--                   x = tryFirst (filter ((`notElem` parents) . norm . snd) . f)
--                       pi
--             -- , x /= ""
--             , not $ ignoredDescription x
--             ]
--         tryFirst f pi
--             | Just t <- lookup "og" (f pi) = t
--             | Just t <- lookup "twitter" (f pi) = t
--             | ((_,title):_) <- f pi = title
--             | otherwise = ""
-- --    print $ sum $ map (Set.size . snd) cts
-- --     forM_ cts $ \ (mime, urls) -> do
-- --         print (mime, Set.size urls)
-- --         forM_ (Set.toList urls) $ \ u -> do
-- --             putStr "  "
-- --             print u
-- --    mapM_ print $ Set.toList $ Set.fromList $ lefts pis
--     top piDescription


-- У Facebook 300 символов и … если нет картинки
-- И 185 и … если есть
-- 3 строки 219 (Associated Press), 239 -- чуть больше

getImage bg = fmap liImage . getLinkInfo' bg
getAvatar bg = fmap liAvatar . getLinkInfo' bg

-- если нет картинки автора и фида, то ищем по ссылке автора или по ссылке фида

getAvatarFromLink bg u
    | Just g <- getAvatarFromLink_ bg u = Just $ force =<< unRDM g
    | otherwise = Nothing

-- | Либо ссылка на ранее скачанный аватар, либо ссылка на API для скачивания
getAvatarFromLinkLazy u
    | Just g <- getAvatarFromLink_ False u = do
        rd <- unRDM g
        return $ case rd of
            Read x -> x
            Download _ -> Just $ "/avatar?u=" <> encodeURIComponentT u
    | otherwise = return $ Nothing

getAvatarFromLink_ bg u
    | [[_, user]] <- regexGet "https?://([^\\.]+).livejournal\\.com/?$" u =
        Just $ getImage bg (T.concat ["http://", user, ".livejournal.com/profile?nojs=1&format=light"])
    | [[_, user]] <- regexGet "http://users\\.livejournal\\.com/([^/]+)/?$" u =
        Just $ getImage bg (T.concat ["http://users.livejournal.com/", user, "/profile?nojs=1&format=light"])
    | regexTest "https?://(www\\.)?youtube\\.com/channel/.+" u =
        Just $ getImage bg u
    | regexTest "feedburner\\.com|pipes\\.yahoo\\.com|reddit\\.com/user|twitter\\.com/|facebook\\.com|blogspot\\.com|wordpress\\.com|delcious\\.com|stackexchange\\.com/users" u =
        Nothing
    | otherwise =
        Just $ getAvatar bg u
-- у youtube картинка канала прямо в liImage
-- getLinkInfo "http://www.youtube.com/channel/UCdxTX9FGFPrW16WQPEXJJlA"
-- у tumblr в PIICSShortcutIcon
-- readOrDownloadPageInfo "http://securityreactions.tumblr.com/"
-- а может быть и в image
-- readOrDownloadPageInfo "http://startupquotes.startupvitamins.com"
-- http://33.media.tumblr.com/avatar_5fe77b30b5db_128.png
--     ^^ можно по слову avatar искать
--     (у startupvitamins еще и авторов нет)
-- у ЖЖ надо заходить в профиль и искать
-- -- <img src='http://l-userpic.livejournal.com/113213537/670251' width='' height='' border='0' class='user_pic' alt='Юзерпик'/>
-- иногда бывает apple-touch-icon -- это стоит использовать
-- readOrDownloadPageInfo "http://www.thisiscolossal.com"
-- ссылки на yahoo pipes или feedburner не нужны
-- Надо также parent-ов учитывать
-- а то http://codedot.dreamwidth.org/
-- показывает apple-touch-icon-precomposed от dreamwidth.org

-- Стоит обработать ЖЖ и reddit-ы
-- C reddit-ом хуже, т.к. у есть ссылки на конкретных пользователей, а не на
-- весь reddit
