{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards, TupleSections,
             BangPatterns, TransformListComp, ScopedTypeVariables,
             MultiWayIf #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-imports  #-}
module APIServer
    ( runApiServer
    ) where

import Text.Printf
import Control.Monad
import Control.Applicative
import qualified Control.Exception as E
import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Short as SB
import qualified Data.Text as T
import Generated.DataTypes
import Lib.UrTime
import Lib.Log
import Lib.ReadUtils
import Lib.Regex
import Lib.StringConversion
import Data.Maybe
import Resolvables
import Generated.RiakIO
import qualified Data.Text.Encoding as T
import qualified Data.CaseInsensitive as CI
import qualified Control.Concurrent.MSem as MSem
import Control.Concurrent
import Control.Concurrent.Async
import qualified Network.HTTP.Types as N
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as B
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Parse
import Network.HTTP.Types (status200, badRequest400, notFound404, unauthorized401, forbidden403, found302, notModified304, gatewayTimeout504)
    -- noContent204
import qualified Network.HTTP.Types as H
import Blaze.ByteString.Builder (copyByteString, copyLazyByteString)
import Control.Monad.Trans
import System.Timeout
import System.Random (randomIO)
import System.IO.Unsafe
import qualified Data.Aeson as JSON
import Data.IORef
import UrCalls
import Riak
import Search
import API
import Network.HTTP.Conduit.Downloader (RawDownloadResult(..), dsTimeout)
import Config (mediaProxyDownloaderSettings, readerApiPort, readerMediaProxyPort, proxiedDomain)
import URL (isHttp)
import PageInfo
import UsageFlags
import Lib.ElasticSearch (obj')
import Preprocess (proxyUrl, ProxyType(PTAvatar))
import Session (newSession, clearSession', clearSession, looksLikePublicFeed)
import Account (getUserByLogin)
import Subscriptions (isPaid)
import OPML (userOPML, opmlSubscriptions)

runApiServer = do
    void $ forkIO runApiServer'
    void $ forkIO runProxyServer'

runApiServer' =
    runSettings
        (setPort readerApiPort $
         setOnExceptionResponse exceptionResponse
         defaultSettings)
        handleApiRequest
runProxyServer' =
    runSettings
        (setPort readerMediaProxyPort $
         setServerName "" $ -- чтобы не кешировало имя сервера
         setTimeout (2 * dsTimeout mediaProxyDownloaderSettings) $
         setOnExceptionResponse exceptionResponse
         defaultSettings)
        handleProxyRequest

exceptionResponse :: E.SomeException -> Response
exceptionResponse e = unsafePerformIO $ do
    logS $ "Exception in API: " <> show e
    return $ responseBuilder H.internalServerError500
        [(H.hContentType, "text/plain; charset=utf-8")]
        $ T.encodeUtf8Builder $ T.pack $ "Exception: " ++ show e

handleProxyRequest req respond = respond =<< do
    t <- getUrTime
    body <- strictRequestBody req
    if | Just path <- B.stripPrefix "/proxy/" (rawPathInfo req)
       ->  proxy $ bst path <> bst (rawQueryString req)
       | ["private", "call_proxy"] <- pathInfo req
       , [("pass_to", Just url)] <- queryString req
       ->  proxy $ bst url
       | ["private", "call_resize", size] <- pathInfo req
       , [("pass_to", Just url)] <- queryString req
       , [[_, opts, tryReadUnsignedInt -> Just w, tryReadUnsignedInt -> Just h]]
           <- regexGet "(f?t?)([0-9]+)x([0-9]+)" size
       ->  let o x = T.any (== x) opts in
           withLogger $ \ l -> do
               logLT l $ T.concat ["/resize/", size, "/", bst url]
               resize l (o 'f') (o 't') w h (BL.toStrict body)
       | otherwise ->
           retString notFound404 $
               "Not found (proxy) " <> rawPathInfo req <> rawQueryString req <> "\n"
    where int = tryReadUnsignedInt
          proxy url = withLogger $ \ l -> do
              (t, r) <- time $ rateLimitedDownload' False (proxiedDomain url)
                  url (proxyOpts req)
              logLT l $ T.concat ["/proxy/", url, " ", showT (proxyOpts req)
                  , " ", T.pack $ showSecs t]
              case r of
                  Ok _ _ rdr -> retRdr l rdr
                  Redirect _ u rdr -> retRdr l (changeLocation u rdr)
                  Err "Too much data" _ ->
                      retString badRequest400 "Requested file is too large"
                  Err _ (Just rdr) -> retRdr l rdr
                  Err e Nothing
                      | T.isInfixOf "timeout" $ T.toLower e ->
                          retString gatewayTimeout504 "Timeout fetching"
                      | otherwise ->
                          retString badRequest400 (tbs e)
                          -- invalid url
                  NotModified -> retString notModified304 ""
          changeLocation u rdr =
              rdr { rdrHeaders = ("Location", tbs u)
                      : filter ((/= "Location") . fst) (rdrHeaders rdr) }
              -- меняем относительный путь на абсолютный
          proxyOpts req =
              [ B.unpack (CI.foldedCase h) <> ": " <> B.unpack v
              | (h,v) <- requestHeaders req
              , h `elem` ["Via", "Forwarded", "Range"]]
              ++
              if range then ["Accept-Encoding: "] else []
              -- сжатие не имеет смысла для видео, мы хотим перематывать,
              -- а не распаковывать файл с самого начала
          range = any (\ (n, _) -> n == "Range") $ requestHeaders req
          retRdr l (RawDownloadResult {..}) = do
              ih <- if not range then identifyHeader l rdrBody else return []
              -- сразу же вызываем identify, чтобы при последующих resize
              -- не узнавать повторно размер изображения
              return $ responseBuilder fixedRdrStatus
                  (contentLength rdrBody : go Nothing rdrHeaders <> ih)
                  (copyByteString rdrBody)
              where fixedRdrStatus
                        | N.statusCode rdrStatus == 502 =
                            -- чтобы не происходило переключения upstream
                            -- при "502 Bad gateway"
                            rdrStatus { N.statusCode = 500 }
                        | otherwise = rdrStatus
                    go c [] = [cacheControl $ fromMaybe minCacheTime c]
                    go c (h:hs) = case h of
                        -- ("Vary", "*") -> [h]
                        -- у нас заголовки не меняются, но "Vary: *" оставляем
                        ("Content-Range", _)
                            | range -> h : go c hs
                        -- мы всегда возвращаем несжатое содержимое
                        -- и устанавливаем свой Content-Length. Однако, для
                        -- сжатого содержимого Content-Range, возвращенный
                        -- сервером, может не совпадать с Content-Length
                        -- распакованного содержимого. Для range-запросов
                        -- мы отключаем Accept-Encoding, а для обычных
                        -- запросов на всякий случай убираем Content-Range
                        -- из ответа сервера
                        ("Expires", eh)
                            | Just dh <- lookup "Date" rdrHeaders
                            , Just d <- parseHttpTime $ B.unpack dh
                            , Just e <- parseHttpTime $ B.unpack eh
                            , dt <- round (diffUrTime e d)
                            , dt > minCacheTime
                            -> go (c <|> Just dt) hs
                            | otherwise -> go c hs -- не можем распарсить
                        ("Cache-Control", v)
                            | [[_, d]] <- regexGet maxAgeRegex (bst v)
                            , Just a <- tryReadUnsignedInt d
                            , a > minCacheTime
                            -> go c hs
                        (n, _)
                            | n `elem` allowedHeaders -> h : go c hs
                            | otherwise -> go c hs
                    minCacheTime :: Int
                    minCacheTime
                        | N.statusCode rdrStatus `elem` [500, 502, 503, 504]
                        = 5*60
                        | otherwise = 3600
                    cacheControl :: Int -> (N.HeaderName, B.ByteString)
                    cacheControl maxAge =
                        ("Cache-Control", tbs ("public, max-age=" <> showT maxAge))

maxAgeRegex :: Regex
maxAgeRegex = "max-age=([0-9]+)"

allowedHeaders =
    [ "Location"
    , "Date"
    , "Last-Modified"
    , "ETag"
    , "Content-Type"
    , "Content-Range"
    , "Accept-Ranges"
--     , "Content-Disposition" -- может превратить видео в attachment
    ]

resizeSem :: MSem.MSem Int
resizeSem = unsafePerformIO $ MSem.new 4
{-# NOINLINE resizeSem #-}

resizeOptions dat = case imageExtensionByMagic dat of
    Just e@"jpg" ->
        ok e ["-quality", "75", "-interlace", "JPEG"]
        -- -colorspace RGB        -- убирает цветовой профиль и портит цвета
        --                           у некоторых изображений
        -- -sampling-factor 4:2:0 -- портит цвета, а сжатие сильно не меняет
    Just e@"png" ->
        ok e ["-quality", "100"]
        --  ^ у PNG -quality обозначает сжатие
        -- -define png:include-chunk=none -- также убирает цветовой профиль
    Just e@"gif" ->
        Right (e, "png", ["-quality", "100"])
        -- При resize gif-а, получается много лишних цветов и, если оставить
        -- формат gif-ом, некоторые цвета будут плохими. По-этому переводим
        -- в png.
        -- Анимированным изображениям мы вообще resize не делаем, т.к.
        -- это очень плохо влияет на качество, см.
        -- https://www.imagemagick.org/Usage/anim_mods/#resize
        -- convert exp.gif -coalesce -bordercolor White -border 0 -resize 320x640 -layers Optimize -quality 100 exp-320.gif
        -- всё равно даёт плохие цвета, даже если в webp переводить
    Just e@"webp" ->
        ok e ["-quality", "75"]
    Just e ->
        Left $ "Unsupported image format: " <> T.pack e
    _ ->
        Left $ "Unknown image format. Unknown header: "
            <> T.pack (show $ B.take 10 dat)
    where ok e o = Right (e, e, o)

-- Можно искать размер картинки в бинарных данных, чтобы не вызывать identify.
-- Но пока не будем заморачиваться.
identifyHeader l dat = case resizeOptions dat of
    Right (ext, _, _) -> do
        --  %[quality]
        r <- runIM l "identify"
            (["-quiet", "-format", "%[width] %[height] %[scenes]\n", ext <> ":-"])
            dat
        case r of
            Right (map T.words . T.lines . bst ->
                   ([ tryReadUnsignedInt -> Just w
                    , tryReadUnsignedInt -> Just h
                    , tryReadUnsignedInt -> Just s]:_)) -> do
                logLT l $ T.concat ["Image size: ", showT w, "x", showT h
                    , if s > 1 then ", " <> showT s <> " frames" else ""]
                return [("X-Identify", tbs $ T.concat
                    [showT w, " ", showT h, " ", showT s])]
            Right r -> do
                logLT l $ "Can't parse identify output: " <> bst r
                err
            Left e -> do
                logLT l $ "identify error: " <> e
                err
    Left e -> do
        logLT l e
        err
    where err = return []

resize l force thumbnail width height dat = case resizeOptions dat of
    Right (e, oe, o) -> go e oe o
    _ -> ret dat
    where go ext oExt opts = do
              r <- runIM l "convert"
                  ([ext <> ":-" <> if thumbnail then "[0]" else "", "-quiet"
                   -- , "-strip"
                   --   ^ убирает color profile и делает некоторые изображения
                   --     более темными
                   ,"-resize", show width <> "x" <> show height <> ">"]
                   ++ opts ++ [oExt <> ":-"])
                  dat
              case r of
                  Right r -> do
                      let bigger = B.length r > B.length dat
                      logLT l $ T.concat ["converted ok ("
                          , showT (B.length dat), " -> ", showT (B.length r)
                          , " bytes"
                          , if bigger then
                                ", bigger)\n"
                                <> if not force then "returning original"
                                   else "forced resize"
                            else
                                ")"
                            -- некоторые оптимизированные png (или чёткие png,
                            -- например UI), которые незначительно уменьшаются
                            -- и замыливаются, могут «сжаться» так,
                            -- что станут больше оригинала (больше цветов,
                            -- больше деталей)
                          ]
                      ret $ if bigger && not force then dat else r
                  Left e ->  do
                      logLT l $ "convert error: " <> e
                      ret dat
          ret = retString' "application/octet-stream" status200

runIM l c a dat = MSem.with resizeSem $
    logTime l (T.pack c) $ runLimitedUser c a dat

handleApiRequest req respond = respond =<< do
    t <- getUrTime
    (params, _files) <- parseRequestBody lbsBackEnd req

    let qs = toText $ [(n, fromMaybe "" v) | (n,v) <- queryString req] ++ params
        toText [] = []
        toText ((bst -> !n, bst -> !v) : xs) = (n,v) : toText xs
        cookie =
            (do auth <- lookup "authorization" $ requestHeaders req
                [_,s] <- return $ T.split (== '=') $ bst auth
                return $ T.strip s)
            <|>
            (do c <- lookup "Cookie" $ requestHeaders req
                h <- lookup "Host" $ requestHeaders req
                let fromUrWeb = T.replace ".2D" "-" . T.replace ".2E" "."
                                . T.replace ".5F" "_"
                    v = [(n,v) | [n, fromUrWeb -> v] <-
                         map (map T.strip . T.split (== '=')) $
                         T.split (== ';') $ bst c]
                lookup (if "beta" `B.isInfixOf` h then "Main/sid_beta"
                        else if "local" `B.isInfixOf` h then "Main/sid_local"
                        else "Main/sid") v)
        fixEmail e = case appByReq qs req of
           UFApp ATJustReader _ ->
               fromMaybe e $ T.stripSuffix "@bazqux.com" e
           _ ->
               e

    s <- case cookie of
        _ | Just fever_key <- lookup "api_key" qs -> do -- Fever
            u <- liftIO $ getUserByLogin (LTFeverApiKey fever_key) Nothing
            return $ Session fever_key maxBound False <$> u
          | ("feed":f:_) <- filter (/= "") $ pathInfo req -> do
            let retDummy =
                    return (Just $ Session f maxBound False
                                 emptyUserForExternalFeedsAccess)
            if looksLikePublicFeed f then do
                pf <- liftIO $ readPublicFeed' f
                maybe retDummy
                      -- если фида нет, все равно идем дальше,
                      -- чтобы ошибки правильно отрабатывать,
                      -- а не unauthorized выдавать
                      (return . Just . Session f maxBound False)
                      (pfUser pf)
            else
                retDummy
        Just sid | sid /= "" -> do
            s <- liftIO $ cachedReadSession sid
--             liftIO $ print s
            case s of
                Just s | sessionExpire s < t -> do
                    liftIO $ clearSession' "expired" s
                    return Nothing
                _ -> return s
        _ -> return Nothing
--     liftIO $ print cookie
    let badAuth u =
            retStringLoginError u forbidden403
                "Error=BadAuthentication\n"
                "BadAuthentication"
        retStringLoginError mbu s r jr =
            retString''
                [("X-BQ-LoginErrorReason", loginErrorReason u)| Just u <- [mbu]]
                (if json then "application/json" else "text/plain")
                s
                (if json then BL.toStrict $ JSON.encode $ obj' $
                    [( "reason", JSON.String $ loginErrorReason u)
                    | Just u <- [mbu]]
                    ++
                    [( "error", JSON.String jr)]
                 else
                    r)
        loginErrorReason u = case uvmPaidTill (uViewMode u) of
            PTPaidFinished _ -> "YearSubscriptionExpired"
            PTPaid _ -> "YearSubscriptionExpired"
            _ -> "FreeTrialExpired"
        fever = isJust (lookup "api" qs) || pathInfo req == ["fever"]
                || filter (/= "") (pathInfo req) == ["reader","api","0","fever"]
        json =
            lookup "output" qs == Just "json" &&
            lookup "client" qs /= Just "Reeder"
            -- Reeder зачем-то при ClientLogin делает output json
        unauthorized u = do
            logS $ "Unauthorized: requestHeaders = "
                ++ show (requestHeaders req)
                ++ ", cookie = " ++ show cookie ++ ", s = " ++ show s
            unauthorized' u
        unauthorized' u
            | fever =
                -- retStringLoginError u status200
                retString' "application/json" status200
                                    "{\"api_version\":3,\"auth\":0}\n"
            | otherwise =
                retStringLoginError u unauthorized401
                    "Unauthorized"
                    "Unauthorized"
--        delS = maybe (return ()) clearSession' s
-- не надо в badAuth удалять сессию, а то щелкнул на ClientLogin и текущая
-- сессия удалилась

    log <- liftIO $ newIORef ""

--     liftIO $ withLogger $ \ l -> do
--         logLS l "---------------------------"
--         logLS l $ show $ pathInfo req
--         logLS l $ show $ requestHeaders req
--         logLS l $ show qs

    r <- case (s, filter (/= "") $ pathInfo req) of
        (_, ["accounts","ClientLogin"])
            | Just (fixEmail -> email) <- lookup "Email" qs
            , Just passwd <- lookup "Passwd" qs
            , email /= "" && passwd /= "" -> liftIO $ do
                mbu <- getUserByLogin (LTEmail email) (Just passwd)
                case mbu of
                    Just uid -> do
                        u <- cachedReadUser uid
                        if isPaid t u then do
                            s <- newSession True (LTEmail email) LATNone
                                 (filter ((/=) "Passwd" . fst) qs ++
                                  [ (bst h,
                                     maybe "-" bst $ lookup (CI.mk h) $
                                     requestHeaders req)
                                  | h <- ["Country", "User-Agent"] ])
                            if json then
                                retString' "application/json" status200 $
                                B.concat
                                     ["{\"SID\":\"unused\","
                                     ,"\"LSID\":\"unused\","
                                     ,"\"Auth\":\""
                                     ,T.encodeUtf8 $ sessionKey s
                                     ,"\"}"]
                            else
                                retString status200 $
                                B.concat
                                     ["SID=unused\nLSID=unused\nAuth="
                                     ,T.encodeUtf8 $ sessionKey s
                                     ,"\n"]
                        else
                            badAuth u
                    _ -> badAuth Nothing
         -- Mr.Reader поддерживает ServiceDisabled
         -- Reeder нет (мигает окошко логина, а с BadAuthentication говорит
         -- "проверьте ваш аккаунт")
-- при протухании ClientLogin возвращается unauthorized401
--  curl -v https://www.google.com/accounts/ClientLogin -d Email=E -d Passwd=P -d accountType=GOOGLE -d source=curlExample -d service=reader
            | otherwise ->
                liftIO $ badAuth Nothing
        (Nothing, _) ->
            liftIO $ unauthorized Nothing
        (Just s, path) -> liftIO $ do
            let user = sessionUser s
            u <- cachedReadUser user
            if externalResourceRequest req then
                handleApiRequest' log qs user req
                -- без блокировки
            else if isPaid t u || user == emptyUserForExternalFeedsAccess then
                blockBucketKey "API" (BL.fromStrict $ T.encodeUtf8 user) $ do
                    -- не позволяем параллельные запросы для одного
                    -- пользователя
                    t' <- getUrTime
                    let blockTime = diffUrTime t' t
                        to = 60*1000000
                    if blockTime > 60 then
                        -- Учитываем, что nginx не знает о задержке блокировки
                        -- и может посчитать, что прокси не отвечает,
                        -- для этого учитываем время блокировки.
                        -- Если минуту ждали обработки других запросов,
                        -- то отваливаемся (обработка, в итоге, никогда не будет
                        -- больше 2 минут). Нормальные клиенты не должны
                        -- делать параллельных запросов
                        retString (N.Status 429 "Too Many Requests") $ B.pack $
                            "Was blocked handling previous requests \
                            \for the last " ++ show blockTime ++ " seconds"
                    else do
                        r <- timeout to $ handleApiRequest' log qs user req
                        case r of
                            Just r -> return r
                            Nothing -> do
                                modifyIORef log (`T.append` " TIMEOUT!")
                                retString status200 "TIMEOUT"
            else do
                clearSession "not paid" (sessionKey s)
                unauthorized u

    t1 <- getUrTime
    l <- liftIO $ readIORef log
    logT $ T.concat [ "-- ", bst $ rawPathInfo req, "  "
                    , maybe "-" sessionUser s, " ", l, " "
                    , T.pack $ printf "%.3f" (diffUrTime t1 t :: Double) ]
    return r

withGatewayTimeout log act = do
    r <- timeout (20*1000*1000) act
    case r of
        Just r -> return r
        Nothing -> do
            modifyIORef log (`T.append` " TIMEOUT!")
            retString gatewayTimeout504 "TIMEOUT"

randomString n = do
    rnd <- replicateM n randomIO :: IO [Word8]
    return $ Base64.encode $ B.pack $ map (toEnum . fromEnum) rnd

fixInvalidXML = T.filter validXmlChar
    -- T.map (\ c -> if not (validXmlChar c) then ' ' else c)
    -- замена на пробел может выдать лишний пробел перед запятой
validXmlChar c =
    (c >= '\x20' && c <= '\xD7FF') || c == '\xA' || c == '\xD' || c == '\x9' ||
    (c >= '\xE000' && c <= '\xFFFD') ||
    (c >= '\x10000' && c <= '\x10FFFF')
-- http://www.w3.org/TR/REC-xml/#charsets
-- #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
-- /* any Unicode character, excluding the surrogate blocks, FFFE, and FFFF. */

-- Запросы к внешним ресурсам (изображениям/API), которые не требуют блокировки
-- (т.к. практически не напрягают БД и ограничений в nginx должно хватить),
-- а также не сохраняются в usageFlag.
externalResourceRequest req = case filter (/= "") $ pathInfo req of
    ("avatar":_) -> True
    ("feedicon":_) -> True
    ("favicon":_) -> True
    ("img":_) -> True
    ("api_proxy":_) -> True
    _ -> False

appByReq qs req =
    appByClientAndUserAgent $ T.append
        (fromMaybe "" $ lookup "client" qs)
        (maybe "" bst $ lookup "User-Agent" $ requestHeaders req)

handleApiRequest' log qs user req = do
    let appRequest =
            not (externalResourceRequest req)
            && case filter (/= "") $ pathInfo req of
                ("feed":_) -> False
                p -> notElem "unread-count" p
    when appRequest $
        -- unread-count может идти с браузерных расширений,
        -- не считаем это использованием ридера.
        -- avatar/favicon идут от браузера, а не приложений и идут вследствии
        -- запросов, которые уже помечают usageFlag
        -- feed не от пользователя вообще
        usageFlag user $ appByReq qs req

    let headers content =
            [ ("Content-Type", B.append content "; charset=UTF-8")
            , ("x-reader-google-version", "555-100")
            , ("x-reader-user", api0user user)]
        xml f = do
            r <- fmap (T.encodeUtf8 . fixInvalidXML) $ f user
            seq (B.length r) $ return $ responseBuilder status200
                (contentLength r : headers "text/xml") $ copyByteString r
        json = json' "text/javascript" -- Google Reader возвращал так
        feverJSON x = json' "application/json" x
        json' mime f = do
            r <- f user
            let s = apiDataToJSON r
            seq (BL.length s) $ return $ responseBuilder status200
                (headers mime) $
                copyLazyByteString s
        handle f
            | ("output", "json") `elem` qs = json f
            | otherwise = xml (fmap apiDataToXML . f)
        handleEdit f = do
            r <- f user
            if r then
                retString status200 "OK"
            else
                retString badRequest400 "Bad Request"
        hostName = fromMaybe "" $ lookup "Host" (requestHeaders req)

--     withLogger $ \ l -> do
--         logLS l "---------------------------"
--         logLS l $ show $ user
--         logLS l $ show $ pathInfo req
--         logLS l $ show $ requestHeaders req
--         logLS l $ show qs
    let cantFindFeed f =
            retString notFound404 $ tbs $ T.unlines $
            [ "Can’t find feed"
            , f ]
            ++
            (if "http" `T.isPrefixOf` f then
            (if rawQueryString req /= "" then
            [ ""
            , "Make sure that feed address is escaped (%3F instead of ?)." ]
            else [])
            ++
            [ ""
            , "bazqux.com/feed only returns feeds that are in use by our clients."
            , "So perhaps nobody is subscribed to this feed at the moment."
            ] else [])
        favicon noImage =
            case lookup "u" qs of
                Just u -> do
                    gfi <- async $ getFavicon False u
                    def <- async $ do
                        threadDelay (20*1000*1000)
                        return $ (defaultFavicon u)
                            { faviconFile = Left (UrTime 0 0, "overloaded") }
                        -- нет особого смысла ждать дольше, лучше показать
                        -- пустышку. Плюс, после 40 секунд пойдет запрос
                        -- на следующий backend
                    (_,r) <- waitAny [gfi, def]
                    case faviconFile r of
                        Left (_,e) -> do
                            writeIORef log $
                                T.concat ["not found for ", u, ": ", e]
                            noImage
                        Right i -> do
                            writeIORef log $
                                T.concat [ "found for ", u, " -> "
                                         , faviconSourceUrl r ]
                            retImage i
                Nothing ->
                    retString badRequest400 "No “u” parameter"

    case filter (/= "") $ pathInfo req of
        ("avatar":_) -> do
            let noImage =
                    -- retString notFound404
                    retString status200 "" -- чтобы кешировалось
            case lookup "u" qs of
                Just u
                    | Just act <- getAvatarFromLink False u -> do
                        r <- act
                        case r of
                            Nothing -> do
                                writeIORef log $
                                    T.concat ["not found for ", u]
                                noImage
                            Just r -> do
                                writeIORef log $
                                    T.concat ["found for ", u, " -> ", r]
                                retRedirect found302 $ tbs $
                                    proxyUrl PTAvatar (bst hostName) r
                    | otherwise ->
                        noImage
                Nothing ->
                    retString badRequest400 "No “u” parameter"
        ("feedicon":_) -> favicon (retImage defaultFeedIcon)
        ("favicon":_) -> favicon (retString status200 "")
        ["img", "vimeo_thumbnail", id] ->
            withGatewayTimeout log $
                either (retString badRequest400 . tbs)
                    (retRedirect found302 . tbs) =<<
                vimeoThumbnail (bst hostName) id
        ("api_proxy":path) ->
            withGatewayTimeout log $
                either (retString badRequest400 . tbs)
                    (retString' "application/json" status200 . SB.fromShort) =<<
                apiProxy (bst hostName) path
        ("feed":f:_) | looksLikePublicFeed f -> do
            r <- processFeed (T.toLower f) publicFeedPath qs
            case r of
                Nothing -> cantFindFeed f
                Just x -> xml $ const $ return x
        ("feed":f:_) -> do
            let qs' = addSub' ("dummy_atom":) qs
                Just (T.drop 5 -> s) = lookup "s" qs'
            p <- readPosts s
            if p == Nothing then
                cantFindFeed s
            else
                xml $ api0_atom path Nothing qs'
        ["fever"] ->
            feverJSON (processFever log qs)
        ["reader","api","0","fever"] ->
            feverJSON (processFever log qs)
        ["reader","api","0"] ->
            retString notFound404 "Hello! Now it’s time to make some calls to API ;)"
        ["reader","api","0","token"] -> do
            token <- randomString 18
            retString status200 token
            -- http://undoc.in/token.html
            -- токен протухает через полчаса
            -- и 401 HTTP с заголовком "X-Reader-Google-Bad-Token" = true
        ["reader","ping"] ->
            retString status200 "OK"
        ["reader","api","0","import","opml"]
            | Just o <- lookup "opml" qs -> do
                opmlSubscriptions (T.encodeUtf8 o) user
                importPercentComplete user
            | otherwise ->
                retString badRequest400 "Please use “opml” parameter"
        ["reader","api","0","import","percent-complete"] -> do
            importPercentComplete user
        ["reader","directory","search"] ->
-- http://www.google.com/reader/directory/search?q=[searchterm] searches the GReader taxonomy (HTML results)
            return $ responseBuilder status200
                (headers "text/html") $ copyByteString
                "Search is not yet supported"
        ["reader","subscriptions","export"] -> do
            o <- userOPML False user
            return $ responseBuilder status200
                (headers "text/xml" ++
                 [( "Content-Disposition"
                  , "attachment; filename=google-reader-subscriptions.xml" )]) $
                copyByteString $ T.encodeUtf8 o
        ["reader","api","0","user-info"] ->
            json api0_userinfo
        ["reader","api","0","preference","list"] -> do
            handle api0_preference_list
        ["reader","api","0","friend","list"] ->
            handle api0_friend_list
        ["reader","api","0","preference","stream","list"] -> do
            handle api0_preference_stream_list
        ["reader","api","0","preference","stream","set"] -> do
            api0_preference_stream_set qs user
            retString status200 "OK"
        ["reader","api","0","tag","list"] ->
            handle api0_tag_list
        ["reader","api","0","subscription","list"] ->
            handle api0_subscription_list
        ["reader","api","0","subscription","quickadd"]
            | ("output", "xml") `elem` qs ->
                xml $ fmap apiDataToXML . api0_subscription_quickadd qs
            | otherwise ->
                json $ api0_subscription_quickadd qs
        ["reader","api","0","subscription","edit"] -> do
            handleEdit $ api0_subscription_edit qs
        ["reader","api","0","unread-count"] ->
            handle (api0_unread_count $
                    isJust $ lookup "Cookie" $ requestHeaders req)
                    --  ^ запрос с браузерного расширения, кешируем меньше
        ["reader","api","0","stream","items","ids"] ->
            handle (api0_stream_items_ids qs)
        ["reader","api","0","stream","items","contents"]
            | ("output", "atom") `elem` qs ->
                xml $ api0_stream_items_contents_atom path qs
            | otherwise ->
                json $ api0_stream_items_contents_json path qs
        ("reader":"api":"0":"stream":"contents":_)
            | ("output", "atom") `elem` qs ->
                xml $ api0_atom path Nothing (addSub "contents" qs)
            | otherwise ->
                json $ api0_stream_contents path (addSub "contents" qs)
        ("reader":"atom":_) ->
            xml $ api0_atom path Nothing (addSub "atom" qs)
            -- игнорирует output=atom
        ["reader","api","0","edit-tag"] -> do
            api0_edit_tag log qs user
            retString status200 "OK"
        ["reader","api","0","rename-tag"] -> do
            api0_rename_tag qs user
            retString status200 "OK"
        ["reader","api","0","disable-tag"] -> do
            -- удаление папки (но не подписок)
            api0_disable_tag qs user
            retString status200 "OK"
        ["reader","api","0","mark-all-as-read"]
            | Nothing <- lookup "s" qs ->
                retString badRequest400 "No subscription specified"
            | otherwise -> do
                api0_mark_all_as_read log qs user
                retString status200 "OK"
        i -> do
            logS $ take 10000 $ show ("unknown request", pathInfo req, qs)
            retString notFound404 "Not found (API entry)\n"
--                show ("unknown request", pathInfo req, qs)
        -- если не залогинен, то возвращает badRequest400
    where path = T.concat $ "https://www.bazqux.com" : subPath
          subPath = [bst $ rawPathInfo req, bst $ rawQueryString req]
          publicFeedPath = T.concat $ "https://bazqux.com" : subPath
          addSub till qs = addSub' (dropWhile (/= till)) qs
          addSub' skip qs
              | null $ filter ((== "s") . fst) qs = ("s", sub) : qs
              | otherwise = qs
              where s = addHttps $
                        -- decodeURIComponentT $
                        --  ^ вроде warp и так декодирует
                        T.intercalate "/" $
                        dropWhile (== "") $ tail $
                        skip $ pathInfo req
                    addHttps x
                        | Just f <- T.stripPrefix "feed/" x
                        = if isHttp f then
                            x
                          else
                            "feed/https://" <> f
                            -- добавляем https://
                            -- для bazqux.com/feed/www.facebook.com/1234
                        | otherwise = x
                    sub | s == "" = "user/-/state/com.google/reading-list"
                        | otherwise = s

retImage (SB.fromShort -> mime, SB.fromShort -> dat) =
    retString' mime status200 dat
retString s = retString' "text/plain" s
retString' = retString'' []
retString'' headers mime status s = return $
    responseBuilder status
                    ([ ("Content-Type", B.append mime "; charset=UTF-8")
                     , contentLength s ] ++ headers) $
                    copyByteString s
retRedirect status url = return $
    responseBuilder status
                    [ ("Location", url)
                    , contentLength url ] $
                    copyByteString url

contentLength s = ("Content-Length", B.pack $ show $ B.length s)

-- только сами фиды
importPercentComplete user = do
    (_, _, sirAll : _, _, _) <- subscriptionsAndViewMode "" False False False UFNone "" "" "" user
    let c = sirCounters sirAll
        total = cScanning c + cFeed c + cError c
        ready = cScanning c == 0
    retString status200 $ B.pack $ show $
        if ready || total == 0 then 100 else (total - cScanning c) * 100 `div` total
