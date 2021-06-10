{-# LANGUAGE RecordWildCards, ViewPatterns, OverloadedStrings,
             BangPatterns, TupleSections,
             GADTs, RankNTypes, MultiWayIf #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module ParseServer
    ( withWorkerRequestScheduler
    , WorkerRequest(..), WorkerRequestScheduler
    , runParseServer

    , processParseResult, sortFms, processFeedItems
    , countProcess, incrGauge, decrGauge, count
    , roundScanTime, scanListStep
    , urlError, addRescan, cancelRescans
    , setBlogPostsScannedUts, ppGuids, isBlogUts, isCommentUts
    , parserEnvironmentGet, parserEnvironmentSet
    )
    where

import Control.Applicative
import Control.Monad.Trans
import qualified Control.Exception as E
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, unauthorized401)
import Blaze.ByteString.Builder (fromLazyByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as SB
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Lib.Regex
import Parser
import Parser.Types
import Parser.Custom (defaultFeedMsg, groupByN)
import Parser.Facebook (parseFacebookUsageRdr)
import Parser.Telegram (telegramError)
import qualified Control.Concurrent.MSem as MSem
import Data.Binary
import Data.Maybe
import Data.Either
import Data.List
import Network.HTTP.Conduit.Downloader
import Lib.DnsCache
import Lib.Log
import Lib.UrTime
import Lib.ElasticSearch
import Lib.StringConversion
import qualified Lib.BArray as BA
import URL
import qualified Network.HTTP.Client as C
import Auth (signTwitter, signTwitterWithAccessToken)
import UrCalls -- (hashData, feedMsgToItem)
import Subscriptions
import Account (updApiKeys)
import Control.Concurrent
import Control.Monad
import Data.Array
import qualified Data.HashMap.Strict as HM
import Data.IORef
import System.Random
import Resolvables
import Riak
import Generated.DataTypes
import Generated.RiakIO
import Data.Ord
import Lib.Merge
import Lib.Hash
import qualified Data.HashSet as HS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified System.Remote.Gauge as Gauge
import Lib.Stats
import PubSubHubbub
import qualified Lib.ReadSet as ReadSet
import Data.Ratio
import Text.Printf
import Discovery
import Search
import qualified Data.Default as C
import Data.String
import Control.Concurrent.Async
import Config
import HotLinks
import PageInfo (getFavicon)

type WorkerRequestScheduler =
    forall a b . Logger -> Text -> WorkerRequest a b -> a -> IO b

data WorkerRequest a b where
    ParseRequest :: WorkerRequest ParseServerRequest ParseServerResult
    ProcessRequest :: WorkerRequest ProcessUrlRequest ProcessUrlResult

instance Show (WorkerRequest a b) where
    show ParseRequest = "ParseRequest"
    show ProcessRequest = "ProcessRequest"

withWorkerRequestScheduler :: (WorkerRequestScheduler -> IO a) -> IO a
withWorkerRequestScheduler f = withDownloaderSettings ds $ \ cd -> do
    hostAddrs <- fmap (listArray (0, parseServersCount - 1)) $
        forM [1 .. parseServersCount] $ \ n ->
            resolveHostEntry $ "ps" ++ show n
    counters <- newMVar HM.empty
    let joinPur Nothing pur = pur
        joinPur (Just p1) pur =
            ProcessUrlResult
            { purNewUrls = purNewUrls p1 ++ purNewUrls pur
            , purLog =
                T.concat [ purLog p1
                         , maybe "" (`T.append` "\n") (purException p1)
                         , "=====\n", purLog pur ]
            , purException = purException pur
            , purStatsMap = HM.unionWith (+) (purStatsMap p1) (purStatsMap pur)
            }
        joinPurM p1 (Just pur) = Just $ joinPur p1 pur
        joinPurM p1 Nothing = p1
        req :: Int -> Maybe Int -> Maybe ProcessUrlResult -> Logger -> Text -> WorkerRequest a b -> a -> IO b
               -- WorkerRequestScheduler
        req 0 _ _ _ _ ParseRequest _ =
            return $ ParseServerResult (DPRError "No parse servers running?") Nothing ""
        req 0 _ r _ _ ProcessRequest _ =
            return $ joinPur r $ ProcessUrlResult [] "" (Just "No process servers running?") HM.empty
        req attempts prevN p1 logger hostName reqType reqData = do
            let succPrevOr act
                    | Just n <- prevN = return $ succ n `mod` parseServersCount
                      -- в случае retry идем к следующему серверу,
                      -- а не к следующему по счетчикам (может и повториться)
                    | otherwise = act
            n <- succPrevOr $ modifyMVar counters $ \ c ->
                case HM.lookup hostName c of
                    Just pcr -> do
                        pc <- readIORef pcr
                        let pc' = succ pc `mod` parseServersCount
                        writeIORef pcr pc'
                        return (c, pc')
                    Nothing -> do
                        pc <- randomRIO (0, parseServersCount - 1)
                        pcr <- newIORef pc
                        return (HM.insert hostName pcr c, pc)
            let psHost = "ps" ++ show (n+1)
                (path, dat) =
                    case reqType of
                        ParseRequest -> ("/parse", encode reqData)
                        ProcessRequest -> ("/process", encode reqData)
--             logLS logger $ psHost ++ " "
--                       ++ showHostAddress (hostAddrs ! n)
                addTimeout r =
                    r { C.responseTimeout = C.responseTimeoutMicro 250000 }
                    -- таймаут только на заголовки (чтобы быстро отваливаться,
                    -- если parse server не отвечает)
                    -- таймаут нужен небольшой, т.к. domainworker-ов всего 2
                    -- и может провиснуть очередь доменов
                postData = BL.toStrict dat
            dr <- postData `seq`
                  downloadG (return . addTimeout . postRequest postData)
                  cd ("http://" ++ psHost ++ ":" ++ show parseServerPort ++ path)
                  (Just $ hostAddrs ! n) []

            let -- retry :: WorkerRequest a b -> a -> IO b
                -- TODO: по-хорошему конечно надо помечать плохие сервера
                -- и не обращаться к ним
                retry p = do
                    (case reqType of
                        ParseRequest ->
                            threadDelay 100000
                            -- притормаживаем parseRequest-ы,
                            -- чтобы за rateLimit-ы не вылезать
                        ProcessRequest -> do
                            d <- randomRIO (100000, 500000)
                            -- если был exception из-за RetriesExceeded
                            -- даем время остальным на обработку
                            threadDelay d) :: IO ()
                    req (attempts - 1) (Just n) (joinPurM p1 p)
                        logger hostName reqType reqData
                dec' :: Binary b => B.ByteString -> Either String b
                dec' dat =
                    case decodeOrFail (BL.fromStrict dat) of
                        Right (_,_,r) -> Right r
                        Left (_,_,e) -> Left e
                dec :: WorkerRequest a b -> B.ByteString
                    -> Either String (Maybe ProcessUrlResult, b)
                dec ParseRequest = fmap (Nothing,) . dec'
                dec ProcessRequest =
                    fmap (\ p -> if isJust (purException p)
                                 then (Just p, p) else (Nothing, p)) .
                    dec'
            case dr of
                DROK dat [] ->
                    case dec reqType dat of
                        Right (Nothing, r) -> return r
                        Right (Just p, _) -> do
                            logLS logger $ "purException: " ++ show (purException p)
                            incrStat "exceptions" 1
                            retry (Just p)
                        -- если был purException повторяем попытку
                        Left e -> do
                            logLS logger $ "decode failed: " ++ e
                            retry Nothing
                _ -> do
                    logLS logger $ "ps download failed (" ++ psHost ++ "): " ++ show dr
                    -- threadDelay $ 2000000 -- в retry есть задержка,
                    -- download failed -- это при перегрузке ParseServer-а,
                    -- exception-е или упавшем компе
                    -- при задержке в 2 секунды не мог обрабатывать более
                    -- 20-30 url/sec с одной упавшей нодой
                    retry Nothing
    f (req (parseServersCount*3) Nothing Nothing)
    where ds =
              C.def
              { dsTimeout = 900 -- как-то удаление фида заняло 11минут
              , dsMaxDownloadSize = psMaxDownloadSize
              , dsManagerSettings =
                  (dsManagerSettings C.def)
                  { C.managerConnCount = 150 }
              }

psMaxDownloadSize = 100*1024*1024

runParseServer =
    withDownloaderSettings botDownloaderSettings $ \ cd ->
    withDnsCache $ \ dnsCache -> do
    runEkgServer "localhost" parseServerEkgPort

    parseLimit <- MSem.new maxParallelParses
    adlLimit <- MSem.new maxParallelAdditionalDownloads
    host <- resolveHostEntry "private"
    runSettings
        (setPort parseServerPort $
         setHost (fromString $ showHostAddress host) $
         setTimeout 600
         defaultSettings
--         , settingsServerName = "ParseServer"
         ) $ parseServerClient cd dnsCache parseLimit adlLimit

testPRQ = withDownloaderSettings (botDownloaderSettings {dsUserAgent = "curl/7.50.1" }) $ \ cd -> withDnsCache $ \ dnsCache -> withWorkerRequestScheduler $ \ sc -> do
    let u = "https://www.opendemocracy.net/xml/rss/home/index.xml"
        hn = hostNameFromUrl u
        l = directLogger
    Right a <- resolveA dnsCache hn
--    print $ showHostAddress a
    s <- MSem.new 1
    t <- getUrTime
    r <- -- sc l (T.pack hn) ParseRequest
        processParseRequest cd s s dnsCache $
        ParseServerRequest
        { psrqUrl = T.pack u
        , psrqHostAddress = a
        , psrqRedownloadOptions = []
        , psrqDataHash = ""
        , psrqQueueTime = t
        , psrqSubscribersCount = 123
        , psrqProxyIndex = Nothing
        }
    case psrDownloadAndParseResult r of
        dpr@(DPROK {}) -> do
            print "OK"
            uts <- readUrlToScan' $ T.pack u
            t <- getUrTime
            let pur@(ProcessUrlRequest {..}) = decode d
                d = encode $ ProcessUrlRequest
                    { purqDownloadAndParseResult = dpr
                    , purqUrlToScan = uts
                    , purqQueueType = QTBlogFeed
                    , purqUtsUrl' = T.pack u
                    , purqQueueTime = t
                    , purqSubscribersCount = 2
                    , purqFromHub = False
                    , purqProxyIndex = Nothing
                    }
            print $ BL.length d
            r' <- do
--                  sc l (T.pack hn) ProcessRequest pur
                newUrls <- newIORef []
                modifyMVar_ (iuqCurTime iuq) $ return . const purqQueueTime
                parseLimit <- MSem.new maxParallelParses
                adlLimit <- MSem.new maxParallelAdditionalDownloads
                r <- processParseResult 0 cd parseLimit adlLimit dnsCache
                     purqQueueTime
                     newUrls l purqDownloadAndParseResult
                     purqUrlToScan iuq
                     purqQueueType purqSubscribersCount purqFromHub
                     purqProxyIndex
                     purqUtsUrl'
                readIORef newUrls
            print r'
        e -> print e

processParseRequest :: Downloader -> MSem.MSem Int -> MSem.MSem Int -> DnsCache
                    -> ParseServerRequest -> IO ParseServerResult
processParseRequest cd parseLimit adlLimit dnsCache (ParseServerRequest {..}) =
    withLogger $ \ l -> do
    let utsUrl = psrqUrl
    logT $ T.concat [ "Processing ", maskUrlPasswordT utsUrl ]
    (utsUrl_', u2, post) <- transformURL psrqUrl parserEnvironmentGet
--    logLT l $ T.concat [ "Fixed url ", utsUrl_'] -- может содержать api keys
    let (utsUrl', skipDownload)
            | utsUrl_' == "" = (utsUrl, True)
            | otherwise = (utsUrl_', False)
        hostName = hostNameFromUrlT utsUrl'
        needProxy = proxiedFeedDomain utsUrl'
        proxy forceProxy f rq
            | needProxy || forceProxy = f <$> addProxy psrqProxyIndex rq
            | otherwise = return $ f rq
        signReq forceProxy
            | hostName == "api.twitter.com" =
                showProxy <=< proxy forceProxy signTwitter
            | otherwise                     =
                showProxy <=< proxy forceProxy id
        addBotHeaders = botRequestHeaders utsUrl (max 1 psrqSubscribersCount)
        showProxy r@(C.proxy -> Just p) = do
            liftIO $ logLTL l ["Using proxy (", proxyName $ C.proxyHost p, ")"]
--             liftIO $ logLS l $ showHostAddress psrqHostAddress ++
--                 (if isJust (C.proxy r) then " proxied" else "")
            -- liftIO $ print r
            return r
        showProxy r = return r
        addCookieJar c r = r { C.cookieJar = Just c }
        addCurl r = r { C.requestHeaders = map fix $ C.requestHeaders r }
            where fix ("User-Agent", a) = ("User-Agent", "curl, " <> a)
                  fix x = x
        rateLimitDelay parseTime (_, Just rdr)
            | hostName == "graph.facebook.com"
            =   -- Дополнительная задержка, чтобы не получить.
                -- (#4) Application request limit reached
                -- Поскольку rate limit у FB зависит от числа
                -- активных пользователей, а также от некоего cpu_time,
                -- то он очень непостоянный и его надо отдельно проверять.
                -- С новым page based rate limit проблемы уже нет, но
                -- всё-таки оставляем проверку
                let usage = parseFacebookUsageRdr rdr
                    (msg, d) = case usage of
                        Left e -> do
                            ("Can’t parse facebook app usage: " <> T.pack e, 0)
                        Right (n, details) ->
                            (details
                            ,if n > 80 then (min n 100 - 80) * 2 else 0)
                in
                    Just (msg, max 0.0 (fromIntegral d - parseTime))
            | hostName == "api.twitter.com" = Just $
                case calculateTwitterRateLimitDelay (rdrHeaders rdr) of
                    Just (descr, delay) ->
                        (T.concat [T.pack $ showSecs delay, ", ", descr]
                        ,max 0.0 $
                         fromIntegral twitterWorkers * delay - parseTime)
                    Nothing ->
                        ("Can’t calculate Twitter rate limit delay"
                        ,fromIntegral twitterWorkers)
        rateLimitDelay _ _ = Nothing
        downloadWithAccessTokenOr other
            | hostName == "api.twitter.com" = do
               tryAccessTokens "Twitter" akTwitterAccessToken
                   (const id) signTwitterWithAccessToken other
            | otherwise = other
        tryAccessTokens :: Text -> (ApiKeys -> Maybe (UrTime, a))
            -> (a -> Text -> Text)
            -> (a -> C.Request -> IO C.Request)
            -> IO (DownloadResult, Maybe RawDownloadResult)
            -> IO (DownloadResult, Maybe RawDownloadResult)
        tryAccessTokens silo token fixUrl fixReq other = do
            ak <- feedUsersApiKeys psrqUrl
            let ats = [(u, k) | (u, token -> Just k) <- ak]
            go $ sortBy (comparing $ Down . fst . snd) ats
            where go [] = other
                  go ((u, (_,at)) : ats) = do
                      logLTL l ["Using ", silo, " access token of ", u]
                      dr <- logTime l "download" $ rawDownload
                          (fixReq at
                           . maybe id (postRequest . tbs . fixUrl at . bst) post
                           . addBotHeaders)
                          cd
                          (T.unpack $ fixUrl at utsUrl')
                          (Just psrqHostAddress) []
                      let err rmKey msg = do
                              logLTL l
                                  ["User ", u
                                  ," is not authenticated anymore:\n"
                                  ,msg
                                  ,"\nRemoving user’s access token"]
                              updApiKeys u rmKey
                              go ats
                      case dr of
                          (DRError _, Just (rdrBody -> b))
                              | isFacebookOAuthException b -> do
                                  err (\ k -> k { akFacebookAccessToken = Nothing })
                                      (either T.pack id $ parseFacebookError b)
                              | Right e <- twitterErrors b
                              , eText <- T.unlines
                                  [showT c <> ": " <> m | (c,m) <- e]
                              ->  if notNull $ intersect (map fst e)
                                      badTwitterTokenErrors
                                  then
                                      err (\ k -> k { akTwitterAccessToken = Nothing })
                                          eText
                                  else do
                                      logLTL l
                                          ["Twitter error:\n", eText
                                          ,"retrying with another access token"]
                                      -- бывают закрытые/удаленные аккаунты,
                                      -- и даже You have been blocked
                                      -- from viewing this user's profile.
                                      -- В таких случаях токен оставляем
                                      go ats
                          _ ->
                              return dr
        badTwitterTokenErrors =
            [89 -- Invalid or expired token. (The access token used in the request is incorrect or has expired.)
            ,64 -- Your account is suspended and is not permitted to access this feature (Corresponds with HTTP 403. The access token being used belongs to a suspended user.)
            ,99 -- Unable to verify your credentials. (Corresponds with HTTP 403. The OAuth credentials cannot be validated. Check that the token is still valid.)
            -- 32  	Could not authenticate you (Corresponds with HTTP 401. There was an issue with the authentication data for the request.)
            -- 135 Could not authenticate you (Corresponds with HTTP 401. Timestamp out of bounds (often caused by a clock drift when authenticating - check your system clock))
            --  ^ это уже ошибки протокола, а не токена
            ]
        tryDL forceProxy curlUserAgent cookieJar n
            | Just (range, scope, desc) <- reservedHostAddress psrqHostAddress =
                return ((DRError "", Nothing), DPRError $ T.concat
                    ["Fetching from reseved IP addresses is prohibited.<br/>"
                    ,"IP address ", T.pack $ showHostAddress psrqHostAddress
                    ," is in ", showT range, " range.<br/>"
                    ,"Range scope: ", scope, ".<br/>"
                    ,"Range description: ", desc])
            | otherwise = do
            dr <- downloadWithAccessTokenOr $
                logTime l "download" $ rawDownload
                    (maybe id (fmap . postRequest) post . signReq forceProxy .
                     (if curlUserAgent then addCurl else id) .
                     addBotHeaders .
                     addCookieJar cookieJar)
                    cd
                    (T.unpack utsUrl')
                    (if needProxy || forceProxy then
                         Nothing
                     else
                         Just psrqHostAddress)
                    -- по-идее, прокси сам найдет IP-шник
                    (map T.unpack psrqRedownloadOptions)
            dr <- case (u2, dr) of
                (Just (T.unpack -> url2, cat), (DROK d1 rdo1, rdr)) -> do
                    r2 <- logTime l "download2" $
                         rawDownload return cd url2
                                      Nothing []
                                      -- (Just hostAddr) []
                         --  ^ второй url только в googleapis,
                         --  так что не паримся по поводу hostAddr
                    case r2 of
                        (DROK d2 _,_) -> return (DROK (cat d1 d2) rdo1, rdr)
                        r -> return r2
                _ -> return dr
            let ret x = return (dr, x)
                err e = do
                    logLT l e
                    return (dr, DPRError e)
            case fst dr of
                DRNotModified
                    | psrqRedownloadOptions == [] ->
                        err "Got 'HTTP 304 Not Modified' on first download"
                    | otherwise -> do
                        logLT l "Not modified"
                        ret DPRNotModified
                DRError e
                    | "HTTP 5" `isPrefixOf` e -> do
                        logLT l "Server error, let’s pause domain"
                        threadDelay $ 8*1000*1000
                        if n == 1 then
                            tryDL forceProxy curlUserAgent cookieJar 2
                        else
                            err $ T.pack e
                        -- при HTTP 503 пытаемся выкачать url еще раз
                        -- а то с подписками беда
                    | Just rdr <- snd dr
                    , hostName == "graph.facebook.com"
                      -- возвращает HTTP 4xx с текстом ошибки
                    , Right e <- parseFacebookError (rdrBody rdr) ->
                        err e
                    | Just rdr <- snd dr
                    , hostName == "api.twitter.com"
                    , rdrStatus rdr == unauthorized401 ->
                        err "This account either does not exists, suspended or its tweets are protected."
                    | otherwise -> do
                        threadDelay $ 1*1000*1000
                        -- чуть еще подождем в случае ошибки,
                        err $ T.pack e
                DRRedirect ru
                    | Just retry <- selfRedirect forceProxy curlUserAgent "Redirect" ru (snd dr) n ->
                        retry
                DRRedirect (T.pack . normalizeURL -> r)
                    | r == utsUrl
                      && "https://t.me/" `T.isPrefixOf` utsUrl
                      && "https://t.me/s/" `T.isPrefixOf` utsUrl' ->
                        -- если канала нет, то перенаправляет обратно
                        err $ Parser.Telegram.telegramError utsUrl
                    | r == utsUrl || r == utsUrl' ->
                        err $ T.append "Redirect to the same url? " r
                    | "https://www.instagram.com/accounts/login" `T.isPrefixOf` r -> do
--                         logLT l "Instagram account redirect, pausing"
--                         if n == 1 then
--                             tryDL forceProxy curlUserAgent cookieJar 2
--                             -- часто может скачать со второго раза
--                         else
                        err "Instagram rate limit. Please, try again later."
                    | "livejournal.com/" `T.isInfixOf` r && "&updplz" `T.isInfixOf` r ->
                        err $ T.append "Strange LJ redirect? " r
                        -- 24.10.12  16:34:55-18:15:02 ЖЖ зачем-то редиректил сам на себя
                        -- http://vshabanov.livejournal.com/845.html
                        -- Redirect to http://vshabanov.livejournal.com/845.html?nojs=1&format=light&updplz106061
--                     | hasCustomParser utsUrl && not hasCustomParser (T.pack r) -> do
--                         err $ "Redirect on custom parsed URL:<br/>" ++ r
--         http://habrahabr.ru/post/156783/ -> http://habrahabr.ru/company/apps4all/blog/156783/ нормальный redirect
                    | any (`T.isInfixOf` T.toLower r) ["gdpr", "consent"]
                      && not (forceProxy || needProxy) -> do
                        logLT l $ T.append "GDPR redirect? Trying again via proxy " r
                        tryDL True curlUserAgent cookieJar n
                    | "tumblr.com/safe-mode" `T.isInfixOf` r
                      && not curlUserAgent -> do
                        logLT l $ T.append "Tumblr safe-mode? Trying again with curl User-Agent" r
                        tryDL True True cookieJar n
                    | otherwise -> do
                        logLT l $ T.append "Redirect to " r
                        ret $ DPRRedirect r
                DROK dat opts -> do
                    let dHash = hashData dat
                    pr <- MSem.with parseLimit $ do
                        logTime l "parse   " $ do
                            let pr = parse psrqQueueTime (T.unpack utsUrl) dat
                            pr `seq` return pr
                    if | PRAdditionalDownload w u c <- pr ->
                        (dr,) <$> processAdditionalDownload w u c
                       | PRRedirect ru <- pr
                       , Just retry <- selfRedirect forceProxy curlUserAgent
                           "Parsed redirect" (T.unpack ru) (snd dr) n ->
                        retry
                       | dHash == psrqDataHash -> do
                        logLT l "Same hash"
                        let hubs = case pr of
                                       PRFeed baseUri fm _ ->
                                           [ fromRelUrl (T.unpack baseUri) u
                                           | (LKHub, u) <- fixFmLinks fm ]
                                       _ -> []
                        ret $ DPRSameHash hubs
                       | otherwise ->
                        ret $
                            DPROK
                            { dprRedownloadOptions = map T.pack opts
                            , dprDataHash = dHash
                            , dprPr = pr
                            }
        selfRedirect forceProxy curlUserAgent prefix ru mbRdr n
            | u <- T.pack $ normalizeURL ru
            , Just rdr <- mbRdr
            , (u == utsUrl || u == utsUrl') && n <= 1 = Just $ do
                threadDelay $ 200*1000
                let cj = rdrCookieJar rdr
                    cookies = C.destroyCookieJar cj
                logLS l $ prefix ++ " to the same url " ++ ru
                when (utsUrl' /= u) $
                    logLS l "Remark that downloaded url is different"
                    -- был баг -- урлы google news трансформировались
                    -- из HTTPS в HTTP, а гугл редиректил их обратно в HTTPS
--                 logLS l $ "Need proxy " ++ show needProxy
--                 forM_ (rdrHeaders $ fromJust $ snd r) $ \ h ->
--                    logLS l $ show h
--                 logLS l $ show $ rdrBody $ fromJust $ snd r
                logLT l $ if null cookies then "No cookies" else "Cookies:"
                forM_ cookies $ \ c ->
                    logLT l $ T.concat
                        [bst $ C.cookie_name c, "=", bst $ C.cookie_value c]
                tryDL forceProxy curlUserAgent cj (n+1)
                -- бывают долбанутые фиды, у которых идет redirect
                -- сам на себя, и он действительно в следующий раз
                -- правильно срабатывает.
            | otherwise = Nothing
        processAdditionalDownload what urls (CombineDownloads combine) = do
            dls <- forM urls $ \ u -> async $ MSem.with adlLimit $ do
                logLT l $ T.concat ["Downloading ", what, " ", rmKeysFromUrl u]
                let hn = hostNameFromUrl $ T.unpack u
                    err e = do
                        logLT l e
                        return $ Left $ T.concat
                            ["Can’t download ", what
                            , " (", rmKeysFromUrl u, "): ", e]
                mbIP <- resolveA dnsCache hn
                incrStat "resolveCount" 1
                case mbIP of
                    Left e -> do
                        incrStat "dnsErrors" 1
                        err $ T.concat [ "Can’t resolve “"
                            , humanReadableHostName (T.pack hn), "”: ", e]
                    Right hostAddress -> do
                        let tryAdditionalDL n = do
                                dr <- logTime l "downloadA" $
                                    download cd
                                    (T.unpack u) (Just hostAddress) []
                                case dr of
                                    DROK d _ -> return $ Right d
                                    DRError e
                                        | "HTTP 5" `isPrefixOf` e && n < 2 -> do
                                            logLT l "Server error, retrying"
                                            -- YouTube может при перегрузке
                                            -- выдать HTTP 500
                                            threadDelay $ 5*1000*1000
                                            tryAdditionalDL 2
                                        | otherwise ->
                                            err $ T.pack e
                                    DRNotModified -> err "Not modified??"
                                    DRRedirect r ->
                                        err $ T.concat
                                        [ "Unexpected redirect from "
                                        , rmKeysFromUrl u, " to "
                                        , rmKeysFromUrl $ T.pack r ]
                        tryAdditionalDL 1
            dls <- mapM wait dls
            pr <- MSem.with parseLimit $ do
                logTime l "combine " $ do
                    let pr = combine dls
                    pr `seq` return pr
            case pr of
                PRAdditionalDownload w u c ->
                    processAdditionalDownload w u c
                pr ->
                    return $
                        DPROK
                        { dprRedownloadOptions = []
                        , dprDataHash = ""
                        , dprPr = pr
                        }
        showExn :: E.SomeException -> String
        showExn = show
        dl = do
            startTime <- getUrTime
            (dr, r) <- tryDL False (".tumblr.com" `T.isSuffixOf` hostName) (C.createCookieJar []) 1
            t' <- getUrTime
            lt <- loggerGetAndClean l
            return $ ParseServerResult
                { psrDownloadAndParseResult = r
                , psrRateLimitDelay =
                    rateLimitDelay (diffUrTime t' startTime) dr
                , psrLog = lt
                }
    dl `E.catch` \ e -> do
        let t = T.pack $ "Exception: " ++ showExn e
        logLT l t
        lt <- loggerGetAndClean l
        return $ ParseServerResult (DPRError t) Nothing lt

parserEnvironmentGet k =
    peValue <$> readParserEnvironment' k
parserEnvironmentSet l e =
    forM_ e $ \ (k,v) -> do
        logLT l $ "SetEnv " <> k <> " = " <> v
        mergeWriteParserEnvironment $ ParserEnvironment k (Just v)

feedUsersApiKeys feed = do
    ps <- readPostsSubscribers' feed
    us <- mapM readUserSettings $ HS.toList $ psSubscribers ps
    return [(u, k) | Just (UserSettings { ustUser = u, ustApiKeys = Just k }) <- us]

parseServerClient cd dnsCache parseLimit adlLimit req respond = respond =<< do
    body <-
        fmap (BL.fromStrict . fromMaybe "") $
        sinkRequestBody psMaxDownloadSize req
    return $ responseStream status200 [] $ \write flush -> do
        flush
        -- C.yield C.Flush -- чтобы заголовки ушли и responseTimeout не сработал
        rsp <- liftIO $ case filter (/= "") $ pathInfo req of
            ["parse"] ->
                fmap encode $
                processParseRequest cd parseLimit adlLimit dnsCache $
                decode body
            ["process"] -> do
                let ProcessUrlRequest {..} = decode body
                    showExn :: E.SomeException -> Text
                    showExn = T.pack . show
                l <- newLogger
                newUrls <- newIORef []
                modifyMVar_ (iuqCurTime iuq) $ return . const purqQueueTime
                r <- E.try $ processParseResult 0 cd
                     parseLimit adlLimit dnsCache
                     purqQueueTime
                     newUrls l purqDownloadAndParseResult
                     purqUrlToScan iuq
                     purqQueueType purqSubscribersCount purqFromHub
                     purqProxyIndex
                     purqUtsUrl'
                log <- loggerGetAndClean l
                nu <- readIORef newUrls
                sm <- getStatsMapAndClean
--                 when (isLeft r) $ do
--                     t <- getUrTime
--                     BL.writeFile (urTimeFileName t ++ ".ProcessRequest.received.bin") body
                return $ encode $
                    ProcessUrlResult
                    { purNewUrls = nu
                    , purLog = log
                    , purException = either (Just . showExn) (const Nothing) r
                    , purStatsMap = sm
                    }
            _ ->
                return "invalid request"
        write $ fromLazyByteString rsp
--        C.yield $ C.Chunk $ copyLazyByteString rsp
--         yield C.Flush
--         return ()
--    return $ ResponseBuilder status200 [] $ copyLazyByteString rsp

-- записывать, что отправляем и сверять с тем, что получаем
-- записывать exception-ы и смотреть
-- дата/время/url

------------------------------------------------------------------------------
-- Обработка URL-а

scanListStep = (5 % 10) :: Ratio Int -- секунд
scanListStepU = truncate $ 1000000 * scanListStep
roundScanTime (UrTime s u) = UrTime s ((u `div` scanListStepU) * scanListStepU)
saveUrl time typ url =
    void $ mergeWriteScanList $ ScanList (roundScanTime time) [(url, typ)]

processParseResult :: UQ uq
                   => Int -> Downloader -> MSem.MSem Int -> MSem.MSem Int
                   -> DnsCache -> UrTime
                   -> IORef [AddUrl] -> Logger -> DownloadAndParseResult
                   -> UrlToScan -> uq
                   -> QueueType -> Int -> Bool -> Maybe Int -> TURL -> IO ()
processParseResult redirectCount cd parseLimit adlLimit dnsCache t
                   newUrls l dr u@(UrlToScan {..})
                   uq qt subscribers fromHub proxyIndex utsUrl' = do
    let dlError e = do
            urlError uq True l utsUrl e
            count u "downloadErrors"
            addRescan' (RKError 3) l uq id utsUrl qt
        loopRedirect r = do
            when (r /= utsUrl) $
                deleteSubscriptionUrlInfo $ defaultSubscriptionUrlInfo utsUrl
            --  ^ чтобы при следующей подписке искали новый фид
            --  учитываем странные фиды, которые перенаправляются сами на себя

            incrStat "blogRedirects" 1
            logLT l $ T.concat ["Blog redirected to ", maskUrlPasswordT r]
            hostName <- transformedUrlHostName r
            mbIP <- resolveA dnsCache (T.unpack hostName)
            incrStat "resolveCount" 1
            case mbIP of
                Left err -> do
                    incrStat "dnsErrors" 1
                    dlError $
                        T.concat ["Can’t resolve redirected “",
                                  humanReadableHostName hostName, "”: ", err]
                Right hostAddress -> do
                    dr' <- fmap psrDownloadAndParseResult $
                        processParseRequest cd parseLimit adlLimit dnsCache $
                        ParseServerRequest
                        { psrqUrl = r
                        , psrqHostAddress = hostAddress
                        , psrqRedownloadOptions = []
                        , psrqDataHash = ""
                        , psrqQueueTime = t
                        , psrqSubscribersCount = subscribers
                        , psrqProxyIndex = proxyIndex
                        }
                    case dr' of
                        DPROK _ _ (PRFeed {}) -> do
                            incrStat "blogRedirectsFeed" 1
                            logLT l "Redirected blog parsed as feed"
                        _ ->
                            return ()
--                     processRedirect newUrls l uq u r qt
--                     count u "redirects"
--                     cancelRescans l uq id utsUrl qt
                    processParseResult (redirectCount+1) cd
                        parseLimit adlLimit dnsCache
                        t newUrls l dr' u uq qt subscribers fromHub proxyIndex r

    case dr of
        DPRNotModified -> do
            logLT l "not modified"
            count u "notModified"
            addRescan l uq id utsUrl qt
            updateBlogFavicon l u
        DPRError e ->
            dlError e
        DPRRedirect r | qt == QTBlogFeed && redirectCount < 4 ->
            loopRedirect r
        DPROK _ _ (PRRedirect r) | qt == QTBlogFeed && redirectCount < 4 ->
            loopRedirect r
        DPRRedirect r -> do
            processRedirect newUrls l uq u r qt
            count u "redirects"
            cancelRescans l uq id utsUrl qt
        DPRSameHash hubs -> do
            logLT l "Same hash"
            count u "sameHash"
            hf <- newIORef False
            listenToHubs l (writeIORef hf True) hubs utsUrl utsUrl'
            hubFound <- readIORef hf
            uts <- readUrlToScan' utsUrl
            setBlogPostsScannedUts uq uts [(utsUrl, CUSError "Same hash?")]
            addRescan' (if hubFound then RKHubFound else RKNormal 0)
                       l uq id utsUrl qt
            updateBlogFavicon l u
        DPROK opts dHash pr ->
            processData newUrls l uq (T.unpack utsUrl') u pr opts dHash qt utsUrl' fromHub
        DPRToRemove -> do
            logLT l "Removing feed"
            count u "removed"
            removeFeed l uq utsUrl

removeFeed l uq utsUrl = do
    modifyPosts'_ utsUrl $ \ p0 -> do
        p <- removeOldPosts' 0 l uq p0
        let mt = pMsgTree p
        return $ p { pMsgTree =
                         mt { mtHeaders =
                                  BA.listArray
                                  (mkDeletedPostsBounds $
                                   BA.bounds $ mtHeaders mt)
                                  [] } }
    deleteSubscriptionUrlInfo $ defaultSubscriptionUrlInfo utsUrl
    --  ^ на всякий пожарный, хотя SUI появляется после обновления постов,
    --  которое сейчас заблокировано
    uts <- readUrlToScan' utsUrl
    when (null $ utsSubscriptionParentPaths uts) $ do
        atomicDeleteUrlToScan uts
        deleteBlogPostsScanned $ defaultBlogPostsScanned utsUrl
    -- к моменту окончания удаления старых постов может появиться
    -- запрос на подписку в UTS. Делаем дополнительную проверку,
    -- чтобы уменьшить окно времени.

writeParseError url dat = do
    return ()
--     t <- getUrTime
--     let p = path ++ urTimeFileDir t
--     createDirectoryIfMissing True p
--     B.writeFile (p ++ "/" ++ (take 80 $ escapeQueryValue (T.unpack url))) dat
--     where path | os == "darwin" = "parse_errors/"
--                | otherwise = "/tank/crawler/parse_errors/"

processData :: UQ uq =>
               IORef [AddUrl] -> Logger -> uq -> String
            -> UrlToScan -> ParseResult
            -> [Text] -> Text -> QueueType -> TURL -> Bool -> IO ()
processData newUrls l uq url u@(UrlToScan {..}) pr opts dHash qt utsUrl' fromHub = do
    let updUts u =
            u { utsDataHash = dHash, utsRedownloadOptions = opts }
    case pr of
        PRSetEnv e pr' -> do
            parserEnvironmentSet l e
            processData newUrls l uq url u pr' opts dHash qt utsUrl' fromHub
        PRRedirect r
            | r == utsUrl || r == utsUrl' -> do
                urlError uq False l utsUrl $ "Parsed redirect to the same url? " `T.append` r
                count u "downloadErrors"
                addRescan' (RKError 5) l uq updUts utsUrl qt
        PRRedirect r -> do
            processRedirect newUrls l uq u r qt
            count u "p_redirect"
            cancelRescans l uq updUts utsUrl qt
        PRError e -> do
            urlError uq False l utsUrl $
                T.replace "\n" "<br/>" $ T.append "Can’t parse:\n" e
            count u "parseErrors"
            addRescan' (RKError 5) l uq updUts utsUrl qt
            -- writeParseError utsUrl dat
        PRAdditionalDownload _ _ _ -> do
            fail "PRAdditionalDownload in processData?"
        PRHtml {..} -> do
            let list _ _ [] = []
                list name f x = name : map f x
                comm (JSCSupported c) = T.append "  " c
                comm (JSCUnsupported c) = T.append "  x " c
            logLT l $ T.intercalate "\n" $ ("HTML":) $
                list "feeds:" (T.append "  ") prHtmlFeeds ++
                list "commentFeeds:" (T.append "  ") prCommentFeeds ++
                list "jsComments:" comm prJsComments
            let
--                 feedSuffix =
--                     case prCommentFeeds of
--                         [] -> False
--                         (x:xs) -> utsUrl `T.isPrefixOf` x &&
--                                   "http://www.kraynov.com" `T.isPrefixOf` x
--                                   --  ^ для других фидов нельзя гарантировать,
--                                   -- что они не пересекаются с disqus
                supportedJsc = [u | JSCSupported u <- prJsComments]
                unsupportedJsc = [u | JSCUnsupported u <- prJsComments]
            t <- uqGetUrTime uq
            modifyUrlToScan'_ utsUrl $ \ uOrig@(UrlToScan {..}) -> do
                -- ситуации:
                --   Подписка  -- обрабатываем только фиды (нет -- ошибка)
                --   Блог -- ничего не делаем
                --           (cancelRescans оставит пересканирование)
                --   Коммент -- есть unsupported js и нет supported -- ошибка
                --              иначе supported js или фид
                let sub = notNull utsSubscriptionParentPaths
                    -- blog = isBlogUts uOrig
                    comments = isCommentUts uOrig
                    dbg = "" -- T.pack $ show pr
--                    commentFeeds = supportedJsc ++ selectCommentFeed utsUrl prHtmlFeeds
                when sub $
                    if null prHtmlFeeds then do
                        logLT l $ "Subscription error: no feeds"
                        subError t False utsSubscriptionParentPaths
                                 "Can’t find any feed on the specified URL"
                    else do
                        logLT l $ T.concat [ "Redirect to (subscription) "
                                           , head prHtmlFeeds ]
                        addChildUrl newUrls PushFront
                            False l uq (uOrig { utsParentPaths = [] }) t t
                            (SpuHtml (head prHtmlFeeds) dbg)
                            id False False qt
                when comments $
                    let redir spu what r pps = do
                            let uts' = uOrig
                                       { utsSubscriptionParentPaths = []
                                       , utsParentPaths = pps
                                       }
                            logLT l $ T.concat [ "Redirect to (", what, ") ", r]
                            addChildUrl newUrls PushFront
                                False l uq
                                uts' (Generated.DataTypes.utsModifyTime uOrig) t
                                (spu r dbg)
                                id False (qt == QTNewComment1) qt
                            setBlogPostsScannedUts uq uts' [(utsUrl, CUSRedirect r)]
                    in
                    if notNull unsupportedJsc && null supportedJsc then do
                        let msg = "Unsupported js comments"
                        logLT l msg
                        count u "jsComments"
                        setBlogPostsScannedUts uq uOrig [(utsUrl, CUSError msg)]
                    else if notNull supportedJsc then do
                        redir SpuHtml "js comment" (head supportedJsc) $
                              filter (notNull . ppParents)
                                     -- только для комментариев
                                     utsParentPaths
                    else do
                        forM_ utsParentPaths $ \ pp ->
                            case dropWhile (not . isFeed) (ppParents pp) of
                                pf@(PuFeed {}) : PuCommentsFeed cf : pps ->
                                    redir (\ r _ -> SpuRedirect r)
                                          "comments rss" cf
                                        [pp { ppParents = pf : pps}]
                                _ | Just cf <- selectCommentFeed
                                                 utsUrl prHtmlFeeds ->
                                    redir SpuHtml "comments feed" cf [pp]
                                _ -> do
                                    let msg = "No comment feeds"
                                    logLT l msg
                                    setBlogPostsScannedUts uq
                                        (uOrig { utsParentPaths = [pp] })
                                        [(utsUrl, CUSError msg)]
                return $
                    uOrig
                    { utsSubscriptionParentPaths = []
                    , utsParentPaths = filter (null . ppParents) utsParentPaths
                    }
            count u "html"
            cancelRescans l uq updUts utsUrl qt

        PRFeed (T.unpack -> bu) fm fms -> do
            logLS l $ "Feed (" ++ show (length fms) ++ " posts)"
            processFeedItems newUrls Nothing False fromHub updUts l u uq bu
                             NoSwitch fm (sortFms utsUrl bu fms) id qt utsUrl'
            count u "feed"
        PRParsedHtml msg swUrl msgs links -> do
            logLS l $ "Parsed HTML (" ++ show (length msgs) ++ " msgs, "
                ++ show (length links) ++ " links)"
--            logLS l $ show $ length msgs
            --let ru (p, u) = (p, B.pack $ relUri u url)
            t <- uqGetUrTime uq
            processFeedItems newUrls msg False fromHub updUts l u uq url
                             swUrl defaultFeedMsg msgs
                             (const [(fmap tsb g,l,[],t,False,gs) | (g,l,gs) <- links])
                             qt utsUrl'
            count u "parsedHtml"
            -- parsedHtml p u (map' toMsg msgs) links

    return ()

sortFms utsUrl baseUri fms =
    map ((Nothing,) . snd) $ reverse $ takeWhile ((< 500) . fst) $
    (if hasCustomParser utsUrl then id else go HS.empty HM.empty []) $
    --  ^ доверяем нашим parser-ам и их id-шкам, хотя возможно стоит
    --  доверять только парсерам, работающим через API.
    --  (доверять id нужно, т.к. иногда facebook выдает несколько постов
    --  c одним и тем же guid, но чуть разными ссылками на видео внутри,
    --  и в итоге, вместо обновления поста, появляется куча почти одинаковых
    --  постов)
    zip [0..] $
    if all (isJust . feedMsgTime) fms then
        sortBy (flip $ comparing feedMsgTime) fms
    else
        fms -- если где-то времени нет, ничего не меняем, т.к. новые посты
        -- без времени могут оказаться за 100 постов до постов с временем
        -- и не добавиться
    where -- убираем сообщения с повторяющимися guid-ами, копим ссылки,
          -- а затем помечаем повторяющиеся ссылки
          go !g !l !acc [] = filt l [] acc
          go !g !l !acc ((i,fm):fms)
              | guid /= "" && HS.member guid g = go g l acc fms
              | otherwise =
                  go (HS.insert guid g)
                     (HM.insertWith (+) link 1 l)
                     ((link,i,fm) : acc) fms
              where link = fromMaybe "" $ fmMsgLink baseUri fm
                    guid = T.strip $ fmGuid fm
          filt l !acc [] = acc
          filt l !acc ((link,i,fm):fms) =
              filt l ((i, fm { fmLinkNotUnique = HM.lookupDefault 0 link l > 1
                             }) : acc) fms

-- обрабатываем только последние 500 элементов, т.к. больше у нас
-- все равно не хранится
-- а добавляем элементы начиная со старых (reverse),
-- чтобы modifyCleanPosts их правильно подчищал.

-- Хабр и жж не всегда удаляют
-- version conflict, current [2], provided [1]
-- Видимо, относится

selectCommentFeed url fs =
    find feed fs <|>
    t "feeds/[0-9]+/comments/default" <|> -- blogspot
--    t "\\?feed=rss2&p=[0-9]+$" <|>
      -- http://blog.felipe.lessa.nom.br/?feed=rss2&p=68
      -- http://msinilo.pl/blog/?feed=rss2&#038;p=965
      --  у них commentsRss есть, так что нефиг
    t "/cgi-bin/openforum/rss_forum.cgi\\?forum=.*&om=[0-9]+" <|>
    t "roem.ru/rss/comments/[0-9]+" <|>
      -- http://roem.ru/rss/comments/55947/
    t "/feed-atom-topic[0-9]+.xml" <|>
      -- http://it-talk.org/feed-atom-topic15912.xml
    t "/feeds/question/[0-9]+"
      -- http://stackoverflow.com/feeds/question/13156944
--    t "rss.juick.com/[0-9]+"
      -- http://rss.juick.com/2114135
      -- https://ocaml.janestreet.com/?q=crss/node/110
    where feed u = suffix "feed" u
                   -- …my-post-title/feed/ wordpress-а
                || suffix "feed/atom" u
                || suffix "feed/rss" u
                || suffix "comments/atom.xml" u -- typepad
                || suffix "comments/rss.xml" u
                || suffix "?output=rss" u
                || suffix "?output=atom" u
          suffix s u
              = T.append (addSlash url) (addSlash s) == addSlash u
                || T.append (addSlash (rmHtml url)) (addSlash s) == addSlash u
                || T.append url (addSlash s) == addSlash u
          t x = find (regexTest x) fs
          rmHtml u
              | T.isSuffixOf ".html" u = T.take (T.length u - 5) u
              | otherwise = u
          addSlash "" = ""
          addSlash x
              | T.index x (T.length x - 1) == '/' = x
              | otherwise = T.append x "/"

-- TODO: добавление link-ов очень похоже, только из исходного
-- uOrig ничего не надо удалять
processRedirect newUrls l uq u r qt = void $ do
    logLT l $ T.concat ["Redirect to ", r]
    t <- uqGetUrTime uq
    modifyUrlToScan'_ (utsUrl u) $ \ uOrig -> do
        let uOrig_r =
                uOrig
                { utsParentPaths =
                      filter (notNull . ppParents) $ utsParentPaths uOrig }
                -- не делаем redirect для блогов
                -- TODO: или все-таки стоит???
        addChildUrl newUrls PushFront False l uq uOrig_r (utsModifyTime uOrig) t (SpuRedirect r) id
              False -- temporary для задания более высокого приоритета
                    -- нет, temporary удаляются, а нам этого не надо
                    -- надо как-то хитро в начало очереди comment1 добавлять,
                    -- а не в конец
              (qt == QTNewComment1)
              qt
        setBlogPostsScannedUts uq uOrig [(utsUrl u, CUSRedirect r)]

        -- TODO: а может вообще удалять url после redirect-а
        -- или может добавить волшебный флаг isEmpty/isDefault,
        -- и удалять автоматом???
        return $
            uOrig
            { utsSubscriptionParentPaths = []
            , utsParentPaths =
                filter (null . ppParents) $ utsParentPaths uOrig
              -- TODO: по-идее, здесь стоит оставлять ParentPath url [],
              -- если он был, чтобы после какого-нить кривого html продолжать
              -- работать??? а надо ли???
--            , utsModifyTime = t
-- не надо менять на redirect-е, все равно они не пересканируются
            }

--numScanLists = 10000
-- пока делаем кучу случайных сканлистов, а то получился scanlist на 3 мега
-- и мы можем добавить только 10-20 url-ов в секунду
-- Кстати, ZFS при таких больших записях ведет себя намного лучше ext4
-- ext4 -- 90% utilization, zfs -- 5%
--         20 wMB/s                4 wMB/s
-- (хотя на ext4 еще система, куча логов и бд на 700 мегов,
--  а zfs на чистом диске).
--

addChildUrl newUrls delay rescanAdd l uq uOrig tParent t spu plusPps temporary priority qtOrig
    | r == utsUrl uOrig = do
        goSPP [] $ utsSubscriptionParentPaths uOrig
        --  ^ дабы ошибки вывело
        logLT l "err: Redirect URL is the same as orig url"
      --  ^ сразу отваливаемся и не пытаемся рекурсивно модифицировать UrlToScan
    | null (utsParentPaths uOrig) && null (utsSubscriptionParentPaths uOrig)
      --  ^ не к чему child-а добавлять
      = logLT l "err: empty parent paths"
    | otherwise = logTime l "addChildUrl" $ void $ do
--    add <- newIORef (return ())
--    logLBS l "addChildUrl/modifyUrlToScan"
    spps <- goSPP [] $ utsSubscriptionParentPaths uOrig
    pps <- goPP [] $ utsParentPaths uOrig
    when (notNull spps || notNull pps) $ do
        (utsR, keep) <- logTime l "modifyUrlToScan" $ atomicModifyUrlToScan' r $ \ uRedir -> do
            let uRedir' =
                    uRedir
                    { utsSubscriptionParentPaths =
                        unionByWith sppSubscriptionUrl const
                        (utsSubscriptionParentPaths uRedir) spps
                    , utsParentPaths = unionByWith ppBlogFeedUrl const
                        (utsParentPaths uRedir) pps
                        --  ^ для одинаковых фидов оставляем начальный путь
                    , utsModifyTime =
                        if utsModifyTime uRedir == UrTime 0 0 && tParent /= t
                        then tParent else utsModifyTime uRedir
                      -- ???? вроде правильно
                    }
            let qt | temporary = QTTemporary1
                   | priority  = QTNewComment1
                   | otherwise = QTNewComment
                qtSub
                   | qtOrig == QTSubscriptionOPML = QTSubscriptionOPML
                   | otherwise = QTSubscription
            let immediateScan cause qt = do
                    logLT l $ "immediateScan " `T.append` cause
--                    [sl] <- saveNewUrls [(r, qt)]
                    setBlogPostsScannedUts uq uRedir' [(r, CUSNew)]
                    let time = newUrlScanListKey r
                    modifyIORef' newUrls (AddUrl delay qt time r :)
--                     writeIORef add $ writeChan (uqAddNewUrl uq) $
--                         addUrl delay uq qt (slTime sl) r
--                         в переменную
                    return (uRedir' { utsNextScanTime = time }, True)
            (uRedir'',keep) <- if utsNextScanTime uRedir /= UrTime 0 0 then do
                if notNull spps then
                    immediateScan "force subUrl" qtSub
                else if uRedir' /= uRedir then
                    immediateScan "force url" qt
                --  ^ форсированно добавляем url в очередь,
                --  даже если он есть в обычной очереди
                else
                    return (uRedir', True)
            else if notNull spps then
                -- url еще не в очереди, но он участвует в подписке
                immediateScan "new subUrl" qtSub
            else if uRedir' /= uRedir || temporary then do
                -- url не в очереди и это не урл подписки и мы тут еще не были
                -- или уже были и снесли temporary url
--                        | diffParentTime < day = QTNewComment1
--                        | otherwise = QTNewComment
                if rescanAdd then do
                    logLT l "Rescan add (no immediate scan)"
                    -- setBlogPostsScannedUts uRedir' [(r, CUSNew)]
                    --  ^ не нужно, комментов ведь еще нет
                    addRescan'' (RKNormal 0) l uq t
                        (uRedir' { utsModifyTime = tParent }) qt
                else
                    immediateScan "new url" qt
            else -- if utsNextScanTime uRedir == UrTime 0 0 then
                immediateScan "addChildUrl rescan forced" QTNewComment1
            let r = if uRedir'' /= uRedir || temporary || notNull spps then
                        uRedir'' { utsRedownloadOptions = [], utsDataHash = "" }
                        --  ^ что-то поменялось, надо повторно обработать,
                        --    а не отвалиться с not modified
                    else
                        uRedir
            return (r,(r, keep))
        logLS l $ "diffParentTime " ++ showSecs diffParentTime
--         when (notNull (utsSubscriptionParentPaths utsR)
--               || notNull (utsParentPaths utsR)) $
--             join $ readIORef add
        when (not keep) $ deleteUrlToScanC l utsR

--    logLBS l "addChildUrl/addedUrl"
    where diffParentTime = t `diffUrTime` tParent
          r = spuUrl spu
          pu = case spu of
                 SpuRedirect r -> PuRedirect r
                 SpuHtml r d -> PuHtml r d
          goSPP acc [] = return acc
          goSPP acc (spp@(SubscriptionParentPath {..}) : spps)
              | length sppParents > 10 = do
                  subscriptionError uq l spp $ "Too many redirects:" : urls
                  goSPP acc spps
              | r `elem` urls = do
                  subscriptionError uq l spp $
                      "Redirect loop found:" : r : "is already in" : urls
                  goSPP acc spps
              | isHtml spu && any isHtml sppParents = do
                  subscriptionError uq l spp $
                      "HTML found instead of feed at " : head urls : "" :
                      "path:" : tail urls
                  goSPP acc spps
              | otherwise =
                  goSPP (spp { sppParents = spu : sppParents } : acc) spps
              where urls = map spuUrl sppParents ++ [sppSubscriptionUrl]
                    isHtml (SpuHtml {}) = True
                    isHtml _ = False
          goPP acc [] = return acc
          goPP acc (pp@(ParentPath {..}) : pps)
              | length (takeWhile isRedirect ppParents) > 10 = do
                  logLT l $ T.unlines $ "Too many redirects:" : urls
                  goPP acc pps
              | r `elem` urls = do
                  logLT l $ T.unlines $
                      "Redirect loop found:" : r : "is already in" : urls
                  goPP acc pps
              | otherwise = do
                  ok <- checkParentPaths l newParents
                  if ok then
                      goPP (pp { ppParents = newParents } : acc) pps
                  else
                      goPP acc pps
              where urls = map puUrl ppParents ++ [ppBlogFeedUrl]
                    newParents = pu : plusPps ppParents
                    isRedirect (PuRedirect _) = True
                    isRedirect (PuHtml _ _) = True
                    isRedirect _ = False

checkParentPaths l pps = case pps of
    PuHtml {} : ps
        | feedsCount >= 2 ->
            cancel $ "feed->…->feed->html " ++ show pps
            -- если уже был найден блог и комменты, дальше сканировать html-и
            -- смысла нет, т.к. деревья работают только через wfw:commentrss
            -- т.е. через feed->feed->feed->feed без промежуточных страниц

            -- TODO: также feed->html->feed->feed тоже лажа
            -- фиды подряд обычно у блогов
            -- Т.е., если есть wfw:commentrss, но сверху был html,
            -- то это фид блога, а не комментариев
        | any isHtml ps ->
            cancel $ "HTML found instead of feed " ++ show pps
    (redirect -> Just u) : ps
        | feedsCount > 0 && blogFeed u ->
            cancel $ "Blog feed inside comments " ++ show u ++ " " ++ show ps
                   -- в feedburner комментов нет
        | feedsCount > 0 && ".mp3" `T.isSuffixOf` u ->
            cancel $ "Found MP3? " ++ show u ++ " " ++ show ps
    _ -> ok
        -- TODO: по идее их надо еще и при обнаружении фида проверять???
        -- или уже по redirect-у можно отловить???
--     [PuFeed {}] -> ok
--     [PuFeed {}] -> ok
--     [PuParsedHtml]
    where feedsCount = length (filter isFeed pps)
          isHtml (PuHtml {}) = True
          isHtml _ = False
          redirect (PuHtml u _) = Just u
          redirect (PuRedirect u) = Just u
          redirect _ = Nothing
          ok = return True
          cancel s = do
              logLS l $ "checkParentPaths: cancelling " ++ s
              return False

blogFeed = \ u ->
    ("http://feedproxy.google.com" `T.isPrefixOf` u
     && not ("http://feedproxy.google.com/~" `T.isPrefixOf` u))
    ||
    ("https://feedproxy.google.com" `T.isPrefixOf` u
     && not ("https://feedproxy.google.com/~" `T.isPrefixOf` u))
       --  ссылки на блог посты имеют вид
       --  http://feedproxy.google.com/~r/ezyang/~3/BPSbOag4obE/
       --  http://feedproxy.google.com/~r/blogspot/pydev/~3/LVBmppQr4TI/pydev-eclipse-42-ie-stick-with-eclipse.html
    || isToplevelUrl (T.unpack u)
    || any (`T.isInfixOf` u)
        [ "index.php?act=rssout" -- phpBB форумы
        ]
        -- заблочить http://rss. не выйдет, т.к.
        --  есть http://rss.juick.com/2109939
    || ignoredHost u
    || any (`T.isInfixOf` hostNameFromUrlT u)
        [ ".amazon."
        , ".yahoo.co." -- https://page1.auctions.yahoo.co.jp
        , "www.google."
        ]
    || googleNewsUrl u
    || looksLikePublicBlogFeed u

ignoredHost = testDomains
    [ "twitter.com"
    , "youtube.com"
    , "facebook.com"
    , "wikipedia.org"
    , "feedburner.com"
    , "yahoo.com" -- https://tw.rd.yahoo.com/referurl/…
    , "amazon.com"
    , "flickr.com"
    , "delicious.com"
    , "rss.slashdot.org"
    , "rss.cnn.com"
    , "rss.cbc.ca"
    , "nytimes.com"
    , "tinydl.com" -- файлопомойка с кучей постов, забивает disqus
    , "firstpost.com" -- индусские новости с disqus
    , "livemint.com" -- тоже какие-то новости
    , "gazeta.ru"
    , "4pda.ru"
    , "avito.ru"
    , "spd.rss.ac"
    , "softpedia.com"
    , "pinterest.com"
    , "escapistmagazine.com"
    , "vk.com"
    , "lenta.ru" -- так, на всякий случай
    , "oglobo.globo.com"
    , "huffingtonpost.com"
    , "feeds.americanpublicmedia.org"
    , "feeds.newscientist.com"
    , "feeds.boston.com"
    , "syndication.boston.com"
    , "feeds.abcnews.com"
    , "feeds.reuters.com"
    , "cdn.abcnews.go.com"
    , "espn.go.com"
    , "theglobeandmail.com"
    , "faz.net"
    , "haaretz.com"
    , "feeds.geekzone.co.nz"
    , "dailymail.co.uk"
    , "nydailynews.com"
    , "slate.com"
    , "dailystar.com.lb"
    , "npr.org" -- хитрый disqus, пока заблокировал
    , "telegraph.co.uk" -- отрубаем, хотя там disqus
    , "medicinenet.com"
    , "macnn.com"
    , "heute.de" -- вообще нет комментов
    , "sportschau.de" -- тоже нет
    , "hr-online.de"
    , "news.cnet.com"
    , "techrepublic.com" -- свои комменты
    , "thesun.co.uk" -- тоже свои
    , "odesk.com"
    , "israbox.com"
    , "israbox.io"
    , "zakupki.gov.ru"
    , "wz-newsline.de"
    , "news.google.com"
    , "www.google.com"
    , "bing.com"
    , "almasryalyoum.com" -- много facebook комментов
    , "usatoday.com"
    , "eluniversal.com.mx"
    , "dailyherald.com"
    , "instagram.com"
    , "websta.me"
    , "news.ycombinator.com" -- все равно уже давно не парсится
    , "guardian.co.uk"
    , "bbc.co.uk" -- http://newsrss.bbc.co.uk/rss/…
    , "bbci.co.uk" -- http://feeds.bbci.co.uk/news/…
    , "bbc.com"
    , "cnet.com"
    , "people.com"
    , "aefj.es"
    , "crunchyroll.com"
    , "bensbargains.net", "bensbargains.com"
    , "webhostingtalk.com", "laozuo.org", "predb.me"
    , "politepol.com", "feed43.com", "lemonde.fr"
    , "rsshub.app"
    , "scientect.com" -- море новостей, которые никто не комментирует
    , "tromaktiko.gr", "usnews.com", "tauler.seu.cat"
    , "upwork.com", "nitter.net"
    , "wechat.com", "qq.com" -- wechat расположен на "weixin.qq.com"
    , "bilibili.com"
    ]

isFeed (PuFeed {}) = True
isFeed _ = False

subscriptionError uq l spp e = do
    logLT l $ T.unlines ("subscriptionError" : e)
    t <- uqGetUrTime uq
    mergeWriteSubscriptionUrlInfo $
        SubscriptionUrlInfo
        (sppSubscriptionUrl spp) t
        (SUKErrorPath (T.intercalate "<br/>" e) (sppParents spp))

subError t dlError spps e =
    forM_ spps $ \ (SubscriptionParentPath {..}) -> do
        modifySubscriptionUrlInfo'_ sppSubscriptionUrl $ \ sui -> do
            let checkErrTime te = diffUrTime t te < week
            kind <-
                case suiKind sui of
                    SUKFeed f
                        -- в случае, если ранее (в течении недели)
                        -- по данному URL был фид или перенаправление на фид
                        -- и в течении недели фид работал, то подписываемся
                        -- на него, а ошибку скачивания игнорируем.
                        | dlError &&
                          (checkErrTime (suiTime sui)
                           -- не совсем правильная вещь, т.к. suiTime
                           -- это время последней подписки, а не время
                           -- начала появления ошибки.
                           || f == suiUrl sui
                          ) -> do
                        fu <- readUrlToScan f
                        return $ fromMaybe (SUKErrorPath e sppParents) $ do
                            u <- fu
                            te <- utsErrorStartTime u <|> Just t
                            guard (checkErrTime te)
                            return $ SUKFeed f
                    _ ->
                        return $ SUKErrorPath e sppParents
            return $ SubscriptionUrlInfo sppSubscriptionUrl t kind

urlError uq dlError l utsUrl e = void $ do
    logLT l $ T.concat ["Error: ", e]
    atomicModifyUrlToScan'_ utsUrl $ \ uOld -> do
        t <- uqGetUrTime uq
        subError t dlError (utsSubscriptionParentPaths uOld) e
        -- TODO: а надо-ли что-нить c фидами делать?
        -- видимо, надо в очередь повторно добавлять, если был фид
        setBlogPostsScannedUts uq uOld [(utsUrl, CUSError e)]
        return $
            uOld { utsSubscriptionParentPaths = []
                 -- , utsModifyTime = t
                 -- не обновляем время, т.к. нет пересканирования???
                 }

setBlogPostsScannedUts uq uts states =
    forM_ (utsParentPaths uts) $ \ p -> case ppGuids p of
        postGuid:_ -> setBlogPostsScanned uq (ppBlogFeedUrl p) postGuid states
        _ -> return ()

setBlogPostsScanned :: UQ uq => uq -> TURL -> SB.ShortByteString -> [(TURL, CommentUrlState)] -> IO ()
setBlogPostsScanned uq bfu postGuid states = do
    t <- uqGetUrTime uq
    c <- readMVar $ uqCompleteBPS_ uq
    let complete
            | Just t' <- HM.lookup bfu c = diffUrTime t' t < 12*3600
              -- каждые 12 часов сбрасываем, т.к. фид может удалиться
              -- (в принципе, может удалиться раньше,
              -- но лучше в редких случаях не показывать процент сканирования,
              -- чем постоянно читать эти bps)
            | otherwise = False
    when (not complete) $ modifyBlogPostsScanned'_ bfu $ \ bps ->
        if diffUrTime t (bpsSubscribeTime bps) > 3660 then do
            modifyMVar_ (uqCompleteBPS_ uq) $ \ c -> do
                when (not $ HM.member bfu c) $ do
                    withEkgServer (return ()) $ do
                        g <- getGauge "completeBpsCount"
                        Gauge.inc g
                return $ HM.insert bfu t c
            return $ bps { bpsUrls = Map.empty }
        else
        return $ case Map.lookup postGuid (bpsUrls bps) of
            Nothing -> bps
            Just u ->
                let s CUSNew = (t, Nothing, CUSNew)
                    s cus    = (t, Just t, cus)
                in
                resolve bps $ bps { bpsUrls = Map.singleton postGuid
                                              (Map.fromList
                                              [(u,s cus) | (u,cus) <- states]) }

type Link = (Maybe SB.ShortByteString, -- pguid
             TURL, -- r
             [ParentUrl],
             UrTime, -- tParent
             Bool, -- noCommentsYet
             Maybe [(Guid, Text, Maybe UrTime)]) -- chGuids

processFeedItems :: UQ uq =>
                    IORef [AddUrl] -> Maybe Text -> Bool -> Bool
                 -> (UrlToScan -> UrlToScan) -> Logger -> UrlToScan
                 -> uq -> String -> SwitchUrl -> FeedMsg -> [(Parent, FeedMsg)]
                 -> ([Link] -> [Link]) -> QueueType -> TURL -> IO ()
processFeedItems newUrls postMsgText parsedHtml fromHub updUts l u uq baseUri switchUrl rootItem (map tsbParent -> items) addLinks qt utsUrl' = do
    keep <- modifyUrlToScan' url $ \ uOld -> do
        t <- uqGetUrTime uq

        let prevMt = utsModifyTime uOld

        modTime <- newIORef prevMt
        hubFound <- newIORef False
        maxCommentsCount <- newIORef 0

        -- обновляем инфу для подписок и добавляем ParentPath, если надо
        let spps = utsSubscriptionParentPaths uOld
        let u = (updUts uOld)
                { utsSubscriptionParentPaths = []
                , utsParentPaths =
                    if not (null spps) && not parsedHtml then
                        -- не позволяем подписываться на пост жж,
                        -- т.к. лишняя возня с древовидностью постов
                        union [ParentPath url []] (utsParentPaths uOld)
                    else
                        utsParentPaths uOld
--                      , utsModifyTime =
--                          maximum $ utsModifyTime uOld :
--                          catMaybes (map (feedMsgTime . snd) items)
                         -- TODO: тут должно быть время только новых сообщений,
                         -- причем с dlTime, если нет просто времени
                }
            whenNewChildGuids Nothing et act =
                act False (qt == QTNewComment1 || notNull spps) qt
                -- ничего не знаем по child guid-ы, не временный url
            whenNewChildGuids (Just g) et act = do
                modifyIORef modTime $ \ t ->
                    maximum $ t : [ct | (_,_,Just ct) <- g]
                go 0 g
                where go 0 [] =
                          logLT l "No new child messages found, skipping thread"
                      go n [] = do
                          logLT l $ T.append (T.pack $ show n)
                                    " child messages found, adding thread"
                          act True False qt
                      go !n ((guid, author, time) : gs)
                          | tsb guid `HM.member` etGuids et = go n gs
                          | otherwise = go (n+1) gs

        postsRemoved <- newIORef 0

        -- побежали по ParentPath-ам
        forM_ (utsParentPaths u) $ \ p -> case ppGuids p of
            [] -> do
                modifyCleanPosts t l uq (ppBlogFeedUrl p) $ ignoreDeletedPosts spps $ \ posts -> do
                    let toFeedItem rootItem rootMsg (parent, fm) =
                            feedMsgToItem True rootItem rootMsg t
                            (if pTotalPosts posts > 0
                             then prevMt else UrTime 0 0)
                            baseUri
                            (\ g -> MsgKey (ppBlogFeedUrl p) (Just g) Nothing)
                            Nothing -- не используем Parent-ов при подписке
                            -- на комментарии как на фид
                            fm
                        rootMsg = pRootMessage posts
                        rootFi = toFeedItem defaultFeedMsg
                                            (defaultMsg (error "defaultMsg"))
                                            (Nothing, rootItem)
                        newRootMsg = fiMsg rootFi
                        rm m = m { msgTime = Nothing
                                 , msgDlTime = UrTime 0 0
                                 , msgText = ""
                                 , msgShortText = ""
                                 , msgShorterText = ""
                                 , msgTags = []
                                 -- в http://joemygod.blogspot.com/feeds/posts/default
                                 -- немеряно тегов
                                 }
                        linkTime ( _, _, _, t, _, _) = t
                        stripLinks =
                            takeWhile (\ l -> linkTime l
                                              > (t `plusUrTime` (-30*day))) .
                            take 10 . reverse . sortBy (comparing linkTime)
                    updatePostsFavicon l $
                        posts { pRootMessage = newRootMsg }
                    listenToHubs l (writeIORef hubFound True)
                                 (fiHubs rootFi) url utsUrl'
                    let checkHash = not (hasCustomParser $ utsUrl u)
                        stripOlder = not checkHash &&
                            rt "twitter\\.com/" (utsUrl u) ||
                            rt "facebook\\.com/" (utsUrl u) ||
                            rt "vk\\.com/" (utsUrl u)
                    ((stripLinks . addLinks) -> links, mt', et, _) <-
                        editMsgTree Nothing checkHash stripOlder fromHub
                                    True modTime
                                    l (ppBlogFeedUrl p) uq
                                    (fromMaybe t . msgTime) newRootMsg
                                    (pMsgTree posts)
                                    (toFeedItem rootItem newRootMsg)
                                    (reverse $
                                     take (if stripOlder then 200 else 100) $
                                     reverse items)
                                    -- добавляем не более 100 последних
                                    -- постов, т.к. из-за неверного порядка
                                    -- их добавления в начале (и последующего
                                    -- удаления), происходит
                                    -- постоянное повторное добавление,
                                    -- если в фиде 500 постов
                    when (notNull spps) $
                         modifyBlogPostsScanned'_ (ppBlogFeedUrl p) $ \ bps ->
                             return $
                                 if bpsSubscribeTime bps /= UrTime 0 0
                                    && not (deletedPosts posts) then
                                     bps -- уже есть и posts не были удалены
                                 else
                                     bps
                                     { bpsSubscribeTime = t
                                     , bpsUrls =
                                         Map.fromList
                                         [( pguid, Map.empty ) -- addChildUrl добавит
--                               , Map.singleton (normalizeTURL r)
--                                               (t, Nothing, CUSNew) )
                                         | (Just pguid, r, _, _, _, _) <- links ]
                                     }
                    let postsCount = succ . snd . BA.bounds . mtHeaders
                        checkComments = not
                            (IntMap.size (pCommentCounts posts) == 0 &&
                             postsCount (pMsgTree posts) > 300)
                            -- отсекаем сайты, где не было комментов
                            || url == "http://feeds.feedburner.com/org/LOR"
                               --  ^ для него добавил обратно
                            || "http://www.linux.org.ru/" `T.isPrefixOf` url
                        --  || ".livejournal.com/" `T.isInfixOf` url
                            || postsCount (pMsgTree posts) `mod` 10 == 0
                               -- все-таки изредка проверяем,
                               -- если фид добавляет за раз кучу постов, то
                               -- проверка будет срабатывать реже
                               -- (если только они не десятками добавляются)
                               -- ну и ладно
                    when checkComments $ forM_ (zip [0..] links) $
                        \ ( delay
                          , link@( pguid, r, pp, tParent
                                 , noCommentsYet, chGuids) ) -> do
                        logLT l $ T.concat ["addChildUrl ", maskUrlPasswordT r]
                        whenNewChildGuids chGuids et $
                            addChildUrl newUrls (Delay delay)
                                    noCommentsYet l uq u tParent t
                                    (SpuRedirect $ normalizeTURL r)
                                    ((PuFeed url pguid : pp) ++)
--                        logLS l $ "addedChildUrl " ++ show link
                    logLS l $ "Posts count: " ++ show (postsCount mt')
--                     when (postsCount mt' > postsCount (pMsgTree posts)) $ do
--                         u <- readIORef modTime
--                         esUpdateDiscoveryFeed l url u
                    return $ posts { pRootMessage =
                                         if not fromHub &&
                                            -- "хаб" от realtime ЖЖ не содержит
                                            -- userpic-а
                                            rm newRootMsg /= rm rootMsg
                                         then rm newRootMsg else rootMsg
                                   , pMsgTree = mt'
                                   }
            [postGuid]
              | any isHtml (ppParents p) && any (hasComments . snd) items -> do
                    let msg = T.pack $ "Blog feed (has comments) in comments: "
                              ++ show url ++ " " ++ show postGuid
                    logLT l msg
                    setBlogPostsScanned uq (ppBlogFeedUrl p) postGuid
                                       [(url, CUSError msg)]
            postGuid : commentGuids
              | Just rl <- msgLink $ fiMsg $ feedMsgToItem True defaultFeedMsg (defaultMsg (error "defaultMsg1")) t prevMt baseUri
                          (\ g -> MsgKey (ppBlogFeedUrl p) (Just g) Nothing)
                          (error "feedMsgToItem2") rootItem
              , isToplevelUrl $ T.unpack rl -> do
                    let msg = T.pack $ "Blog feed (root url) in comments: "
                              ++ show rl ++ " " ++ show url ++ " "
                              ++ show (postGuid : commentGuids)
                    logLT l msg
                    setBlogPostsScanned uq (ppBlogFeedUrl p) postGuid
                                       [(url, CUSError msg)]
              | otherwise -> do
                case postMsgText of
                    Just t ->
                        modifyMsg'_ (MsgKey (ppBlogFeedUrl p) (Just postGuid) Nothing) $ \ m -> do
                            let fi = feedItemFromMsgAndText baseUri m t
                            if msgText (fiMsg fi) /= msgText m then do
                                logLT l "Post msg text changed"
                                posts <- readPosts' (ppBlogFeedUrl p)
                                logTime l "wSearchP" $
                                    saveSearchMsgs l uq (ppBlogFeedUrl p)
                                        (\ m -> fromMaybe (msgDlTime m)
                                                          (msgTime m))
                                        (pRootMessage posts)
                                        [fi]
                                return (fiMsg fi)
                                -- TODO: возможно, изменится short text
                            else
                                return m -- ничего не изменилось
                    Nothing -> return ()
                let checkSwitch =
                        case switchUrl of
                            Switch immediate su -> do
                                logLT l $ T.concat ["Switch url ", su]
                                mt <- readIORef modTime
                                addChildUrl newUrls PushFront
                                            (not immediate) l uq u mt t
                                            (SpuRedirect $ normalizeTURL su)
                                            id
                                            False -- NOT temporary
                                            (qt == QTNewComment1) qt
                            _ -> return ()
                if null items then do
                    logLT l "Empty comments feed"
                    checkSwitch
                    -- нет комментов -- ничего не делаем
                else do
                 -- дабы лишний раз не читать Posts (графики показывают
                 -- большой объем чтения, раз в 5-10 больше, чем на запись)
                 -- сначала читаем comments и проверяем, есть ли новые
                 -- guid-ы (content hash и прочее не проверяем, и так
                 -- слишком сложно)
                 let cKey = CommentsKey (ppBlogFeedUrl p) postGuid
                 c0 <- readComments cKey
                 let hasNew
                         | Just (etGuids . mkEditTree True . cMsgTree -> g) <- c0 =
                             any (\ (_,fm) ->
                                      not (tsb (fmGuid fm) `HM.member` g))
                                 items
                         | otherwise = True
                 if not hasNew then do
                    logLT l "No new comment guids found"
                    checkSwitch
                 else modifyCleanPosts t l uq (ppBlogFeedUrl p) $ \ posts ->
                  let postsEditTree = mkEditTree True (pMsgTree posts) in
                  case HM.lookup postGuid (etGuids postsEditTree) of
                   Just (_, _, pid) -> do
                    let postHdr = mtHeaders (pMsgTree posts) BA.! pid
                        !postTime = fromMaybe (mhDlTime postHdr)
                                              (mhTime postHdr)
                        commentParent
                            | null commentGuids = Nothing
                            | otherwise = Just $ last commentGuids
                        prevCommentCount = postCommentsCount pid maxBound posts
                        toFeedItem (parent, fm) =
                            feedMsgToItem False defaultFeedMsg
                            (defaultMsg (error "defaultMsg2"))
                            t
                            (-- if prevCommentCount > 0
                             -- then prevMt else
                             -- пока вообще не исправляем время в комментах,
                             -- а то lj thread-ы оказываются с кривыми датами
                             UrTime 0 0)
                            -- комментарии могут отсканироваться сначала
                            -- с одного блога, потом с другого (поиска яндекса)
                            -- в итоге время модификации будет свежим,
                            -- а комментов еще нет
                            baseUri
                            (\ g -> MsgKey (ppBlogFeedUrl p) (Just postGuid)
                                           (Just g))
                            (parent <|> commentParent) fm

                        filtCheck acc [] = (True, reverse acc)
                        filtCheck acc ((toFeedItem -> fi) : is)
                             | fi `inEditTree` postsEditTree =
                                 if mhGuid (fiHeader fi) == postGuid
                                 || mhContentHash (fiHeader fi) ==
                                    mhContentHash postHdr then
                                    --  ^ попадаются повторяющиеся в комментах
                                    -- посты с другим guid-ом
                                     filtCheck acc is
                                 else
                                     (False, [])
                             | otherwise = filtCheck (fi:acc) is
                        (check, fis) = filtCheck [] items
                    if not check then do
                        let msg = "Found non-parent post in comments"
                        logLT l msg
                        when (not $ any (null . ppGuids) (utsParentPaths u)) $
                             modifyIORef modTime
                                        (max $ t `plusUrTime` (-2*month))
                             --  ^ таким макаром отменим пересканирование
                             -- причем только в случае, если нет Posts
                             -- (чтобы блог сканировало)
                        setBlogPostsScanned uq (ppBlogFeedUrl p) postGuid
                                           [(url, CUSError msg)]
                        return posts
                    else do
                        -- modifyIORef modTime (max postTime)
                        --  ^ вроде как это теперь не нужно
                        let checkHash = not (hasCustomParser $ utsUrl u)
                                        -- не проверяем у ЖЖ/facebook/…
                        cs <- modifyComments' cKey $ \ cs -> do
                            (addLinks -> links, mt', et, sameGuidFound) <-
                                editMsgTree (Just pid) checkHash False
                                            fromHub False modTime
                                            l (ppBlogFeedUrl p) uq
                                            (const postTime)
                                            (defaultMsg (error "defaultMsg3"))
                                            (cMsgTree cs)
                                            id fis
                            forM_ (zip [0..] links) $
                                \ ( delay
                                  , link@( pguid, r, _pp, tParent
                                         , noCommentsYet, chGuids) ) -> do
                                logLS l $ "addChildUrl " ++ show link
                                whenNewChildGuids chGuids et $
                                    addChildUrl newUrls (Delay delay)
                                            noCommentsYet l uq u tParent t
                                            (SpuRedirect $ normalizeTURL r)
                                            (PuFeed url pguid :)
    --                            logLS l $ "addedChildUrl " ++ show link
--                             Кстати addChildUrl добавляет child сразу всем
--                             ParentPath-ам, а выдаем мы его только для одного.
--                             По идее, блог только один, так что проблема
--                             только у комментов, и она, похоже, не
--                             проявляется, т.к. пока url сканируется,
--                             все остальные Comments успевают обновиться

                            case lookup LKNext (fmLinks rootItem) of
                                Just nu | notNull fis && not sameGuidFound -> do
                                    -- все добавленные комментарии новые,
                                    -- значит еще какие-то новые
                                    -- могут быть в next
                                    -- проверяем новизну только по guid, т.к.
                                    -- у нас еще доп фильтрация по содержимому
                                    logLT l $ T.concat ["Adding next url ", nu]
                                    addChildUrl newUrls
                                                PushFront False l uq u t t
                                                (SpuRedirect $ normalizeTURL nu)
                                                id
                                                True -- temporary
                                                False qt
                                                -- по-этому tParent пофиг
                                _ -> return ()
                            checkSwitch
                            let cOld = mtSize (cMsgTree cs)
                                cNew = mtSize mt'
                                r = cs { cMsgTree = mt' }
                            logLS l $ show (cNew - cOld) ++ " new comments ("
                                      ++ show cNew ++ " in total)"
                            modifyIORef' maxCommentsCount (max cNew)
                            return (r,r)
                        let (posts', commentsAdded) =
                                updateCommentsCount t postsEditTree postGuid
                                    posts
                                    (mtSize $ cMsgTree cs)
                        logLS l $ "commentsAdded = " ++ show commentsAdded
                              ++ "\n"
                              ++ "total " ++ show (pTotalComments posts)
                              ++ " -> " ++ show (pTotalComments posts')
                        -- let uc = pUpdatedComments posts
                        return posts'
                   _ -> do
                     let msg = "Post was removed: "
                                ++ show (postGuid : commentGuids)
                     logLS l msg
                     modifyIORef postsRemoved succ
                     return posts

                setBlogPostsScanned uq (ppBlogFeedUrl p) postGuid [(url, CUSOK)]

        -- подписки добавляем тут, чтобы Posts был уже готовый.
        -- а то сообщения от больших фидов (типа lenta.ru)
        -- могут все еще сохраняться, а пользователь будет думать,
        -- что уже подписался
        let writeSUI spp =
                mergeWriteSubscriptionUrlInfo $
                SubscriptionUrlInfo (sppSubscriptionUrl spp) t
                     (if not parsedHtml
                      then SUKFeed url
                      else SUKErrorPath "Can’t find any feed on the specified URL" (sppParents spp))
        mapM_ writeSUI spps
        when (notNull spps && url `notElem` map sppSubscriptionUrl spps) $
            writeSUI $ SubscriptionParentPath url []
            -- была подписка, но не было такого spp?

        pr <- readIORef postsRemoved
        uts' <- case switchUrl of
            NoSwitch | pr < length (utsParentPaths u) -> do
                mt <- readIORef modTime
                hf <- readIORef hubFound
                mcc0 <- readIORef maxCommentsCount
                let mcc | isBlogUts u = 0
                        | mcc0 == 0 = 50 -- нет комментов, накинем еще полчаса
                        | otherwise = mcc0
                    count | "livejournal.com" `T.isInfixOf` utsUrl u = 100
                          | otherwise = 300
                addRescan'' (if hf then RKHubFound
                             else RKNormal (min 10 (fromIntegral mcc / count)))
                    l uq t
                    (u { utsModifyTime = if mt == UrTime 0 0 then t else mt })
                    qt
            _ ->
                cancelRescans' l uq id u qt
        when (notNull spps && looksLikePublicBlogFeed url) $ do
            e <- esDiscoveryFeedExists url
            when (not e) $ do
                t <- getUrTime
                posts <- readPosts url
                sfi <- mkSubFeedInfo url t posts (Just (fst uts'))
                case sfi of
                    Right i ->
                        esIndexDiscoveryFeeds l
                            [(mergeSubFeedInfoToDiscoveryFeed i $
                              defaultDiscoveryFeed url)
                             { dfSubscribers           = 1
                             , dfNormalizedSubscribers = 0.01
                             }]
                    _ -> return ()
        return uts'
    when (not keep) $ deleteUrlToScanC l u
    where url = utsUrl u
          isHtml (PuHtml {}) = True
          isHtml _ = False
          hasComments (FeedMsg {..}) =
              isJust $ lookup LKApiFeed fmLinks <|>
                       lookup LKFeed fmLinks <|>
                       lookup LKCommentsHtml fmLinks
                       -- wfw:commentrss или comments, или alternate xml

updateBlogFavicon l uts
    | isBlogUts uts = do
        updatePostsFavicon l =<< readPosts' (utsUrl uts)
    | otherwise = return ()

updatePostsFavicon l posts =
    void $ forkIO $ withLogger $ \ l -> logTime l "total   " $ do
    let link = fromMaybe (pBlogFeedUrl posts) $ msgLink $ pRootMessage posts
    logLT l $ T.append "Updating favicon " link
    r <- getFavicon True link
    case faviconFile r of
        Left (_,e) -> logLT l $ T.append "Icon error: " e
        Right _ ->
            logLT l $ T.append "Icon found: " (faviconSourceUrl r)

listenToHubs l ok hubs url utsUrl' = forM_ hubs $ \ hub -> do
    ldr <- listenToHub (uqDownloader_ iuq) hub utsUrl' -- url
    case ldr of
        DROK (read . B.unpack -> dr) _ -> case dr of
            DROK _ _ -> do
                logLT l $ T.concat $
                    ["Subscribed to hub ", hub]
                    ++
                    if utsUrl' /= url then
                        [" but to different url ", utsUrl']
                    else
                        []
                -- в любом случае подписываемся, даже
                -- если было перенаправление, но частоту
                -- сканирования в таких случаях не меняем
                when (utsUrl' == url) ok
            dr ->
                logLT l $ T.concat
                    ["Subscribing to hub ", hub
                    ," failed: ", T.pack $ show dr]
        ldr ->
            logLT l $ T.concat
                ["Calling subscribe to hub ", hub
                ," failed: ", T.pack $ show ldr]

tsbParent (p,fm) = (fmap tsb p, fm)

deleteUrlToScanC l uts = do
    u' <- readUrlToScan' (utsUrl uts)
    if utsNextScanTime u' /= UrTime 0 0 then
        logLT l $ "Removing UTS with non-zero nst?"
    else
        atomicDeleteUrlToScan u'

ppGuids = reverse . catMaybes . map guid . ppParents
    where guid (PuFeed _ g) = g
          guid _ = Nothing

findPostsHotLinks eu isHighVolume bfu guids =
    esSimpleIdsSearch' eu "hot_link" "hot_link" 50000 -- 500posts*50links = 25k max
        Nothing bfu $
        esMust [ esTerms "post_guid" guids
               , esTerm "blog_feed_url" bfu
               ]

deletePostList posts l uq bfu imhs = do
    pt <- readPostsTagged' bfu
    t <- getUrTime
    let pguids = map (mhGuid . snd) imhs
        isHighVolume
            | mh:_ <- BA.elems $ mtHeaders $ pMsgTree posts =
                diffUrTime t (fromMaybe (mhDlTime mh) (mhTime mh)) < 3*day
            | otherwise = False
        csAndKeys cs (_,mh) =
            ( cs
            , sort $
              keyPrefix Nothing :
              maybe [] (map (keyPrefix . Just . mhGuid)
                        . BA.elems . mtHeaders . cMsgTree) cs )
            where keyPrefix = MsgKey bfu (Just guid)
                  guid = mhGuid mh
    css <- logTime l "readComm" $ readManyCommentss [CommentsKey bfu g | g <- pguids]
    let ck = zipWith csAndKeys css imhs
        delOld :: Logger -> String -> URL -> IO ()
        delOld l suffix esU = do
            hlids <- if hotLinksDisabled then return [] else
                logTime l
                     (T.pack $ (if isHighVolume then "findHLHV" else "findHotL")
                      ++ suffix) $
                findPostsHotLinks esU isHighVolume bfu (map sbt pguids)
--                 -- поиск работает быстро, а delete by query тупит по 0.5-1.5сек
--                 esDeleteByQuery' l uq "hot_link" "hot_link" Nothing $
--                 esMust [ esTerm "blog_feed_url" bfu
--                        , esTerms "post_guid" $ map sbt pguids ]
            mapM_ (logTime l "delMsgs " . esBulkRequest' esU ("Deleted" ++ suffix) l) $
                groupByN 50 $
                (if hotLinksDisabled then [] else
                [ esBulkAction r "hot_link" "hot_link" "delete" i | (i,r) <- hlids ]
                ++
                [ esBulkAction bfu "msg_links" "msg_links" "delete" $
                  binHashToRemove $ MsgKey bfu (Just g) Nothing
                | g <- pguids ] ++
                [ esBulkAction bfu "msg_links" "msg_links" "delete" $
                  msgLinkId $ MsgKey bfu (Just g) Nothing
                | g <- pguids ]
                -- из-за удаления несуществующих msg_links растет translog
                -- (видимо, из-за отсутствия индексации новых msg_links,
                -- не вызывается merge)
                ) ++
                [ esBulkAction bfu "msg" "msg" "delete" $
                  msgKeyToJSON key
                | (cs,keys) <- ck, key <- keys ]
    esDel <- async $ withLogger $ \ l -> do
        delOld l "" esUrl
        withEsUrl2 $ \ u2 ->
            delOld l "2" u2
        loggerGetAndClean l
    logTime l "delRiak " $ do
        keys <- forM (zip imhs ck) $ \ ((idx,_), (cs, keys)) ->
            if ReadSet.member idx (ptSet pt) then do
                logLS l $ "Not removing tagged post #" ++ show idx
                return []
            else
                return $
                    map deleteComments (maybeToList cs) ++
                    map (deleteMsg . defaultMsg) keys
        let list = concat keys
            (l1, l2) = splitAt (length list `div` 2) list
        a <- async $ sequence_ l1
        sequence_ l2
        wait a
--    logTime l "delete post" $
--    esDeletePost l uq bfu (sbt guid) -- а вот из поиска удаляем в любом случае
    t <- wait esDel
    logLT l t

testRemoveOldPosts = withLogger $ \ l ->
    modifyPosts'_ bfu $ removeOldPosts' 3 l iuq
    where bfu = -- "http://thesz.livejournal.com/data/rss"
--               "http://api.flickr.com/services/feeds/photos_public.gne?tags=schweiz,swiss,suisse,switzerland,svizzera&tagmode=any&lang=en-us&format=rss_200"
--               "http://api.flickr.com/services/feeds/photos_public.gne?tags=italy,italia&tagmode=any&lang=en-us&format=rss_200"
--                "http://news.ycombinator.com/rss"
--                "http://feeds2.feedburner.com/Techcrunch"
                "http://feeds.feedburner.com/AndroidPolice"

removeOldPosts uq = removeOldPosts' 500 uq
removeOldPosts' maxPosts l uq posts@(Posts {..})
    | lo' <= lo = return posts
    | otherwise =  do
        logLS l $ "Removing " ++ show (lo' - lo) ++ " old posts"
        logTime l "delete  " $ deletePostList posts l uq pBlogFeedUrl $
            take (lo' - lo) $ BA.assocs mtHeaders
--         case minTime of
--             Just t ->
--                 logTime l "Deleting old search" $
--                 -- для фидов с запятыми в URL, у которых routing не работал
--                 esDeleteByQuery l uq "msg" pBlogFeedUrl $
--                      esFilter $ esAnd $ map esNoCache
--                          [ esField "blog_feed_url" pBlogFeedUrl
--                          , esLessThanDateFilter "post_time" t ]
--             Nothing -> return ()
        return $
            posts
            { pCommentCounts = snd $ IntMap.split (lo'-1) pCommentCounts
            , pDeletedComments = pDeletedComments
                + sum (map cc $ IntMap.elems $
                       fst $ IntMap.split lo' pCommentCounts)
            , pMsgTree = mt'
            }
    where MsgTree {..} = pMsgTree
          mt' =
              MsgTree (BA.listArray (lo', hi) $ drop (lo' - lo) $
                       BA.elems mtHeaders)
                      (IntMap.map (Set.filter ((\ x -> x >= lo' && x <= hi) . tiId)) mtChildren)
          minTime
              | (MsgHeader {..} :_) <-
                  BA.elems (Generated.DataTypes.mtHeaders mt')
              , Just s <- IntMap.lookup (-1)
                          (Generated.DataTypes.mtChildren mt')
              , Just (TimeId t i,_) <- Set.minView s =
                  Just $ min t (fromMaybe mhDlTime mhTime)
              | otherwise = Nothing
          (lo, hi) = BA.bounds mtHeaders
          lo' = hi - maxPosts+1
          cc x = case IntMap.maxView x of
                   Just (c,_) -> c
                   Nothing -> 0

ignoreDeletedPosts spps act p
    | spps == [] && deletedPosts p = return p
    | otherwise = act p
-- игнорируем удаленные посты, т.к. в процессе удаления может
-- прийти обновление от хаба

-- | Подчищает старые версии комментариев и старые посты
modifyCleanPosts :: UQ uq => UrTime -> Logger -> uq -> TURL
                 -> (Posts -> IO Posts) -> IO ()
modifyCleanPosts t l uq u f = modifyPosts'_ u $ \ posts0 -> do
    postsF <- f posts0
    -- удаляем посты после обновления, чтобы не было больше 500
    posts@(Posts {..}) <- fixBrokenPostsTimes l =<< removeOldPosts l uq postsF

    let go acc n l [] = Nothing
        go acc n !l (v@(vt,vtc):vs)
            | n > 1000 || t `diffUrTime` vt > day =
                if l > 30 then Just (v:acc, vtc, l-1) else Nothing
            | otherwise = go (v:acc) (n+1) (l-1) vs
        stripOld cc ccs
            | IntMap.null keep = IntMap.singleton maxV maxC
            | otherwise = keep
            where (rm, keep) = IntMap.split (cc-1) ccs
                  (maxV, maxC) = IntMap.findMax ccs


    case go [] 0 (Map.size pCCVersions) (Map.toDescList pCCVersions) of
        Just (vs, vtc, left) -> do
            logLS l $ "Removing " ++ show left ++ " old comments versions"
            return $ posts
                { pCCVersions = Map.fromDistinctAscList vs
                , pCommentCounts = IntMap.map (stripOld vtc) pCommentCounts
                }
        Nothing ->
            return posts

updateCommentsCount t et@(EditTree {..}) postGuid posts@(Posts {..}) count =
    case HM.lookup postGuid etGuids of
        Just (_, _, idx) ->
            if commentsAdded <= 0 then
                -- именно <=, а не ==, т.к. при отвале Riak-а может прочитаться
                -- пустое значение и число "добавленных" комментариев будет
                -- меньше изначального.
                -- Конечно, при уменьшении числа комментов можно было бы
                -- увеличивать pDeletedComments, но это неправильно,
                -- т.к. комментарии в данном случае не удаляются
                -- Так что просто игнорируем кривые числа комментов.
                (posts, 0)
            else
                (posts
                 { pUpdatedComments = IntSet.empty
                                      -- IntSet.insert idx pUpdatedComments
                 , pTotalComments = tc'
                 , pCommentCounts =
                     IntMap.insert idx (IntMap.insert tc' count ccs)
                                   pCommentCounts
                 , pCCVersions = Map.insert t tc' pCCVersions
                 }
                , commentsAdded)
            where ccs = IntMap.findWithDefault IntMap.empty idx pCommentCounts
                  cc = commentsCount pTotalComments ccs
                  commentsAdded = count - cc
                  tc' = commentsAdded + pTotalComments
        Nothing -> (posts, 0) -- по-идее, так не бывает ж)

count u what = do
    incrStat "urlsScanned" 1
    when (isBlogUts u) $ incrStat "blogUrls" 1
    when (isCommentUts u) $ incrStat "commentUrls" 1
    when (notNull $ utsSubscriptionParentPaths u) $ incrStat "subUrls" 1
    incrStat what 1

isBlogUts = any (null . ppParents) . utsParentPaths
isCommentUts = any (notNull . ppParents) . utsParentPaths

-- | Если нет повторного добавления в очередь, то url и так не будет
-- пересканироваться, но надо почистить utsNextScanTime, чтобы в дальнейшем,
-- при необходимости, url все же добавился в очередь.
cancelRescans l uq f url qt = do
    keep <- atomicModifyUrlToScan' url $ \ uts -> cancelRescans' l uq f uts qt
    when (not keep) $ deleteUrlToScanC l $ defaultUrlToScan url
cancelRescans' l uq f uts qt =
    if isBlogUts (f uts) then do
        t <- uqGetUrTime uq
        logLT l "cancelRescans: blog found"
        addRescan'' (RKError 3) l uq t (f uts) qt
    else
        return ((f uts) { utsNextScanTime = UrTime 0 0 }, False)

data RescanKind
    = RKNormal Double
    | RKError Double
    | RKHubFound
    deriving (Show, Eq)

addRescan :: UQ uq => Logger -> uq -> (UrlToScan -> UrlToScan) -> TURL -> QueueType -> IO ()
addRescan = addRescan' (RKNormal 0)

addRescan' :: UQ uq => RescanKind -> Logger -> uq -> (UrlToScan -> UrlToScan) -> TURL -> QueueType -> IO ()
addRescan' rk l uq f url qt = do
    keep <- atomicModifyUrlToScan' url $ \ uts -> do
        t <- uqGetUrTime uq
        addRescan'' rk l uq t (f uts) qt
    when (not keep) $ deleteUrlToScanC l $ defaultUrlToScan url

addRescan'' :: UQ uq => RescanKind -> Logger -> uq -> UrTime -> UrlToScan -> QueueType
            -> IO (UrlToScan, Bool)

addRescan'' rk l uq t uts qt = do
    let rndMax = 900
    rnd <- fmap ((/ 1000000) . fromIntegral) $
           randomRIO (1, rndMax * 1000000 :: Int)
    let umt = utsModifyTime uts
        mt = if umt == UrTime 0 0 || umt > t then t else umt
        diff = t `diffUrTime` mt
        blog = isBlogUts uts
        e = case rk of
            RKError n -> Just n
            _ -> Nothing
        mult = case rk of
            RKError n -> n
            RKNormal n
                | blog &&
                  (".blogspot.com/" `T.isInfixOf` utsUrl uts --  ||
                  -- "tumblr.com/" `T.isInfixOf` utsUrl uts
                  ) -> 4
                  -- при Not modified мы не знаем про хаб,
                  -- но у blospot/tumblr он точно есть.
                | lj || disqus -> 3
                | otherwise -> 1
            RKHubFound
                | goodPuSH (utsUrl uts) -> 4
                | otherwise -> 2
        add | RKNormal a <- rk = a*hour
            | otherwise = 0
        next =
--            min (24*hour) $
--            max (if rk == RKHubFound then (4+4*rnd/rndMax)*hour else 0) $
                -- TODO: было 8+8, повозиться с протухшими хабами
            (if blog then nextPostsScanTime else nextCommentsScanTime) diff
--        rndPlus = rnd :: Int
        -- if blog then (rnd :: Int) else 0
                  -- дабы фиды более равномерно работали, а не пульсировали
                  -- каждые полчаса
        t' = roundScanTime $ t `plusUrTime`
            (min (24*hour - fromIntegral rndMax) (next * mult + add) + rnd)
        errDiff = do errStart <- utsErrorStartTime uts
                     _ <- e
                     return (t `diffUrTime` errStart)
        errST = case e of
            Just _ -> utsErrorStartTime uts <|> Just t
            Nothing -> Nothing
        lj = ".livejournal.com/" `T.isInfixOf` utsUrl uts
        disqus = "disqus.com/" `T.isInfixOf` utsUrl uts
        fb = "facebook.com/" `T.isInfixOf` utsUrl uts
        reddit = "reddit.com" `T.isInfixOf` utsUrl uts
        maxCommentsDays
            | disqus = 4
            | fb = 2
            | lj || reddit = 5
            | otherwise = 7
        cancels =
            [ (not blog && qt == QTTemporary1, "temporary url")
              -- отменяем даже при ошибке, т.к. иначе мы навечно повиснем
              -- в очереди QTTemporary1
            , (not blog && regexTest ".*livejournal.com.*thread.*" (utsUrl uts),
               "livejournal thread")
              --  отменяем старые нитки
            , (not blog && diff > maxCommentsDays*day, "too old comment")
              -- не сканируем комментарии, не обновлявшиеся более 7 дней
            , (null (utsParentPaths uts) &&
               null (utsSubscriptionParentPaths uts), "no parents")
            , (maybe False (> 2*day) errDiff && not blog, "2 days of errors")
            , (maybe False (> 90*day) errDiff, "90 days of errors (blog)")
            ]
    logLS l $ "mt " ++ show mt
        ++ (if umt /= mt then " (" ++ show umt ++ ")" else "")
        ++ " -- " ++ showSecs diff ++ " ago"
        ++ " (+" ++ showSecs (diffUrTime t' t) ++ ") --> " ++ show t'
    when (blog && maybe False (> week) errDiff) $ do
        e <- esDiscoveryFeedExists $ utsUrl uts
        when e $ do
            logLT l "Removing from discovery (a week of errors)"
            esDeleteDiscoveryFeed l $ utsUrl uts
    if any fst cancels
    then do
        logLT l $ T.concat $ "Rescans canceled: " :
                  intersperse ", " (map snd $ filter fst cancels)
        return (uts { utsNextScanTime = UrTime 0 0
                    , utsModifyTime = mt
                    , utsErrorStartTime = errST
                    }, False)
    else do
        -- TODO: учитывать глубину очереди
        saveUrl t' (if blog then QTBlogFeed else
                    QTRescan
                   -- if diff < day then QTRescan1 else QTRescan
                   ) (utsUrl uts)
        return (uts { utsNextScanTime = t'
                    , utsModifyTime = mt
                    , utsErrorStartTime = errST
                    }, True)

goodPuSH = testDomains
    ["blogspot.com"
    ,"tumblr.com"
    ,"wordpress.com"
    ,"typepad.com"
    --  ^ у них хаб точно работает
    ,"feedburner.com"
    ]

incrGauge what = do
    g <- getGauge what
    Gauge.inc g

decrGauge what = do
    g <- getGauge what
    Gauge.dec g

countProcess what mvar act = withEkgServer act $ do
    g <- getGauge what
    E.bracket
        (do
            Gauge.inc g
            -- c <- modifyMVar mvar (\ c -> return (c+1, c+1))
            -- logT $ T.concat [what, ": ", T.pack $ show c]
        )
        (\ _ -> do
            Gauge.dec g
            -- c <- modifyMVar mvar (\ c -> return (c-1, c-1))
            -- logT $ T.concat [what, ": ", T.pack $ show c]
        )
        (\ _ -> act)

allCommentKeys :: IO [CommentsKey]
allCommentKeys = do
    Just (User {..}) <- readUser "1"
    posts <- readManyPostss [ feed
                            | sn <- uSubscriptions
                            , SSFeed feed <- [sState sn] ]
    let postsCommentKeys (Posts {..}) =
            [ CommentsKey pBlogFeedUrl (mhGuid mh)
            | mh <- BA.elems $ mtHeaders pMsgTree]
    return $ concatMap postsCommentKeys $ catMaybes posts

allCommentHeaders = do
    keys <- allCommentKeys
    comments <- readManyCommentss keys
    return $ map (BA.elems . mtHeaders . cMsgTree) $ catMaybes comments

allCommentCounts = do
    keys <- allCommentKeys
    comments <- readManyCommentss keys
    return $ avg $ filter (< 1000) $
        map (succ . snd . BA.bounds . mtHeaders . cMsgTree) $ catMaybes comments
    where avg [] = 0
          avg l = fromIntegral (sum l) / fromIntegral (length l)

allComments = do
    keys <- allCommentKeys
    comments <- readManyCommentss keys
    let cMsgs (Comments { cKey = CommentsKey {..}, ..}) =
            [ MsgKey ckBlogFeedUrl (Just ckPostGuid) (Just $ mhGuid mh)
            | mh <- BA.elems $ mtHeaders cMsgTree]
    fmap catMaybes $ readManyMsgs $ concatMap cMsgs $ catMaybes comments

allAuthors = do
    c <- allComments
    return $ Map.size $ foldl' calc Map.empty c
    where calc map (Msg {..}) = Map.insertWith (+) (T.toLower msgAuthor) 1 map

-- 30k сообщений
--  9 Мб деревья комментариев
--  7.5  заголовки
--  3    ShortText
--  0.9  guid
--  1.1  contentHash

-- 44 Мб всего
-- 20 Мб текст
-- 10 Мб отладка
--  1 Мб subj
--  4 Мб ключи

commentsHistogram = do
    keys <- allCommentKeys
    comments <- readManyCommentss keys
    let hist :: IntMap Int -> Comments -> IntMap Int
        hist r (Comments {..}) =
            foldl' (\ r d -> IntMap.insertWith (+) d 1 r) r $
            diffs $
            sort [ time | MsgHeader { mhTime = Just time }
                            <- BA.elems (mtHeaders cMsgTree) ]
        diffs a@(_:_) = [ truncate (diffUrTime t2 t1)
                        | (t1, t2) <- zip a (tail a) ]
        diffs [] = []
        h = foldl' hist IntMap.empty $ catMaybes comments
        total = fromIntegral (sum $ IntMap.elems h) :: Double
        percent = 1/3
    (_, _, _, l) <-
        foldM (\ (acc, nextPercent, pd, l) (d, c) -> do
            let c' = fromIntegral c
                tp = (acc+c') / total
            if tp >= nextPercent then do
                printf "%d\t%d\t%.6f\t%.6f\n" ((d -pd) `div` 60) c (c'/total) tp
                return (acc + c', (1 - nextPercent)*percent + nextPercent
                       , d, l ++ [fromIntegral d])
            else
                return (acc + c', nextPercent, pd, l)
        )
        (0.0, percent, 0, []) (IntMap.assocs h)
    mapM_ print [b/a | (a,b) <- zip l (tail l)]
-- Статистика, через сколько минут сколько процентов
-- 4    33%
-- 11   50%
-- 89   75%
-- 543  87.5%
-- 2950 93.75%  -- 49 часов/2 дня
-- 18614 96.875%  -- 13 суток

-- TODO: хорошо бы учитывать статистику.
-- Если в какой-то час < 1% сообщений, то стоит считывать в другой час
nextPostsScanTime t = l
    where l --  | t < hour = min halfAnHour $ max (10*60) (2*t)
            | t < 3*hour = 15 * minute * (1 + t / hour) -- 0:15..1
            | t < week = hour * (1 + t / week)   -- 1..2
            | t < month = hour * (2 + t / month) -- 2..3
            | otherwise = min week (hour * (1 + 2*t / month))  -- 3..
--             | t < 2*month = 3*hour
--             | t < 4*month = 5*hour
--             | t < 6*month = 8*hour
--             | t < year = 12*hour
--            | otherwise = day
nextCommentsScanTime t = l
    where l --  | t < hour = min halfAnHour $ max (10*60) (2*t)
            --  | t < 2*hour = halfAnHour
            | t < 5*hour = 20 * minute * (1 + t / hour) -- 0:20..2
--            | t < 2*day = hour * (2 + t / day) -- 2*hour  -- 93% комментов
--            | t < 4*day = 3*hour
--            | t < week = 4*hour -- 96% комментов
            | otherwise = hour * (2 + t / day)  -- 2..33
--             | t < 2*week = 8*hour
--             | t < month = day
--             | t < 2*month = 2*day
--             | t < 4*month = 4*day
--               -- надо ли вообще пересканировать комментарии месячной давности?
--             | t < 6*month = week
-- --             | t < year = 2*week
-- --             | t < 2*year = month
--             | otherwise = 3*month

-- С поиском
--   beam.smp 25m
--   Queue:
--     real  20m52.965s
--     user  5m3.460s
--     sys   0m42.010s
--   (27600+2526)/( 21 60) = 24 сообщения в секунду
--   9175 / (21 60) = 7.2 url-а в сек
--   2403 / (21 60) = 1.9 жж url-а в сек
--   1G данных (656MB leveldb / 409MB merge_index)
--   долго разгребает один домен -- жж, т.е. возможна большая скорость
--   при большей параллельности

-- Без поиска
-- (вышло чуть меньше сообщений и чуть больше real из-за curl timeout-ов в жж)
--   beam.smp 7m (в 3.5 раза меньше)
--   Queue:
--     real  15m28.202s  -- можно пару минут срезать от таймаутов
--     user  4m20.655s   -- на 12% меньше без преобразований в JSON
--     sys   0m36.653s
--   (27079+2570) / (14 60) = 35 сообщений в сек
--   9121 / (14 60) = 10.8 url/sec
--   2331 / (14 60) = 2.7 жж url/sec
--   463MB данных (462MB leveldb)

-- Т.е. поиск:
--    (25+5+45/60)/(7+5) = +2.5 cpu
--    (656+409) / 462 = +2.3 hdd
--    21/14 = +1.5 time (разница меньше из-за ожидания жж)

-- Получается, поиском мы утяжеляем систему в 2.5 раза
-- Ну и ладно, зато возможностей больше.
-- Еще один расчет -- если 20минут -- 2.5тыс постов,
-- у меня где-то 300 постов за день (читаю ~70),
-- 3*24*2500/300 = 600
-- т.е. можно обработать 600 таких как я, даже с кучей жж-чек.
-- Реально подписки дублируются, параллелизм доменов больше, так что
-- может и побольше выйти. Плюс riak на 3 машины с raid-0 и будет совсем
-- весело. И это с поиском и load averages 1.5

thisIsPostsUrl u =
    atomicModifyUrlToScan'_ u (return . f)
    where f uts = uts { utsParentPaths = filter ((==) u . ppBlogFeedUrl) $
                                         utsParentPaths uts }

-- | Обновление блогов, для которых добавлено разгребание html комментариев.
-- Добавляет для каждого поста его ссылку как дочерний url.
-- Для пересканирования надо перезапустить кравлер (дабы он из newUrls по новой
-- прочел список)
-- addHtmlLinks url = withUrlQueue $ \ uq -> do
--     runEkgServer "127.0.0.1" 50123
--     t <- getUrTime
--     ps <- readPosts' url
--     msgs <- fmap (reverse . catMaybes) $ readManyMsgs $
--             map (\ h -> MsgKey url (Just $ mhGuid h) Nothing) $
--             elems $ mtHeaders $ pMsgTree ps
--     let gls = [(g, l, dlt)
--                | Msg { msgKey = MsgKey { msgKeyPostGuid = g }
--                      , msgLink = Just l, msgDlTime = dlt
-- --                     , msgText = (hnLink . parseTagsT . T.encodeUtf8 -> Just l)
--                      } <- msgs
--                , diffUrTime t dlt < 4*week ]
-- --         hnLink (TagOpen "a" [_, ("href", l)] : TagText "Comments" :
-- --                 TagClose "a" : ts)
-- --             | "http://news.ycombinator.com/item?id=" `T.isPrefixOf` l =
-- --                 Just l
-- --         hnLink (x:xs) = hnLink xs
-- --         hnLink [] = Nothing
-- --    print gls
--     forM_ gls $ \ (pguid, r, tParent) -> withLogger $ \ l -> do
--         u <- readUrlToScan' url
--         print (pguid, r)
--         addChildUrl PushBack False l uq u tParent t
--                         (SpuRedirect $ normalizeTURL r)
--                         (PuFeed url pguid :) False True

debugBlogPostsScanned u = do
    bps <- readBlogPostsScanned' (normalizeTURL $ T.pack u)
    T.putStrLn $ bpsBlogFeedUrl bps
    forM_ (Map.toList $ bpsUrls bps) $ \ (guid, us) -> do
        putStrLn ""
        T.putStrLn $ sbt guid
        forM_ (sortBy (comparing snd) $ Map.toList us) $ \ (u, (start, scan, state)) -> do
            T.putStrLn $ T.append "  * " u
--            putStrLn   $ "     " ++ show start ++ "  " ++ show scan
            s state
    where s (CUSRedirect { cusURL = u }) = return () --putStrLn $ "    " ++ T.unpack u
          s (CUSError { cusMessage = m }) = putStrLn $ "    ERR: " ++ T.unpack m
          s o = putStrLn $ "    " ++ show o
