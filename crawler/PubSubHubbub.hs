{-# LANGUAGE OverloadedStrings, ViewPatterns, ScopedTypeVariables,
             DeriveDataTypeable #-}

-- | Клиент PubSubHubbub-а, принимает сообщения и скидывает их на диск.
-- Работает отдельно от кравлера, дабы тот можно было всегда
-- остановить/перезапустить, не останавливая прием обновлений от хабов.
--
-- Кравлер может обратиться сюда по http и добавить url в хаб.
module PubSubHubbub (
    listenToHub, pubSubHubbubClient, getNewHubFeeds, sinkRequestBody
    ) where

import Network.URI
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.UTF8 as BU
import Network.HTTP.Conduit.Downloader
import URL
import Control.Monad.Trans
import Control.Concurrent
import Control.Monad
import Network.HTTP.Client (urlEncodedBody)
import qualified Control.Exception as E
import Data.Maybe
import Data.List
import Data.Typeable
import Lib.Hash
import qualified Crypto.MAC.HMAC as HMAC
import qualified Data.ByteString.Base16 as Base16
import Lib.Log
import Lib.DnsCache
import System.Info
import Lib.UrTime
import Lib.ReadUtils
import Lib.StringConversion
import qualified Data.HashMap.Strict as HM
import Data.Ratio
import System.Directory
import System.IO.Unsafe
import Lib.Regex
import Parser
import Text.HTML.TagSoup hiding (parseTags, renderTags, (~==))
import Text.HTML.TagSoup.Fast
import Parser.Custom ((~==))
import qualified Network.HTTP.Types as N
import qualified Network.HTTP.Client as C
import Generated.RiakIO
import qualified Data.Default as C
import Config

testMode = os == "darwin"

pubSubHubbubClient = main
main = withDownloaderSettings (botDownloaderSettings { dsTimeout = 5 }) $ \ cd -> withDnsCache $ \ dnsCache -> do
    lastSaveTime <- newMVar $ UrTime 0 0
    activeUrls <- newMVar $ HM.empty
    errorHubs <- newMVar $ HM.empty
    forkIO remover
    ctid <-
        forkIO $ run pshbCrawlerClientPort $ crawlerClient activeUrls errorHubs cd dnsCache
--    htid <-
    ljRun lastSaveTime
    run hubClientPort $ hubClient lastSaveTime activeUrls
--     dr <- listenToHub cd "http://techcrunch.com/?pushpress=hub"
--                 "http://techcrunch.com/feed/"
--     print dr
--     getLine
--     killThread htid
    killThread ctid

remover = forever $ do
    threadDelay (5 * 1000000)
    hf <- getNewHubFeeds `E.catch` \ (e :: E.SomeException) -> do
        logS $ "Can’t getNewHubFeeds: " ++ show e
        return []

    let len = length hf
    when (len > 1500) $ withLogger $ \ l -> do
        let toRemove = len - 1400
            rm fn =
                removeFile fn
                   `E.catch` \ (e :: E.SomeException) ->
                   logLS l $ "Can’t remove " ++ show fn ++ ": " ++ show e
        logLT l $ T.concat [ "Removing ", T.pack $ show toRemove
                           , " unprocessed feeds" ]
        logTime l "remove" $
            mapM_ rm $ take toRemove $ sort $ map fst hf

pshbHostAddr = unsafePerformIO $ resolveHostEntry pubSubHubbubHostName
{-# NOINLINE pshbHostAddr #-}

listenToHub cd hub url
    | any urlHasAuthT [url] =
        return $ DRError "URL is password protected, ignoring hub"
        -- не показываем пароль хабу
    | otherwise =
        post cd ("http://127.0.0.1:" ++ show pshbCrawlerClientPort)
        (Just pshbHostAddr) $ T.encodeUtf8 $ T.unlines [hub, url]

-- sinkRequestBody req = sinkRequestBody (10*1024*1024) req
sinkRequestBody limit req = go [] 0
    where go acc n
              | n > limit = return Nothing
              | otherwise = do
                  chunk <- getRequestBodyChunk req
                  if chunk == "" then
                      return $ Just $ B.concat $ reverse acc
                  else
                      go (chunk : acc) (n + B.length chunk)
--    bsrc C.$$ sinkByteString (10*1024*1024)

crawlerClient activeUrls errorHubs cd dnsCache req respond = respond =<< do
  [hubUrl, feedUrl] <-
      fmap (B.lines . fromMaybe "") $ sinkRequestBody (10*1024*1024) req
  liftIO $ withLogger $ \ l -> do
    let key = (bst hubUrl, bst feedUrl)
    logLS l $ "Subscribing to " ++ B.unpack feedUrl
              ++ " using " ++ B.unpack hubUrl
    au <- readMVar activeUrls
    t <- getUrTime
    case HM.lookup key au of
        Just endTime | t < endTime -> do
            logLS l "Already subscribed"
            retString status200 $ B.pack $ show $ DROK "" []
        _ -> do
            let hubUrlT = bst hubUrl
                errorCheckTime = plusUrTime t 120
                onHoldError = "Hub on hold"
            checkErrorHub <- modifyMVar errorHubs $ \ eh ->
                case HM.lookup hubUrlT eh of
                    Just endTime
                        | t < endTime ->
                            return (eh, const $ return $ DRError onHoldError)
                        | otherwise ->
                            return (HM.insert hubUrlT errorCheckTime eh, id)
                            -- сдвигаем время, чтобы в процессе проверки хаба
                            -- не было параллельных обращений
                    Nothing ->
                        return (eh, id)
            dr <- checkErrorHub $ do
                let hubUrlS = BU.toString hubUrl
                    hostName = hostNameFromUrl hubUrlS
                    params = hubSubscribeParams hubUrlS feedUrl
                mbIP <- resolveA dnsCache hostName
                case mbIP of
                    Left err -> do
                        return $ DRError $ T.unpack $ T.concat
                            [ "Can’t resolve hub host “"
                            , humanReadableHostName $ T.pack hostName
                            , "”: ", err ]
                    Right addr ->
                        downloadG
                            (return . urlEncodedBody params) cd
                            hubUrlS (Just addr)
                            []
            let clearErrorHub =
                    modifyMVar_ errorHubs $ return . HM.delete hubUrlT
            case dr of
                DROK _ _ -> clearErrorHub
                DRError e
                    | "HTTP 4" `isPrefixOf` e -> clearErrorHub
                      -- пользовательские ошибки не считаем за нерабочий хаб
                      -- только таймаут и HTTP 5xx
                    | e == onHoldError -> return () -- не двигаем время
                _ ->
                    modifyMVar_ errorHubs $ return .
                    HM.insert hubUrlT errorCheckTime
            retString status200 $ B.pack $ show dr

hubSubscribePostData hubUrl feedUrl = do
    let C.RequestBodyLBS lbs =
            C.requestBody $
            urlEncodedBody (hubSubscribeParams hubUrl feedUrl) C.defaultRequest
    BL.putStrLn lbs

hubSubscribeParams hubUrlS feedUrl =
    [ ("hub.callback",
       B.concat [ "https://crawler.bazqux.com/pshb_ping"
                , if testMode then "_test" else ""
                , "?hub="
                , B.pack $ encodeURIComponent hubUrlS
                , "&feed="
                , e feedUrl
                , "&verify="
                , e $ pubSubHubbubVerifyToken feedUrl
                ])
    , ("hub.mode", "subscribe")
    , ("hub.topic", feedUrl)
    , ("hub.secret", pubSubHubbubSecret feedUrl )
    , ("hub.lease_seconds", "864000")
    ]
    where e = B.pack . encodeURIComponent . B.unpack

hubClient lastSaveTime activeUrls req respond = respond =<< do
    bs <- fmap (fromMaybe "") $ sinkRequestBody (10*1024*1024) req
    liftIO $ withLogger $ \ l -> do
        let look k a = join $ lookup k a
        case () of
            _ | h <- queryString req
              , Just hub <- look "hub" h
              , Just topic <- look "hub.topic" h
              , Just "subscribe" <- look "hub.mode" h
              , Just challenge <- look "hub.challenge" h
              , Just leaseSeconds <- look "hub.lease_seconds" h
              , Just (min day -> ls) <- tryReadUnsignedInt (bst leaseSeconds)
                -- на всякий пожарный переподпишемся через сутки
              , Just vt <- look "verify" h
              , vt == pubSubHubbubVerifyToken topic
              -> do
                t <- getUrTime
                modifyMVar_ activeUrls $ \ au -> return $
                    HM.insert (bst hub, bst topic) (t `plusUrTime` ls) au
                logLT l $ T.concat [ "Subscribed to ", bst topic
                                   , " using ", bst hub ]
                retString status200 challenge

            _ | Just hub <- look "hub" (queryString req)
              , Just feed <- look "feed" (queryString req)
              , Just sign <- lookup "X-Hub-Signature" (requestHeaders req)
              , sign == B.append "sha1="
                        (Base16.encode $ sha1_digest2bs $ HMAC.hmacGetDigest $
                         HMAC.hmac (pubSubHubbubSecret feed) bs)
              -> do
                logLT l $ T.concat [ "Data arrived for "
                                   , maskUrlPasswordT $ bst feed
                                   , " from ", bst hub]
                when (not $ urlHasAuthT $ bst feed) $
                    -- такого происходить не должно (подписка на запароленные
                    -- фиды отменяется), но, на всякий случай,
                    -- игнорируем такие фиды
                    writeNewHubFeed lastSaveTime feed bs
                retString status200 ""

            _ -> do
                logLT l "Unknown data"
                logLT l "requestHeaders:"
                mapM_ (logLS l . show) (requestHeaders req)
                logLT l "queryString:"
                mapM_ (logLS l . show) (queryString req)
                logLT l "data:"
                logLT l $ T.take 60 $ bst bs
                retString status200 "Are you hub???"
--            retString badRequest400 "Are you hub???"
--     return $
--         case pathInfo req of
--             ["yay"] -> yay
--             x -> index x

writeNewHubFeed lastSaveTime feed bs = do
    t <- getUrTime
    t <- modifyMVar lastSaveTime $ \ lt -> return $
        if lt /= t then (t, t)
        else let t' = t `plusUrTime` (1%1000000) in (t',t')
    B.writeFile (dir ++ urTimeFileName t ++ "-"
                 ++ encodeURIComponent (B.unpack feed) ++ ".xml") bs

retString status s = return $
    responseBuilder status [ ("Content-Type", "text/plain") ] $ copyByteString s

dir | testMode = "PuSH/"
    | otherwise = "/tank/PuSH/"

getNewHubFeeds = do
    let feed fn
            | null f = Nothing
            | otherwise = Just (dir ++ fn, f)
            where f = unEscapeString $
                      reverse $ drop (T.length ".xml") $ reverse $
                      drop (T.length "2012-04-08-22.31.37.646950-") fn
    fmap (catMaybes . map feed) $ getDirectoryContents dir


------------------------------------------------------------------------------
-- Вытаскивание жж

ljWatchdogInterval = 120
-- от ЖЖ примерно каждую секунду приходит <time>1393451244</time>
-- плюс время на соединение. 10 секунд это с запасом.
-- --> теперь у ЖЖ включилась буферизация, может и 20-30 секунд буфер копить

ljRealtimeTest = withDownloader $ \ _ -> do -- для инициализации OpenSSL
    t <- newMVar $ UrTime 0 0
    ljRun t
ljRun st = do
    it <- newMVar $ UrTime 0 0
    tid <- forkIO $ ljRealtimeLoop it st
    forkIO $ forever $ do
        threadDelay $ 500*1000
        t <- getUrTime
        lastT <- readMVar it
        when (diffUrTime t lastT > ljWatchdogInterval) $ do
            logS $ "Restart at " ++ show t
            updateLastTime it
            --  ^ чтобы еще минимум ljWatchdogInterval секунд не перегружал
            E.throwTo tid WatchdogRestart
data WatchdogRestart = WatchdogRestart
    deriving (Show, Typeable)
instance E.Exception WatchdogRestart

ljRealtimeLoop lastInputTime lastSaveTime = E.mask $ \ release -> forever $ do
    r <- E.try $ release $ ljRealtime lastInputTime lastSaveTime
         -- позволяем WatchdogRestart только внутри E.try
         -- а в процессе обработки exception-а нет
         -- (а то при threadDelay после recv resource vanished, может
         -- пришить нитку навечно)
    case r of
        Right () ->
            logS "LiveJournal sink finished?"
        Left (E.fromException -> Just WatchdogRestart) -> do
            logS "LiveJournal watchdog restart"
        Left (e :: E.SomeException) -> do
--            incrStat "exceptions" 1
            logS $ "LiveJournal exception: " ++ show e
            threadDelay $ 500*1000

-- У ЖЖ теперь есть фид с последними постами, можно делать poll,
-- но вроде с ЖЖ и так проблем пока никаких нет
-- http://www.livejournal.com/stats/latest-rss.bml
-- https://www.livejournal.com/bots/

ljRealtime :: MVar UrTime -> MVar UrTime -> IO ()
ljRealtime lastInputTime lastSaveTime = do
    updateLastTime lastInputTime
    manager <- C.newManager (dsManagerSettings C.def)
    req <- C.parseRequest "http://atom.services.livejournal.com/atom-stream.xml"
    C.withResponse (req { C.responseTimeout = C.responseTimeoutNone }) manager $ \ res ->
        if N.statusCode (C.responseStatus res) /= 200 then do
            logS $ "ljRealtime: invalid status code: " ++ show (C.responseStatus res)
            threadDelay $ 500*1000
        else
            sinkLJRealtime lastInputTime lastSaveTime
                $ C.brRead $ C.responseBody res
updateLastTime var =  liftIO $ do
    t <- getUrTime
    modifyMVar_ var $ return . const t
--    logS $ "data at " ++ show t
sinkLJRealtime :: MVar UrTime -> MVar UrTime -> IO B.ByteString -> IO ()
sinkLJRealtime lastInputTime lastSaveTime readChunk = go ""
    where go acc = do
              updateLastTime lastInputTime
              -- liftIO $ logT "C.await"
              inp <- readChunk
              if B.null inp then
                  return ()
              else do
                  -- liftIO $ logT $ bst inp
                  acc' <- checkFeed $ B.append acc inp
                  go acc'
          checkFeed acc
              | (_, p) <- B.breakSubstring "<feed " acc
              , (f, xs) <- B.breakSubstring "</feed>" p
              , xs /= "" = do
                  let dat = B.append f "</feed>"
                      j = findJournal $ parseTagsT dat
                      l = findLink $ parseTagsT dat
                      rp = repost $ parseTagsT dat

                  t <- getUrTime
                  -- liftIO $ logS $ show (j,l,rp)
                  case (parse t "" dat, rp, j, l) of
                      (PRFeed _ m _, False, Just journal, Just link)
                          | Just (atomId, atomDat, rssDat) <- fixId journal dat -> do
                              tryWrite link atomDat
                              tryWrite (T.append link "/") atomDat
                              let rss = T.replace "data/atom" "data/rss" link
                              tryWrite rss rssDat
                              tryWrite (T.append rss "/") rssDat
--                               liftIO $ logT link
--                               liftIO $ putStrLn $ "    checkId " ++ show link
--                                   ++ " " ++ show atomId
--                           liftIO $ B.writeFile (T.unpack $ T.concat ["ljfeeds/", T.replace ":" "_" atomId , ".xml"]) atomDat
--                           liftIO $ B.writeFile (T.unpack $ T.concat ["ljfeeds/", T.replace ":" "_" atomId , ".rss.xml"]) rssDat
--                               liftIO $ print (journal, link, atomId)
--                               liftIO $ B.putStrLn atomDat
                      _ -> return ()
--                   liftIO $ B.putStrLn "=========="
                  checkFeed xs
              | B.length acc > 10*1000*1000 =
                  return "" -- какой-то мусор
              | otherwise =
                  return acc
          tryWrite url dat = liftIO $ do
              u <- readUrlToScan url
              case u of
                  Just _ -> do
                      logT $ T.concat [ "Data arrived for ", url
                                      , " from livejournal stream "]
                      writeNewHubFeed lastSaveTime (T.encodeUtf8 url) dat
                  Nothing -> return ()
          findJournal [] = Nothing
          findJournal (TagOpen "lj:journal" _ : TagText j : _) = Just j
          findJournal (_:xs) = findJournal xs
          findLink [] = Nothing
          findLink (TagOpen "link" (("rel","self"):
                                    ("type","application/atom+xml"):
                                    ("href", href):_) : xs)
              | "/data/atom" `T.isSuffixOf` href = Just href
          findLink (_:xs) = findLink xs
          repost [] = False
          repost (TagOpen "content" _ : TagText t : TagClose "content" : _) =
              case T.words t of
                  [l] | [_] <- regexGet "^http.*livejournal.com.*/([0-9]+)\\.html$" l -> True
                  _ -> False
          repost (_:xs) = repost xs
          fixId journal dat
              | (head, entry) <- B.breakSubstring "<entry>" dat
              , (entry, tail) <- B.breakSubstring "</entry>" entry
              , tail /= ""
              , Just (TagOpen "link" (("href", l):_)) <-
                  find (~== TagOpen "link" []) $ parseTagsT entry
              , [[_,postId]] <- regexGet "/([0-9]+)\\.html$" l
              , atomId <- T.concat [ "urn:lj:livejournal.com:atom1:"
                                   , journal, ":", postId ]
              = let f i = B.concat
                          [ head, entry
                            -- вставляем id в конце
                          , "    <id>", T.encodeUtf8 i, "</id>\n"
                          , tail ]
                in
                  Just (atomId, f atomId, f l)
              | otherwise = Nothing

-- checkId url i = do
--     PRFeed _ _ fms <- parseUrl url
--     when (isNothing (find (\ fm -> fmGuid fm == i) fms)) $ do
--         putStrLn url
--         putStrLn $ T.unpack i
-- check'em = do
--     return ()
