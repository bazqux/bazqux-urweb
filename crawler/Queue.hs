{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, ViewPatterns,
             BangPatterns, ScopedTypeVariables, RankNTypes, StandaloneDeriving #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- | Очередь на скачивание
module Queue
    (main, queueScanListKeys')
    where

import qualified Network.Socket as NS
import Control.Concurrent
import qualified Control.Concurrent.MSem as MSem
import Control.Monad
import Control.Arrow
import Control.DeepSeq
import qualified Control.Exception as E
import Data.IORef
import Data.Maybe
import Data.Either
import Data.List
import Data.Ord
import Data.Bits
import Data.Function
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import System.Posix.Signals
import System.Directory
import System.Exit
import System.Info
import System.IO.Error
import Generated.DataTypes
import Generated.RiakIO
import Resolvables
import Network.HTTP.Conduit.Downloader
import Parser hiding (test)
import Parser.Custom (groupByN)
import Lib.DnsCache
import Lib.Log
import Lib.UrTime
import qualified Lib.BArray as BA
import Lib.Stats
import Lib.Queue
import URL
import ParseServer
import Parser.Types
import PubSubHubbub
import Riak (blockBucketKey, key, riakBucketKeys')
import Discovery
import Search
import Lib.ElasticSearch
import Config
import Lib.Hash
import Subscriptions
import HotLinks (hotLinksDisabled)

-- | Очередь на скачивание для заданного IP.
-- Должно быть 4 очереди:
--  * подписки -- всегда в первую очередь
--  * новый url-ы (линк на новый коммент)
--  * next-ы, для поблочного выгребания (blogspot/disqus)
--    не сохраняются на диске, могут содержать промежуточные данные (сообщения
--    с указателями на parent-ов, которые пока еще не выкачаны для disqus).
--    (вот интересно, касательно ниток в жж -- думается, что страницы будут
--     отдельными url, а вот url-ы ниток не будут сохраняться,
--     т.к. по свернутым сообщениям можно выяснить, сканировались они или нет
--     и не сканировать лишний раз кучу ниток)
--  * обычные url.
--
-- Кроме очереди, интересен размер очереди и среднее время скачивания.
-- Также интересно число добавленных в очередь на диске (через N-минут).
-- Число положенных в очередь уменьшается после скачивания.
-- При добавлении url-а через N-минут к его времени добавляется
--  ср. время скачивания * (размер очередей + число положенных в очередь)
-- таким макаром мы добиваемся чуть более равномерного распределения.
-- И когда подряд кладется куча комментов от жж, например, они будут
-- добавляться с более-менее адекватным интервалом.
--
-- Также не надо перебарщивать с тунелированием запросов, во первых, из-за
-- подписок -- желательно получать их поскорее, а не ждать пачки; во-вторых
-- у нас теперь размер очереди не ограничен и не стоит делать уж слишком большие
-- пачки.
-- Можно либо ограничится 5-ю скачиваемыми url-ами либо, все-таки,
-- сделать вообще дополнительную скачивалку для подписок (что рисковано
-- для lj, но не знаю, насколько там за этим следят).
-- Кстати, в lj говорят не больше 5 соединений в секунду, а не запросов.
data IPQueue
    = IPQueue
      { -- | очереди url-ов на подписку -- обрабатываются в
        -- порядке QueueTypeOrder.
        ipqUrls :: !(Map QueueTypeOrder (Queue UTS))
        -- | Счетчик для перемешивания очередей.
        -- Стоит младший бит, значит первая очередь (например, QTSubscription),
        -- следующий -- следующая, и т.д.
        -- Таким образом мы все-таки сканируем менее приоритетные
        -- очереди даже при переполнении более приоритетных,
        -- правда с 2^n меньшей частотой.
      , ipqInterleaveCounter :: !Int
        -- | Список очередей для повторной обработки.
        -- При отмене сканирования (не совпадает nextScanTime, пост уже удален),
        -- берется следующий URL из той же очереди (т.к. предыдущий
        -- был фактически не обработан)
      , ipqRetryQueues :: ![QueueType]
        -- | время окончания обработки
        -- (max-min)/(n-1) = среднее время обработки
      , ipqProcessTimes :: !(Queue UrTime)
        -- | Число активных скачивателей-обработчиков
      , ipqActiveWorkers :: !Int
      , ipqHostName :: !T.Text
      , ipqKey :: !IPQueueKey
      , ipqRateLimitMVar :: !(MVar ())
      }

newtype QueueTypeOrder = QueueTypeOrder { unQueueTypeOrder :: QueueType }
    deriving (Eq)
deriving instance Enum QueueType
instance NFData QueueType where rnf = rwhnf

instance Ord QueueTypeOrder where
    compare = comparing (o . unQueueTypeOrder)
         where o QTTemporary1 = 0
               o QTSubscription = 1
               o QTSubscriptionOPML = 2
               o qt = fromEnum qt + 10

instance Show QueueTypeOrder where
    show = show . unQueueTypeOrder

data UTS
    = UTS
      { uTime :: {-# UNPACK #-} !UrTime -- (Key ScanList)
      , uUrl  :: {-# UNPACK #-} !T.Text
      }
    deriving (Eq, Ord, Show)

data IPQueueKey
    = IPQKDefault !NS.HostAddress
    | IPQKFacebook
    | IPQKTwitter
    | IPQKReddit
    | IPQKDisqus
    | IPQKGoogleNews
    | IPQKInstagram
    | IPQKCantResolve !T.Text
    | IPQKVK
    deriving (Eq, Ord, Show)

ipqKeyHostAddress (IPQKDefault ha) = Just ha
ipqKeyHostAddress _ = Nothing

-- Для reddit/twitter/g+ должны быть IPQueue без IP, они должны делать
-- resolve прямо после вытаскивания из очереди следующего URL-а

emptyIPQueue host key = do
    rl <- newMVar ()
    return $
        IPQueue
        { ipqUrls = Map.empty
        , ipqInterleaveCounter = 0
        , ipqRetryQueues = []
        , ipqProcessTimes = emptyQ
        , ipqActiveWorkers = 0
        , ipqHostName = T.pack host
        , ipqKey = key
        , ipqRateLimitMVar = rl
        }

data UrlQueue
    = UrlQueue
      { uqDnsCache :: DnsCache
      , uqDownloader :: Downloader
        -- | Список обработынных url-ов, для удаления в нитке выгребания
      , uqProcessedUrls :: MVar [UTS]
        -- | Последний, вытащенный из очереди элемент.
        -- В это время можно сохранять новые url-ы и не бояться, что они
        -- еще раз добавятся в очередь
      , uqLastProcessedTime :: MVar UrTime
        -- | Ограничитель на число одновременоо обрабатываемых хабов
      , uqProcessHubSem :: MSem.MSem Int
        -- | Ограничитель на кол-во нитей, резольвящих IP.
      , uqDnsSem :: MSem.MSem Int
        -- | Ограничитель на число одновременных worker-ов.
        -- Не более maxWorkerThreads
      , uqWorkerSem :: MSem.MSem Int
        -- | Домены, которые обрабатываются сейчас, с кол-вом worker-ов.
      , uqActiveDomains :: MVar (Map IPQueueKey Int)
      , uqWorkersCount :: MVar Int
        -- | Ограничитель на скачивание и парсинг
      , uqDownloadAndParseSem :: MSem.MSem Int
        -- | Ограничитель на обработку (обновление данных)
      , uqProcessSem :: MSem.MSem Int
        -- | Очереди на скачивание по доменам.
        -- IORef чтобы пореже модифицировать карту (очередь может быть
        -- большой, а активных доменов немного)
      , uqQueues :: MVar (Map IPQueueKey (IORef IPQueue))
        -- | Индексы прокси для равномерного проксирования некоторых сервисов
      , uqProxyIndexes :: MVar (Map IPQueueKey Int)
        -- | Добавление новых url-ов. Производится в отдельной нити,
        -- чтобы очередь сама себя не заблокировала.
      , uqAddNewUrl :: Chan (IO ())
      , uqSize :: MVar Int
      , uqDnsCount :: MVar Int
      , uqDnsCount2 :: MVar Int
      , uqDownloadsCount :: MVar Int
      , uqUnprocessedCount :: MVar Int
      , uqProcessingsCount :: MVar Int
      , uqSearchCount :: MVar Int
      , uqRateLimit
          :: TURL -> Logger -> MVar () -> IO () -> IO ()
      , uqCompleteBPS :: MVar (HM.HashMap T.Text UrTime)
      , uqWorkerRequestScheduler :: WorkerRequestScheduler
      , uqFeedRemovesCount :: MVar Int
      }

instance UQ UrlQueue where
    uqDownloader_ = uqDownloader
    uqCompleteBPS_ = uqCompleteBPS
    uqGetUrTime _ = getUrTime

withUrlQueue f =
    withDownloaderSettings botDownloaderSettings $ \ uqDownloader ->
    withDnsCache $ \ uqDnsCache ->
--    DnsCache2.withDnsCache $ \ uqDnsCache2 ->
    withWorkerRequestScheduler $ \ uqWorkerRequestScheduler -> do
        uqDnsSem <- MSem.new maxDnsThreads
        uqWorkerSem <- MSem.new maxWorkerThreads
        uqProcessSem <- MSem.new maxProcessThreads
        uqProcessHubSem <- MSem.new maxProcessHubThreads
        uqDownloadAndParseSem <- MSem.new maxDownloadAndParseThreads
        t <- getUrTime
        uqLastProcessedTime <- newMVar $ UrTime 0 0
        uqProcessedUrls <- newMVar []
        uqAddNewUrl <- newChan
        uqQueues <- newMVar Map.empty
        uqProxyIndexes <- newMVar Map.empty
        uqSize <- newMVar 0
        uqDnsCount <- newMVar 0
        uqDnsCount2 <- newMVar 0
        uqActiveDomains <- newMVar Map.empty
        uqWorkersCount <- newMVar 0
        uqDownloadsCount <- newMVar 0
        uqUnprocessedCount <- newMVar 0
        uqProcessingsCount <- newMVar 0
        uqSearchCount <- newMVar 0
        uqRateLimit <- newRateLimit
        uqCompleteBPS <- newMVar HM.empty
        uqFeedRemovesCount <- newMVar 0
        let uq = UrlQueue {..}
        f uq

incrUqSize (UrlQueue {..}) = modifyMVar_ uqSize $ \ !cnt -> do
--     logS $ "uqSize: incr " ++ show cnt ++ " -> " ++ show (cnt+1)
    return $ cnt+1
decrUqSize (UrlQueue {..}) = modifyMVar_ uqSize $ \ !cnt -> do
--     logS $ "uqSize: decr " ++ show cnt ++ " -> " ++ show (cnt-1)
    return $ cnt-1

addUrl :: UrlQueue -> AddUrl -> IO ()
addUrl uq au = do
    r <- E.try $ addUrl' uq au
    case r of
        Right () -> return ()
        Left (e :: E.SomeException) -> do
            logS $ "Exception in addUrl " ++ T.unpack (auUrl au) ++ " : " ++ show e
            incrStat "exceptions" 1
addUrl' :: UrlQueue -> AddUrl -> IO ()
addUrl' uq@(UrlQueue {..}) (AddUrl {..}) = do
    incrUqSize uq
    trUrl <- transformedUrl auUrl
    let hostName = hostNameFromUrl $ T.unpack trUrl
        url = T.unpack auUrl
        logResolveTime l a = do
            (t, r) <- time a
            logLS l ("resolve: " ++ showSecs t ++ "  " ++ hostName)
            return r
        -- TODO: не обрабатывает url с '[' в запросе
        -- а также '>' и utf символами
        queue key =
            join $ withIPQueue uqQueues hostName key $ \ qs ipqRef ->
                addToIPQueue auPushOrder uq qs ipqRef
                             (QueueTypeOrder auQueueType) auTime auUrl
        handleIP' mbIP = case mbIP of
            Left err -> do
                incrStat "dnsErrors" 1
                queue $ IPQKCantResolve err
            Right hostAddress -> void $ forkIO $ do
                case auPushOrder of
                    Delay sec -> threadDelay (1000000*sec)
                    _ -> return ()
                -- пускай каждую секунду для пущей равномерности
                -- все равно сканятся реже чем в 334000
                let ha = hostAddress
                    hn = T.pack hostName
                    key | Just d <- match trUrl = d
                        | googleNewsUrl trUrl = IPQKGoogleNews
                        | otherwise = IPQKDefault ha
                    match = matchDomains'
                        [ ("twitter.com", \ _ _ -> IPQKTwitter)
                        , ("facebook.com", \ _ _ -> IPQKFacebook)
                        , ("disqus.com", \ _ _ -> IPQKDisqus)
                        , ("reddit.com", \ _ _ -> IPQKReddit)
                        , ("vk.com", \ _ _ -> IPQKVK)
                        , ("instagram.com", \ _ _ -> IPQKInstagram)
                        ]
                queue key
        handleIP mbIP = do
            r <- E.try $ handleIP' mbIP
            case r of
                Right () -> return ()
                Left (e :: E.SomeException) -> do
                    logS $ "Exception in addUrl " ++ url ++ " : " ++ show e
                    incrStat "exceptions" 1
    cr <- resolveCachedA uqDnsCache hostName
    case cr of
        Just r ->
            handleIP r
            -- не занимаем семафор и не форкаем лишних ниток, если IP уже в кеше
        Nothing -> do
            MSem.wait uqDnsSem
            void $ forkIO $ flip E.finally (MSem.signal uqDnsSem) $ countProcess "dns" uqDnsCount $ withLogger $ \ l -> logResolveTime l $ do
                mbIP <- resolveA uqDnsCache hostName
                incrStat "resolveCount" 1
                case mbIP of
                    Left _ -> incrStat "dnsErrorsReq" 1
                    _ -> return ()
                handleIP mbIP

ipqUrlsStats ipq =
    concatMap (\ (t,q) -> " " ++ show t ++ ":" ++ show (lengthQ q))
                  (Map.toList (ipqUrls ipq))
logIPQ uq qs ipq = do
    total <- readMVar $ uqSize uq
    !domains <- fmap Map.size $ readMVar $ uqActiveDomains uq
    !workers <- readMVar $ uqWorkersCount uq
    return ()
--     let blocked = max 0 $ IntMap.size qs - domains
--     logS $ "Queue sizes: " ++ show domains
--          -- ++ " " ++ show workers
--          ++ "/" ++ show total
--          ++ (if blocked > 0 then " [" ++ show blocked ++ "]" else "")
--          -- активные домены / очередей в ожидании / всего url-ов
--          ++ ipqUrlsStats ipq
--          ++ " / " ++ show (ipqActiveWorkers ipq)
--          ++ " " ++ ipqHostName ipq
--          ++ (if ipqBumped ipq then " !" else "")

addToIPQueue pushOrder uq qs ipqRef qt !urlTime !utsUrl = do
    !ipq <- readIORef ipqRef
    logIPQ uq qs ipq
    let !ipqUpd =
            ipq { ipqUrls =
                      Map.alter (Just . (case pushOrder of
                                             PushFront -> pushFrontQ
                                             Delay 0 -> pushFrontQ
                                             PushBack -> pushQ
                                             Delay _ -> pushQ)
                                        (UTS urlTime utsUrl) .
                                 fromMaybe emptyQ) qt
                           (ipqUrls ipq)
                }
        av = ipqActiveWorkers ipqUpd
        -- Запускаем дополнительные параллельные обработчики для фидов,
        -- которые долго качаются, и не могут приблизиться к rate limit
        bumps = [("youtube.com", 4)
                 -- чтобы сканировало остальные, пока
                 -- newsubscriptionvideos обрабатываются

                ,("twitter.com", twitterWorkers)
                ,("facebook.com", facebookWorkers)
                ,("news.google.com", 3)
                ,("ftr.bazqux.com", 4)
                ,("fc.bazqux.com", 4)
                ]
        host = hostNameFromUrlT utsUrl
        dlBump = find ((`T.isSuffixOf` host) . fst) bumps
        workersCount = maybe 1 snd dlBump
    if av < workersCount then do
        writeIORef ipqRef $! ipqUpd { ipqActiveWorkers = succ av }
        return $ do
            -- withLogger $ \ l ->
                -- logTime l "wait worker" $
            MSem.wait (uqWorkerSem uq)
--             when (av == 0) $ do
--                 modifyMVar_ (uqDomainsCount uq) $ \ !(succ -> c) -> return c
--                logS $ "DDD: +Domain " ++ ipqHostName ipq
--                почему доменов больше, чем worker-ов??
--            logS $ "DDD: +Worker " ++ show av ++ " " ++ ipqHostName ipq
            modifyMVar_ (uqWorkersCount uq) $ \ (succ -> !c) -> return c
            incrGauge "workers"
            void $ forkIO $ do
                -- из-за того, что первый worker может запуститься и выйти,
                -- а второй при этом может продолжать ждать семафора,
                -- точно определить кол-во активных доменов простым счетчиком
                -- прибавляющемся на первом воркере и убирающемся на последнем
                -- не выйдет.
                modifyMVar_ (uqActiveDomains uq) $ \ d ->
                    return $! Map.alter (Just . succ . fromMaybe 0)
                               (ipqKey ipq) d

                processLock <- newMVar ()
                domainWorker processLock uq ipqRef
                takeMVar processLock
                --  ^ ждем окончания сохранения в БД

                modifyMVar_ (uqActiveDomains uq) $ \ d ->
                    return $! Map.alter
                               (\ (fromMaybe 0 -> n) ->
                                    if n <= 1 then Nothing
                                    else Just (n-1))
                               (ipqKey ipq) d
                decrGauge "workers"
                modifyMVar_ (uqWorkersCount uq) $ \ (pred -> !c) -> return c
                MSem.signal (uqWorkerSem uq)
    else do
        writeIORef ipqRef ipqUpd
        return (return ())

popIPQueue ipq@(IPQueue {..})
    | (rqt:rqts) <- ipqRetryQueues
    , rqtO <- QueueTypeOrder rqt
    , Just q <- Map.lookup rqtO ipqUrls
    , Just (r, q') <- tryPopQ q =
        Just (rqt, r, ipq { ipqUrls =
                                if lengthQ q' == 0 then Map.delete rqtO ipqUrls
                                else Map.insert rqtO q' ipqUrls
                          , ipqRetryQueues = rqts })
    | otherwise = go 0 [] (map addW $ Map.toAscList ipqUrls)
    where ik = ipqInterleaveCounter + 1
          w QTSubscription = 3  -- в 7 раз чаще остальных
          w QTSubscriptionOPML = 2  -- в 3 раза чаще остальных
          w QTNewComment1 = 2
          w QTNewComment = 2
          w _ = 1
          addW (qt, q) = (qt, w $ unQueueTypeOrder qt, q)
          go _ [] [] = Nothing
          go _ _  [] = popIPQueue $ ipq { ipqInterleaveCounter = ik }
                       -- прошли по счетчику до конца, повторяем с начала
          go n acc ((qt, 0, q) : qs) = go n ((qt,q):acc) qs
          go n acc ((qt, w, q) : qs)
              | testBit ik n || qt == QueueTypeOrder QTTemporary1 --  || null qs
              , Just (r, q') <- tryPopQ q =
                  --  ^ всегда Just, пустые очереди не хранятся
                  Just (unQueueTypeOrder qt, r,
                        ipq { ipqUrls = Map.fromDistinctAscList $ reverse acc ++
                               (if lengthQ q' == 0 then id else ((qt, q') :))
                               [(qt,q) | (qt,_,q) <- qs]
                            , ipqInterleaveCounter = ik
                            })
              | otherwise = go (n+1) acc ((qt, w-1, q) : qs)

withIPQueue uqQueues host key f =
    -- мы не удаляем очереди доменов, так что MVar домена отдельно,
    -- а MVar мапки отдельно
    modifyMVar uqQueues $ \ qs ->
        case Map.lookup key qs of
            Just r -> do
                fr <- f qs r
                return (qs, fr)
            Nothing -> do
                r <- newIORef =<< emptyIPQueue host key
                fr <- f qs r
                return (Map.insert key r qs, fr)

retryQueue l uq qt key = do
    logLS l $ "Retrying " ++ show qt
    withMVar (uqQueues uq) $ \ qs ->
        case Map.lookup key qs of
            Just r ->
                modifyIORef r $ \ ipq ->
                    ipq { ipqRetryQueues = qt : ipqRetryQueues ipq}
            Nothing ->
                return ()

domainWorker processLock uq@(UrlQueue {..}) ipqRef = do
    task <- modifyMVar uqQueues $ \ qs -> do
        !ipq <- readIORef ipqRef
        logIPQ uq qs ipq
        case popIPQueue ipq of
            Just (qt, r, !ipq') -> do
                writeIORef ipqRef ipq'
                return (qs,
                        Right ( qt, ipqRateLimitMVar ipq, r
                              , ipqHostName ipq, ipqKey ipq ))
            _ | ipqActiveWorkers ipq == 1 -> do
--                 logS $ "DDD: -Domain " ++ ipqHostName ipq
--                 modifyMVar_ uqDomainsCount $ \ !(pred -> c) -> return c
                return (Map.delete (ipqKey ipq) qs,
                        Left (ipqHostName ipq, ipqKey ipq))
            _ -> do
                writeIORef ipqRef $!
                    ipq { ipqActiveWorkers = ipqActiveWorkers ipq - 1 }
                return (qs,
                        Left (ipqHostName ipq, ipqKey ipq))
    case task of
        Right (qt, rlMVar, u@(UTS urlTime utsUrl), hn, key) -> do
            let url = utsUrl
            withLogger $ \ l ->
                logExceptions l $
                (do logLT l $ T.concat [ "\nProcessing ", maskUrlPasswordT url ]
                                      -- , "\nIP ", T.pack $ showHostAddress ha
                    uts <- readUrlToScan' url
                    let nst = utsNextScanTime uts
                        end = modifyMVar_ uqProcessedUrls (return . (u:))
                    if nst /= UrTime 0 0 && nst /= urlTime
                           && isQueueScanListKey urlTime then do
                        logLS l $ "Cancel processing: queueTime "
                                  ++ show urlTime ++ " /= nst " ++ show nst
                        retryQueue l uq qt key
                        end
                        -- url уже в другом месте очереди
                    else
                        processUrl rlMVar processLock l uq uts qt hn key end
                 `E.finally` do
                     -- logLT l "Done"
                     decrUqSize uq)
            domainWorker processLock uq ipqRef
        Left (hostName, key) -> do
--             logS $ "DDD: -Worker " ++ show num ++ " " ++ hostName
            return () -- задачи закончились

logExceptions l act =
    E.catch act $
        \ (e :: E.SomeException) -> do
            incrStat "exceptions" 1
            logLS l $ "Exception: " ++ show e

statsSaver = forever $ do
    threadDelay $ 10*1000*1000
    sm <- getStatsMapAndClean
    modifyStats'_ "crawler" $ \ s -> return $
        s { statsMap = Map.unionWith (+) (statsMap s) $
                       Map.fromList $ map (\(n,v) -> (n,fromEnum v)) $
                       HM.toList sm }

showIPQKey hn k = case k of
    IPQKDefault _ -> hn
    IPQKFacebook -> "Facebook"
    IPQKTwitter -> "Twitter"
    IPQKReddit -> "Reddit"
    IPQKDisqus -> "Disqus"
    IPQKInstagram -> "Instagram"
    IPQKGoogleNews -> "Google News"
    IPQKCantResolve _ -> "Can’t resolve"
    IPQKVK -> "VK"

topDomainsLogger uq = forever $ do
    threadDelay $ 60*1000*1000
    ipqs <- withMVar (uqQueues uq) $ \ qs -> forM (Map.elems qs) $ readIORef
    let ipqUrlsCount = sum . map lengthQ . Map.elems . ipqUrls
        ipqsC = reverse $ sortBy (comparing fst)
                [(ipqUrlsCount q, q) | q <- ipqs, ipqUrlsCount q > 10]
--    when (notNull ipqsC) $
    logS $ "Top domain queues:\n"
            ++ unlines [show c ++ replicate (7 - length (show c)) ' '
                        ++ " "
                        ++ T.unpack (showIPQKey (ipqHostName q) (ipqKey q))
                        ++ (if ipqActiveWorkers q > 1 then " !" else "")
                        ++ maybe "" (\ ha -> " (" ++ showHostAddress ha ++ ")")
                               (ipqKeyHostAddress $ ipqKey q)
                        ++ ": "
                        ++ ipqUrlsStats q
                       | (c, q) <- ipqsC]

hubUpdater uq = forever $ do
    nf <- getNewHubFeeds `E.catch` \ (e :: E.SomeException) -> do
        logS $ "Can’t getNewHubFeeds: " ++ show e
        return []
    when (null nf) $
        threadDelay $ 1*1000*1000
    forM_ nf $ \ (fn, f) -> processHubFeed uq fn f
    -- ждем завершения всех обработчиков, чтобы они файлы удалили
    replicateM_ maxProcessHubThreads $ MSem.wait $ uqProcessHubSem uq
    replicateM_ maxProcessHubThreads $ MSem.signal $ uqProcessHubSem uq

withNewUrls uq act = do
    newUrls <- newIORef []
    act newUrls `E.finally` do
        nu <- fmap (nubBy ((==) `on` auUrl) . reverse) $ readIORef newUrls
        mapM_ mergeWriteScanList [ScanList t [(u,qt)] | AddUrl _ qt t u <- nu]
        mapM_ (writeChan (uqAddNewUrl uq) . addUrl uq) nu


processHubFeed uq fn f = do
    MSem.wait $ uqProcessHubSem uq
    threadDelay $ 100*1000
    -- задержка на случай пингов разных адресов одного и того же фида,
    -- чтобы меньше было retry при записи UrlToScan
    void $ forkIO $ flip E.finally (MSem.signal $ uqProcessHubSem uq) $
      withLogger $ \ l -> logTime l "total   " $
      (do logLS l $ "\nProcessing hub feed " ++ f ++ " (" ++ fn ++ ")"
          incrStat "hubFeeds" 1
          dat <- B.readFile fn
          let url = T.pack f
          t <- getUrTime
          u <- readUrlToScan' url
          ps <- readPostsSubscribers' url
          let dHash = hashData dat
              updUts u = u { utsDataHash = dHash }
              goodHub =
                  False
                  -- лучше всегда сканировать, а то какой-нибудь TechCrunch
                  -- все время шлет обновления и, если в работе кравлера
                  -- была задержка, обновление исходного фида постоянно
                  -- откладывается.

                  -- фид, который должен быть нормальным и не требовать
                  -- ручного пересканирования
--                   any (`T.isInfixOf` url)
--                   [ "feeds/posts/default", ".blogspot.", ".blogger.com"
--                   , ".tumblr.", "reddit.", "news.google."
--                     --  ^ хаба нет, но на всякий случай, чтобы очереди
--                     -- не переполнялись
--                   , "livejournal.com"
--                   , "feedburner.com" ]
              unusedFeed =
                  HS.size (psSubscribers ps) == 0 &&
                  diffUrTime t (psLastActionTime ps) > day

          if utsParentPaths u == [] then do
              logLT l "Ping for deleted blog"
              count u "hubNoBlog"
          else if not goodHub || unusedFeed then do
              logLT l $ T.append "Ignoring hub, issuing fetch"
                    (if unusedFeed then " (unused feed)" else "")
              -- обычная обработка неиспользуемого фида, чтобы быстрее
              -- его удалить (иначе часто обновляемые фиды могут долго
              -- не удаляться, т.к. время сканирования по расписанию будет
              -- сдвигаться).
              let time = newUrlScanListKey url
              addUrl uq $ AddUrl PushFront QTBlogFeed time url
          else if dHash == utsDataHash u then do
              logLT l "Same hash"
              count u "hubSameHash"
          else do
              pr <- logTime l "parse   " $ do
                  t <- getUrTime
                  return $! parse t f dat
              case pr of
                  PRFeed bu fm fms ->
                      processParseServerResult l uq u QTBlogFeed (-1) url True Nothing (DPROK (utsRedownloadOptions u) dHash pr)

--                       withNewUrls uq $ \ newUrls ->
--                       processFeedItems newUrls Nothing
--                                        False True updUts l u uq bu NoSwitch fm
--                                    (sortFms bu fms) id QTBlogFeed
                  _ ->
                      logLS l $ "Not a PRFeed? -- " ++ show pr
      )
      `E.finally` do
          removeFile fn
          -- logLT l "--------"
      `E.catch` \ (e :: E.SomeException) ->
          if fmap isDoesNotExistError (E.fromException e) /= Just True then do
              incrStat "exceptions" 1
              logLS l $ "Exception: " ++ show e
          else
              logLS l ("Can’t remove hub feed: " ++ show e)

main = withUrlQueue $ \ uq -> do
    runEkgServer "localhost" queueEkgPort

    forkIO statsSaver
    forkIO $ topDomainsLogger uq
    when (not hotLinksDisabled) $ void $ forkIO $ removeOldHotLinksLoop
    forkIO $ deleteUrlsLoop uq
    forkIO $ rescanUrlsLoop uq
    forkIO $ sequence_ =<< getChanContents (uqAddNewUrl uq)
    forkIO $ hubUpdater uq
    subscribeUrlsLoop uq

removeOldHotLinksLoop = forever $ do
    removeOldHotLinks `E.catch` \ (e :: E.SomeException) -> do
        logS $ "Exception in removeOldHotLinks: " ++ show e
        incrStat "exceptions" 1
    threadDelay $ 68*3*1000*1000 -- refresh_interval 59 и 67 секунд, делаем запас
removeOldHotLinks = do
    removeOldHotLinks' esUrl ""
    withEsUrl2 $ \ u2 -> removeOldHotLinks' u2 "2"
removeOldHotLinks' esU suffix = withLogger $ \ l -> logTime l "total   " $ do
    logLT l "Removing old hot links"
    -- 140M hot_link + 20M msg_link
    -- hot_link видимо так много из-за того, что они не удалялись без routing-а
    let suff x = T.append x suffix
    t <- fmap (`plusUrTime` (-31*day)) getUrTime
    hl <- logTime l (suff "findHotL") $
        esSimpleIdsSearch' esU "hot_link" "hot_link" 10000 -- 70000 -- ~6MB
        Nothing "hot_link_preference" $
        esFilter $ esLessThanDateFilter "time" t
    ml <- logTime l (suff "findMsgL") $
        esSimpleIdsSearch' esU "msg_links" "msg_links" 10000 -- ~7MB
        Nothing "msg_links_preference" $
        esFilter $ esLessThanDateFilter "time" t
    let actions =
            [esBulkAction r "hot_link" "hot_link" "delete" i | (i,r) <- hl]
            ++
            [esBulkAction r "msg_links" "msg_links" "delete" i | (i,r) <- ml]

--    mapM_ (BL.putStrLn . JSON.encode) $ actions
    logLS l $ "Found " ++ show (length hl) ++ " hot links + "
              ++ show (length ml) ++ " msg links"

    mapM_ (logTime l (suff "delLinks") .
           esBulkRequest' esU (T.unpack $ suff "Deleted") l) $
        groupByN 50 actions

-- | Пробегается по элементам списка равномерно,
-- в течении отведенного времени
smoothMapM_ name timeInSec f l = do
    st <- getUrTime
    let len = length l
        mapTime = truncate $ timeInSec*1000000
        dt = mapTime `div` len
        diffUSec a b = truncate $ diffUrTime a b * 1000000
        go _ [] = return ()
        go n (x:xs) = do
            ct <- getUrTime
            let d = n*dt - diffUSec ct st
            --logS $ "Sleeping " ++ show d
            when (d > 0) $ threadDelay d
--            logS $ name ++ " " ++ show n ++ "/" ++ show len
            f x
            go (n+1) xs
    go 0 l
    et <- getUrTime
    let d = mapTime - diffUSec et st
    when (d > 0) $ threadDelay d

smoothForM_ name time l f = smoothMapM_ name time f l

addRescanUrls uq _ [] = return ()
addRescanUrls uq new urls = do
    logQueue $ "adding " ++ show (length urls) ++ " " ++ what ++ " urls…"
    smoothMapM_ "adding" scanListStep (\(t,u,qt) -> addUrl uq (AddUrl PushBack qt t u)) sorted
    where (what, sorted)
              | new = ("new", sortBy (comparing $ \(_,_,qt) -> qt) urls)
              | otherwise = ("rescan", urls)

readScanLists s = do
    sls <- catMaybes <$> readManyScanLists s
    forM sls $ \ s ->
        if empty s then
            alterScanList (slTime s) $ maybe (return (Nothing, s)) $ \ s ->
                if empty s then do
                    logQueue $ "readScanLists: deleted scan list " ++ showUrTime (slTime s)
                    return (Nothing, s)
                else
                    return (Just s, s)
            -- читаем ScanLists только в прошлом и до их обработки,
            -- а newUrls добавляются с блокировкой
        else
            return s
    where empty = null . slUrls

rescanUrlsLoop uq = forever $ do
    lt <- readMVar (uqLastProcessedTime uq)
    ct <- getUrTime
    let rct = roundScanTime ct
        newOrQueue k = isNewUrlScanListKey k || isQueueScanListKey k
    unprocessed <- if lt == UrTime 0 0 then do
        r <- filter newOrQueue . takeWhile (< rct) <$> scanListKeys
        logQueue $ "got " <> show (length r - 1) <> " scan list keys"
        return $ r <> [rct]
        -- для правильной инициализации uqLastProcessedTime добавляем rct
    else
        return $ takeWhile (<= rct)
            [lt `plusUrTime` (scanListStep * i) | i <- [1, 2 ..]]
    let readAndAdd new l = do
--            logS $ "Getting rescan urls…"
            r <- E.try $ readScanLists l
--            logS $ "Got rescan urls…"
            case r of
                Right sls ->
                    addRescanUrls uq new $ scanListsToQueue sls
                Left (e :: E.SomeException) -> do
                    logQueue $ "exception while readScanLists: " ++ show e
                    incrStat "exceptions" 1
                    threadDelay 1000000
                    readAndAdd new l
    if null unprocessed then
        threadDelay $
            truncate $ 1000000 * diffUrTime (lt `plusUrTime` scanListStep) ct
    else do
        let (new, other) = partition isNewUrlScanListKey unprocessed
        readAndAdd True new
        mapM_ (readAndAdd False) $ groupByN 1000 other
        modifyMVar_ (uqLastProcessedTime uq) (return . const (last unprocessed))

logQueue = logS . ("q: " <>)

scanListsToQueue =
    force . concatMap (\ ScanList {..} -> map (\(u,qt) -> (slTime,u,qt)) slUrls)

subscribeUrlsLoop uq = forever $ do
    -- выгребаем url-ы на подписку
    r <- E.try $ forM_ [UrTime 1 i | i <- [0 .. subscriptionUrlScanLists-1]] $ \ t -> do
        aus <- fmap scanListsToQueue <$> modifyScanList' t $ \ sl ->
            fmap (sl { slUrls = [] },) $ saveNewUrls $ slUrls sl

        when (notNull aus) $ do
            logQueue $ "adding " ++ show (length aus) ++ " subscribe urls…"
            forM_ aus $ \ (t, u, qt) ->
                addUrl uq $ AddUrl PushBack qt t u

    -- можно и поспать ;)
    case r of
        Left (e :: E.SomeException) -> threadDelay 50000
        Right _ -> threadDelay 500000

deleteUrlsLoop uq = forever $ do
    t <- getUrTime
    let maxTime = t `plusUrTime` (-delaySecs)
        delaySecs = 60
        -- даем минуту для накопления обработанных UTS, чтобы не редактировать
        -- ScanList-ы, а сразу их удалять

    toRemove <- modifyMVar (uqProcessedUrls uq) $ \ u -> do
        let (ret, keep) = partition ((< maxTime) . uTime) u
        logQueue $ "removing " ++ show (length ret) ++ " (" ++ show (length keep)
            ++ " kept) processed urls…"
        return (keep, ret)
--    logS $ "Removing " ++ show (length toRemove) ++ " processed urls…"
    removed <- newIORef 0
    smoothForM_ "removing" delaySecs
        (groupBy (\ (UTS k1 _) (UTS k2 _) -> k1==k2) $ sort toRemove) $ \ g -> do
          let (UTS key _ : _) = g
              rm [] _  = []
              rm xs [] = xs
              rm al@(ta@(a,_):as) rl@(r:rs)
                  | a == r = rm as rs
                  | a < r = ta : rm as rl
                  | otherwise = rm al rs -- r нет в a ?
              utsU (UTS _ u) = u
              rmSl sl = sl { slUrls = rm (slUrls sl) (map utsU g) }
              modRm = alterScanList key $ maybe (return (Nothing, ())) $ \ sl -> do
                  let sl' = rmSl sl
                  if slUrls sl' == [] &&
                      (isQueueScanListKey key || isNewUrlScanListKey key)
                  then do
                      logQueue $ "deleted scan list " ++ showUrTime key
                      modifyIORef removed succ
                      return (Nothing, ())
                      -- удалять можно, т.к. новые записи только в будущем,
                      -- а ScanLists для newUrls сохраняются только в Queue
                      -- и одновременной записи/удаления быть не может
                  else
                      return (Just sl', ())
          modRm
              `E.catch`
              (\ (e :: E.SomeException) -> do
                  incrStat "exceptions" 1
                  logQueue $ "remove processed urls: Exception: " ++ show e)
              -- в окончании перебалансировки был таймаут, из-за которого
              -- перестали удаляться старые URL-ы, что привело к очень долгой
              -- повторной обработке очереди.
    r <- readIORef removed
    when (r > 0) $
        logQueue $ "deleted " ++ show r ++ " scan lists…"

newRateLimit = do
    d <- forM delays $ \ (f, d) -> do
        mv <- newMVar ()
        return (f, d, mv)
    return $ limit d
    where limit [] u = \ l mv act -> rateLimit defaultDelay mv act
          limit ((f, d, mv) : ds) u
              | f u = \ l _ act -> do
                  logLS l $ "Rate limit delay "
                      <> showSecs (fromIntegral d / 1000000)
                  rateLimit d mv act
              | otherwise = limit ds u
          rateLimit d mv act = do
              takeMVar mv
              minDelay <- newEmptyMVar
              forkIO $ threadDelay d >> putMVar minDelay () >> putMVar mv ()
              -- ограничиваем частоту скачивания, но позволяем
              -- запускать несколько скачиваний одновременно
              r <- act
              -- не возвращаемся пока не пройдет минимальная задержка
              takeMVar minDelay
              return r
              -- По-идее, надо сделать временное окно со счетчиком,
              -- чтобы одно длинное скачивание не тормозило кучу коротких,
              -- но лучше перестраховаться, т.к. неизвестно, как настроены
              -- сервера и позволяют ли они резкие скачки в частоте
              -- скачиваний.
          defaultDelay = 1000000
          delays =
              [ (hn "disqus.com"    , hr `div` 7800) -- 8 disqus приложений
                -- и небольшой запас
--              , (T.isInfixOf "www.googleapis.com/youtube", 300*1000)
              , (hn "reddit.com"    , 1050*1000)
                -- reddit говорит, что не чаще 60 раз в минуту,
                -- но сделаем небольшой запас
                -- не www.reddit.com, т.к. появились ru.reddit.com, lo.reddit.com
              , (hn "twitter.com", 250*1000)
              , (hn "facebook.com", 200*1000)
                -- уменьшаем лимит с 1 секунды, т.к. у twitter/facebook
                -- работает несколько нитей и свои, отдельно рассчитываемые,
                -- ограничения
              , (hn "vk.com", 24*hr `div` 5000)
                -- https://vk.com/dev/data_limits
--               , (googleNewsUrl, 1*sec)
                -- когда-то давно выдавали CAPTCHA. С прокси проблем нет.
                --
                -- Можно обращаться в
                -- https://support.google.com/websearch/contact/ban
                -- не знаю, насколько это работает (бан сняли еще до обращения)
              , (hn "instagram.com", 35*sec)
              ]
          hn h u = h == n || ("." <> h) `T.isSuffixOf` n
              where n = hostNameFromUrlT u
          sec = 1000*1000
          hr = 3600*sec

psLastActionTime ps
    | Just ((maxT,_,_),_) <- Set.maxView $ psActions ps = maxT
    | otherwise = UrTime 0 0

tryCancelUts l uq u@(UrlToScan {..}) qt key _ end act
    | [] <- utsSubscriptionParentPaths
    , [] <- utsParentPaths = do
        retryQueue l uq qt key
        logLT l $ "Cancelling UTS: no parent paths"
        cancelRescans l uq id utsUrl qt
        end
tryCancelUts l uq u QTSubscription key _ _ act = act
tryCancelUts l uq u QTSubscriptionOPML key _ _ act = act
tryCancelUts l uq u@(UrlToScan {..}) QTBlogFeed key (subscribers, ps, t) end act
    | subscribers == 0
    , [] <- utsSubscriptionParentPaths
    , [ParentPath _ []] <- utsParentPaths -- только этот блог
    , diffUrTime t (psLastActionTime ps) > day -- последнее изменение давно
    = do
        mbsui <- readSubscriptionUrlInfo utsUrl
        case mbsui of
            Just sui | diffUrTime t (suiTime sui) < day ->
                act -- совсем недавно подписывались, пока не трогаем
                -- хотя это вроде как лишняя проверка, если psLastActionTime
                -- более суток
            _ -> do
                e <- esDiscoveryFeedExists utsUrl
                if e || isJust mbsui then do
                    incrStat "toRemove" 1
                    logLT l "Remove candidate"
                    when e $ do
                        logLT l "Removing from discovery"
                        esDeleteDiscoveryFeed l utsUrl
                    maybe (return ()) deleteSubscriptionUrlInfo mbsui
                    act -- обрабатываем еще раз, а в следующий раз будем удалять
                        -- (когда подержится без поиска и SubscriptionUrlInfo)
                else do
                    rm <- modifyMVar (uqFeedRemovesCount uq) $ \ c -> return $
                        if c >= maxFeedRemoves then (c, False) else (c+1, True)
                    if rm then
                        (do processParseServerResult l uq u QTBlogFeed (-1) utsUrl False Nothing DPRToRemove
                            retryQueue l uq QTBlogFeed key
                            end)
                        `E.finally`
                        (modifyMVar_ (uqFeedRemovesCount uq) (return . pred))
                    else do
                        logLT l "Postponed feed removing"
                        act
    | otherwise = act
tryCancelUts l uq u@(UrlToScan {..}) qt key _ end act
    | not (any (`T.isInfixOf` utsUrl)
           ["livejournal.com/", "reddit.com", "facebook.com", "disqus.com"])
      || isBlogUts u -- по-идее, bloguts тут быть не должно, но вдруг
        = act
    | otherwise = do
        -- не сканируем комментарии, если пост уже более чем 50-й по списку
        -- не сканируем комментарии, если их уже более 500
        t <- getUrTime
        if isQueueScanListKey utsNextScanTime &&
           diffUrTime t utsNextScanTime > 3*day
        then
            go ["Too many items in queue"] []
            -- сразу отменяем, если уже 3 дня не можем обновить
        else
            go [] utsParentPaths
    where go causes [] = do
              retryQueue l uq qt key
              logLT l $ T.concat [ "Cancelling comments ("
                                 , T.intercalate ", " (nub causes), ")"]
              setBlogPostsScannedUts uq u [(utsUrl, CUSOK)]
              cancelRescans l uq id utsUrl qt
              end
          go causes (pp:pps) = case ppGuids pp of
              postGuid:_ -> do
                  c <- readComments (CommentsKey (ppBlogFeedUrl pp) postGuid)
                  case c of
                      Just ((succ . snd . BA.bounds . mtHeaders . cMsgTree)
                            -> cc)
                          | cc > 600 ||
                            ("reddit.com" `T.isInfixOf` utsUrl && cc >= 200) ->
                            -- для reddit все равно больше 200 комментов
                            -- не выгребается
                              go ("too many comments" : causes) pps
                      _ -> do
                          p <- modifyPosts' (ppBlogFeedUrl pp) $ \ p ->
                               return (p,p)
                               -- не readPosts, чтобы блокировку обработал
                          let mt = pMsgTree p
                              scanDepth
                                  | "facebook.com" `T.isInfixOf` utsUrl = 10
                                  | "reddit.com" `T.isInfixOf` utsUrl = 20
                                  | otherwise = 30
                          case IntMap.lookup (-1) $ mtChildren mt of
                              Just ch
                                  | l <- take scanDepth $
                                         Set.foldl (flip (:)) [] ch
                                  , any ((== postGuid) . mhGuid .
                                         (BA.!) (mtHeaders mt) . tiId) l
                                  -> act -- пост среди последних 40 (было 50)
                              _ ->
                                  go ("too old post" : causes) pps
              _ -> act

processUrl rlMVar processLock l uq@(UrlQueue {..}) u@(UrlToScan {..}) qt qHostName key end = do
   startTime <- getTime
   (subscribers, ps) <-
       if isBlogUts u then do
           ps <- readPostsSubscribers' utsUrl
           let c = HS.size $ psSubscribers ps
           logLS l $ show c ++ " subscribers"
           return (c,ps)
       else
           return (-1, defaultPostsSubscribers utsUrl)
   t <- getUrTime
   tryCancelUts l uq u qt key (subscribers, ps, t) end $ do
--    logLS l $ show u

    (utsUrl_', u2, post) <- transformURL utsUrl parserEnvironmentGet

    let (utsUrl', skipDownload)
            | utsUrl_' == "" = (utsUrl, True)
            | otherwise = (utsUrl_', False)
        hostName = hostNameFromUrlT utsUrl'

    proxyIndex <- if key `elem` [IPQKInstagram, IPQKGoogleNews]
        && not skipDownload
    then
        modifyMVar uqProxyIndexes $ \ pi -> do
            let i = Map.findWithDefault 0 key pi
            return (Map.insert key (i+1) pi, Just i)
    else
        return Nothing

    let resolveError err =
            handleDR' "" $ DPRError $ T.concat
                ["Can’t resolve “", humanReadableHostName hostName, "”: ", err]
        withHA act = case key of
            IPQKDefault ha -> act ha
            IPQKCantResolve e -> resolveError e
            _ ->
                MSem.with uqDnsSem $ countProcess "dns" uqDnsCount $ do
                mbIP <- logTime l "resolve " $
                    resolveA uqDnsCache (T.unpack hostName)
                incrStat "resolveCount" 1
                case mbIP of
                    Left err -> do
                        incrStat "dnsErrorsReq" 1
                        resolveError err
                    Right ha ->
                        act ha
        handleDR (ParseServerResult dr (Just (details, delay)) lt) = do
            let lt' = T.concat
                    [lt, "Rate limit delay ", T.pack $ showSecs delay
                    , " (", details, ")"]
            handleDR' lt' dr
            threadDelay $ round $ delay * 1000000
        handleDR (ParseServerResult dr Nothing lt) = handleDR' lt dr
        handleDR' ltps dr = do
            lt <- (<> ltps) <$> loggerGetAndClean l
            takeMVar processLock
            incrUqSize uq -- при выходе из processUrl будет сделан decrUqSize
            void $ forkIO $ withLogger $ \ l ->
                logExceptions l (logTime' l startTime "total   " $ do
                    logLT l $ T.dropWhileEnd (== '\n') lt
                    processParseServerResult l uq u qt subscribers utsUrl'
                        False proxyIndex dr
                    end)
                `E.finally` do
                    putMVar processLock ()
                    decrUqSize uq

    if skipDownload then do
        logLT l "Skipping download"
        let pr = parse (UrTime 0 0) (T.unpack utsUrl) ""
        handleDR' "" $ case pr of
            PRRedirect r -> DPRRedirect r
            -- чтобы отработало новые перенаправления на facebook у существующих
            -- фидов
            _ -> DPROK [] "" pr
    else withHA $ \ hostAddr -> uqRateLimit utsUrl' l rlMVar $ do
        dr <- MSem.with uqDownloadAndParseSem $
            countProcess "Downloads" uqDownloadsCount $ do
            now <- getUrTime
            badRDO <- case redownloadOptionsLastModified utsRedownloadOptions of
                Just t | t < utsModifyTime -> do
                    incrStat "badLastModified" 1
                    logLS l $ "last modified " ++ showUrTime t
                    logLS l $ "< modify time " ++ showUrTime utsModifyTime
                        ++ ", ignoring it"
                    return True
                _ ->
                    return False
            logLS l $ "q delay : " ++ showSecs (diffUrTime now utsNextScanTime)
                      ++ ", " ++ show qt
            -- logTime l "parse   " $
            let subUrl = qt `elem` [QTSubscription, QTSubscriptionOPML]
            uqWorkerRequestScheduler l
                (showIPQKey qHostName key)
                ParseRequest $
                ParseServerRequest
                { psrqUrl = utsUrl
                , psrqHostAddress = hostAddr
                , psrqRedownloadOptions =
                    if subUrl || badRDO then [] else utsRedownloadOptions
                , psrqDataHash =
                    if subUrl then "" else utsDataHash
                , psrqQueueTime = now
                , psrqSubscribersCount = subscribers
                , psrqProxyIndex = proxyIndex
                }
        -- обрабатываем внутри uqRateLimit, чтобы не дожидаться окончания
        -- задержки перед сохранением в БД
        handleDR dr

processParseServerResult l uq@(UrlQueue {..}) u@(UrlToScan {..}) qt subscribers utsUrl' fromHub proxyIndex dr =
--    logLS l $ "Downloaded " ++ url
    countProcess "Unprocessed" uqUnprocessedCount $
     MSem.with uqProcessSem $
     logTime l "process " $ countProcess "Processings" uqProcessingsCount $
     withNewUrls uq $ \ newUrls -> do
--          t <- getUrTime
--          pl <- MSem.new 1000
--          processParseResult 0 uqDownloader pl uqDnsCache t newUrls l dr u uq qt utsUrl'
         let posts = sort $ map ppBlogFeedUrl utsParentPaths
             comments =
                 sort
                 [ CommentsKey (ppBlogFeedUrl pp) postGuid
                 | pp <- utsParentPaths,
                   postGuid <- take 1 $ ppGuids pp ]
             block [] act = act
             block ((bucket, k):ks) act =
                 blockBucketKey bucket k $ block ks act
             keys =
                 map (("Posts",) . key) posts ++
                 map (("Comments",) . key) comments
         t <- getUrTime
         ProcessUrlResult {..} <-
             block keys $
             uqWorkerRequestScheduler l "" ProcessRequest $
             ProcessUrlRequest
             { purqDownloadAndParseResult = dr
             , purqUrlToScan = u
             , purqQueueType = qt
             , purqUtsUrl' = utsUrl'
             , purqQueueTime = t
             , purqSubscribersCount = subscribers
             , purqFromHub = fromHub
             , purqProxyIndex = proxyIndex
             }
         addStatsMap purStatsMap
         modifyIORef newUrls (++ purNewUrls)
         logLT l purLog
         case purException of
             Just e -> error $ T.unpack e
             Nothing -> return ()


queueScanListKeys' cont = do
    t <- getUrTime
    first (filter $ \ k -> isQueueScanListKey k && k < t) <$> scanListKeys' cont

scanListKeys = go [] ""
    where go acc cont = do
              ((:acc) -> r, c) <- scanListKeys' cont
              case c of
                  Nothing -> return $ sort $ concat $ reverse r
                  Just c -> go r c

scanListKeys' cont = do
    (k, c) <- riakBucketKeys' "ScanList" 10000 cont
    return (map key k, c)
    where key k = case map (read . T.unpack) $ T.split (== '.') k of
              [s]     -> UrTime s 0
              [s, us] -> UrTime s us
              _ -> error $ "Bad UrTime key: " <> show k
