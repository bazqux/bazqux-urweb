{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards, TupleSections,
             BangPatterns, TransformListComp, ScopedTypeVariables, LambdaCase,
             PatternSynonyms
#-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Subscriptions
    ( userCheckSubscriptions, userSubscriptionsReady
    , userAddSubscriptions, subscribeUrls, saveSubscriptionUrls, saveNewUrls
    , forceRescan, subscriptionUrlScanListKey
    , isPaid, ssFeed
    , initPostsRead, blogPostsScannedPercent
    , addActiveCheckSubscriptions
    , atomicDeleteUrlToScan, atomicModifyUrlToScan', atomicModifyUrlToScan'_
    , newUrlScanListKey, subscriptionUrlScanLists
    , isNewUrlScanListKey, isQueueScanListKey
    )
    where

import Control.Monad
import Control.Applicative
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Generated.DataTypes
import Lib.UrTime
import qualified Lib.BArray as BA
import Lib.Log
import Data.Maybe
import URL
import Resolvables
import Riak
import Generated.RiakIO
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Lib.ReadSet as ReadSet
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import Lib.Regex
import Parser.Custom hiding (post)
import Discovery
import Search
import Config

subscriptionUrlScanLists = 32
subscriptionUrlScanListKey url =
    UrTime 1 (hash url `mod` subscriptionUrlScanLists)

saveSubscriptionUrls urls = do
    let groups = groupBy (\ (k1,_) (k2,_) -> k1 == k2) $ sort
                 [(subscriptionUrlScanListKey u, u) | u <- urls]
        qt | [_] <- urls = QTSubscription
           | otherwise   = QTSubscriptionOPML
    forM_ groups $ \ g@((k,_):_) ->
        mergeWriteScanList $ ScanList k [(u, qt) | (_,u) <- g]

-- В большинстве мест UrlToScan надо менять атомарно.
-- Особенно это касается urlError, который, в случае can't resolve
-- может практически одновременно вызваться для одного и того же url
-- (задержка по секундам при ошибках DNS уже не работает, да и с задержкой
-- при почти одновременных обновлениях с PubSubHubbub мы можем попасть
-- на одновременную обработку одного url-а)
atomicModifyUrlToScan' u f =
    blockBucketKey "blockableUrlToScan" (key u) $ modifyUrlToScan' u f
atomicModifyUrlToScan'_ u f =
    blockBucketKey "blockableUrlToScan" (key u) $ modifyUrlToScan'_ u f
atomicDeleteUrlToScan u =
    blockBucketKey "blockableUrlToScan" (key $ utsUrl u) $ deleteUrlToScan u

forceRescan url = do
    UrTime s _ <- getUrTime
    let t = UrTime (s+3) 0
    u <- readUrlToScan url
    when (isJust u) $ atomicModifyUrlToScan'_ url $ \ uts -> do
        ScanList _ sl <- readScanList' $ utsNextScanTime uts
        qt <- case lookup url sl of
            Just qt -> return qt
            Nothing -> do
                putStrLn $
                    "WARNING: not in ScanList " ++ show (utsNextScanTime uts)
                T.putStrLn url
                return QTBlogFeed
        print qt
        mergeWriteScanList $ ScanList t [(url, qt)]
        return $ uts { utsNextScanTime = t
                     , utsRedownloadOptions = []
                     , utsDataHash = "" }

fixNonScanningBlogs = do
    blogs <- riakBucketKeys "Posts"
    putStrLn $ "Checking " ++ show (length blogs) ++ " blogs"
    t <- getUrTime
    forM_ blogs $ \ url -> do
        let rescan reason = do
              p <- readPosts' url
              when (not $ deletedPosts p) $ do
                  T.putStrLn $ T.append "RESCAN: " url
                  T.putStrLn $ T.append "reason: " $ T.pack reason
                  forceRescan url
        u <- readUrlToScan url
        case u of
            Nothing -> return ()
            Just (utsNextScanTime -> nst)
                | nst == UrTime 0 0 ->
                    return () -- может удалять? результат отмены сканирования
                | otherwise -> do
                    ScanList _ sl <- readScanList' nst
                    if lookup url sl == Nothing then do
                        rescan $ "not in ScanList " ++ show nst
                    else if diffUrTime t nst > 2*day then do
                        rescan $ "haven’t scanned since " ++ show nst
                    else
                        return ()

forceRescanUserSubscriptions = forceRescanUserSubscriptions' ".*"
forceRescanUserSubscriptions' regex user = do
    u <- readUser' user
    mapM_ forceRescan $ filter (rt regex) $ mapMaybe ssFeed $ uSubscriptions u

ssFeed s =
    case sState s of
        SSFeed u -> Just u
        _ -> Nothing

-- | Новые, еще не отсканированные url-ы (подписки, перенаправления, новые
-- комментарии), сохраняемые на случай падения.
newUrlScanListKey url =
    UrTime 2 (hash url `mod` 1000000)

isNewUrlScanListKey (UrTime n _) = n == 2
isQueueScanListKey = (>= UrTime 3 0)

saveNewUrls urls = do
    let groups = groupBy (\ (k1,_,_) (k2,_,_) -> k1 == k2) $ sort
                 [(newUrlScanListKey u, u,t) | (u,t) <- urls]
        sls = map (\ g@((k,_,_):_) -> ScanList k [(u, t) | (_,u,t) <- g]) groups
    mapM_ mergeWriteScanList sls
    return sls

subscribeUrls :: [TURL] -> IO ()
subscribeUrls urls = do
--    putStrLn ""
    forM_ (zip [0..] urls) $ \ (n,u) -> atomicModifyUrlToScan'_ u $ \ u -> do
--        putStr $ show n ++ "\r"
        return $
            u { utsRedownloadOptions = [], utsDataHash = "",
                utsSubscriptionParentPaths = f (utsUrl u)
                                         (utsSubscriptionParentPaths u) }
--    putStrLn "\nSave subscription urls"
    saveSubscriptionUrls urls
    where f url sp
              | isJust $ find ((== url) . sppSubscriptionUrl) sp = sp
              | otherwise = SubscriptionParentPath url [] : sp

isPaid curTime Nothing = False
isPaid curTime (Just u) = case uvmPaidTill (uViewMode u) of
    PTUnknown -> True
    PTPaid pt | pt > curTime -> True
    PTFreeTrial pt | pt > curTime -> True
    _ -> False

initPostsRead uid (Posts {..}) = do
    t <- getUrTime
    let i = ignored (t `plusUrTime` (-30*day))
        (lo,hi) = BA.bounds $ mtHeaders pMsgTree
    when (i /= ReadSet.empty) $ modifyPostsRead_ (uid, pBlogFeedUrl) $ \ mbpr ->
        return $ case mbpr of
            Just pr
                | (case ReadSet.maxIndex (prSet pr) of
                       Just i -> i >= lo -- читаем в районе существующих постов
                       Nothing ->
                           case ReadSet.maxIndex (prIgnoredPosts pr) of
                               Just i -> i >= lo
                               -- игнорим в районе существующих постов
                               Nothing -> False -- фид уплыл дальше чем мы были
                  )
                -> pr -- не меняем, если уже были подписаны
                      -- или идет повторный import и url смержился
            _ ->
                (defaultPostsRead (uid, pBlogFeedUrl))
                       { prIgnoredPosts = i }
    where ignored t = ReadSet.fromList
              [ idx | (TimeId pt idx, n) <- zip (Set.toList ch)
                                            [Set.size ch, Set.size ch - 1 ..]
              , (pt < t && pt /= UrTime 0 0) || n > 10 ]
          ch = mtChildrenSet pMsgTree (-1)

checkSubscriptions u = do
    curTime <- getUrTime
    let (toCheck_, ok) = partition newSubscripion $
                         filter ((/= "") . sUrl) $ uSubscriptions u
        maxSubs | isPaid curTime (Just u) = maxPaidSubscriptions (uId u)
                | otherwise = 0
        toCheck = take (max 0 $ maxSubs - length ok) toCheck_
        urls = map sUrl toCheck
    suis' <- forkReadPar2 readManySubscriptionUrlInfos urls
            -- нельзя cached, смысл теряется
    posts' <- forkReadPar2 cachedReadManyPostss urls
    suis <- suis'
    let ignoreDeleted (Just p) | deletedPosts p = Nothing
        ignoreDeleted p = p
    posts <- fmap (map ignoreDeleted) posts'
    let -- sameSui Nothing _ = True
        sameSui (Just (suiKind -> SUKFeed u)) p = pBlogFeedUrl p == u
        sameSui _ _ = False
        go sacc acc renAcc [] = do
            subscribeUrls sacc
            return (reverse acc, renAcc)
        go sacc acc renAcc ((s@(Subscription {..}), mbsui, Just p) : ss) | sameSui mbsui p = do
            -- с таким url-ом уже есть готовые Posts
            let bfu = pBlogFeedUrl p
                title = postsTitle p
                fresh =
                    case mbsui of
                        Nothing -> False
                        Just sui ->
                            diffUrTime curTime (suiTime sui) < 3*3600 ||
                            livejournalFeed bfu
                            -- не добавляем повторную подписку для ЖЖ,
                            -- его фид не меняется, а сканированию мешает
            logT $ T.concat [ "User ", uId u, " subscribed to existing ", maskUrlPasswordT bfu ]
            initPostsRead (uId u) p
            postsAddSubscriber bfu (uId u) True
            go (if fresh then sacc else sUrl : sacc)
               -- даже если есть Posts все равно запускаем сканирование,
               -- т.к. фид может измениться
               (s { sState = SSFeed bfu
                  , sTitle = sTitle <|> title } : acc) renAcc ss
                  -- чтобы при изменении заголовка блога ничего не менялось
        go sacc acc renAcc ((s@(Subscription {..}), Nothing, _) : ss) = case sState of
            SSAdded ->
                go (sUrl : sacc) (s { sState = SSScanning curTime } : acc) renAcc ss
            SSScanning startTime
                | diffUrTime curTime startTime >
                  (if livejournalFeed sUrl then 7200 else 1800) -> do
                  -- в принципе, ошибка бывает редко, когда куча url-ов
                  -- редиректятся в одно и то же место, можно не делать
                  -- маленькое время, т.к. при перегрузке будет плохо
                logT $ T.concat ["Retrying subscription to "
                                , maskUrlPasswordT sUrl
                                , " for user ", uId u]
                go (sUrl : sacc) (s { sState = SSScanning curTime } : acc) renAcc ss
            _ -> go sacc (s:acc) renAcc ss
        go sacc acc renAcc ((s@(Subscription {..}),
                      Just (SubscriptionUrlInfo {..}),p) : ss) = case sState of
            SSAdded
                | diffUrTime curTime suiTime < 3600 ->
                    sub p s suiKind sacc acc renAcc ss
                | otherwise -> -- недостаточно свежая инфа
                    go (sUrl : sacc) (s { sState = SSScanning curTime }:acc) renAcc ss
            SSScanning startTime
                | diffUrTime startTime suiTime < 3600 -> do
                    sub p s suiKind sacc acc renAcc ss
                | diffUrTime curTime startTime >
                  (if livejournalFeed sUrl then 7200 else 1800) -> do
                    logT $ T.concat ["Retrying subscription to "
                                    , maskUrlPasswordT sUrl
                                    , " for user ", uId u]
                    go (sUrl : sacc) (s { sState = SSScanning curTime } : acc) renAcc ss
                | otherwise -> do -- пока еще старая инфа
                    -- print (sUrl, diffUrTime startTime suiTime)
                    go sacc (s : acc) renAcc ss
            _ -> fail "new subscription here?"
        sub _ s (SUKError msg) sacc acc renAcc ss = do
            logT $ T.concat ["User ", uId u, " can’t subscribe to "
                            , maskUrlPasswordT $ sUrl s
                            , " -- ", msg]
            go sacc (s { sState = SSError msg } : acc) renAcc ss
        sub _ s (SUKErrorPath msg path) sacc acc renAcc ss = do
            logT $ T.concat ["User ", uId u, " can’t subscribe to "
                            , maskUrlPasswordT $ sUrl s
                            , " -- ", msg]
            go sacc (s { sState = SSErrorPath msg path } : acc) renAcc ss
--         sub Nothing s (SUKFeed url) sacc acc renAcc ss =
        sub p s (SUKFeed url) sacc acc renAcc ss
            | url == sUrl s = checkP p $ \ p t -> do
                logT $ T.concat [ "User ", uId u, " subscribed to "
                                , maskUrlPasswordT $ sUrl s]
                initPostsRead (uId u) p
                postsAddSubscriber url (uId u) True
                go sacc (t (s { sState = SSFeed url }) : acc) renAcc ss
            | otherwise = do
                p <- cachedReadPosts url
                checkP p $ \ p t -> do
                    logT $ T.concat ["User ", uId u, " subscribed to "
                                    , maskUrlPasswordT (sUrl s)
                                    , " -- ", maskUrlPasswordT url]
                    initPostsRead (uId u) p
                    postsAddSubscriber url (uId u) True
                    go sacc (t (s { sUrl = url, sState = SSFeed url }) : acc)
                            ((curTime, sUrl s, url) : renAcc) ss
            where checkP p act =
                      case p of
                          Just p -> act p (\ s -> s { sTitle = sTitle s <|>
                                                             postsTitle p })
                          Nothing ->
                              go sacc (s : acc) renAcc ss
                              -- не подписываемся, если Posts еще не готовы
    (toCheck', rens) <- go [] [] [] $ zip3 toCheck suis posts
    let -- для filter (not . newSubscripion) toCheck'
        -- добавляется новый режим просмотра по-умолчанию (свернутые комменты)
        -- для ok, т.е. существующих устанавливается старый (развернутые)
        fixMtvm mtvm subs vms =
            vms `HM.union` HM.fromList [ (sUrl s, (0, mtvm)) | s <- subs ]
            -- если ключ есть в vms, то будет использован он
        addRens u =
            u { uViewMode =
                    (uViewMode u)
                    { uvmSubUrlRenames =
                        take maxSubs $
                        hashSetNub' (\ (_,u,_) -> u) $
                        takeWhile (\ (t,_,_) ->
                                       curTime `diffUrTime` t < day) $
                        rens ++ uvmSubUrlRenames (uViewMode u)
                    , uvmSubViewModes =
                        fixMtvm
                           defaultMsgTreeViewMode
                           (filter (not . newSubscripion) toCheck') $
                        fixMtvm
                           (defaultMsgTreeViewMode
                            { mtvmExpandedComments = True })
                           ok $
                        uvmSubViewModes $ uViewMode u
--                     , uvmPaidTill = case uvmPaidTill (uViewMode u) of
--                           PTUnknown ->
--                               PTFreeTrial (curTime `plusUrTime` (30*day-1))
--                           pt -> pt
-- не нужно, и так в getPaidTill устанавливается

                    } }
        u' = addRens $
             resolve (u { uSubscriptions = ok ++ toCheck' })
                     (u { uSubscriptions = [] }) -- для убирания дубликатов
    when (notNull toCheck) $ void $ getGRIds' (uId u) (uSubscriptions u')
    if any newSubscripion toCheck' then
        addActiveCheckSubscriptions (uId u)
    else
        modifyActiveCheckSubscriptions'_ () $ \ c ->
            return $ c { acsUsers = HM.delete (uId u) (acsUsers c) }
    return u'

addActiveCheckSubscriptions user = do
    UrTime s _ <- getUrTime
    modifyActiveCheckSubscriptions'_ () $ \ c -> do
        let ct = UrTime (s `div` 60 * 60) 0
                 -- округляем, чтобы постоянно не перезаписывать
        return $ c { acsUsers = HM.insert user ct (acsUsers c) }

newSubscripion s =
    case sState s of
        SSAdded -> True
        SSScanning _ -> True
        _ -> False

livejournalFeed bfu =
    regexTest ".*livejournal\\.com/.*data/(rss|atom)" bfu

modifyUserSubscriptions user act = modifyUser' user $ \ u0 -> do
    u <- act u0
    let feedSet = HS.fromList . mapMaybe ssFeed . uSubscriptions
        new = HS.toList $ feedSet u `HS.difference` feedSet u0
    when (not $ null new) $
        addSubscriptionsToFilters user new
    return (u, u)

userAddSubscriptions :: Key User -> [Subscription] -> IO ()
userAddSubscriptions key ss = void $ modifyUserSubscriptions key $ \ uRead -> do
    t <- getUrTime
    uRead <- checkSubscriptions uRead
    let go sacc rens [] = (sacc, rens)
        go sacc rens (s:ss)
           | sUrl ns /= sUrl s =
               go (ns:sacc) ((t, sUrl s, sUrl ns) : rens) ss
           | otherwise = go (s:sacc) rens ss
           where ns = s { sUrl = normalizeTURL $ sUrl s }
        uvm = uViewMode uRead
        (ss', rens) = go [] (uvmSubUrlRenames uvm) ss
        u = resolve uRead $ uRead { uSubscriptions = ss'
                                  , uViewMode = uvm { uvmSubUrlRenames = rens }
                                  }
    checkSubscriptions u

userCheckSubscriptions :: Key User -> IO User
userCheckSubscriptions user = do
    u <- readUser' user
    -- не cached, чтобы при обновлении страницы обновилась информация о платеже,
    -- которая может прийти на другой сервер
    let needRefresh s =
            newSubscripion s
            || not (HM.member (sUrl s) (uvmSubViewModes $ uViewMode u))
    if any needRefresh (uSubscriptions u) then
        modifyUserSubscriptions user checkSubscriptions
    else
        return u

-- blogPostsScannedPercent u =
--     fmap (fmap c) $ readBlogPostsScanned (normalizeTURL $ T.pack u)
blogPostsScannedPercent t bps
    | size == 0 || diffUrTime t (bpsSubscribeTime bps) > 3600 = 100
    | otherwise =
        (length $ filter (all (\(_,_,s) -> s /= CUSNew) . Map.elems) $
         Map.elems $ bpsUrls bps) * 100 `div` size
    where size = Map.size (bpsUrls bps)

-- | Для отправки письма о готовности подписок. Готово и сколько
userSubscriptionsReady :: Key User -> IO (Maybe Int)
userSubscriptionsReady u = do
    user <- userCheckSubscriptions u
    let subscriptions = uSubscriptions user
        scanning s =
            case sState s of
                SSAdded -> True
                SSScanning {} -> True
                _ -> False
        feeds = catMaybes $ map ssFeed subscriptions

    if any scanning subscriptions then
        return Nothing
    else do
        t <- getUrTime
        bps <- cachedNothingReadManyBlogPostsScanneds feeds
        if all ((==100) . blogPostsScannedPercent t) (catMaybes bps) then
            return $ Just $ length feeds
        else
            return Nothing
