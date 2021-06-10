{-# LANGUAGE RecordWildCards, ViewPatterns, OverloadedStrings #-}
module UsageFlags
    ( usageFlag
    , recordWebUsage
    , appByClientAndUserAgent
    , usageFlagsSaver
    , readUF, readUF', showUsageFlag
    )
    where

import Control.Monad
import Control.Concurrent
import Data.List
import qualified Data.Text as T
import Generated.DataTypes
import Lib.UrTime
import Data.Maybe
import Resolvables
import Riak
import Generated.RiakIO
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HM
import System.Random
import System.IO.Unsafe
import Test.QuickCheck
import Lib.BinaryInstances

------------------------------------------------------------------------------
-- Usage flags

{-# NOINLINE usageFlagsMVar #-}
usageFlagsMVar :: MVar (HM.HashMap T.Text UserUsageFlags)
usageFlagsMVar = unsafePerformIO $ newMVar HM.empty

usageFlagsSaver = forever $ do
    d <- randomRIO (5*1000*1000, 15*1000*1000)
    -- дабы меньше было шансов на одновременную запись
    threadDelay d
    uf <- modifyMVar usageFlagsMVar $ \ f -> return (HM.empty, f)
    UrTime s _ <- getUrTime
    mergeWriteUsageFlags $
        (defaultUsageFlags $ UrTime (s `div` aggBase * aggBase) 0) { uflFlags = uf }

usageFlag user flag = do
    u <- cachedReadUser' user
    us <- cachedReadUserStats' user
    let uuf =
            defaultUserUsageFlags
            { uufPaidTill = uvmPaidTill $ uViewMode u
            , uufCountry =
                fromMaybe "-" $ lookup "Country" (usFirstSignInDetails us)
            , uufUsageFlags = Set.singleton flag
            }
        seqUUF u =
            uufPaidTill u `seq` uufCountry u `seq` Set.size (uufUsageFlags u)
            `seq` u
    modifyMVar_ usageFlagsMVar $ \ m ->
        case HM.lookup user m of
            Just val -> return $! HM.insert user (seqUUF $ resolve uuf val) m
            Nothing -> return $! HM.insert user (seqUUF uuf) m

recordWebUsage user (unJunkText . fromMaybe "" -> ua) =
    usageFlag user $ UFWeb (browserByUserAgent ua) (osByUserAgent ua)

browserByUserAgent (T.toLower -> ua) =
    maybe BTUnknown snd $ find ((`T.isInfixOf` ua) . fst)
    [ ("ipad", BTIPad)
    , ("ipod", BTIPod)
    , ("iphone", BTIPhone)
    , ("opera mini", BTOperaMini)
    , ("opera", BTOpera)
    , ("edge", BTEdge)
    , ("iemobile", BTIEMobile)
    , ("msie", BTIE)
    , ("trident", BTIE)
    , ("firefox", BTFirefox)
    , ("conkeror", BTFirefox)
    , ("iceweasel", BTFirefox)
    , ("seamonkey", BTFirefox)
    , ("vivaldi", BTVivaldi)
    , ("chrome", BTChrome)
      -- многие браузеры добавляют Chrome, так что смотрим в последнюю очередь
    , ("safari", BTSafari)
      -- а сам Chrome добавляет Safari
    , ("android", BTAndroid)
    ]

osByUserAgent (T.toLower -> ua) =
    if ("darwin/" `T.isInfixOf` ua && "x86_64" `T.isInfixOf` ua) then
        OSMac
    else
    maybe OSUnknown snd $ find ((`T.isInfixOf` ua) . fst)
    [ ("ios", OSIOS)
    , ("ipad", OSIOS)
    , ("iphone", OSIOS)
    , ("ipod", OSIOS)
    , ("cfnetwork", OSIOS) -- есть и под mac и под iOS
    , ("darwin/", OSIOS)
    , ("mac", OSMac)
    , ("windows", OSWindows)
    , ("android", OSAndroid)
    , ("dalvik", OSAndroid)
    , ("cros", OSChromeOS)
    , ("linux", OSLinux)
    , ("x11", OSLinux)
    ]

-- Все варианты User-Agent для API:
-- sudo cat /tank/log/nginx/reader.access.log | grep -E " /((reader/|\?)api|accounts/ClientLogin)" | grep -v unread-count | cut -d\" -f 6 | sort | uniq | less

appByClientAndUserAgent (T.toLower -> cua) =
    maybe (UFApp ATUnknown os) snd $
    find ((`T.isInfixOf` cua) . fst)
    [ ("feeddler", UFApp ATFeeddler OSIOS)
    , ("reeder", UFApp ATReeder os)
    , ("feedme", UFApp ATFeedMe OSAndroid)
    , ("fiery", UFApp ATFieryFeeds OSIOS)
    , ("netnewswire", UFApp ATNetNewsWire OSUnknown)
    , ("mr. reader", UFApp ATMrReader OSIOS)
    , ("unread", UFApp ATUnread OSIOS)
    , ("lire", UFApp ATLire OSIOS)
    , ("focusreader", UFApp ATFocusReader OSAndroid)
    , ("justreader", UFApp ATJustReader OSAndroid)
    , ("newsplus", UFApp ATNewsPlus OSAndroid)
    , ("vienna", UFApp ATVienna OSMac)
    , ("readkit", UFApp ATReadKit os)
    , ("slowfeeds", UFApp ATSlowFeeds OSIOS)
    , ("slow feeds", UFApp ATSlowFeeds OSIOS)
    , ("slow%20feeds", UFApp ATSlowFeeds OSIOS)
    , ("web subscriber", UFApp ATWebSubscriber OSIOS)
    , ("newsjet", UFApp ATNewsJet OSAndroid)
    , ("amber", UFApp ATAmber OSAndroid)
    , ("readably", UFApp ATReadably OSAndroid)
    , ("fluent-reader", UFApp ATFluentReader os)
    , ("ravenreader", UFApp ATRavenReader os)
    , ("okhttp", UFApp ATokhttp OSUnknown)
      -- readably использует okhttp/3.11.0
    , ("gzip", UFApp ATgzip OSUnknown)
      -- newsplus в stream/items/ids не устанавливает client=newsplus
    ]
    where os = osByUserAgent cua

showUsageFlag f = case f of
    UFWeb {..} ->
        drop 2 (show ufBrowserType) ++ "/" ++ drop 2 (show ufOperatingSystem)
    UFApp {..} ->
        drop 2 (show ufAppType) ++ "/" ++ drop 2 (show ufOperatingSystem)
    UFShareAction sa ->
        drop 2 $ show sa
    UFMarkAllAsReadD o -> "MarkAllAsReadD " ++ show o
    UFMarkSearchAsReadD o -> "MarkSearchAsReadD" ++ show o
    o -> drop 2 $ show o

-- | Агрегаты по суткам, меньше/больше не делаем, т.к. при самом плохом запросе
-- за последний год будет 365+2*(288-1) = 939 чтений, что значительно
-- меньше 30*288 = 8640 (запрос за месяц) или 365*288 = 105120 (запрос за год)
-- при этом не заниматся слишком много места (а большая скорость нам
-- здесь не нужна), а так же можно определить последнее посещение с точностью
-- до дня.
aggStep = 12*24
aggBase = 5*60
aggTime  n = UrTime (aggBase * aggStep * n) aggStep
baseTime n = UrTime (aggBase * n) 0

prop_splitAggregates = do
    s <- choose (-100, 100 :: Int)
    e <- choose (-100, 100)
    step <- choose (1, 10)
    return $ counterexample ("splitAggregates " ++ show s ++ " " ++ show e ++ " " ++ show step) $
        let (l, a, r) = splitAggregates s e step in
        l ++ concatMap (\ x -> [x*step .. (x+1)*step - 1]) a ++ r
        ===
        [s..e]

splitAggregates start end step
    | startAgg > endAgg = ([], [], [start .. end])
    | otherwise =
       ([start .. startAgg * step - 1],
        [startAgg .. endAgg],
        [(endAgg + 1) * step .. end])
    where endAgg = (end + 1) `div` step - 1
          (startAgg0, startRem) = start `divMod` step
          startAgg
              | startRem == 0 = startAgg0
              | otherwise     = startAgg0 + 1

UrTime firstUsageFlagsTime _ = readYmdHMS "2014-02-11 20:25:00"

readUF' lastTime n = do
    UrTime now _ <- getUrTime
    let end = now `div` aggBase - lastTime
        start = max (firstUsageFlagsTime `div` aggBase) (end - (n-1))
        (l, agg, r) = splitAggregates start end aggStep
        readFlags = join . forkReadPar2 cachedNothingReadManyUsageFlagss
        checkAgg (Just a,  _) = return a
        checkAgg (Nothing, a) = do
            auf <- readManyUsageFlagss $
                map baseTime [a*aggStep .. (a+1)*aggStep - 1]
            -- print ("Aggregating ", aggTime a, catMaybes auf)
            let f = (aggUF $ catMaybes auf)
                    { uflTime = aggTime a }
            mergeWriteUsageFlags f
            return f
    -- print (length l, length agg, length r)
    ufsAgg0 <- readFlags $ map aggTime agg
    au <- mapM checkAgg $ zip ufsAgg0 agg
    lu <- readFlags $ map baseTime l
    ru <- readFlags $ map baseTime r
    return $ catMaybes lu ++ au ++ catMaybes ru

readUF lastTime n = aggUF <$> readUF' lastTime n

aggUF = rm . foldl' resolve (defaultUsageFlags $ UrTime 0 0)
    where rm u = u { uflFlags = HM.filter ((/= PTUnknown) . uufPaidTill) $
                                uflFlags u }
