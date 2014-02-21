{-# LANGUAGE ViewPatterns #-}
module MuninPlugin where

import Data.Map (Map)
import qualified Data.Map as Map
import Generated.RiakIO
import Generated.DataTypes
import Control.Monad
import System.Environment
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.List
import Lib.UrTime
import Resolvables
import Riak
import Data.Maybe
import Data.Ord
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set

main = do
    args <- getArgs
    prog <- getProgName
    if args == ["config"] then
        config prog
    else
        printValues prog

riakConfig title vtitle info suffix next prog
    | ("_" ++ suffix) `isInfixOf` prog = do
        putStrLn $ "graph_title " ++ title
        putStrLn $ "graph_vlabel " ++ vtitle
        putStrLn "graph_category crawler"
        forM_ [ "UrlToScan", "ScanList"
              , "Posts", "Comments", "Msg"
              , "BlogPostsScanned", "SubscriptionUrlInfo"] $ \ n -> do
            let p k v = putStrLn $ n ++ "_" ++ suffix ++ "." ++ k ++ " " ++ v
            p "label" n
            p "info" (n ++ info)
            p "type" "DERIVE"
            p "min" "0"
    | otherwise = next prog

config =
    riakConfig "Size of Riak values written" "bytes per ${graph_period}"
               " written" "ws" $
    riakConfig "Size of Riak values read" "bytes per ${graph_period}"
               " read" "rs" $
    riakConfig "Size of Riak values deleted" "bytes per ${graph_period}"
               " delete" "ds" $
    riakConfig "Count of Riak values read" "Count / sec"
               " read count" "rc" $
    riakConfig "Count of Riak values written" "Count / sec"
               " write count" "wc" $
    riakConfig "Count of Riak values deleted" "Count / sec"
               " delete count" "dc" $
    postsComments $
    activeUsers "paid" $
    activeUsers "trial" $
    const defaultConfig

activeUsersGraphs =
    [ ("monthly", 30*24*12)
    , ("weekly" ,  7*24*12)
    , ("daily"  ,    24*12)
    , ("hourly" ,       12)
    , ("15min"  ,        3)
    , ("5min"   ,        1) ]

activeUsers what next prog
    | what `isInfixOf` prog = do
    putStrLn $ "graph_title Active " ++ what ++ " users"
    putStrLn "graph_args --base 1000 -l 0"
    putStrLn "graph_category stats"
    putStrLn "graph_scale no"
    putStrLn "graph_printf %6.0lf"
    putStrLn "graph_vlabel Users"
    mapM_ (\ (n,_) -> do
        let p k v = putStrLn $ what ++ "_" ++ n ++ "." ++ k ++ " " ++ v
        p "label" n
        p "type" "GAUGE"
        )
        activeUsersGraphs
    | otherwise = next prog

postsComments next prog
    | "_pc" `isInfixOf` prog = do
    putStrLn "graph_title Posts/comments statistics"
    putStrLn "graph_vlabel Count / sec"
    putStrLn "graph_category crawler"
    printConfig
        [("blogUrls","Blog URLs processed"),
         ("commentUrls","Comment URLs processed"),
         ("posts", "New posts"),
         ("comments", "New comments"),
         ("subUrls","Subscription URLs processed"),
         ("updatedPosts", "Updated posts"),
         ("updatedComments", "Updated comments")
        ]
    | otherwise = next prog

defaultConfig = do
    putStrLn "graph_title Crawler statistics"
    putStrLn "graph_vlabel Count / sec"
    putStrLn "graph_category crawler"
    printConfig
        [("urlsScanned","URLs scanned"),
         ("messages","New messages"),
         ("feed","Feeds scanned"),
         ("parsedHtml","Parsed HTMLs"),
         ("html","Useless/not parsed HTMLs"),
         ("hubFeeds","Feeds arrived from hubs"),
         ("hubMessages","Messages arrived from hubs"),
         ("hubSameHash","Same data from hub"),
         ("jsComments","HTML with js comments"),
         ("notModified","Not modified"),
         ("sameHash","Same data"),
         ("redirects","Redirects"),
         ("p_redirect","Parsed redirects"),
         ("parseErrors","Parse errors"),
         ("dnsErrors","DNS errors"),
         ("downloadErrors","Download errors"),
         ("toRemove","Feeds to remove"),
         ("removed","Feeds removed"),
         ("updatedMessages","Updated messages"),
         ("exceptions","Exceptions")]

printConfig =
    mapM_ (\ (n,i) -> do
        let p k v = putStrLn $ n ++ "." ++ k ++ " " ++ v
        p "label" n
        p "info" i
        p "type" "DERIVE"
        p "min" "0"
        )


printValues prog
    | "paid" `isInfixOf` prog = printActive "paid" paidUsers
    | "trial" `isInfixOf` prog = printActive "trial" trialUsers
    | otherwise = do
        s <- readStats' (T.pack "crawler")
        forM_ (Map.toList $ statsMap s) $ \ (x,y) ->
            putStrLn (T.unpack x ++ ".value " ++ show y)

printActive what f = forM_ activeUsersGraphs $ \ (name, n) -> do
    uf <- readUF n
    putStrLn $ what ++ "_" ++ name ++ ".value " ++ show (f $ uflFlags uf)

check_ws =
  mapM_ print . filter ((T.pack "_ws" `T.isSuffixOf`) . fst) . Map.toList . statsMap =<< readStats' (T.pack "crawler")

roundTime mul i (UrTime s _) = UrTime ((s `div` mul - i) * mul) 0

readUF' n = do
    t <- getUrTime
    ufs <- cachedReadManyUsageFlagss [roundTime 300 i t | i <- [1..n]]
    return $ catMaybes ufs
readUF n =
    fmap (rm . foldl' resolve (defaultUsageFlags $ UrTime 0 0)) $ readUF' n
    where rm u = u { uflFlags = HM.filter ((/= PTUnknown) . uufPaidTill) $
                                uflFlags u }

paidUsers = length . filter paid . HM.elems
    where paid u = case uufPaidTill u of
                       PTPaid _ -> True
                       _ -> False
trialUsers = length . filter paid . HM.elems
    where paid u = case uufPaidTill u of
                       PTFreeTrial _ -> True
                       _ -> False
trialUsers' t d = length . filter paid . HM.elems
    where paid u = case uufPaidTill u of
                       PTFreeTrial till ->
                           diffUrTime till t < (30-d)*86400
                       _ -> False
trialFinishedUsers = length . filter paid . HM.elems
    where paid u = case uufPaidTill u of
                       PTFreeTrialFinished _ -> True
                       _ -> False
mobileUsers = length . filter mobile . HM.elems
    where mobile u = not $ null [() | UFApp {} <- Set.elems $ uufUsageFlags u]
browserUsers = length . filter browser . HM.elems
    where browser u = not $ null [() | UFWeb {} <- Set.elems $ uufUsageFlags u]
skipUsers = length . filter (Set.member UFSkip . uufUsageFlags) . HM.elems
ignoreUsers = length . filter (Set.member UFIgnore . uufUsageFlags) . HM.elems
skipAndIgnoreUsers = length . filter (m . uufUsageFlags) . HM.elems
    where m f = Set.member UFSkip f && Set.member UFIgnore f

-- trialUsers x = HM.size x - paidUsers x

usage = do
    uf <- readUF (12*24)
    let counts = Map.fromListWith (+)
                 [ (f, 1)
                 | uuf <- HM.elems (uflFlags uf)
                 , f <- Set.toList (uufUsageFlags uuf) ]
        top = reverse $ sortBy (comparing snd) $ Map.toList counts
        app (UFApp {},_) = True
        app _ = False
        web (UFWeb {},_) = True
        web _ = False
        f = uflFlags uf
    let (apps, partition web -> (webs, acts)) = partition app top
    t <- getUrTime
    print ("total", HM.size f)
    print ("paid", paidUsers f)
    print ("trial", trialUsers f)
    forM_ [1..29] $ \ d ->
        putStrLn $ show (truncate d) ++ ";" ++ show (trialUsers' t d f)
    print ("trialFinished", trialFinishedUsers f)
    print ("skipAndIgnore", skipAndIgnoreUsers f)
    mapM_ (mapM_ print) [apps, webs, acts]
