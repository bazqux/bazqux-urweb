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

main = do
    args <- getArgs
    prog <- getProgName
    if args == ["config"] then
        config prog
    else
        printValues

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
    const defaultConfig

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
         ("subUrls","Subscription URLs processed")]
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
         ("exceptions","Exceptions")]

printConfig =
    mapM_ (\ (n,i) -> do
        let p k v = putStrLn $ n ++ "." ++ k ++ " " ++ v
        p "label" n
        p "info" i
        p "type" "DERIVE"
        p "min" "0"
        )


printValues = do
    s <- readStats' (T.pack "crawler")
    forM_ (Map.toList $ statsMap s) $ \ (x,y) ->
        putStrLn (T.unpack x ++ ".value " ++ show y)

check_ws =
  mapM_ print . filter ((T.pack "_ws" `T.isSuffixOf`) . fst) . Map.toList . statsMap =<< readStats' (T.pack "crawler")
