-- | Каталог фидов
{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards, BangPatterns,
             TupleSections #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- для ghci :set -fobject-code -O2
module Discovery
    ( searchSubscriptions
    , expiredBigUsers
    , lookupDiscoveryMsgTreeViewMode, nothingIfEmpty, postsTitle
    , postsAddSubscriber
    , looksLikePublicBlogFeed
    , esDiscoveryFeedExists, esDeleteDiscoveryFeed, esIndexDiscoveryFeeds
    , esUpdateDiscoveryFeed
    , deletedPosts, mkDeletedPostsBounds
    , SubFeedInfo(..), mkSubFeedInfo, mergeSubFeedInfoToDiscoveryFeed
    , faviconUrl
    , prepareIndexSubscriptionsData, indexSubscriptions
    , readUserFromStatsUsers
    )
    where

import Control.Monad
import Control.Applicative
import Control.Concurrent
import Data.List
import Data.Ord
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Generated.DataTypes
import Data.Binary
import Lib.UrTime
import qualified Lib.BArray as BA
import Lib.Log
import Lib.ElasticSearch
import Data.Maybe
import Data.Either
import URL
import Resolvables
import Generated.RiakIO
import Riak
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Network.HTTP.Conduit.Downloader
import Parser.Custom (hashSetNub', groupByN)
import Preprocess (xmlText, xmlLinksImgsVideosAndText, proxyUrl, ProxyType(PTFavicon))
import Lib.Regex
import Data.IORef
import System.IO
import System.IO.Unsafe
import System.Directory
import Text.HTML.TagSoup.Fast (escapeHtmlT, parseTagsT)
import Text.Printf
import qualified Data.Scientific as Scientific
import Lib.StringConversion
-- TODO: сделать модуль Common, чтобы был один импорт, а не такая куча
import Config

userStats = do
    users <- fmap T.lines $ T.readFile "/home/volodya/bazqux/crawler/users.txt"
    t <- getUrTime
    let monthAgo (usFirstSignInTime -> ft) = d > 30 && d <= 60
            where d = (t `diffUrTime` ft) / day
    uss <- fmap (filter monthAgo . catMaybes) $ readManyUserStatss users
    us <- fmap catMaybes $ readManyUsers $ map usId uss
    let new = fromIntegral $ length uss
        paidUser u = case uvmPaidTill (uViewMode u) of
            PTPaid pt | pt > t -> True
            _ -> False
        npus = filter (not . paidUser) us
        pus = filter paidUser us
        paid = fromIntegral $ length pus
        nzSubs u = length (uSubscriptions u) > 0
        feed (sState -> SSFeed _) = True
        feed _ = False
        avgSubs u = sum (map (length . uSubscriptions) u) `div` length u
        avgFSubs u = sum (map (length . filter feed . uSubscriptions) u) `div` length u
    print (new, paid, paid / new, length $ filter (not . nzSubs) us)
    print (avgSubs us, avgSubs (filter nzSubs us), avgSubs npus, avgSubs $ filter nzSubs pus)
    print (avgFSubs us, avgFSubs (filter nzSubs us), avgFSubs npus, avgFSubs $ filter nzSubs pus)
    print $ sort $ map (length . filter feed . uSubscriptions) pus

postsAddSubscriber bfu user add = modifyPostsSubscribers'_ bfu $ \ ps -> do
    t0 <- getUrTime
    let t | Just ((tMax,_,_),_) <- Set.maxView (psActions ps)
              = max t0 (tMax `plusUrTime` 1)
          | otherwise = t0
        ps' | add && user `HS.member` psSubscribers ps = ps
            | not add && not (user `HS.member` psSubscribers ps) = ps
            | otherwise =
                ps
                { psActions = Set.insert (t, user, add) $ psActions ps
                , psSubscribers =
                    (if add then HS.insert else HS.delete) user $
                    psSubscribers ps
                }
    return ps'

addAllPostsSubscribers = do
    us <- getAllUsers
    putStrLn $ "Total users: " ++ show (length us)
    forM_ (zip [1..] us) $ \ (n,u) -> do
        putStrLn $ show n ++ "\t" ++ T.unpack u
        ss <- fmap uSubscriptions $ readUser' u
        forM_ [sUrl s | s <- ss, SSFeed _ <- [sState s] ] $ \ bfu ->
            postsAddSubscriber bfu u True

privateUrl u =
    "key=" `T.isInfixOf` T.toLower u
    ||
    "code=" `T.isInfixOf` T.toLower u
     -- а вот private ничего не дает
    || urlHasAuthT u
    || ".feedsportal.com" `T.isSuffixOf` hostNameFromUrlT u
       --  ^ все фиды теперь возвращают пост "домен на продажу"
    || "bazqux.com/feed/" `T.isInfixOf` u
looksLikePublicBlogFeed (fromFtr -> u) =
    not (privateUrl u)
    && ((path /= "" && HS.fromList parts `HS.difference` ignored == HS.empty)
        || publicDomain u || any (`regexTest` u) publicFeedRegexes)
    where path = T.toLower $ T.pack $ fromMaybe "" $
                 urlPathAndQuery $ T.unpack $ fixLJ u
          parts = T.split (`elem` ("/.-_?=&" :: [Char])) path
          ignored =
              HS.fromList
              [ "", "rss", "rss2", "rss20", "atom", "rdf", "xml"
              , "feeds", "feed", "blogs", "blog", "web", "index"
              , "news", "newsticker", "index", "main", "posts", "default"
              , "home"
              , "data", "alt", ""
              , "current", "recent", "new", "articles", "full", "all"
              , "php", "aspx", "asp", "pl", "js"
              ]
          fixLJ u
              | [[_, user]] <- regexGet ljUserRegex u =
                  T.replace user "" u -- чтобы data/rss все равно отработал
              | otherwise = u

publicDomain = testDomains
    ["vk.com", "t.me", "telegram.me", "instagram.com"
    ,"feedburner.com"
    ,"pinterest.com", "flickr.com", "soundcloud.com", "slideshare.net"]

publicFeedRegexes =
    [ "^https?://[^/]*reddit.com/r/[^?=/]+/.rss$"
    , "^https?://twitter\\.com/[^?=/]+$"
    , "^https?://www\\.youtube\\.com/channel/[^?=/]+$"
    , "^https?://www\\.youtube\\.com/user/[^?=/]+$"
    , "^https?://www\\.youtube\\.com/playlist?list="
    , "^https?://compulenta.computerra.ru/rss.*\\.xml$"
    , "^https?://api\\.twitter\\.com/1/statuses/user_timeline\\.rss\\?screen_name="
--    , "^https?://twitter\\.com/statuses/user_timeline/[0-9]+.rss$"
    , "^https?://www\\.facebook\\.com/[0-9]+$"
    , "^https?://vimeo\\.com/.*/videos/rss$"
    ]

ljUserRegex = "https?://users\\.livejournal\\.com/([^/]+)/data"

fromFtr u
    | "ftr.bazqux.com/makefulltextfeed.php" `T.isPrefixOf` rmHttpPrefix u
    , Right (lookup "url" -> Just fu) <- urlQueryStringUtf8Only u
    , du <- fromMaybe fu (rmHttpPrefix' fu <> T.stripPrefix "sec://" fu)
    = "http://" <> du
    | otherwise = u

saveStatsUsers = do
--    users <- fmap T.lines $ T.readFile "lwusers.txt"
    users <- getAllUsers
    dusKeys <- riakBucketKeys "DeletedUser"
    us <- readManyUsers users
    uss <- readManyUserStatss users
    dus <- fmap catMaybes $ readManyDeletedUsers dusKeys
    let firstBackup d = map (duUser d ,) $ take 1 (duBackups d)
    ubs <- fmap catMaybes $ readManyUserBackups $ concatMap firstBackup dus
    let u = hashSetNub' (fmap uId . fst) $
            zip us uss ++
            map (\ub -> (Just $ ubUser ub, Just $ ubUserStats ub)) ubs
    BL.writeFile "statsUsers" $ encode $ unzip u

saveStatsSettings = do
--    users <- fmap T.lines $ T.readFile "lwusers.txt"
    users <- getAllUsers
    us <- readManyUsers users
    uss <- readManyUserSettingss users
    BL.writeFile "statsSettings" $ encode (us, uss)

settingsStats = do
    (us,usts) <- fmap decode $ BL.readFile "statsSettings"
    let st x = T.pack $ show x
        s = foldl' (HM.unionWith (+)) HM.empty
            [HM.fromList $ map (,1)
                [ st ustScrollMode, st ustListViewMode
                , st ustMarkReadMode
                , if ustExactUnreadCounts then "eucExact" else "euc500+"
                , if ustUltraCompact
                  then "ultraCompactTrue" else "ultraCompactFalse"
                ]
            | (Just u, Just (UserSettings {..})) <- zip us usts
            , case uvmPaidTill (uViewMode u) of
                PTPaid pt -> True
                _ -> False]
    forM_ (sort $ HM.toList s) print

readUserFromStatsUsers uid = do
    r <- readUserCountryPaidList
    let [(u,_,_)] = filter (\(u,_,_) -> uId u == uid) r
    return u

readUserCountryPaidList :: IO [(User, T.Text, Bool)]
readUserCountryPaidList = do
    (us,uss) <- fmap decode $ BL.readFile "statsUsers"
    let uc = catMaybes $ zipWith uAndC us uss
        uAndC (Just u) (Just (lookup "Country" . usFirstSignInDetails -> Just c)) =
            Just (u, c)
        uAndC (Just u) _ = Just (u,"-")
        uAndC _ _ = Nothing
        paidUser u = uPayments u /= []
--             case uvmPaidTill (uViewMode u) of
--               PTPaid pt -> True
--               _ -> False
    return $ map (\(u,c) -> (u,c,paidUser u)) uc

expiredBigUsers = do
    t <- getUrTime
    (us,uss) <- fmap decode (BL.readFile "statsUsers") :: IO ([Maybe User], [Maybe UserStats])
    print "read"
    let expired u = case uvmPaidTill (uViewMode u) of
              PTFreeTrial pt -> diffUrTime t pt > 60*day
              PTFreeTrialFinished pt -> diffUrTime t pt > 30*day
              _ -> False
        paidUser u = case uvmPaidTill (uViewMode u) of
              PTPaid pt -> True
              _ -> False
        users = [ u | Just u <- us, paidUser u ]
                -- , expired u
--                , length (uSubscriptions u) <= 400 ]
        usersH = [ u | Just u <- us --, paidUser u
                 , expired u, "://" `T.isInfixOf` uId u
                 || length (uSubscriptions u) > 200 ]
        hist =
            IntMap.fromListWith (+) $
            map ((,1) . length . uSubscriptions) users
        subs = HS.fromList [ sUrl s | s <- concatMap uSubscriptions users
                           , SSFeed _ <- [sState s] ]
        subsH = HS.fromList [ sUrl s | s <- concatMap uSubscriptions usersH
                           , SSFeed _ <- [sState s] ]
        subsC = HS.intersection subs subsH
        pdel = HS.filter looksLikePublicBlogFeed $ HS.difference subsH subs
        s = fromIntegral . HS.size
    print (length users, length usersH, HS.size subs, HS.size subsH - HS.size subsC, HS.size pdel,
           (s subsH - s subsC) / s subs)
    print (HS.size subsC)
    writeFile "subsHist.csv" $
        unlines [show s ++ ";" ++ show n | (s,n) <- IntMap.toList hist]

-- | Промежуточная статистика о подписках.
-- Сохраняется, чтобы дальше экспериментировать быстрее
data SubStatsImm' a
    = SubStatsImm
      { ssiPaidCountries :: !(HM.HashMap T.Text a)
      , ssiTotalCountries :: !(HM.HashMap T.Text a)
      , ssiTags :: !(HM.HashMap T.Text a)
      }
    deriving (Show, Eq)
type SubStatsImm = SubStatsImm' Int

unionSSI f (SubStatsImm a1 b1 c1) (SubStatsImm a2 b2 c2) =
    SubStatsImm (u a1 a2) (u b1 b2) (u c1 c2)
    where u = HM.unionWith f

instance Functor SubStatsImm' where
    fmap f (SubStatsImm a b c) = SubStatsImm (f' a) (f' b) (f' c)
        where f' = HM.map f

instance Binary a => Binary (SubStatsImm' a) where
    put (SubStatsImm x1 x2 x3)
          = do put x1
               put x2
               put x3
    get
          = do !x1 <- get
               !x2 <- get
               !x3 <- get
               return (SubStatsImm x1 x2 x3)

saveSubStatsImm = do
    ucp <- readUserCountryPaidList
    putStrLn "read"
    dups <- findDuplicateUsers ucp
    let collect !a (s,u,co,paid) =
            HM.insertWith (unionSSI HS.union) (sUrl s)
                (let hs = HS.singleton u in
                 SubStatsImm
                 { ssiPaidCountries =
                     if paid then HM.singleton co hs else HM.empty
                 , ssiTotalCountries = HM.singleton co hs
                 , ssiTags = HM.fromList $ map (,hs) $ sFolders s
                 })
                a
        feed (sState -> SSFeed _) = True
        feed _ = False
        publicBlog u ssi
            | privateUrl u = False
            | otherwise =
                sum (HM.elems $ ssiTotalCountries ssi) >= 10 ||
                looksLikePublicBlogFeed u
        r = HM.filterWithKey publicBlog $
            HM.map (fmap HS.size) $ -- пользователей в их кол-во
            dedupFeedUrls $
            foldl' collect HM.empty $
            concatMap (\ (u,c,p) ->
                           if uId u `HS.member` dups then []
                           else
                               map (,uId u, c,p) $ filter feed $
                               uSubscriptions u)
                      ucp
    putStrLn "Calculating sub stats and deduplicating feed urls"
    BL.writeFile "subsStatsImm" $ encode r
--     r' <- readSubStatsImm
--     print $ r' == r

readSubStatsImm :: IO (HM.HashMap TURL SubStatsImm)
readSubStatsImm = fmap (decode) $ BL.readFile "subsStatsImm"

newFixTagMemo = do
    fixMemo <- newIORef HM.empty
    return $ \ t -> unsafePerformIO $ do
        m <- readIORef fixMemo
        case HM.lookup t m of
            Just r -> return r
            Nothing -> do
                let r = fixTag t
                writeIORef fixMemo $ HM.insert t r m
                return r

findDuplicateUsers ucp = do
--     ucp <- readUserCountryPaidList
--     putStrLn "read"
    fixTagMemo <- newFixTagMemo
    let subsSets = [ (u, s)
                   | (u,_,_) <- ucp
                   , let s = HS.fromList $ map sUrl $
                             filter ((/= []) . mapMaybe fixTagMemo . sFolders) $
                             -- 12.6sec -> 9.4sec
                             -- и так больше фильтруется именно дублирующихся
                             -- папок, бывают разные подписки, но почему-то
                             -- с одинаковыми папками.
                             uSubscriptions u
                   , HS.size s /= 0 ]
        level = 70
        -- 70 -- 300
        -- 60 -- 740
        -- в принципе, можно было бы игнорировать наборы < 10 фидов,
        -- но лучше ложные срабатывания, чем левые теги
        sameness a b
            | 100 * min sa sb `div` max sa sb < level = 0
            | otherwise =
                100 * HS.size (HS.intersection a b) `div` max sa sb
            where sa = HS.size a
                  sb = HS.size b
        go d [] = return d
        go !d ((u,s):us)
            | uId u `HS.member` d = go d us
            | otherwise = do
                let same =
                        [ (uId u', ss, HS.size s') | (u',s') <- us
                        , let ss = sameness s s'
                        , ss >= level ]
                when (same /= []) $
                    print (uId u, same)
                go (foldl' (\ d (u,_,_) -> HS.insert u d) d same) us
    print $ length subsSets
    putStrLn "Searching duplicated users"
    withLogger $ \ l -> logTime l "" $ go HS.empty subsSets

dedupFeedUrls m = foldl' fixUrl m (HM.toList m)
    where fixUrl m (u, ssi)
              | u' <- fixU u
              , u' /= u
              , Just ssi' <- HM.lookup u' m =
                  let ussi = unionSSI HS.union ssi ssi' in
                  -- fixUrl
                  (HM.insert u' ussi $ HM.delete u m)-- (u', ussi)
              | otherwise = m
          fixU u
              | "livejournal.com/" `T.isInfixOf` u =
                  (if "//users.livejournal.com" `T.isInfixOf` u then id
                   else T.replace "_" "-") $
                  T.replace "/data/rss/" "/data/rss" $
                  T.replace "/data/atom" "/data/rss" u
              | [[_,reddit]] <-
                  regexGet "^https?://[^/]*reddit.com/r/([^/]+)/.rss$" u =
                  T.concat ["http://www.reddit.com/r/", reddit, "/.rss"]
              | [[_,user]] <-
                  regexGet "^https?://([^.]+)\\.blogspot\\..*/" u =
                  T.concat ["http://",user,".blogspot.com/feeds/posts/default"]
              | otherwise =
                  normU $
                  rmDSlash $
                  T.replace "?format=xml" "" $ -- feedburner.com/asdf?format=xml
                  T.replace "?alt=rss" "" $ -- /feeds/posts/default?alt=rss
                  T.replace "http://twitter.com" "https://twitter.com" u
          rmDSlash x = T.append h (T.replace "//" "/" t)
              where (h,t) = T.splitAt (T.length "https://") x
          normU u = HM.lookupDefault u u normalizedUrls
          -- всё (кроме feedburner?) надо автоматически пытаться переводить
          -- на https вторым этапом (сначала убрав лишние http)
          -- или даже сразу пытаться объединить группами,
          -- http://www.x, https://x?alt=rss можно объединить в один
          -- (пусть даже не HTTPS, важно, чтобы фид был один и с максимально
          -- простым url (можно еще число подписчиков учитывать,
          -- но, при одинаковых host/path, предпочитаем https)
          -- также стоит искать варианты без www
          normalizedUrls = HM.fromList
              [ ("http://blog.bazqux.com/feeds/posts/default", "https://blog.bazqux.com/feed")
              , ("https://blog.bazqux.com/feeds/posts/default", "https://blog.bazqux.com/feed")
              , ("http://blog.bazqux.com/feeds/posts/default?alt=rss", "https://blog.bazqux.com/feed")
              , ("https://blog.bazqux.com/feeds/posts/default?alt=rss", "https://blog.bazqux.com/feed")
              , ("http://xkcd.com/atom.xml", "https://xkcd.com/rss.xml")
              , ("http://www.xkcd.com/rss.xml", "https://xkcd.com/rss.xml")
              , ("http://www.xkcd.com/atom.xml", "https://xkcd.com/rss.xml")
              , ("http://feeds2.feedburner.com/codinghorror", "http://feeds.feedburner.com/codinghorror")
              , ("http://feeds2.feedburner.com/codinghorror/", "http://feeds.feedburner.com/codinghorror")
              , ("http://feeds.feedburner.com/codinghorror/", "http://feeds.feedburner.com/codinghorror")
              , ("http://www.codinghorror.com/blog/index.xml", "http://feeds.feedburner.com/codinghorror")
              , ("http://www.the-digital-reader.com/feed", "http://feeds.feedburner.com/TheDigitalReader")
              , ("http://www.the-digital-reader.com/feed/", "http://feeds.feedburner.com/TheDigitalReader")
              , ("http://whattheduck.net/strip/rss.xml", "http://www.whattheduck.net/strip/rss.xml")
              , ("http://feeds.penny-arcade.com/pa-mainsite", "http://penny-arcade.com/feed")
              , ("http://feeds.penny-arcade.com/pa-mainsite/", "http://penny-arcade.com/feed")
              , ("http://feeds.feedburner.com/HighScalability", "http://highscalability.com/rss.xml")
              , ("http://news.ycombinator.com/rss", "https://news.ycombinator.com/rss")
              , ("http://feeds.boingboing.net/boingboing/iBag", "http://feeds.feedburner.com/boingboing/iBag")
              , ("http://feeds.feedburner.com/Mashable", "http://feeds.mashable.com/Mashable")
              , ("http://www.insidefacebook.com/feed/", "http://feeds.feedburner.com/InsideFacebook")
              , ("http://images.apple.com/main/rss/hotnews/hotnews.rss", "http://www.apple.com/main/rss/hotnews/hotnews.rss")
              , ("http://cultofmac.com.feedsportal.com/c/33797/f/606249/index.rss", "http://feeds.feedburner.com/cultofmac/bFow")
              , ("http://feeds.arstechnica.com/arstechnica/index/", "http://feeds.arstechnica.com/arstechnica/index")
              , ("http://feeds.arstechnica.com/arstechnica/everything", "http://feeds.arstechnica.com/arstechnica/index")
              , ("http://feeds.gawker.com/lifehacker/full", "http://feeds.gawker.com/lifehacker/vip")
              , ("http://lifehacker.com/rss", "http://feeds.gawker.com/lifehacker/vip")
              , ("http://www.siliconrus.com/feed/", "http://feeds.feedburner.com/siliconrus")
              , ("http://feeds.feedburner.com/Swissmiss", "http://feeds2.feedburner.com/Swissmiss")
              , ("http://feeds.feedburner.com/Techcrunch", "http://feeds2.feedburner.com/Techcrunch")
              , ("http://feeds.feedburner.com/TechCrunch", "http://feeds2.feedburner.com/Techcrunch")
              , ("http://feeds.feedburner.com/Techcrunch/", "http://feeds2.feedburner.com/Techcrunch")
              , ("http://feeds.feedburner.com/TechCrunch/", "http://feeds2.feedburner.com/Techcrunch")
              , ("http://feeds.feedburner.com/oatmealfeed", "http://theoatmeal.com/feed/rss")
              , ("http://feeds.feedburner.com/TEDBlog", "http://feeds.feedburner.com/tedblog")
              , ("http://lenta.ru/rss/", "https://lenta.ru/rss")
              , ("http://www.lenta.ru/rss", "https://lenta.ru/rss")
              , ("http://lenta.ru/rss/news", "https://lenta.ru/rss")
              , ("http://lenta.ru/rss/news/", "https://lenta.ru/rss")
              , ("http://feeds.feedburner.com/cl_news/", "http://compulenta.computerra.ru/rss.xml")
              , ("http://feeds.feedburner.com/cl_news", "http://compulenta.computerra.ru/rss.xml")
              , ("http://feeds2.feedburner.com/cl_news/", "http://compulenta.computerra.ru/rss.xml")
              , ("http://feeds2.feedburner.com/cl_news", "http://compulenta.computerra.ru/rss.xml")
              , ("http://feeds.feedburner.com/psdisasters/dqgx/", "http://www.psdisasters.com/feed")
              , ("http://feeds.feedburner.com/psdisasters/dqgx", "http://www.psdisasters.com/feed")
              , ("http://www.boston.com/bigpicture/index.xml", "http://feeds.boston.com/boston/bigpicture/index")
              , ("http://feeds.dilbert.com/DilbertDailyStrip",
                 "http://feed.dilbert.com/dilbert/daily_strip")
              , ("http://feeds.feedburner.com/DilbertDailyStrip",
                 "http://feed.dilbert.com/dilbert/daily_strip")
              , ("http://geekandpoke.typepad.com/geekandpoke/atom.xml",
                 "http://feeds.feedburner.com/GeekAndPoke")
              , ("http://netzpolitik.org/feed/", "https://netzpolitik.org/feed/")
              , ("http://abstrusegoose.com/feed", "http://abstrusegoose.com/feed.xml")]

sortTags t = sortBy (comparing $ \(t,n) -> (Down n,t)) $ HM.toList t

normalizeSubsTags = do
    putStr "normalizeSubsTags… "
    s <- readSubStatsImm
    fixTagMemo <- newFixTagMemo
    top <- topUsersTags False
    let collect _ acc !allTags [] = (acc, allTags)
        collect checkTag !acc !allTags ((u,ssi):xs) =
            let tags = fixOboobs $
                    HM.filterWithKey checkTag $ HM.fromListWith (+)
                    [ (t',n) | (t,n) <- HM.toList $ ssiTags ssi
                    , t' <- mapMaybe fixTagMemo [t] ]
                fixOboobs
                    | "oboobs" `T.isInfixOf` u =
                        HM.delete "photo" . HM.delete "fun"
                        -- а то вылезают сиськи в поиске
                    | otherwise =
                        id
            in
              collect checkTag ((u,ssi { ssiTags = tags }):acc)
                  (HM.unionWith (+) allTags (HM.map (const 1) tags))
                  xs
        (_, all5) = collect (\ k n -> n >= 5) [] HM.empty $ HM.toList s
        goodTags =
            all5 `HM.union` top `HM.union` HM.map (const 1) translatedTags
        (s', all) =
            collect (\ k n -> n >= 2 && k `HM.member` goodTags) [] HM.empty $
                    HM.toList s
        r = HM.fromList s'
    HM.size r `seq` do
        putStrLn "done"
        return r
--     print $ HM.size all
--     forM_ s' $ \ (u, ssiTags -> tags) ->
--         when (HM.size tags > 1) $ do
--             T.putStrLn u
--             forM_ (zip [1..] $ sortTags tags) $
--                 \ (i,(t,n)) -> do
--                     when (i == 11) $
--                         T.putStrLn "-------"
--                     T.putStrLn $ T.concat ["  ", t, "\t", T.pack (show n)]
--     forM_ (sortBy (comparing $ Down . snd) $ HM.toList all) $ \ (t,n) ->
--         when (T.length (T.filter isDigit t) > 0) $
--             T.putStrLn t

testStrangeTags = do
    pus <- readUserCountryPaidList
--     let subs =
--             filter (T.isInfixOf "mantrabox.livejournal" . sUrl) $
--             concatMap (\ (u,c,p) -> uSubscriptions u)
--                       pus
--     mapM_ print subs
    let s = filter (any ((== Just "sd: #76 - #100") . fixTag) .
                    concatMap sFolders . uSubscriptions . \(u,_,_) -> u) pus
    print $ map (\(u,_,_) -> uId u) s

topUsersTags report = do
    ucp <- readUserCountryPaidList
    fixTagMemo <- newFixTagMemo
    let collect !allTags !cTags [] = (allTags, cTags)
        collect !allTags !cTags ((u,c,_):xs) =
            -- топ для каталога надо смотреть с учетом перевода
            -- францию 1.5% и испанию 1.1% можно и не переводить?
            -- fromMaybe "" . fixTag . T.dropWhile (\ c -> c == '_' || (c >= '0' && c <= '9')) .
            let tags = HM.fromList $ map (,1) $
                       -- map translateTag $
                       mapMaybe fixTagMemo $
                       concatMap sFolders $ uSubscriptions u
            in
            collect (HM.unionWith (+) allTags tags)
                    (HM.insertWith (HM.unionWith (+)) c tags cTags)
                    xs
--         collect !allTags !cTags (_:xs) =
--             collect allTags cTags xs
        (all, c) = collect HM.empty HM.empty ucp
        filtDigits = HM.filterWithKey (\ k _ -> regexTest "^[ !_=]+" k)
        -- 0x00
    if report then do
        print $ HM.size $ HM.filter (>= 10) all
        forM_ (take 10000 $ sortTags $ HM.filter (>=10) all) $ \ (t,n) ->
            T.putStrLn $ T.concat ["  ", t, "\t", T.pack (show n)]
        return HM.empty
    else
        return $ HM.filter (>= 10) all
    -- 47105 уникальных
    -- 42096 toLower
    -- 41883 toLower + drop '_'
    -- 41440 toLower + drop '_' + drop '0-9' -- 3d printing??
    -- 40039 … + fixTag
    -- 39411 … + fixTag с удалением префикса и доп фильтрами
    -- по сути, toLower 5k, 20 символов 2k, и все остальное 2.5k
    -- 36881 … <= 20 символам
    -- с цифр начинается только 1800
         -- еще !
         -- вывести те, что начинаются с цифр и знаков препинания
         -- 1.news
         -- 1. news
         -- 2-news
         -- 2 news
         -- sd: #11 - #25 ???
         -- sd: #26 - #50
         -- sd: #101 - etc…
         -- h@ck3r5 and hack9  -- не больше одной цифры?
         --
         -- "xxx-news", "xxx - news", "xxx news"
         -- "^fun.xxx"
         -- [asdf]
         -- -= asdf =-
         -- --- 1 ---
         -- - games -
    --  4175 … filter >= 3  -- стало 3961 -- 5% убрал
    --  1149 … filter >= 10
    --
    --  2191 >= 5

-- смысла особого в таком преобразовании тегов нет, т.к. шанс, что
-- нумерованные теги совпадут для одной подписки невелик.
-- с другой стороны, если делать преобразование до, то можно получить доп теги
digitsOk = regexTest "^[^0-9]*(4chan|4pda|2ch|2d|3d|1c|1с|911|4bsd|3dnews|49ers|500px|9gag|9 gag|360°|100%|365 day|9to5|96black|96 black|[0-9]+(st|nd|rd|th))"
rmPrefix t
    | [l@(_:_)] <- regexGet "^[ !_=\\)\\.]*([0-9]+[. 、_|\\)@#:\\-]*)+(.*)$" t
    , not $ digitsOk t --  не обрабатывается вариант 123. 4chan
    = last l
    | [[_,x]] <- regexGet "^[ !_=\\)]+(.*)$" t
    = x
    | otherwise = t

fixTag (rmPrefix . T.toLower . T.strip -> t)
       -- 1569 -> 1549 всего 20 дополнительных тегов вылезает
       -- нужен ли этот rmPrefix? только если для топа
       -- нет, надо, а то _linux и _default вылезает
    | ignored t = Nothing
--    | t == "IT" = Just "IT" -- не меняем
      -- PC, AI
      -- mac + ios  - два тега
    | otherwise = Just $ case t of
        "technology" -> "tech"
        "photography" -> "photo"
        "fashon" -> "fashion"
        "listen subscriptions" -> "podcasts"
        "humour" -> "humor"
        "beauty blogs" -> "beauty"
        "development" -> "dev"
        t -> t
    where ignored t =
              T.length t == 0 || T.length t > 20 ||
              (T.all (== T.index t 0) t && t `notElem`
                    ["xxx", "жж", "美", "书", "書"]) ||
              -- a, aaa, bbb, и xxx заодно )
         -- # -- c#, f# и всё
         -- # -- dev:pyhon, zeit online: politik
              regexTest "global\\.|misc|feed|folder|feedly|other|friend|freund|family|my |malware|warez|etc|sites|favorite" t ||
              any (`T.isInfixOf` t)
                  [ "мудак", "говнюк", "мусор", "блеват"
                  , "лучш", "интересн", "варез"
                  , "семья", "друз", "свои", "мои ", "мое ", "моё ", "моя "
                  , "чтени", "читат", "лента", "каналы", "абилон"]
              -- regex почему-то с русскими буквами не работает
              || badTitle t
              || t `HS.member` badTags
          badTags = HS.fromList $
              [ "blogger-following", "google reader", "default", "all"
              , "subscriptions", "подписки", "suscripciones"
              , "netvibes", "page2rss", "del.icio.us", "pinboard"
              , "vienna", "readability", "newsrob"
              , "read later", "лента для чтения"
              , "to read", "read", "toread"
              , "из жж"
              , "local news", "local", "home", "lokales"
              , "[my feeds]", "[мои каналы]", "my feeds"
              , "main folder", "main"
              , "temp", "tmp", "~tmp" , "test"
              , "топ", "редко"
              , "важное", "important", "infrequent", "low"
              , "daily", "often", "rare", "private", "must read"
              , "rare updates", "rarely updated", "low priority"
              , "todo"
              , "#newssquares.xl"
              , "my blogs", "друзья", "знакомые", "любимое"
              , "личное", "personal", "rss"
              , "notifications"
              , "моя лента", "блоги-1", "me", "test", "first"
              , "blogroll", "friend", "buy", "old", "my", "aggregators"
              , "その他" -- other JP
              , "otros" -- other ES
              , "sonstiges" -- other DE
              , "everything else", "always", "everyday"
              , "другое", "остальное", "свалка", "хрень", "прочее", "разное"
              , "diff", "different", "general"
              , "хуита", "херня"
              , "халява", "скидки"
              , "moje", "neighbours"
              , "feedly.others", "uncategorized", "ignore"
              , ". news", "-news", "news2"
              , "jailbreak"
              , "познавательно", "uncategorised"
              ]

badTitle t = any bad badTitles
    where bad = all inParts
          inParts b = any (b `T.isPrefixOf`) parts
          parts = filter (/= "") $ T.split (not . isAlphaNum) $ T.toLower t
          badTitles = map T.words
              [ "isis", "игил"
              , "ufo", "нло", "paranormal"
              , "alternative medicine"
              , "chiroprac"
              , "xxx", "fuck", "sex", "porn", "ero", "nsfw", "kink", "queer"
              , "секс", "порн", "эро"
              ]

data SubFeedInfo
    = SubFeedInfo
      { sfiTitle :: !T.Text
      , sfiLink  :: !(Maybe TURL)
      , sfiImageLink :: !(Maybe TURL)
      , sfiLastRefreshTime :: !UrTime
      , sfiPostsPerDay :: !Double
      , sfiAveragePostLength :: !Double
        -- число постов в день и их размер (либо за последнюю неделю, либо за
        -- последние 10 постов)
      }
    deriving Show

mergeSubFeedInfoToDiscoveryFeed (SubFeedInfo {..}) df =
    df
    { dfTitle             = sfiTitle
    , dfWebsite           = sfiLink
    , dfImage             = sfiImageLink
    , dfLastRefreshTime   = UrTime (urtSeconds sfiLastRefreshTime) 0
    , dfPostsPerDay       = sfiPostsPerDay
    , dfAveragePostLength = sfiAveragePostLength
    }

instance Binary SubFeedInfo where
    put (SubFeedInfo x1 x2 x3 x4 x5 x6)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
    get
          = do !x1 <- get
               !x2 <- get
               !x3 <- get
               !x4 <- get
               !x5 <- get
               !x6 <- get
               return (SubFeedInfo x1 x2 x3 x4 x5 x6)

saveSubFeedInfos = do
    s <- readSubStatsImm
    print ("Total", HM.size s)
    t <- getUrTime
    let go acc [] = return acc
        go !acc (s:ss) = do
            t' <- getUrTime
            print (HM.size acc, showSecs $ diffUrTime t' t)
            posts' <- forkRead $ readManyPostss s
            posts <- posts'
                     -- убрал параллельность,
                     -- пусть два часа индексирует, но меньше систему
                     -- нагружает
            uts' <- forkRead $ readManyUrlToScans s
            uts <- uts'
            sfis <-
                -- join $ forkReadPar2
                    (mapM $ \ (f,p,u) -> do
                        threadDelay $ 15*1000
                        -- у нас где-то 70 фидов в секунду обрабатываются,
                        -- растянем в два раза, чтобы меньше систему грузить.
                        -- по-хорошему, средний размер сообщений вообще стоит
                        -- где-нить кешировать.
                        fmap (f,) $ mkSubFeedInfo f t p u)
                    (zip3 s posts uts)
            go (HM.union acc $ HM.fromList sfis) ss
    sfis <- go HM.empty $ groupByN 1000 $ sort $ HM.keys s
    BL.writeFile "subFeedInfos" $ encode sfis

readSubFeedInfos :: IO (HM.HashMap TURL (Either T.Text SubFeedInfo))
readSubFeedInfos = do
    putStr "readSubFeedInfos… "
    x <- fmap decode $ BL.readFile "subFeedInfos"
    HM.size x `seq` do
        putStrLn "done"
        return x

feedSubFeedInfo :: TURL -> IO (Either T.Text SubFeedInfo)
feedSubFeedInfo u = do
    posts <- readPosts u
    uts <- readUrlToScan u
    t <- getUrTime
    mkSubFeedInfo u t posts uts

mkSubFeedInfo :: TURL -> UrTime -> Maybe Posts -> Maybe UrlToScan
              -> IO (Either T.Text SubFeedInfo)
mkSubFeedInfo feed t p u = do
--     p <- readPosts feed
--     u <- readUrlToScan feed
--     t <- getUrTime
    sfi <- mkSubFeedInfo' t p u
    imgFix <- case (sfi, p) of
        (Right (SubFeedInfo {..}), Just (Posts {..}))
            | "Twitter / " `T.isPrefixOf` sfiTitle
            , author <- T.drop (T.length "Twitter / ") sfiTitle
            , authorUri <-
                Just $ T.toLower $ T.append "https://twitter.com/" author
            , regexTest "http.*twitter.com/.*" feed ->
                findImg pMsgTree
                    ((== authorUri) . fmap T.toLower . msgAuthorUri)
            | [[_,T.toLower -> author]] <-
                regexGet "http://(.*).wordpress.com/feed" feed
            , case sfiImageLink of
                  Nothing -> True
                  Just l -> "/buttonw-com.png" `T.isInfixOf` l
            ->
                findImg pMsgTree ((== author) . T.toLower . msgAuthor)
            | [[_,profile]] <-
                regexGet "^http://www\\.facebook\\.com/([0-9]+)$" feed
            ->
                return $ Just $ T.concat [ "https://graph.facebook.com/"
                                         , profile, "/picture" ]
        _ ->
            return Nothing
    return $ fmap (\ sfi -> sfi { sfiImageLink = fmap mkSecureGravatar $
                                    imgFix <|> sfiImageLink sfi }) sfi
    where findImg mt f = go f $ take 50 $ reverse $ BA.elems $ mtHeaders mt
          go f [] = return Nothing
          go f (mh:mhs) = do
              mbm <- readMsg $ MsgKey feed (Just $ mhGuid mh) Nothing
              case mbm of
                  Just m
                      | f m -> return $ msgAuthorPic m
                  _ -> go f mhs

mkSecureGravatar u
    | [[_,id]] <- regexGet "gravatar.com/avatar/([0-9a-fA-F]{32})" u
    = T.concat
      [ "https://secure.gravatar.com/avatar/", id, "?d=404&r=g" ]
    | [[_,id]] <- regexGet "gravatar.com/blavatar/([0-9a-fA-F]{32})" u
    = T.concat
      [ "https://secure.gravatar.com/blavatar/", id, "?d=404&r=g" ]
    | otherwise = u

mkDeletedPostsBounds (lo,hi) = (hi+1+deletedPostsBoundsOffset, hi+1)
deletedPostsBoundsOffset = 1000
deletedPostsBounds (lo,hi) = lo-hi == deletedPostsBoundsOffset
deletedPosts = deletedPostsBounds . BA.bounds . mtHeaders . pMsgTree

median :: (Ord t, Fractional t) => [t] -> t
median [] = 0
median l
    | odd n     = s !! (n `div` 2)
    | otherwise = (s !! (n `div` 2) + s !! (n `div` 2 - 1)) / 2
    where n = length l
          s = sort l

testPostLengths = do
    u <- readUser' "1"
    curTime <- getUrTime
    sfis <- fmap rights $ mapM feedSubFeedInfo [f | SSFeed f <- map sState $ uSubscriptions u]
    forM_ (take 1000 $ sortBy (comparing $ Down . sfiAveragePostLength) sfis) $ \ s ->
        let t x = T.pack $ show x
            df = mergeSubFeedInfoToDiscoveryFeed s $ defaultDiscoveryFeed ""
        in
            T.putStrLn $ T.concat
            [sfiTitle s, " = ", t $ sfiAveragePostLength s
            , " / ", t $ postLengthPenalty df
            , " / ", t $ highFreqPenalty df
            , " / ", t $ lastRefreshPenalty curTime df
            , " : ", t $ postLengthPenalty df * highFreqPenalty df
                - lastRefreshPenalty curTime df
                                  ]

mkSubFeedInfo' t (Just p@(Posts {..})) (Just (UrlToScan {..}))
    | deletedPosts p = err "Deleted posts"
    | utsNextScanTime == UrTime 0 0 = err "No more scanning"
    | Just e <- utsErrorStartTime
    , diffUrTime t e > week = err "A week of errors"
      -- TODO: надо учитывать redirect-ы
    | title == "" = err "No title"
    | any badTitle $ maybeToList link ++ [title, utsUrl] = err "Bad title"
    | Set.size ch == 0 = err "Empty feed"
    | Set.size ch == 1 = err "Single post feed"
    | otherwise = do
        sizes <- fmap (map msgSize . catMaybes) $ readManyMsgs
            [MsgKey pBlogFeedUrl (Just $ mhGuid $ mtHeaders pMsgTree BA.! i)
                Nothing
            |TimeId _ i <- take postSizes $ Set.toDescList ch]
--        print (sizes, median (map toEnum sizes), toEnum (sum sizes) / toEnum (length sizes))
        return $ Right $
            SubFeedInfo
            { sfiTitle = title
            , sfiLink  = link
            , sfiImageLink = msgAuthorPic pRootMessage
            , sfiLastRefreshTime = lastRefreshTime
            , sfiPostsPerDay = postsPerDay
            , sfiAveragePostLength = median $ map toEnum sizes
            }
    where err = return . Left
          postSizes = min (Set.size ch) (max nLastPosts 20)
          (nLastPosts, postsPerDay) =
              ppd 1 lastRefreshTime $ tail $ Set.toDescList ch
          ch = fromMaybe Set.empty $ IntMap.lookup (-1) $ mtChildren pMsgTree
          title = fromMaybe "" $ postsTitle p
          link = msgLink pRootMessage
          -- где-то с msgSize ~ 100 начинают появляться нормальные блоги
          msgSize m = max 0 $
              length i * imgSize + length v * vidSize +
--              T.length t
              HS.size (HS.fromList $ filter (\ x -> T.length x > 2 && T.length x < 30) $ map (T.filter isAlpha . T.toLower) $ T.words $ T.toLower t)
              - length l
              -- ссылки вычитаем, учитывая фильтрацию длинных слов, сумма
              -- может стать отрицательной
              where (l,i,v,t) =
                        xmlLinksImgsVideosAndText $ parseTagsT $ tbs $ msgText m
                    imgSize = 3
                    vidSize = 5
                    attSize a = case a of
                        AImage {} -> imgSize
                        AVideo {} -> vidSize
                        AAudio {} -> vidSize
                        AIframe {} -> vidSize
                        AOther {} -> imgSize
                        AVideo2 {} -> vidSize
                        AGrOrigin {} -> 0
                        AThumbnail {} -> 0
          lastRefreshTime =
              case Set.maxView ch of
                  Just (TimeId t _,_) -> t
                  Nothing -> error "no posts??"
          ppd n t []
              | diffUrTime lastRefreshTime t < 1 || n <= 1 = (n,1)
                --  ^ все посты одновременно, непонятно, сколько
              | otherwise =
                  (n,day / (diffUrTime lastRefreshTime t / (toEnum n-1)))
          ppd !n t (TimeId pt _:ps)
              | diffUrTime lastRefreshTime pt > week && n >= 9 = ppd (n+1) pt []
              | otherwise = ppd (n+1) pt ps
mkSubFeedInfo' _ _ Nothing = return $ Left "No UrlToScan"
mkSubFeedInfo' _ Nothing _ = return $ Left "No Posts"

paidUsersCountryStats = do
    putStr "paidUsersCountryStats… "
    pus <- readUserCountryPaidList
    let countryStats =
            HM.fromListWith (\(pa,ta) (pb,tb) -> (pa+pb, ta+tb))
            [(c, (if p then 1 else 0, 1)) | (_,c,p) <- pus]
        countryStatsNorm =
            HM.adjust (* 0.5) "RU" $
            HM.adjust (* 0.5) "UA" $
            HM.adjust (* 0.5) "BY" $
            HM.map (\(p,t) -> fromIntegral p / fromIntegral t) countryStats
    HM.size countryStatsNorm `seq` do
        putStrLn "done"
        return countryStatsNorm
cachedRead fn a = do
    ex <- doesFileExist fn
    if ex then do
        putStr $ fn ++ " (cached)… "
        r <- fmap decode $ BL.readFile fn
        HM.size r `seq` do
            putStrLn "done"
            return r
    else do
        r <- a
        BL.writeFile fn $ encode r
        return r

prepareIndexSubscriptionsData = do
    hSetBuffering stdout LineBuffering
    saveStatsUsers
    saveSubStatsImm
    saveSubFeedInfos

indexSubscriptions = do
    hSetBuffering stdout LineBuffering
    countryStatsNorm <- cachedRead "paidUsersCountryStats" paidUsersCountryStats
    ssis <- cachedRead "normalizeSubsTags" normalizeSubsTags
    sfis <- readSubFeedInfos
    let errStats = HM.fromListWith (+) [(e,1) | Left e <- HM.elems sfis]
        okSfis = HM.fromList [(u,sfi) | (u,Right sfi) <- HM.toList sfis]
        toRemove = sort [f | (f, Left _) <- HM.toList sfis]
        feeds = map discoveryFeed $ HM.toList $
                HM.intersectionWith (,) ssis okSfis
        top = sortBy (comparing $ Down . dfNormalizedSubscribers) feeds
        subscribers = sum . HM.elems . ssiTotalCountries
        fixCo (c, fromIntegral -> n) = case c of
             "RU" -> n*0.5
             "UA" -> n*0.5
             "BY" -> n*0.5
             _    -> n
        normalizedSubscribers ssi =
            max (sum $ map fixCo $ HM.toList $ ssiPaidCountries ssi)
                -- все-таки не уверен, что надо напрямую учитывать платников
                -- могут сильно вылезти специфичные фиды
                -- (blog.bazqux.com, mrreader)
                (sum [ HM.lookupDefault 1 co countryStatsNorm * fromIntegral n
                     | (co,n) <- HM.toList $ ssiTotalCountries ssi ])
        -- число постов в день (либо за последнюю неделю, либо за
        translateTags (HM.fromList -> tags) =
            HM.fromListWith (HM.unionWith (+))
            [ (co, HM.singleton trt n)
            | (t, n) <- HM.toList tags
            , (co, trt) <- translateTag t
            , not (trt `HM.member` tags)
            ]
        discoveryFeed (u, (ssi, sfi)) =
            let topTags = take 10 $ sortTags $ ssiTags ssi
                category =
                    case topTags of
                        ((t,n):_) -> t
                        _ -> ""
            in
            (mergeSubFeedInfoToDiscoveryFeed sfi $ defaultDiscoveryFeed u)
            { dfCategory              = category
            , dfTranslatedCategory    =
                HM.fromList $ translateTag category
            , dfTags                  = map fst topTags
            , dfTranslatedTags        =
                HM.map (map fst . take 10 . sortTags) $
                translateTags topTags
            , dfSubscribers           = subscribers ssi
            , dfNormalizedSubscribers = normalizedSubscribers ssi
            , dfPaidCountries   = HM.filter (>= 10) (ssiPaidCountries ssi)
            , dfCountries       = HM.filter (>= 10) (ssiTotalCountries ssi)
            }

--    print errStats
    print (HM.size sfis, HM.size okSfis)
--     forM_ (take 10 top) $ \ t ->
--         when (discoveryFeedFromJSON (discoveryFeedToJSON t) /= Just t) $
--             print "ouch"
--    mapM_ print $ filter ((== "http://www.osyan.net/atom.xml") . dfUrl) top
    forM_ (groupByN 10000 top) $ \ dfs ->
        withLogger $ \ l -> esIndexDiscoveryFeeds l dfs

    print ("toRemove", length toRemove)
    forM_ (groupByN 10000 toRemove) $ \ tr ->
        -- Мы удаляем фид из поиска при ошибках скачивания, но может и
        -- заголовок почиститься (или изменятся условия индексирования).
        -- По-этому, удаляем и здесь.
        withLogger $ \ l -> esDeleteDiscoveryFeeds l tr

esIndexDiscoveryFeeds l dfs = do
    esBulkRequest "Discovered" l $ concatMap req dfs
    withEsUrl2 $ \ u ->
        esBulkRequest' u "Discovered2" l $ concatMap req dfs
    where req df =
              [ esBulkAction "" "feed" "feed" "index" (dfUrl df)
              , discoveryFeedToJSON df
              ]

esUpdateDiscoveryFeed l url lastRefreshTime = do
    esBulkRequest "Discovery updated" l req
    withEsUrl2 $ \ u ->
        esBulkRequest' u "Discovery updated2" l req
    where req =
              [ esBulkAction "" "feed" "feed" "update" url
              , JSON.Object $ HM.singleton "doc" $ JSON.Object $
                HM.singleton "updated" $ JSON.Number $ fromIntegral $
                urtSeconds lastRefreshTime
              ]

esDiscoveryFeedExists url =
    liftM2 (||)
        (esDiscoveryFeedExists' esUrl url)
        (maybe (return False) (\ u -> esDiscoveryFeedExists' u url) esUrl2)
esDiscoveryFeedExists' prefix url = do
    dr <- postSearchDownloader
          (prefix ++ "/feed/feed/_search") $
          BL.toStrict $ JSON.encode $ JSON.Object $ HM.fromList
          [("query", esField "_id" url)
          ,esTimeout
          ]
    case decodeSearchResults (\ id hit -> return id) dr of
        Right (srTotal, srTook, _, ids) ->
            return $ ids /= []
        Left e ->
            logT (seErrorMessage e) >> return True

esDeleteDiscoveryFeed l u = esDeleteDiscoveryFeeds l [u]

esDeleteDiscoveryFeeds l urls = do
    esBulkRequest "Deleted" l a
    withEsUrl2 $ \ u -> esBulkRequest' u "Deleted2" l a
    where a = map (esBulkAction "" "feed" "feed" "delete") urls

faviconUrl readerHostName hostName =
    proxyUrl PTFavicon readerHostName $ "/feedicon?u=" <>
        encodeURIComponentT hostName

searchSubscriptions readerHostName user country query = withLogger $ \ lt ->
    logTime lt (T.concat ["Search for ", country, " ", maskUrlPasswordT query]) $ do
    let postBody =
            BL.toStrict $ JSON.encode $ obj'
            [("query",
--               obj "custom_score" $ obj
--               [( "script"
--                , JSON.String "_score * doc[\"normalized_subscribers\"].value")
--               ,( "query",
                 bool 0 $
                 [("must",
                   [constantScore 1.0 $
                    bool 0 [("must", [field "content" (T.unwords [q, t])] ++
                                     if t /= "" then [field "tags" t] else [])]]
                  )
                 ,("should",
                   (if country /= "-" then
                        [constantScore 1.0 $ termQuery ("pco", country)
                        ,constantScore 1.0 $ termQuery ("co", country)
                        ]
                    else
                        [])
                   -- дополнительный boost подпискам из заданной страны
                   -- например, достаточно 152 для выдачи "RU","#news"
                   -- и 350 для выдачи "US", "#news" и "US", "#tech"
                   ++
                   [constantScore 1000.0 (field "category" tag)
                   | tag <- tags])]
               )
            ,esTimeout
            ,("size", JSON.Number 350)
            ,("sort", JSON.Array $ V.fromList $
               (if country /= "-" then (JSON.String "_score" :) else id)
               [obj "nsubs" (JSON.String "desc")]
            )
            ]
        field f q =
            bool 1
            [ ("should",
               morphAndNoMorph $ [(f, q)] ++
                   case T.unpack country of
                       [a,b] | isAlpha a && isAlphaNum b ->
                           [ (T.concat ["tr_", f, ".", country], q) ]
                       _ -> [] ) ]
        bool minimumShouldMatch fields =
            obj "bool" $ obj' $
            [ ( "disable_coord", JSON.Bool True)
            , ( "minimum_should_match"
              , JSON.Number (fromInteger minimumShouldMatch)) ]
            ++
            [ ( f, JSON.Array $ V.fromList qs ) | (f,qs) <- fields ]
        morphAndNoMorph l =
            map (matchQuery False) l ++
            map (matchQuery True) l
        constantScore boost query =
            obj "constant_score" $ obj'
            [ ("boost", JSON.Number (fromRational boost))
            , ("query", query) ]
        matchQuery noMorphology (field, q) =
            obj "match" $ obj field $ obj' $
               --  ^ старый ES требует text, а не match
               [ ("query", JSON.String q)
               , ("operator", JSON.String "AND") ]
               ++
               if noMorphology then
                   [( "analyzer", JSON.String "no_morphology_analyzer" )]
               else
                  []
        termQuery (field, q) =
            obj "term" $ obj field (JSON.String q)
        (tags', qwords) = partition ("#" `T.isPrefixOf`) $ T.words query
        tags = filter (/= "") $ map (T.dropWhile (== '#')) tags'
        q = T.unwords qwords
        t = T.unwords tags

--    B.putStrLn postBody
    dr <- postSearchDownloader (esUrl ++ "/feed/feed/_search") postBody

    let ~(Right (srTotal, srTook, _, dfs)) =
            decodeSearchResults
            (\ id hit -> do
                 source <- HM.lookup "_source" hit
                 JSON.String url <- HM.lookup "_id" hit
--                 JSON.Number (AP.D score) <- HM.lookup "_score" hit
                 df <- discoveryFeedFromJSON url source
                 let ns = dfNormalizedSubscribers df
                 return ( 1.0 -- if ns > 0 then score / ns else 0.0
                        , df))
            dr

--     logLS lt $ "Result size: " ++ show (B.length r)
--     logTime lt "decode only" $
--         case JSON.decode (BL.fromChunks [r," "]) of
--             Just (JSON.Object _) -> return ()
--             _ -> fail "can’t decode?"
--     logTime lt "dfs " $ length dfs `seq` return ()
--     B.putStrLn r
--     mapM_ print dfs

    curTime <- getUrTime

    let maxFst [] = 1.0
        maxFst x  = maximum $ map fst x
        sameTag a b = l > 0 && T.take l a == T.take l b
            where l = min (T.length a) (T.length b)
        groupTags 0 dfs = groupCountries 0 dfs
        groupTags n dfs =
            groupCountries (fromIntegral n * 3) t ++ groupTags (n-1) f
            where (t,f) = partition topN dfs
                  topN df = length (topSameTags df) == n
                  topSameTags (_,df) =
                      [ tag | tag <- tags
                      , any (sameTag tag) $
                        take n (dfTags df) ++
                        maybeToList (HM.lookup country $
                                     dfTranslatedCategory df)
                      ]
        sorted =
--            groupTags (length tags) dfs
            sortBy (comparing $ Down . fst)
            [ (sc - lastRefreshPenalty curTime df, (("sc",sc):d, df))
            | (sc, x@(d,df)) <- groupTags (length tags) dfs ]
--            group [] [] [] ndfs
--         ndfs = [(s / maxFst dfs, s, df) | (s, df) <- dfs]
        -- разбиваем группы по платным подписчикам с заданной страны,
        -- затем по бесплатным, затем по всем остальным
        -- сортируем и склеиваем.
        groupCountries b = group b [] [] []
        group b p c o [] =
            sortGroup (b+2) p ++ sortGroup (b+1) c ++ sortGroup b o
        group b p c o (x@(sc,df):xs)
            | Just su <- HM.lookup country $ dfPaidCountries df
            , country /= "-" =
                group b ((fromIntegral su, x):p) c o xs
            | Just su <- HM.lookup country $ dfCountries df
            , country /= "-" =
                group b p ((fromIntegral su, x):c) o xs
            | otherwise =
                group b p c ((dfNormalizedSubscribers df, x):o) xs
        sortGroup base g = -- sortBy (comparing $ Down . fst)
            [ (logBase (maxFst g) su -- su / maxFst g
               -- лучше логарифмически, 200 и 300 подписчиков
               -- и 100 и 200 -- не одно и то же,
               -- и так объем текста и остальные пенальти больше влияют
               * sc * penalty df + base,
               ([("p", penalty df)
                ,("hfp", highFreqPenalty df)
                ,("plp", postLengthPenalty df)
                ,("lb", logBase (maxFst g) su)
                ,("su",su)
                ,("sc", sc)]
               ,df))
            | (su, (sc, df)) <- reverse g ]
        penalty df =
            highFreqPenalty df * specificPenalty df * postLengthPenalty df
        dfToHtml (_,(_,df@(DiscoveryFeed {..}))) =
            T.concat $
            [ "<li class='discoveryFeed' data-url='", t dfUrl
            , "' data-link='", t $ fromMaybe "" dfWebsite
            , "' onclick='bq.dfClick(event,this)'>"
            , "<div class='dfTitle' title='", title, "'>"
            , "<span class='favicon' style='"
            , "background-image: url(\"", faviconUrl readerHostName h, "\")", "'></span>"
            , "<span class=buttonText dir=auto>", title, "</span></div>"
--             , "<div class=dfSubscribers title=''>", t $ displaySubscribersCount dfSubscribers, " readers</div>"
--             , "<div class=dfSubscribers title=''>", ts dfNormalizedSubscribers, "/", ts dfPostsPerDay, "/", ts dfAveragePostLength, "</div>"
--             , "<div class=dfSubscribers title=''>", ts $ postLengthPenalty df, "/", ts $ highFreqPenalty df, "/", ts $ lastRefreshPenalty curTime df, "</div>"
            ]
            ++ (if tags /= "" then
                    [ "<div class=dfTags dir=auto>"--  title='", tags, "'>"
                    , tags, "</div>" ]
                else
                    [])
            ++
            [ "<div class=dfWebsite dir=auto>"--  title='", t link, "'>"
            , website, "</div>"
            , "</li>"
            ]
            where title = t dfTitle
                  tags = t $ T.intercalate ", " $ take 3 dfTags
                  link = fromMaybe dfUrl dfWebsite
                  sp p x = fromMaybe x $ T.stripPrefix p x
                  ss p x = fromMaybe x $ T.stripSuffix p x
                  website = t $ ss "/" $ sp "www." $ rmHttpPrefix $ humanReadableURL $ rmIgnoredUrlQuery link
                  t = escapeHtmlT
                  ts x = t $ T.pack $ printf "%.3f" (x :: Double)
                  h = hostNameFromUrlT link
        top = take 50 sorted
        html = map dfToHtml top
--     logLS lt $ "Search for " ++ show (country,query) ++ ": " ++ show srTotal ++ " (" ++ show srTook ++ "ms)"
--     T.writeFile "discovery.html" $ TL.toStrict $ TL.concat $
--         ["<?xml version=\"1.0\" encoding=\"utf-8\" ?><head><link href=\"../css/basic.css\" media=\"all\" rel=\"stylesheet\" type=\"text/css\" /><meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\" /></head><body><script>function setDiscoveryFeed(x) { alert(x); }</script><div class=left><div class=subscriptions>"]
--         ++
--         html
--         ++
--         ["</div></div></body>"]
--     forM_ (take 50 sorted) $ \ (sc, (sc0, df)) -> do
--         T.putStrLn ""
--         T.putStrLn $ dfTitle df
--         T.putStrLn $ fromMaybe "" $ dfWebsite df
--         T.putStrLn $ dfUrl df
--         T.putStrLn $ T.intercalate ", " $ dfTags df
--         print (sc, sc0)
-- --        print $ dfTags df
--         putStrLn $
--             maybe "-" show (HM.lookup country $ dfPaidCountries df) ++ "/" ++
--             maybe "-" show (HM.lookup country $ dfCountries df) ++ "\t" ++
--             show (dfNormalizedSubscribers df) ++ "/" ++
--             show (dfSubscribers df) ++ "\t" ++
--             show (penalty df) ++ "\t" ++
--             show (lastRefreshPenalty curTime df) ++ "\t" ++ show (dfLastRefreshTime df)
    let h = T.concat html
--     logTime lt "sort" $ length top `seq` return ()
--     logTime lt "html" $ T.length h `seq` return ()
    u <- cachedReadUser' user
    h `seq` return ( if null sorted then Nothing else Just
                     ( h
                     , [ (url, m) | (_,(_, dfUrl -> url)) <- top
                       , let m = lookupDiscoveryMsgTreeViewMode u url
                       , m /= defaultDiscoveryMTVM
                       ]
                     ))

highFreqPenalty df =
    (3 - (max 0 $ min 2 $ logBase 6 (dfPostsPerDay df))) / 3
    -- пенальти за слишком частое обновление
    -- если logBase 6, то уже на 36 постах будет множитель 0.33
postLengthPenalty df =
    (max 1 $ min 3 $ logBase 10 (dfAveragePostLength df)) / 3
    -- может коротким 0.1, а большим 2 (чтобы даже мог из другой категории
    -- вверх перейти?)
lastRefreshPenalty curTime df =
--    max 0 (logBase 7 (diffUrTime curTime (dfLastRefreshTime df) / day))
    -- меньше дня все равно
    -- два дня 0.35
    -- неделя -- на категорию ниже (бесплатники)
    -- 7 недель -- еще на категорию ниже (остальные)
    -- 49 недель (почти год) -- ниже всех
    -- а надо ли так резко?
    -- --> нет, слишком резко
    let days = diffUrTime curTime (dfLastRefreshTime df) / day
    in
        min 10 $ max 0 $ (days - 7) / (6*30)
    -- полтора месяца было резко, хорошие блоги, которые несколько
    -- месяцев не обновлялись уходили слишком далеко,
    -- а полгода более-менее ничего
    -- также в течении последней недели лучше ничего не двигать

specificPenalty df
    | dfUrl df `HS.member` specificFeeds = 0.5
    | otherwise = 1
    where specificFeeds = HS.fromList
              [ "http://blog.bazqux.com/feeds/posts/default"
              , "http://mrreaderblog.curioustimes.de/rss"
              , "http://blog.chebinliu.com/feeds/posts/default"
              , "http://blog.feedly.com/feed/"
              ]

nothingIfEmpty "" = Nothing
nothingIfEmpty a = Just a
postsTitle = nothingIfEmpty . xmlText . msgSubject . pRootMessage

lookupDiscoveryMsgTreeViewMode u url =
    maybe defaultDiscoveryMTVM snd
          (HM.lookup url $ uvmSubViewModes $ uViewMode u)

defaultDiscoveryMTVM = defaultMsgTreeViewMode { mtvmUnreadOnly = False }

displaySubscribersCount :: Int -> T.Text
displaySubscribersCount c
--     | c < 10 = "10"
--     | c < 950 = T.pack $ show $ c `div` 10 * 10 + 10
    | c < 950 = T.pack $ show c
    | otherwise = T.replace ".0K" "K" $ T.pack $ printf "%.1gK" (fromIntegral c / 1000 :: Double)

discoveryFeedFromJSON dfUrl o = do
    JSON.Object f <- return o
    let str x = str' =<< HM.lookup x f
        str' x = do
          JSON.String r <- return x
          return r
        int x = int' =<< HM.lookup x f
        int' x = do
          JSON.Number (truncate -> r) <- return x
          return r
        double x = do
          JSON.Number (fromRational . toRational -> r) <- HM.lookup x f
          return (r :: Double)
        mbStr x = fmap Just (str x) <|> Just Nothing
        arr x = arr' str' =<< HM.lookup x f
        iarr x = arr' int' =<< HM.lookup x f
        arr' f x = do
          JSON.Array (V.toList -> r) <- return x
          mapM f r
--    dfUrl <- str "url"
    dfTitle <- str "title"
    dfCategory <- str "category" <|> Just ""
    dfTags <- arr "tags" <|> Just []
    dfTranslatedTags <- case HM.lookup "tr_tags" f of
        Just (JSON.Object (HM.toList -> trts)) ->
            fmap HM.fromList $ forM trts $ \ (n,x) -> do
                ts <- arr' str' x
                return (n, take (length ts - length dfTags) ts)
        _ -> return HM.empty
    dfTranslatedCategory <- case HM.lookup "tr_category" f of
        Just (JSON.Object (HM.toList -> trc)) ->
            fmap HM.fromList $ forM trc $ \ (n,x) -> do
                c <- str' x
                return (n, c)
        _ -> return HM.empty
    dfWebsite <- mbStr "website"
    dfImage <- mbStr "image"
    lrt <- int "updated"
    let dfLastRefreshTime = UrTime lrt 0
    dfNormalizedSubscribers <- double "nsubs"
    dfPostsPerDay <- double "rate"
    dfAveragePostLength <- double "post_len" <|> return 0.0
    dfSubscribers <- int "subs"
    pc <- arr "pco" <|> Just []
    pcn <- iarr "pcn" <|> Just []
    c <- arr "co" <|> Just []
    cn <- iarr "cn" <|> Just []
    let dfPaidCountries = HM.fromList $ zip pc pcn
        dfCountries = HM.fromList $ zip c cn
    return $ DiscoveryFeed {..}

discoveryFeedToJSON (DiscoveryFeed {..}) =
    JSON.Object $ HM.fromList $
    [ -- ("url", t dfUrl)
      ("title", t dfTitle)
    , ("content", t content)
    , ("subs", i dfSubscribers)
    , ("nsubs", d dfNormalizedSubscribers)
    , ("rate", d dfPostsPerDay)
    , ("updated", i $ urtSeconds dfLastRefreshTime)
    , ("post_len", d dfAveragePostLength)
    ]
    ++ mbs "website" dfWebsite
--    ++ mbs "image" dfImage
    ++ incWhen (dfCategory /= "") ("category", t dfCategory)
    ++ incWhen (dfTags /= [])     ("tags", arr $ map t dfTags)
    ++ tr_obj "tr_content"
       [ (c, t $ T.unwords $ content : tags)
       | (c, tags) <- HM.toList dfTranslatedTags ]
    ++ tr_obj "tr_tags"
       [ (c, arr $ map t $ tags ++ dfTags)
       | (c, tags) <- HM.toList dfTranslatedTags ]
    ++ tr_obj "tr_category"
       [ (c, t cat)
       | (c, cat) <- HM.toList dfTranslatedCategory ]
    ++ splitMap "pc" dfPaidCountries
    ++ splitMap "c" dfCountries
    where urls = dfUrl : maybeToList dfWebsite
          content =
              T.unwords $ dfTitle : dfTags ++ urls
                  ++ map humanReadableURL urls
                  ++ concatMap domainParts urls
          -- может быть foo.bar.baz.com, хорошо бы уметь искать по всем
          -- частям домена, а elasticsearch будет считать это одним словом
          domainParts = fromMaybe [] .
              fmap (\ x -> parts x ++ parts (humanReadableHostName x)) .
              maybeHostNameFromUrl . T.unpack
          parts x = x : if tl /= "" then parts (T.drop 1 tl) else []
              where (hd,tl) = T.break (== '.') x
          tr_obj name [] = []
          tr_obj name x  = [ (name, JSON.Object $ HM.fromList x) ]
          i = JSON.Number . fromIntegral
          d = JSON.Number . Scientific.fromFloatDigits
              -- очень важно именно fromFloatDigits, а не fromRational,
              -- т.к. значения типа 1/3 в scientific не представимы
          t = JSON.String
          arr = JSON.Array . V.fromList
          incWhen True x = [x]
          incWhen False x = []
          mbs n Nothing = []
          mbs n (Just v) = [ (n, t v) ]
          sortCountries = sortTags
          splitMap n (HM.size -> 0) = []
          splitMap n (sortCountries -> m) =
              [ (T.append n "o", arr $ map (t . fst) m)
              , (T.append n "n", arr $ map (i . snd) m) ]
          time = JSON.String . T.pack . showUrTime . rmUSec
          rmUSec (UrTime s _) = UrTime s 0

translateTag t = maybe [] HM.toList $ HM.lookup t translatedTags

translatedTags =
    HM.fromListWith HM.union $
    -- стоит переводить только те теги, которые будут в каталоге
    -- возможно даже как-то учитывать страну
    -- (чтобы с русскими тегами "фото" не показывать испанцам)
    country "RU"
    [ t "новости" "news"
    , t "технологии" "tech"
    , t "комиксы" "comics"
    , t "фото" "photo"
    , t "музыка" "music"
    , t "спорт" "sports"
    , t "игры" "games"
    , t "наука" "science"
    , t "политика" "politics"
    , t "финансы" "finance"
    , t "еда" "food"
    , t "кулинария" "cooking"
    , t "юмор" "humor"
    , t "дизайн" "design"
    , t "подкасты" "podcasts"
    , t "подкаст" "podcast"
    , t "программирование" "programming"
    , t "бизнес" "business"
    , t "путешествия" "travel"
    , t "андроид" "android"
    , t "линукс" "linux"
    , t "книги" "books"
    , t "развлечения" "entertainment"
    , t "здоровье" "health"
    , t "безопасность" "security"
    , t "экономика" "economics"
    , t "искусство" "art"
    , t "мода" "fasion"
    , t "фитнес" "fitness"
    , t "красота" "beauty"
    , t "продуктивность" "productivity"
    , t "история" "history"
    , t "философия" "philosophy"
    , t "образование" "education"
    , t "разработка" "dev"
    ]
    ++ country "UA"
    [ t "новини" "news"
    , t "технології" "tech"
    , t "комікси" "comics"
    , t "фото" "photo"
    , t "музика" "music"
    , t "спорт" "sports"
    , t "гри" "games"
    , t "наука" "science"
    , t "політика" "politics"
    , t "фінанси" "finance"
    , t "їжа" "food"
    , t "кулінарія" "cooking"
    , t "гумор" "humor"
    , t "дизайн" "design"
    , t "подкасти" "podcasts"
    , t "подкаст" "podcast"
    , t "програмування" "programming"
    , t "бізнес" "business"
    , t "подорожі" "travel"
    , t "андроїд" "android"
    , t "лінукс" "linux"
    , t "книги" "books"
    , t "розваги" "entertainment"
    , t "здоров'я" "health"
    , t "безпека" "security"
    , t "економіка" "economics"
    , t "мистецтво" "art"
    , t "мода" "fasion"
    , t "фітнес" "fitness"
    , t "краса" "beauty"
    , t "продуктивність" "productivity"
    , t "історія" "history"
    , t "філософія" "philosophy"
    , t "освіта" "education"
    ]
    ++ country "BY"
    [ t "навіны" "news"
    , t "тэхналогіі" "tech"
    , t "коміксы" "comics"
    , t "фота" "photo"
    , t "музыка" "music"
    , t "спорт" "sports"
    , t "гульні" "games"
    , t "навука" "science"
    , t "палітыка" "politics"
    , t "фінансы" "finance"
    , t "ежа" "food"
    , t "кулінарыя" "cooking"
    , t "гумар" "humor"
    , t "дызайн" "design"
    , t "падкасты" "podcasts"
    , t "падкаст" "podcast"
    , t "праграмаванне" "programming"
    , t "бізнэс" "business"
    , t "падарожжа" "travel"
    , t "андроіда" "android"
    , t "лінукс" "linux"
    , t "кнігі" "books"
    , t "забавы" "entertainment"
    , t "здароўе" "health"
    , t "бяспека" "security"
    , t "эканоміка" "economics"
    , t "мастацтва" "art"
    , t "мода" "fasion"
    , t "фітнес" "fitness"
    , t "прыгажосць" "beauty"
    , t "прадуктыўнасць" "productivity"
    , t "гісторыя" "history"
    , t "філасофія" "philosophy"
    , t "адукацыя" "education"
    ]
    ++ country "DE"
    [ t "nachrichten" "news"
    , t "technologie" "tech"
    , t "technik" "tech"
    , t "fotografie" "photo"
    , t "foto" "photo"
    , t "wissenschaft" "science"
    , t "musik" "music"
    , t "spiele" "games"
    , t "finanzen" "finance"
    , t "finanz" "finance"
    , t "Spaß" "fun"
    , t "wirtschaft" "economics"
    , t "politik" "politics"
    , t "kochen" "cooking"
    , t "filme" "films"
    , t "Programmierung" "programming"
    , t "Geschäft" "business"
    , t "Reise" "travel"
    , t "Unterhaltung" "entertainment"
    , t "Gesundheit" "health"
    , t "Sicherheit" "security"
    , t "Kunst" "art"
    , t "Mode" "fasion"
    , t "Schönheit" "beauty"
    , t "Produktivität" "productivity"
    , t "Geschichte" "history"
    , t "Philosophie" "philosophy"
    , t "Bildung" "education"
    -- , t "medien" "сми"

--     , t "sport" "sports" -- чисто чтобы статистику подправить
--     , t "funny" "fun"
--     , t "webcomics" "comics"
--     , t "financial" "finance"
--     , t "economic" "economics"
--     , t "football" "sports"
--     , t "gaming" "games"
--     , t "cooking" "food"
    ]
    ++ country "ES"
    [ t "noticias" "news"
    , t "foto" "photo"
    , t "música" "music"
    , t "deportes" "sports"
    , t "juegos" "games"
    , t "ciencia" "science"
    , t "política" "politics"
    , t "finanzas" "finance"
    , t "diversión" "fun"
    , t "comida" "food"
    , t "cocina" "cooking"
    , t "diseño" "design"
    , t "programación" "programming"
    , t "viajes" "travel"
    , t "androide" "android"
    , t "libros" "books"
    , t "entretenimiento" "entertainment"
    , t "salud" "health"
    , t "seguridad" "security"
    , t "economía" "economics"
    , t "arte" "art"
    , t "moda" "fasion"
    , t "gimnasio" "fitness"
    , t "belleza" "beauty"
    , t "productividad" "productivity"
    , t "historia" "history"
    , t "filosofía" "philosophy"
    , t "educación" "education"
    ] ++
    country "FR"
    [ t "nouvelles" "news"
    , t "actualité" "news"
    , t "actualités" "news"
    , t "technologie" "tech"
    , t "musique" "music"
    , t "politique " "politique"
    , t "nourriture" "food"
    , t "cuisine " "cooking"
    , t "humour " "humor"
    , t "programmation" "programming"
    , t "voyage" "travel"
    , t "livres" "books"
    , t "divertissement" "entertainment"
    , t "santé" "health"
    , t "sécurité" "security"
    , t "économie" "economics"
    , t "mode" "fasion"
    , t "beauté" "beauty"
    , t "productivité " "productivity"
    , t "histoire" "history"
    , t "philosophie" "philosophy"
    , t "éducation" "education"
    ]
    ++ country "NL"
    [ t "nieuws" "news"
    , t "technologie" "tech"
    , t "foto" "photo"
    , t "muziek" "music"
    , t "spel" "games"
    , t "wetenschap" "science"
    , t "politiek" "politics"
    , t "koken" "cooking"
    , t "ontwerp" "design"
    , t "programmeren" "programming"
    , t "reizen" "travel"
    , t "boek" "books"
    , t "gezondheid" "health"
    , t "veiligheid" "security"
    , t "kunst" "art"
    , t "mode" "fasion"
    , t "productiviteit" "productivity"
    , t "geschiedenis" "history"
    , t "filosofie" "philosophy"
    , t "onderwijs" "education"
    ]
    ++ country "SE"
    [ t "nyheter" "news"
    , t "teknik" "tech"
    , t "foto" "photo"
    , t "musik" "music"
    , t "spel" "games"
    , t "vetenskap" "science"
    , t "politiska" "politics"
    , t "politik" "politics"
    , t "mat" "food"
    , t "matlagning" "cooking"
    , t "programmering" "programming"
    , t "resa" "travel"
    , t "bok" "books"
    , t "böcker" "books"
    , t "underhållning" "entertainment"
    , t "hälsa" "health"
    , t "säkerhet" "security"
    , t "ekonomi" "economics"
    , t "konst" "art"
    , t "mode" "fasion"
    , t "skönhet" "beauty"
    , t "produktivitet" "productivity"
    , t "historia" "history"
    , t "filosofi" "philosophy"
    , t "utbildning" "education"
    ]
    ++ country "NO"
    [ t "nyheter" "news"
    , t "teknologi" "tech"
    , t "tegneserier" "comics"
    , t "bilde" "photo"
    , t "musikk" "music"
    , t "spill" "games"
    , t "vitenskap" "science"
    , t "politikk" "politics"
    , t "finans" "finance"
    , t "mat" "food"
    , t "podcaster" "podcasts"
    , t "programmering" "programming"
    , t "reise" "travel"
    , t "boken" "books"
    , t "bøker" "books"
    , t "underholdning" "entertainment"
    , t "helse" "health"
    , t "sikkerhet" "security"
    , t "økonomi" "economics"
    , t "kunst" "art"
    , t "mote" "fasion"
    , t "skjønnhet" "beauty"
    , t "produktivitet" "productivity"
    , t "historie" "history"
    , t "filosofi" "philosophy"
    , t "utdanning" "education"
    ]
    ++ country "IT"
    [ t "notizie" "news"
    , t "tecnologia" "tech"
    , t "fumetti" "comics"
    , t "foto" "photo"
    , t "gioco" "games"
    , t "giochi" "games"
    , t "scienza" "science"
    , t "politica" "politics"
    , t "finanza" "finance"
    , t "cibo" "food"
    , t "cucina" "cucina"
    , t "programmazione" "programming"
    , t "viaggi" "travel"
    , t "viaggio" "travel"
    , t "androide" "android"
    , t "libro" "books"
    , t "libri" "books"
    , t "intrattenimento" "entertainment"
    , t "salute" "health"
    , t "sicurezza" "security"
    , t "economia" "economics"
    , t "arte" "art"
    , t "moda" "fasion"
    , t "bellezza" "beauty"
    , t "produttività" "productivity"
    , t "storia" "history"
    , t "filosofia" "philosophy"
    , t "educazione" "education"
    ]
    ++ country "PT"
    [ t "notícia" "news"
    , t "tecnologia" "tech"
    , t "foto" "photo"
    , t "música" "music"
    , t "esporte" "sports"
    , t "jogos" "games"
    , t "ciência" "science"
    , t "política" "politics"
    , t "finanças" "finance"
    , t "comida" "food"
    , t "cozinhar" "cooking"
    , t "programação" "programming"
    , t "viajar" "travel"
    , t "livros" "books"
    , t "livro" "books"
    , t "entretenimento" "entertainment"
    , t "saúde" "health"
    , t "segurança" "security"
    , t "economia" "economics"
    , t "arte" "art"
    , t "beleza" "beauty"
    , t "produtividade" "productivity"
    , t "história" "history"
    , t "filosofia" "philosophy"
    , t "educação" "education"
    ]
    ++ country "JP"
    [ t "ニュース" "news"
    , t "技術" "tech" -- technology
    , t "ハイテク" "tech" -- high-tech
    , t "テク" "tech"
    , t "漫画" "comics" -- произносится manga
    , t "コミックス" "comics" -- комикуссу
    , t "写真" "photo"
    , t "フォト" "photo"
    , t "スポーツ" "sports"
    , t "ゲーム" "games"
    , t "科学" "science"
    , t "サイエンス" "science" -- saiensu
    , t "政治" "politics"
    , t "財政" "finance"
    , t "ファイナンス" "finance" -- fainansu
    , t "食べ物" "food"
    , t "フード" "food"
    , t "クッキング" "cooking"
    , t "料理" "cooking"
    , t "ユーモア" "humor"
    , t "デザイン" "design"
    , t "意匠" "design"
    , t "プログラミング" "programming"
    , t "ビジネス" "business"
    , t "企業" "business"
    , t "旅行" "travel"
    , t "トラベル" "travel"
    , t "アンドロイド" "android"
    , t "図書" "books"
    , t "エンターテイメント" "entertainment"
    , t "健康" "health"
    , t "ヘルス" "health"
    , t "警備" "security"
    , t "セキュリティ" "security"
    , t "経済" "economics"
    , t "経済学" "economics"
    , t "アート" "art"
    , t "芸術" "art"
    , t "美術" "art"
    , t "ファッション" "fashion"
    , t "流儀" "fashion"
    , t "フィットネス" "fitness"
    , t "美" "beauty"
    , t "生産性" "productivity"
    , t "履歴" "history"
    , t "哲学" "philosophy"
    , t "教育" "education"
    ]
    -- simplified Chinese в континентальном Китае и Сингапуре
    ++ country "CN"
    [ t "新闻" "news"
    , t "技术" "tech"
    , t "高科技" "tech" -- high-tech
    , t "漫画" "comics" -- manhua
    , t "照片" "photo"
    , t "音乐" "music"
    , t "运动" "sports"
    , t "游戏" "games"
    , t "科学" "science"
    , t "政策" "politics"
    , t "金融" "finance"
    , t "财经" "finance"
    , t "食物" "food"
    , t "烹饪" "cooking"
    , t "烹调" "cooking"
    , t "幽默" "humor"
    , t "设计" "design"
    , t "播客" "podcasts"
    , t "程序设计" "programming"
    , t "业务" "business"
    , t "旅行" "travel"
    , t "软件" "software"
    , t "书籍" "books"
    , t "书" "book"
    , t "娱乐" "entertainment"
    , t "健康" "health"
    , t "安全" "security"
    , t "经济" "economics"
    , t "经济学" "economics"
    , t "艺术" "art"
    , t "时尚" "fashion"
    , t "身体素质" "fitness"
    , t "美女" "beauty"
    , t "生产力" "productivity"
    , t "历史" "history"
    , t "哲学" "philosophy"
    , t "教育" "education"
    ]
    -- traditional Chinese в Гонконге и Тайване
    ++ country "HK"
    [ t "新聞" "news"
    , t "技術" "tech"
    , t "高科技" "tech" -- high-tech
    , t "漫畫" "comics"
    , t "照片" "photo"
    , t "音樂" "music"
    , t "運動" "sports"
    , t "遊戲" "games"
    , t "科學" "science"
    , t "政策" "politics"
    , t "金融" "finance"
    , t "財經" "finance"
    , t "食物" "food"
    , t "烹飪" "cooking"
    , t "烹調" "cooking"
    , t "幽默" "humor"
    , t "設計" "design"
    , t "播客" "podcasts"
    , t "程序設計" "programming"
    , t "業務" "business"
    , t "旅遊" "travel"
    , t "軟件" "software"
    , t "書" "book"
    , t "書籍" "books"
    , t "娛樂" "entertainment"
    , t "健康" "health"
    , t "安全" "security"
    , t "經濟" "economics"
    , t "經濟學" "economics"
    , t "藝術" "art"
    , t "時尚" "fashion"
    , t "身體素質" "fitness"
    , t "美女" "beauty"
    , t "生產率" "productivity"
    , t "歷史" "history"
    , t "哲學" "philosophy"
    , t "教育" "education"
    ]
    where t a b = (T.toLower $ T.strip a, T.strip b)
          {-# NOINLINE t #-}
          country c = map (\(n,t) -> (n, HM.singleton c t))
          {-# NOINLINE country #-}
          -- noinline чтобы не тормозила компиляция с включенным -O2
