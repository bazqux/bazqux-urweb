{-# LANGUAGE BangPatterns, RecordWildCards, ViewPatterns, TupleSections,
             OverloadedStrings, MonomorphismRestriction, ScopedTypeVariables,
             LambdaCase #-}
module Search
    ( mtChildrenSet
    , commentsCount
    , postCommentsCount, imTotalComments
    , readPostsAndFilteredPostsRead, readSetIntMap, commentsReadSets
    , feedId, feedUrl, folderId, folderName
    , feedIds, feedUrls, isActiveFeed, isActiveFeedId
    , grItemId
    , midGrItemId
    , splitGrItemId
    , midFromGrItemId, lmidFromGrItemId
    , postCommentsItemTags
    , feedItemTags
    , notNull
    , emptyUserForExternalFeedsAccess, grBfuPrefix, grBfu
    , getGRIds'
    , getGRIds
    , updateGRIds, addFeedUrlToGRIds
    , feedViewMode
    , folderViewMode, folderViewMode'
    , esSimpleIdsSearch, esSimpleIdsSearch'
    , getFeedMasks, getFeedMasksE
    , updateUserFeedMasks
    , fixFeedMask
    , msgKeyToJSON
    , getTagsFilterIds, getTagsFilterIdsE
    , checkQuerySyntax
    , addFilter
    , editFilter
    , deleteFilter
    , addSmartStream
    , editSmartStream
    , deleteSmartStream
    , removeSubscriptionsFromFilters
    , addSubscriptionsToFilters
    , findSmartStream, fidFeedMask, lookupSmartStreamFeedMasks
    , checkSmartStreamError, checkFilterError
    , saveSearchMsgs
    , saveSearchTagged
    , deleteSearchTagged
    , esUpdateTaggedItem
    , userDeleteTaggedItems
    , feedItemFromMsgAndText
    , reindexAllPosts, reindexAllTags, reindexUserTags, gifUrl
    , maxFiltersOrSmartStreams
    , fFilters, fSmartStreams, migrateOldFilters
    )
    where

import Control.Monad
import Control.Arrow
import Control.Applicative
import Control.Concurrent
import Data.List
import Data.Ord
import Data.Char
import qualified Control.Exception as E
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Short (ShortByteString)
import Generated.DataTypes
import Lib.UrTime
import qualified Lib.BArray as BA
import Lib.Log
import Lib.ReadUtils
import Lib.Regex
import Data.Maybe
import URL
import Resolvables
import Riak
import Generated.RiakIO
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntMap.Internal as IntMap (merge, dropMissing, zipWithMaybeMatched)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Lib.ReadSet (ReadSet)
import qualified Lib.ReadSet as ReadSet
import qualified Data.Aeson as JSON
import Lib.Json
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import qualified Data.Vector as V
import qualified Control.Concurrent.MSem as MSem
import Network.HTTP.Conduit.Downloader
import Text.HTML.TagSoup.Fast
import Parser.Custom (hashSetNub, hashSetNub', groupByN)
import Preprocess
import Parser.Types (FeedItem(..))
import Lib.ElasticSearch
import Lib.StringConversion
import System.IO.Unsafe
import Control.Concurrent.Async
import Config
import HotLinks
import UsageFlags

-- import qualified HaskellWorks.Data.Json.Standard.Cursor.Fast as HWJSON
-- import qualified HaskellWorks.Data.Json.PartialValue as HWJSON

-- parseHwJson bs = HWJSON.jsonPartialJsonValueAt c
--     where !ibip = HWJSON.simdToIbBp bs
--           !c    = HWJSON.fromBsIbBp bs ibip

-- падает

maxFiltersOrSmartStreams :: Key User -> Int
maxFiltersOrSmartStreams _ = 500

mtChildrenSet mt parentId =
    IntMap.findWithDefault Set.empty parentId (mtChildren mt)

-- с учетом версии
commentsCount totalComments ccs =
    maybe 0 snd $ IntMap.lookupLE totalComments ccs
    -- maybe 0 fst $ IntMap.maxView $ fst $ IntMap.split (totalComments+1) ccs

postCommentsCount pid totalComments posts =
    fromMaybe 0 $ do
        ccs <- IntMap.lookup pid (pCommentCounts posts)
        return (commentsCount totalComments ccs)

-- | ID папки, тега или smart stream
folderId f gri = HM.lookupDefault 0 f (griFolderIds gri)
folderName fid gri = fromMaybe "-" $ IntMap.lookup fid (griFolderNames gri)

feedId bfu gri = HM.lookupDefault 0 bfu (griFeedIds gri)
--    where err = error $ "feedId: not found " <> T.unpack bfu
feedUrl fid gri = fromMaybe "-" $ IntMap.lookup fid (griFeedUrls gri)

feedIds gri = map (`feedId` gri)
feedUrls gri = map (`feedUrl` gri)

grBfuPrefix = "https://bazqux.com/gri/"
grBfu = T.append grBfuPrefix

grItemId :: Int -> Int -> Maybe Int -> Int
grItemId feedId postId Nothing =
    feedId * 0x100000000 + postId
grItemId feedId postId (Just commentId) =
    2^63 + (feedId * 0x10000000000 + postId * 0x4000 + commentId)
-- 1  флаг коммента
-- 23=8M фид
-- 26=64M пост   100 постов за раз * 2*24 (раз в полчаса) хватит на 38 лет ))
-- 14=16k коммент
midGrItemId (MsgId {..}) = grItemId midFeedId midPostId midCommentId

splitGrItemId :: Int -> (Int, Int, Maybe Int)
splitGrItemId i
    | i > 0 = let (fid, pid) = quotRem i 0x100000000 in (fid, pid, Nothing)
    | otherwise = (fid, pid, Just cid)
    where im = i - 2^63
          (fid, im') = quotRem im 0x10000000000
          (pid, cid) = quotRem im' 0x4000

midFromGrItemId grid = MsgId {..}
    where (midFeedId, midPostId, midCommentId) = splitGrItemId grid

lmidFromGrItemId grid lmidMsgKey = LongMsgId {..}
    where lmidMsgId = midFromGrItemId grid

-- | Вырезаем из griItemTags комментарии к заданному посту
postCommentsItemTags fid pid gri =
    cutIntMap (grItemId fid pid (Just 0) - 1) (grItemId fid (pid+1) (Just 0))
    $ griItemTags gri

-- | границы не включаются
cutIntMap lo hi = fst . IntMap.split hi . snd . IntMap.split lo

-- | Вырезаем из griItemTags всё, что относится к заданному фиду
feedItemTags fid gri = IntMap.union posts comments
    where posts = cutIntMap
              (grItemId  fid    0 Nothing - 1)
              (grItemId (fid+1) 0 Nothing) $ griItemTags gri
          comments = cutIntMap
              (grItemId  fid    0 (Just 0) - 1)
              (grItemId (fid+1) 0 (Just 0)) $ griItemTags gri


-- | Пользователь, у которого grIds не обновляются,
-- т.е. тот фид, который раздается через bazqux.com/feed/http:…
-- будет всегда иметь ID=0
emptyUserForExternalFeedsAccess = "feed_user"

getGRIds' user s
    | user == emptyUserForExternalFeedsAccess = return $ defaultGRIds user
getGRIds' user s = do
    i <- cachedReadGRIds' user
    fs <- cachedReadFilters' user
    let ss = map ssName $ fSmartStreams fs
        i' = updateGRIds user ss s i
    if griLastId i' /= griLastId i then
        modifyGRIds' user $ \ i ->
            let i' = updateGRIds user ss s i in return (i',i')
    else
        return i
addFeedUrlToGRIds gri feed
    | HM.member feed (griFeedIds gri) = gri
    | otherwise =
        gri { griLastId = i + 1
            , griFeedIds = HM.insert feed i (griFeedIds gri)
            , griFeedUrls = IntMap.insert i feed (griFeedUrls gri)
            }
        where i = griLastId gri
updateGRIds user ss s i =
    let folders = nub $ concatMap sFolders s
        urls = grBfu user : map sUrl s
        (lastId', feedIds, feedUrls) =
            foldl' (\ (!i,!f,!fu) u ->
                        if HM.member u f then (i,f,fu) else
                            (i+1, HM.insert u i f, IntMap.insert i u fu))
                   (griLastId i, griFeedIds i, griFeedUrls i)
                   urls
        (lastId'', folderIds, folderNames) =
            foldl' insFolder
                   (lastId', griFolderIds i, griFolderNames i)
                   folders
        (lastId''', folderIds', folderNames') =
            foldl' insFolder
                   (lastId'', folderIds, folderNames) $
                   [t | ITTag t <- Map.keys (griTaggedItems i)]
                   ++ ss
        insFolder (!i,!f,!fn) n
            | Just idx <- HM.lookup n f =
                if IntMap.member idx fn then (i,f,fn)
                else (i,f,IntMap.insert idx n fn)
                     -- при переименовании папок не обновлялись griFolderNames
                     -- после переименования не работает drag'n'drop через API
                     -- и пометка папки прочитанной в fever
            | otherwise =
                (i+1, HM.insert n i f, IntMap.insert i n fn)

        activeFeeds = HS.fromList [u | SSFeed u <- map sState s]
    in
        i { griLastId = lastId''' +
                        (if Just activeFeeds /= griActiveFeeds i then 1 else 0)
          , griFeedIds = feedIds
          , griFeedUrls = feedUrls
          , griFolderIds = folderIds'
          , griFolderNames = folderNames'
          , griActiveFeeds = Just activeFeeds
          }
getGRIds user
    | user == emptyUserForExternalFeedsAccess = return $ defaultGRIds user
    | otherwise = do
        u <- cachedReadUser' user
        getGRIds' user (uSubscriptions u)

isActiveFeed bfu gri =
    maybe False (HS.member bfu) (griActiveFeeds gri)
isActiveFeedId fid gri =
    maybe False (HS.member (feedUrl fid gri)) (griActiveFeeds gri)

feedViewMode feed user =
    maybe defaultMsgTreeViewMode snd $
    HM.lookup feed $ uvmSubViewModes $ uViewMode user
folderViewMode = folderViewMode' defaultMsgTreeViewMode
folderViewMode' def folder user =
    (maybe def snd $
     HM.lookup folder $ uvmFolderViewModes $ uViewMode user)
--     { mtvmExpandedComments = True
--     , mtvmPosts = PVMFull
--     -- для папок запоминаем только unread/ascending
--     }

checkQuerySyntax query = do
    dr <- postSearchDownloader
        (esUrl ++ "/msg/msg/_search") $
        BL.toStrict $ JSON.encode $ JSON.Object $ HM.fromList
        [ ("query", esMust [esQueryString query, esTerms "blog_feed_url" []])
        , esTimeout
        , ("size", JSON.Number 0)
        ]
    case decodeSearchResults (\ _ _ -> Just ()) dr of
        Right _ -> return Nothing
        Left (SESyntaxError e) -> return $ Just e
        Left e@(SESystemError _) -> E.throwIO $ SearchException e

esSimpleIdsSearch = esSimpleIdsSearch' esUrl
--esSimpleIdsSearch2 = maybe (fail "") esSimpleIdsSearch' esPort2
esSimpleIdsSearch' eu idx typ size routing preference query = do
    dr <- postSearchDownloader
        ((eu ++ "/" ++ idx ++ "/" ++ typ ++ "/_search"
          ++ esMaybeRouting routing) `urlAddParam` esPreference preference) $
        BL.toStrict $ JSON.encode $ JSON.Object $ HM.fromList
        [ ("query", query)
        , esTimeout
        , ("size", JSON.Number $ fromIntegral (size :: Int))
        , ("fields", JSON.Array $ V.fromList [JSON.String "_routing"])
        ]
    let (total0,took0,sid,ids) =
            either err id $ flip decodeSearchResults dr $ \ id hit ->
            do JSON.Object f <- HM.lookup "fields" hit
               JSON.String r <- HM.lookup "_routing" f
               return (id, r)
            <|> -- в ES 2 routing и так работает
            do JSON.String r <- HM.lookup "_routing" hit
               return (id, r)
        err e = error $ "search " ++ typ ++ " request failed:\n" ++ T.unpack (seErrorMessage e)
    return ids

escapeNewLines = T.replace "\n" "\\n" . T.replace "\\" "\\\\"
unescapeNewLines t
    | T.any ('\\' ==) t = T.pack $ unesc $ T.unpack t
    | otherwise         = t
    where unesc [] = []
          unesc ('\\':'\\':xs) = '\\' : unesc xs
          unesc ('\\':'n':xs) = '\n' : unesc xs
          unesc (x:xs) = x : unesc xs

userMidToJSON user (LongMsgId (MsgKey u pg cg) (MsgId fid pid cid)) =
    T.unlines [user, u, mb pg, mb cg, i fid, i pid, mi cid]
    where mb = maybe "" (escapeNewLines . sbt)
          i = T.pack . show
          mi = maybe "" i
userMidFromJSON k = case T.lines k of
    [user, u,p,c, fid, pid, cid] ->
        (user, LongMsgId (MsgKey u (mb p) (mb c))
                  (MsgId (readUnsignedInt fid) (readUnsignedInt pid) (mi cid)))
    _ -> error $ "userMidFromJSON: " ++ show k
    where mb "" = Nothing
          mb xs = Just $ tsb $ unescapeNewLines xs
          mi "" = Nothing
          mi x  = tryReadUnsignedInt x
msgKeyToJSON (MsgKey u pg cg) = T.unlines [u, mb pg, mb cg]
    where mb = maybe "" (escapeNewLines . sbt)
msgKeyFromJSON k = case T.lines k of
    [u,p,c] -> MsgKey u (mb p) (mb c)
    _ -> error $ "msgKeyFromJSON: " ++ show k
    where mb "" = Nothing
          mb xs = Just $ tsb $ unescapeNewLines xs

escapeLucene = go
    where go [] = []
          go (x:xs)
             | x `elem` ("+-&|!(){}[]^\"~*?:\\" :: [Char]) = '\\' : x : go xs
             | otherwise = x : go xs

getTagsFilterIdsE l user query tags =
    throwSyntaxError $ getTagsFilterIds l user query tags

getTagsFilterIds l user query tags = do
    t0 <- getUrTime
    let url = urlAddParam
              (esUrl ++ "/msg/tagged/_search" ++ esRouting user)
              ("search_type=scan&scroll=1m&" ++ esPreference user)
        notAnalyzed = T.all isAlphaNum -- не определяет морфологию
        anyTag = [obj "exists" $ obj "field" $ JSON.String "user_tags"]
        filters =
            [ esField "user" user ] ++
            (if tags == Just [ITStarred] then
                 [esBoolField "starred" True] else []) ++
            (case tags of
                Just t
                    | ts <- [n | ITTag n <- t]
                    , notNull ts ->
--                         if all notAnalyzed ts then
--                             [esTerms "user_tags" ts]
--                             -- у user_tags стоит my_analyzer вместо not_analyzed
--                             -- приходится искать все теги
--                         else
                            anyTag
                Nothing ->
                    -- все теги, поле user_tags должно быть непустым []
                    -- (exists такж работает для непустых массивов)
--                     [esMustNot [obj "missing" $
--                                 obj "field" $ JSON.String "user_tags"]]
                    anyTag
                _ -> []
            )
        q = esFilter $ esMust $
              filters ++
              [obj "query" $ esQueryString query]

    gri <- getGRIds user

    let maxTime = maximum
            $ UrTime 0 0 : mapMaybe tiTime (Map.elems $ griTaggedItems gri)
        tiTime ((t,_,_):_) = Just t
        tiTime _ = Nothing

    tryFlush l t0 maxTime

--    BL.putStrLn $ JSON.encode q
    dr <- postSearchDownloader url $
        BL.toStrict $ JSON.encode $ JSON.Object $ HM.fromList
        [ ("query", q)
        , esTimeout
        , ("size", JSON.Number 10000) -- не сильно влияет, 1k или 10k
        ]

    let withDec = withDecodeSearchResults
            $ \ i _ -> return $ snd $ userMidFromJSON i
        idOk (lmidMsgId -> mid)
             | Just it <- IntMap.lookup (midGrItemId mid) (griItemTags gri) =
                 case tags of
                     Just ts -> notNull $ intersect ts it
                     Nothing -> True -- любой тег подходит
             | otherwise = False
        go !acc Nothing = do
            t1 <- getUrTime
            logLS l $ show (IntSet.size acc, showSecs (diffUrTime t1 t0))
            return $ Right acc
        go !acc (Just scroll) = do
            dr <- postSearchDownloader
                  (urlAddParam
                   (esUrl ++ "/_search/scroll"  ++ esRouting user)
                   ("scroll=1m&" ++ esPreference user))
                  (tbs scroll)
            withDec dr $ \ (total, took, sid, filter idOk -> ids) ->
                go (IntSet.union acc (IntSet.fromList $ map (midGrItemId . lmidMsgId) ids))
                   (if null ids then Nothing else sid)
    r <- withDec dr $ \ (total0,took0,sid,_) -> go IntSet.empty sid
    case r of
        Left e -> case e of
            SESystemError _ -> E.throwIO $ SearchException e
            SESyntaxError m -> return $ Left m
        Right i -> return $ Right i

fqTimeBased = regexTest hasTimeRegex . fqQuery

hasTimeRegex = "time[[:space:]]*:[[:space:]]*\\[[^]]*now.*\\]"

fqFeedsIM gri =
    IntMap.fromList . map (first (`feedId` gri)) . HM.toList . fqFeeds

filterQueryFromFeeds u gri feedTcs query negate =
    FilterQuery
    { fqQuery     = query
    , fqNegate    = negate
    , fqFeeds     = HM.fromList
        [ (f, mtvmExpandedComments $ feedViewMode f u)
        | (flip feedUrl gri -> f, _) <- feedTcs ]
    , fqReserved1 = 0
    , fqReserved2 = 0
    }

emptyFilterQuery =
    FilterQuery
    { fqQuery     = ""
    , fqNegate    = False
    , fqFeeds     = HM.empty
    , fqReserved1 = 0
    , fqReserved2 = 0
    }

testAddFQS = do
    let user = "1"
               -- "https://www.google.com/accounts/o8/id?id=AItOawkiWhJILhO89mzuUb9za1EN3wTbCEJsTK0"
    u <- cachedReadUser' user
    gri <- cachedReadGRIds' user
    let feeds = [feedId f gri | SSFeed f <- map sState (uSubscriptions u)]
    addFilter user "wetterkamera" True feeds
    addFilter user "soby" True feeds
    addFilter user "elon OR musk OR tesla" True feeds
    addFilter user "author:tema OR comment:false" False
        [feedId "http://tema.livejournal.com/data/rss" gri]
    addSmartStream user "db" "leveldb OR hbase OR cassandra" feeds
    addSmartStream user "countries" "германия OR швейцария" feeds

fFilters = fNewFilters . migrateOldFilters
fSmartStreams = fNewSmartStreams . migrateOldFilters

migrateOldFilters f
    | null (fOldFilters f) && null (fOldSmartStreams f) = f
    | otherwise =
        f { fVersion = fVersion f + length (fOldFilters f) + 1
          , fOldFilters = []
          , fOldSmartStreams = []
          , fNewFilters = zipWith mkFilter [fVersion f..] (fOldFilters f)
          , fNewSmartStreams = map s $ fOldSmartStreams f
          }
    where s o =
              (mkSmartStream (ossName o) (head $ ossQueries o))
              { ssFeedMasks = ossFeedMasks o }

mkFilter i q =
    Filter
    { filterId = i
    , filterQuery = q
    , filterFeedMasks = emptyFilterFeedMasks
    , filterReserved1 = 0
    , filterReserved2 = 0
    }
mkSmartStream name q =
    SmartStream
    { ssName = name
    , ssQuery = q
    , ssFeedMasks = emptyFilterFeedMasks
    , ssUnfilteredFeedMasks = emptyFilterFeedMasks
    , ssReserved1 = 0
    , ssReserved2 = 0
    }

addFilter user query negate feeds = do
    usageFlag user $ if negate then UFFilterHide else UFFilterApply
    u <- cachedReadUser' user
    gri <- cachedReadGRIds' user
    let fq = filterQueryFromFeeds u gri (map (,(0,0)) feeds) query negate
    changeFilters user $ \ f ->
        checkMaxFiltersOrSmartStreams $
        f { fNewFilters = addFilterQuery (fVersion f) fq $ fFilters f }

editFilter user fid query negate feeds = do
    usageFlag user UFEditFilter
    u <- cachedReadUser' user
    gri <- cachedReadGRIds' user
    let fq = filterQueryFromFeeds u gri (map (,(0,0)) feeds) query negate
    changeFilters user $ \ f ->
        f { fNewFilters = replaceFilterQuery fid fq $ fFilters f }

editSmartStream user name query feeds = do
    usageFlag user UFEditSmartStream
    u <- cachedReadUser' user
    gri <- cachedReadGRIds' user
    addSmartStream' gri user name
        $ filterQueryFromFeeds u gri (map (,(0,0)) feeds) query False

addSmartStream user name query feeds = do
    usageFlag user UFNewSmartStream
    u <- cachedReadUser' user
    gri <- cachedReadGRIds' user
    addSmartStream' gri user name
        $ filterQueryFromFeeds u gri (map (,(0,0)) feeds) query False

addSmartStream' gri user name fq = changeFilters user $ \ f ->
    checkMaxFiltersOrSmartStreams $
    f { fNewSmartStreams = e $ fSmartStreams f }
    where e [] = [mkSmartStream name fq]
          e (x:xs)
              | ssName x == name = x { ssQuery = fq } : xs
              | otherwise = x : e xs

deleteFilter user fid = do
    usageFlag user UFDeleteFilter
    gri <- cachedReadGRIds' user
    changeFilters user $ \ f ->
        f { fNewFilters = filter ((/= fid) . filterId) $ fFilters f }

deleteSmartStream user name = do
    usageFlag user UFDeleteSmartStream
    changeFilters user $ \ f ->
        f { fNewSmartStreams = filter ((/= name) . ssName) (fSmartStreams f) }

checkMaxFiltersOrSmartStreams f
    | length (fSmartStreams f) + length (fFilters f)
      > maxFiltersOrSmartStreams (fUser f) =
         error "checkMaxFiltersOrSmartStreams: too many filters or smart streams"
    | otherwise = f

removeSubscriptionsFromFilters user feeds = do
    gri <- cachedReadGRIds' user
    removeSubscriptionsFromFilters' gri user feeds
removeSubscriptionsFromFilters' gri user feeds =
    changeFilters' (filter $ not . (`HM.member` fm)) user $ \ f ->
        f { fNewSmartStreams = mapMaybe rmSS (fSmartStreams f)
          , fFeedMasks = rmffm (fFeedMasks f)
          , fNewFilters = mapMaybe rmF (fFilters f)
          }
    where (fm, ifm) = feedsHMIM gri feeds
          rmffm ffm =
              ffm { ffmFeedMasks = IntMap.difference (ffmFeedMasks ffm) ifm }
          rmSS ss
              | Just q <- rmQ $ ssQuery ss =
                  Just $ ss
                  { ssQuery = q
                  , ssFeedMasks = rmffm $ ssFeedMasks ss
                  , ssUnfilteredFeedMasks = rmffm $ ssUnfilteredFeedMasks ss
                  }
              | otherwise = Nothing
          rmF f
              | Just q <- rmQ $ filterQuery f =
                  Just $ f
                  { filterQuery = q
                  , filterFeedMasks = rmffm $ filterFeedMasks f
                  }
              | otherwise = Nothing
          rmQ fq
              | HM.size (fqFeeds fq') == 0 = Nothing
              | otherwise = Just fq'
              where fq' = fq { fqFeeds = HM.difference (fqFeeds fq) fm }

-- | Автоматически добавляем новые подписки к фильтрам/стримам,
-- работающим на более чем половине подписок.
addSubscriptionsToFilters user feedsList = do
    u <- cachedReadUser' user
    gri <- cachedReadGRIds' user
    let halfSize =
            HM.size
            (HM.fromList [(f,()) | SSFeed f <- map sState $ uSubscriptions u]
             `HM.difference` feeds)
            `div` 2
        feeds = fqFeeds $
            filterQueryFromFeeds u gri
                (map ((,(0,0)) . flip feedId gri) feedsList) "" False
        fixFQ q
            | HM.size (fqFeeds q) > halfSize =
                q { fqFeeds = fqFeeds q `HM.union` feeds }
            | otherwise = q
        fixSS s = s { ssQuery = fixFQ $ ssQuery s }
        fixF f = f { filterQuery = fixFQ $ filterQuery f }
    changeFilters' (<> feedsList) user $ \ f ->
        let fs' = map fixF $ fFilters f
            fset = IntSet.unions . map (fqFeedsSet gri . filterQuery)
            newFilteredFeeds = fset fs' `IntSet.difference` fset (fFilters f)
        in
        f { fNewFilters = fs'
          , fNewSmartStreams = map fixSS $ fSmartStreams f
          , fFeedMasks =
              (fFeedMasks f)
              { ffmFeedMasks =
                  IntMap.fromSet (const $ emptyFeedMask True) newFilteredFeeds
                  -- пока не произошло обновление у новых фидов будут пустые
                  -- маски, чтобы неотфильтрованные посты не были видны в API
                  `IntMap.union`
                  ffmFeedMasks (fFeedMasks f)
              }
          }

-- | Определяем различия между фильтрами и стримами после редактирования
-- и помечаем фиды, маски которых будет необходимо удалить и (возможно)
-- полностью пересчитать.
-- Т.е., определяем добавленные и убранные фиды, а также фиды
-- с измененным запросом.
markEditedFeedMasks gri activeFeeds f0 f =
    f
    { fNewSmartStreams = flip map (fSmartStreams f) $ \ s ->
        let efids =
                editedFids
                    (maybe HM.empty ssFilters $ HM.lookup (ssName s) prevSs)
                    (ssFilters s)
            effids =
                IntSet.intersection (fqFeedsSet gri $ ssQuery s)
                    editedFilterFids
        in
            s
            { ssFeedMasks = markEditedFids (IntSet.union efids effids)
                $ ssFeedMasks s
            , ssUnfilteredFeedMasks = markEditedFids efids
                $ ssUnfilteredFeedMasks s
            }
    , fFeedMasks = markEditedFids editedFilterFids (fFeedMasks f)
    , fNewFilters = flip map (fFilters f) $ \ f ->
        f
        { filterFeedMasks = markEditedFids
            (editedFids
                (maybe HM.empty filterFilters $ HM.lookup (filterId f) prevF)
                (filterFilters f))
            (filterFeedMasks f)
        }
    }
    where editedFids a b = fids $ concat
              [HM.keys $ HM.difference a b -- удаленные
              ,HM.keys $ HM.filter id $ HM.intersectionWith (/=) a b
               -- разные запросы
              ,HM.keys $ HM.difference b a -- добавленные
              ]
          fids = IntSet.intersection af . IntSet.fromList . feedIds gri
          af = IntMap.keysSet activeFeeds
          editedFilterFids = editedFids (feedFilters f0) (feedFilters f)
          prevSs = HM.fromList [(ssName s, s) | s <- fSmartStreams f0]
          prevF = HM.fromList [(filterId f, f) | f <- fFilters f0]
          feedFilters = filters . map filterQuery . fFilters
          ssFilters = filters . (:[]) . ssQuery
          filterFilters = filters . (:[]) . filterQuery
          -- | набор фильтров каждого фида для определения различий
          filters :: [FilterQuery] -> HashMap TURL (HashSet (Text, Bool, Bool))
          filters =
              foldl' (HM.unionWith HS.union) HM.empty . map
              (\ fq -> HM.map (qnSet fq) $ fqFeeds fq)
          qnSet fq e = HS.singleton (fqQuery fq, fqNegate fq, e)

markEditedFids fids fms
    | IntSet.null fids = fms
    | otherwise = fms { ffmLastUpdated = s $ ffmLastUpdated fms }
    where s = \ case
              FUTNever -> FUTNever
              FUTError t e -> FUTError (UrTime 0 0) e
              FUTUpdatedAt t -> FUTEdited t fids
              FUTEdited t f -> FUTEdited t (IntSet.union f fids)

feedsHMIM gri feeds =
    (HM.fromList $ map (,()) feeds
    ,IntMap.fromList $ map (,()) $ feedIds gri feeds)

changeFilters = changeFilters' id
changeFilters' editActiveFeeds u f = do
    us <- uSubscriptions <$> cachedReadUser' u
    -- возможна ситуация создания фильтра/стрима с несуществующим фидом
    -- (отписались от подписки, пока редактировали/смотрели на список фильтров)
    -- убираем здесь эти фиды
    gri <- cachedReadGRIds' u
    let (feedsHM, feedsIM) =
            feedsHMIM gri $ editActiveFeeds [f | SSFeed f <- map sState us]
        fixFFMS m =
            m { ffmFeedMasks = ffmFeedMasks m `IntMap.intersection` feedsIM }
        fixFQ q = q { fqFeeds = fqFeeds q `HM.intersection` feedsHM }
        fixSS s =
            s { ssQuery = fixFQ $ ssQuery s
              , ssFeedMasks = fixFFMS $ ssFeedMasks s
              , ssUnfilteredFeedMasks = fixFFMS $ ssUnfilteredFeedMasks s
              }
        fixF f =
            f { filterQuery = fixFQ $ filterQuery f
              , filterFeedMasks = fixFFMS $ filterFeedMasks f
              }
    modifyFilters'_ u $ \ fs0nm ->
        let fs0 = migrateOldFilters fs0nm
            fs = f fs0
            fs' =
                fs
                { fNewFilters = map fixF $ fFilters fs
                , fFeedMasks = fixFFMS $ fFeedMasks fs
                , fNewSmartStreams = map fixSS $ fSmartStreams fs
                }
        in
            if fs' /= fs0nm then do
                logS $ "changeFilters: changed, new version = " <>
                    show (fVersion fs' + 1)
                return $ markEditedFeedMasks gri feedsIM fs0
                    $ fs' { fVersion = fVersion fs' + 1 }
            else
                return fs0nm

addFilterQuery i fq [] = [mkFilter i fq]
addFilterQuery i fq (x@(filterQuery -> q):xs)
    | fqKey q == fqKey fq =
        x { filterQuery = q { fqFeeds = HM.union (fqFeeds q) (fqFeeds fq) } }
        : xs
    | otherwise = x : addFilterQuery i fq xs

replaceFilterQuery i fq [] = [mkFilter i fq]
replaceFilterQuery i fq (x:xs)
    | filterId x == i = x { filterQuery = fq } : xs
    | otherwise = x : replaceFilterQuery i fq xs

fqKey x = (fqQuery x, fqNegate x)
fKey = fqKey . filterQuery

mapWithKey f m = HM.traverseWithKey (\ k v () -> f k v) m ()

feedsSet gri = IntSet.fromList . feedIds gri . HM.keys
fqFeedsSet gri = feedsSet gri . fqFeeds

-- | Обновление фильтров. Если в конце обновления выяснится, что фильтры
-- были отредактированы вернет Nothing и будет необходимо сделать retry
updateUserFeedMasks _ UFNone fs _ _ _ = return $ Just fs
updateUserFeedMasks updateHotLinks updateFilters fs0 gri pprm user =
    withLogger $ \ l -> logTime l (T.append "updateUserFeedMasks " user) $ do
    logLT l $ T.append "---- uufm: " user
    sem <- MSem.new 2
    -- особого толка от распараллеливания нет, т.к. часто сам elasticsearch
    -- бывает перегружен (может быть и быстрее и медленнее однопоточного
    -- варианта, в зависимости от загруженности elasticsearch)
    t <- getUrTime
    let futs = map (\ (fm, fq) -> updateTypeAndNewFut t updateFilters fs fq
                       $ ffmLastUpdatedEx fm) $ masksAndQueries fs
        fLastUpdated = map (ffmLastUpdatedEx . fst) . masksAndQueries
        masksAndQueries f =
            map (\ f -> (filterFeedMasks f, filterQuery f)) (fFilters f)
            <>
            map (\ ss -> (ssUnfilteredFeedMasks ss, ssQuery ss))
                (fSmartStreams f)
        ffmLastUpdatedEx (FilterFeedMasks {..})
            | not $ IntMap.null ffmOldFeedMasks = FUTNever
              -- новые маски пока считаются только в beta, в master
              -- считаем старые
            | otherwise = ffmLastUpdated
        -- активные отредактированные фиды (новые подписки, новые/измененные
        -- фильтры) для определения необходимости flush
        changedFeeds = IntSet.unions $ map changedFeeds' $ masksAndQueries fs
        changedFeeds' (f, q) = case ffmLastUpdatedEx f of
            FUTNever -> fs
            FUTEdited _ cs -> IntSet.intersection cs fs
            _ -> IntSet.empty
            where fs = fqFeedsSet gri q
        split = splitAt (length $ fFilters fs)
        ffqes = map filterQuery (fFilters fs)
        ssfqes = map ssQuery (fSmartStreams fs)
        gfm :: (FilterQuery, Maybe (UpdateType, FilterUpdateTime))
            -> IO (IO (Maybe (Maybe SearchError, IntMap FeedMask)))
        gfm (fq, Nothing) = return $ return Nothing
        gfm (fq, Just (ut, _))
            | HM.null $ fqFeeds fq =
                return $ return $ Just (Nothing, IntMap.empty)
            | otherwise = MSem.with sem $ do
                l' <- newLogger
                ids <- getFeedMasksFromFQ_idsQuery l' user ut fq gri pprm
                (mbe, m, _, _) <-
                    getFeedMasksFromFQ_process l' user False [] ids
                        fq IntMap.empty gri pprm
                lt <- loggerGetAndClean l'
                return (logLT l lt >> return (Just (mbe, m)))
        version f =
            (fVersion f, fLastUpdated f
            ,fFilters f, [(ssName ss, ssQuery ss) | ss <- fSmartStreams f])
        update act = do
            r <- act
            modifyFilters' user $ \ f -> do
                logLS l $ "fVersion f = " <> show (fVersion f)
                    <> "; fVersion fs = " <> show (fVersion fs0)
                if version f /= version fs0 then do
                    logLS l "Filters/smart streams changed during update, retrying"
                    return (f, Nothing)
                else do
                    return (r, Just r)

    let handleErrors act =
            act `E.catch` \ (e :: E.SomeException) -> do
            logLS l $ "Error in updateUserFeedMasks: " ++ show e
            let errorFM fqs =
                    emptyFilterFeedMasks
                    { ffmLastUpdated = FUTError t (SESystemError $ showT e)
                    , ffmFeedMasks =
                        IntMap.fromSet (const FMError)
                            $ IntSet.unions $ map (fqFeedsSet gri) fqs
                    }
            update $ return $ fs
                { fFeedMasks = errorFM $ map filterQuery $ fFilters fs
                , fNewSmartStreams =
                    map (\ ss -> ss
                         { ssFeedMasks = errorFM [ssQuery ss]
                         , ssUnfilteredFeedMasks = errorFM [ssQuery ss]
                         })
                        $ fSmartStreams fs
                , fNewFilters =
                    map (\ f -> f
                         { filterFeedMasks = errorFM [filterQuery f]
                         })
                        $ fFilters fs
                , fOverloadDelay = 60
                }
    if not $ HM.null newFeeds then do
        -- Проверяем, что в pprm есть все необходимые нам фиды, т.к. возможна
        -- ситуация, когда в процессе чтения Posts/PostsRead был добавлен новый
        -- фид, который автоматически добавился в фильтры.
        logLS l "Some filters have new feeds:"
        mapM_ (logLT l) (sort $ HM.keys newFeeds)
        logLS l "retrying"
        changeFilters user id
        -- на всякий случай убираем удаленные фиды
        return Nothing
    else if all isNothing futs then do
        logLS l "Recently updated"
        let needUpdate =
                isJust . updateTypeAndNewFut t updateFilters fs emptyFilterQuery
                . ffmLastUpdatedEx
        if any (needUpdate . ssFeedMasks) (fSmartStreams fs)
           || needUpdate (fFeedMasks fs)
        then do
            logLS l "Updating computed masks only (no request)"
            -- в случае удаления фильтра поиск проводить не нужно,
            -- а обновить итоговыю маски фильтров и стримов надо
            update (return $ updateComputedMasks t fs)
        else if fVersion fs /= fVersion fs0 then do
            logLS l "Migrating to new filters"
            update (return fs)
        else
            return $ Just fs
    else handleErrors $ do
--         threadDelay $ 10*1000000
        tryFlush l t (maxFeedDlTime pprm $ IntSet.toList changedFeeds)

        let (ffuts, sfuts) = split futs
        logLS l "Filters:"
        forM_ (zip (fFilters fs) ffuts) $ \ (f,fut) ->
            logLTL l ["“", fqQuery (filterQuery f), "”: ", showT fut]
        logLS l "\nSmart streams:"
        forM_ (zip (fSmartStreams fs) sfuts) $ \ (ss,fut) ->
            logLTL l [ssName ss, ": ", showT fut]
        (split -> (ffms, ssfms)) <- sequence =<< mapConcurrently gfm
            (zip (ffqes <> ssfqes) futs)
        r <- logTime l "update " $ update $
            addFMS l t ffqes ssfqes ffms ssfms ffuts sfuts
        when updateHotLinks $ userUpdateHotLinks user
        return r
    where fs = migrateOldFilters fs0
          newFeeds = HM.unions $ map fqNewFeeds
              $ map filterQuery (fFilters fs) <> map ssQuery (fSmartStreams fs)
          feedsHM =
              HM.fromList $ map (,False) $ feedUrls gri $ IntMap.keys pprm
          fqNewFeeds fq = HM.difference (fqFeeds fq) feedsHM
          addFMS l t ffqes ssfqes ffms ssfms ffuts ssfuts = do
              t1 <- getUrTime
              let append fms ut =
                      IntMap.unionWith unionFeedMask fms . rmOld . rmFull ut
                      . IntMap.filter (/= FMError) . ffmFeedMasks
                  rmFull = \ case
                      UTFull -> const IntMap.empty
                      UTChangedOnly set -> rmSet set
                      UTIncremental _ set -> rmSet set
                  rmSet = flip IntMap.withoutKeys
                  rmOld = IntMap.intersectionWith
                      (\ (p,_) fm -> fixFeedMask p fm) pprm
                      -- убираем старые посты, чтобы feedmasks
                      -- не росли до бесконечности
                  updF (Just (e,fms)) (Just (fut, ffut)) f =
                      f
                      { filterFeedMasks = mkFM (mkFUT e ffut)
                          $ append fms fut $ filterFeedMasks f
                      }
                  updF _ _ f = f
                  updSS (Just (e,ssfms)) (Just (sut, sfut)) ss =
                      ss
                      { ssUnfilteredFeedMasks = mkFM (mkFUT e sfut)
                          $ IntMap.filter (not . isEmptyFeedMask False)
                          $ append ssfms sut $ ssUnfilteredFeedMasks ss
                      }
                  updSS _ _ ss = ss
                  r = fs
                      { fNewFilters =
                          zipWith3 updF ffms ffuts $ fFilters fs
                      , fNewSmartStreams =
                          zipWith3 updSS ssfms ssfuts $ fSmartStreams fs
                      , fOverloadDelay =
                          90 + round (3*dt) + 2*(length ffms + length ssfms)
                          -- если минуту обновляли фильтры, то следующее
                          -- обновление отложим еще на несколько минут,
                          -- а если 200 smart stream-ов, то еще добавим
                      }
                  mkFUT e fut = maybe fut (FUTError t) e
                  dt = diffUrTime t1 t
              logLS l $ "search : " ++ showSecs dt
              -- logS $ show ("r /= fs", r /= fs)
              return $ updateComputedMasks t r
          mkFM u m =
              FilterFeedMasks
              { ffmLastUpdated = u
              , ffmFeedMasks = m
              , ffmOldFeedMasks = IntMap.empty
              , ffmReserved2 = 0
              }
          updateComputedMasks t f =
              f
              { fFeedMasks = mkFM (FUTUpdatedAt minT) ffms
              , fNewSmartStreams = map fixSS $ fNewSmartStreams f
              }
              where ffms =
                        IntMap.unionsWith intersectFeedMask
                        $ map ffmFeedMasks fms
                    fms = map filterFeedMasks $ fFilters f
                    minT = minimum
                        $ t : [ut | FUTUpdatedAt ut <- map ffmLastUpdated fms]
                    fixSS ss@(ssUnfilteredFeedMasks -> u) =
                        ss
                        { ssFeedMasks = mkFM (ffmLastUpdated u)
                            $ IntMap.filter (not . isEmptyFeedMask False)
                            $ filterFms $ ffmFeedMasks u }
                    filterFms f =
                        IntMap.unionWith intersectFeedMask f
                        $ IntMap.intersection ffms f

updateTypeAndNewFut :: UrTime -> UpdateFilters -> Filters -> FilterQuery
    -> FilterUpdateTime -> Maybe (UpdateType, FilterUpdateTime)
updateTypeAndNewFut _ UFNone _ _ = const Nothing
updateTypeAndNewFut t uf fs fq = \ case
    FUTNever -> r UTFull
    FUTUpdatedAt ut
        | fresh ut -> Nothing
        | otherwise -> ri ut IntSet.empty
    FUTError et _
        | fresh et -> Nothing
        | otherwise -> r UTFull
    FUTEdited ut cs
        | fresh ut && HM.size (fqFeeds fq) > IntSet.size cs * 2
          -> Just (UTChangedOnly cs, FUTUpdatedAt ut)
          -- при обновлении измененных сохраняем предыдущее время для
          -- последующего инкрементального обновления
        | otherwise -> ri ut cs
    where fresh ut
              | uf == UFChanged = True
              | otherwise = ut `plusUrTime` fromIntegral (fOverloadDelay fs) > t
          r ut = Just (ut, FUTUpdatedAt t)
          ri ut cs
              | fqTimeBased fq = r UTFull
              | otherwise = r $ UTIncremental ut cs

data UpdateType
    = -- | Полное обновление без фильтра по времени (новые фильтры)
      UTFull
      -- | Если измененных фидов меньше 1/2, то обновляем только их,
      -- а остальные не трогаем, чтобы ускорить редактирование и добавление
      -- новых фидов.
    | UTChangedOnly IntSet
      -- | Поиск только в новых сообщениях и полный поиск в отредактированных
      -- фидах
    | UTIncremental
      { utMinTime :: UrTime
      , utEditedFeeds :: IntSet
      }
    deriving (Show, Eq)

maxFeedDlTime pprm feeds = maximum $ UrTime 0 0 : map fMaxDlTime feeds
    where fMaxDlTime fid =
              maybe (UrTime 0 0) (maxDlTime . fst) $ IntMap.lookup fid pprm

flushMVar :: MVar UrTime
flushMVar = unsafePerformIO $ newMVar $ UrTime 0 0
{-# NOINLINE flushMVar #-}

-- | Вызываем flush, если индекс еще не содержит последних обновлений
-- (со времени последнего обновления фида прошло меньше времени, чем
-- в настройке refresh_interval индекса).
-- Запоминаем время последнего обновления, чтобы не делать лишних flush.
tryFlush l t maxDlTime
    | dt > refreshInterval =
        logLS l $ "No need to flush, updated " <> showSecs dt <> " ago ("
            <> showUrTime maxDlTime <> ")"
    | otherwise = do
        t0 <- getUrTime
        modifyMVar_ flushMVar $ \ last -> do
            t1 <- getUrTime
            logLS l $ "Flushing, updated " <> showSecs dt <> " ago ("
                <> show maxDlTime <> ")"
            logLS l $ "Flush wait time: " <> showSecs (diffUrTime t1 t0)
            if last >= maxDlTime then do
                logLS l "Already flushed"
                return last
            else if diffUrTime t1 maxDlTime > refreshInterval then do
                logLS l "Already flushed automatically"
                return last
            else do
                r <- postSearchDownloaderL l "flush" (esUrl <> "/msg/_flush") ""
                case fst r of
                    DROK _ _ -> do
                        logLS l "OK"
                        return maxDlTime
                    e -> do
                        logLS l $ "Flush error: " <> show e
                        return last
    where refreshInterval = 60
          dt = diffUrTime t maxDlTime
          updated
              | maxDlTime == UrTime 0 0 = ", never updated"
              | otherwise =
                  ", updated " <> showSecs dt <> " ago ("
                      <> show maxDlTime <> ")"

data FilterUpdateException = FilterUpdateException SearchError
data SearchException = SearchException SearchError

instance Show FilterUpdateException where
    show (FilterUpdateException e) =
        "Can’t update filters or smart streams" <>
        case e of
            SESyntaxError em ->
                "\n(you may need to edit search queries to remove error):\n"
                    <> T.unpack em
            SESystemError em -> ":\n" <> T.unpack em
instance Show SearchException where
    show (SearchException t) = "Search error:\n" <> T.unpack (seErrorMessage t)

instance E.Exception FilterUpdateException
instance E.Exception SearchException

throwSyntaxError x =
    either (E.throwIO . SearchException . SESyntaxError) return =<< x

getFeedMasksE l u feedTcs vm streamName query =
    throwSyntaxError $ getFeedMasks l u feedTcs vm streamName query

getFeedMasks l u feedTcs vm streamName query = do
    fs <- cachedReadFilters' (uId u)
    gri <- cachedReadGRIds' (uId u)
    let applySS fq
            | Just n <- streamName
            , Just ss <- find ((== n) . ssName) (fSmartStreams fs)
            , fm <- ffmFeedMasks $ ssFeedMasks ss =
                (fq { fqFeeds = HM.intersection (fqFeeds fq)
                     $ HM.fromList $ map (,()) $ feedUrls gri $ IntMap.keys fm
                    }
                ,fm)
                -- ограничиваем поиск фидами, в которых точно есть результат
            | otherwise =
                (fq, ffmFeedMasks $ fFeedMasks fs)
        fixExpanded fq
            | not $ mtvmNoOverride vm =
                fq { fqFeeds = HM.map (const $ mtvmExpandedComments vm)
                     $ fqFeeds fq }
                -- smart stream может быть со свернутыми комментариями,
                -- хотя исходные фиды могут быть с развернутыми и наоборот
            | otherwise =
                fq
        (fqe, filters) = applySS $ fixExpanded
            $ filterQueryFromFeeds u gri feedTcs query False
    (_, prl) <- readPostsAndFilteredPostsRead u
        $ feedUrls gri $ map fst feedTcs

    let pprm = IntMap.fromList [(i, (p,pr)) | (i, (p,pr,_)) <- prl gri fs]

    t <- getUrTime
    tryFlush l t $ maxFeedDlTime pprm $ map fst feedTcs

    m <- getFeedMasksFromFQ l (uId u) (mtvmUnreadOnly vm) UTFull feedTcs
        fqe filters gri pprm

    case m of
        (Just e, _, _, _) -> case e of
            SESystemError _ -> E.throwIO $ SearchException e
            SESyntaxError m -> return $ Left m
        (_, fms, tc, uc) -> return $ Right
            (IntMap.filter (not . isEmptyFeedMask False) fms, tc, uc)

-- | Читаем Posts и PostsRead и применяем к PostsRead фильтры, перед дальшейшим
-- их использованием.
-- Заодно кешируем MsgTreeViewMode, т.к. он часто нужен вместе с PostsRead.
readPostsAndFilteredPostsRead :: User -> [Key Posts]
    -> IO ([Maybe Posts]
          ,GRIds -> Filters -> [(Int, (Posts, PostsRead, MsgTreeViewMode))])
readPostsAndFilteredPostsRead u feeds = do
    ps' <- forkReadPar2 cachedReadManyPostss feeds
    prs' <- forkReadPar2 cachedNothingReadManyPostsReads (map (uId u,) feeds)
    -- где-то на 15% ускоряется на локальной машине
    ps <- ps'
    prs <- prs'
    return (ps, mkPostsReadList u feeds ps prs)

mkPostsReadList u feeds ps prs gri fs =
    [(feedId f gri
     ,(p, applyFeedMasksToPostsRead gri fs p $ fixPostsRead p pr
      ,feedViewMode f u))
    |(f
     ,fromMaybe (defaultPosts f) -> p
     ,fromMaybe (defaultPostsRead (griUser gri,f)) -> pr)
        <- zip3 feeds ps prs]

fixPostsRead (Posts {..}) pr@(PostsRead {..})
    | lo == 0 = pr -- нечего фиксить, если посты не удалялись
    | otherwise =
        pr { prSet = ReadSet.clearTill (lo-1) prSet
           , prCommentsRead = case crMid of
              Nothing -> crHi
              Just c -> IntMap.insert lo c crHi
           , prTotalCommentsRead = prTotalCommentsRead
              - sum (map ReadSet.size $ IntMap.elems crLo)
           , prIgnoredPosts = ReadSet.clearTill (lo-1) prIgnoredPosts
           }
    where (lo', hi) = BA.bounds $ mtHeaders pMsgTree
          lo = min lo' (hi+1) -- для удаленных постов
          (crLo, crMid, crHi) = IntMap.splitLookup lo prCommentsRead

applyFeedMasksToPostsRead gri fs p@(Posts {..}) pr
    | fid <- feedId pBlogFeedUrl gri
    , Just fm <- fidFeedMask Nothing fid Nothing fs
    =
        let (lo', hi) = BA.bounds $ mtHeaders pMsgTree
            lo = min lo' (hi+1) -- для удаленных постов
            cr = IntMap.unionWith ReadSet.union filteredOut (prCommentsRead pr)
            filteredOut
                | Just comments <- fmComments fm =
                    IntMap.differenceWith diff (commentsReadSets p pr) comments
                | otherwise =
                    IntMap.difference (commentsReadSets p pr)
                        (readSetIntMap () (fmPosts fm))
            diff a b
                | ReadSet.size d == 0 = Nothing
                | otherwise = Just d
                where d = ReadSet.difference a b
        in
            pr { prSet =
                   (ReadSet.fromRange lo hi
                       `ReadSet.difference` fmPosts fm
                       `ReadSet.difference` prIgnoredPosts pr)
                   `ReadSet.union` prSet pr
               , prCommentsRead = cr
               , prTotalCommentsRead = sum $ map ReadSet.size $ IntMap.elems cr
               }
    | otherwise = pr

readSetIntMap :: a -> ReadSet -> IntMap a
readSetIntMap x = IntMap.fromDistinctAscList . map (,x) . ReadSet.toList

commentsReadSets :: Posts -> PostsRead -> IntMap ReadSet
commentsReadSets (Posts {..}) pr =
    IntMap.mapMaybe (fmap crs . IntMap.maxView) pCommentCounts
    `IntMap.difference`
    readSetIntMap () (prIgnoredPosts pr)
    where crs (count,_) = ReadSet.fromRange 0 (count-1)


getFeedMasksFromFQ :: Logger -> Key User
     -> Bool -> UpdateType -> [(Int, (Int, Int))]
     -> FilterQuery -> IntMap FeedMask
     -> GRIds -> IntMap (Posts, PostsRead)
     -> IO (Maybe SearchError, IntMap FeedMask, (Int, Int), (Int, Int))
        --  ^ (totalPosts, totalComments), (unreadPosts, unreadComments)
getFeedMasksFromFQ l u unreadOnly ut feedTcs fq filters gri pprm = do
    ids <- getFeedMasksFromFQ_idsQuery l u ut fq gri pprm
    getFeedMasksFromFQ_process l u unreadOnly feedTcs ids fq filters gri pprm

type Guid = ShortByteString
type GuidSet = HashSet Guid

getFeedMasksFromFQ_idsQuery :: Logger -> Key User
     -> UpdateType -> FilterQuery
     -> GRIds -> IntMap (Posts, PostsRead)
     -> IO (Either SearchError (HashMap TURL (GuidSet, HashMap Guid GuidSet)))
getFeedMasksFromFQ_idsQuery l u ut fq gri pprm = do
    logLT l $ "query  : " <> fqQuery fq
    t0 <- getUrTime
    case searchFeedsQuery gri pprm ut fq of
        Nothing -> do
            logLT l "No updated feeds to search"
            return $ Right HM.empty
        Just (url, q, full, incremental) ->
            getFeedMasksFromFQ_idsQuery' t0 url q full incremental l u fq

getFeedMasksFromFQ_idsQuery' t0 url q full incremental l u fq = do
--    logT $ bst $ BL.toStrict $ JSON.encode q
    (st0, dr) <- time $ postSearchDownloaderL l "search " (urlAddParam url $ "search_type=scan&scroll=1m&" ++ esPreference u) $
        BL.toStrict $ JSON.encode $ JSON.Object $ HM.fromList
        [ ("query", q)
--        , ("search_type", JSON.String "scan")
--        , ("scroll", JSON.String "1m")
        --  ^ тут не работает, надо в url
        , esTimeout
        -- в основном все занимает меньше секунды или больше 10секунд
        , ("size", JSON.Number $ if esHasRouting url then 10000 else 1000)
          -- size идет по шардам, т.е. может выдать 64000 за раз
          -- 1000 более-менее оптимально, 500/1500/2000 уже чуть медленнее
        ]
    let withDec = withDecodeSearchResults $ \ id _ -> return $ msgKeyFromJSON id        -- acc == feed -> ([post], post -> [comments])
        go !ids !st Nothing = do
            -- print ids
            t1 <- getUrTime
            let dt = diffUrTime t1 t0
            logLS l $ "found  : " ++ show (HM.size ids)
                ++ "  (" ++ show full ++ "+" ++ show incremental ++ "/"
                ++ show (HM.size $ fqFeeds fq)
                ++ " feeds)  "
                ++ showMSecs dt
                ++ " (" ++ showMSecs st ++ " + " ++ showMSecs (dt - st) ++ ")"
            return $ Right ids
        go !acc !st (Just scroll) = do
            (st', dr) <- time $ postSearchDownloaderL l "scroll "
                (esUrl ++ "/_search/scroll?scroll=1m&" ++ esPreference u)
                (tbs scroll)

            let ins acc (MsgKey bfu (Just pid) Nothing) =
                    -- такая жесть, чтобы меньше union-ов
                    -- хотя это преждевременная оптимизация
                    HM.insertWith (\ _ (!ps, !cs) -> (HS.insert pid ps, cs))
                        bfu (HS.singleton pid, HM.empty) acc
                ins acc (MsgKey bfu (Just pid) (Just cid)) =
                    HM.insertWith (\ _ (!ps, !cs) ->
                        (ps, HM.insertWith (\ _ old ->
                                                HS.insert cid old)
                             pid (HS.singleton cid) cs))
                        bfu (HS.empty, HM.singleton pid (HS.singleton cid)) acc
                ins acc (MsgKey bfu _ _) = acc -- ?
            withDec dr $ \ (total, took, sid, ids) -> do
--                 case dr of
--                     (DROK d _, _) | not (null ids) ->
--                         B.writeFile "idsQuery.json" d
--                     _ -> return ()
                -- print (total, took, length ids)
                go (foldl' ins acc ids) (st+st') (if null ids then Nothing else sid)

    withDec dr $ \ (total0,took0,sid,_) -> go HM.empty st0 sid

getFeedMasksFromFQ_process l _ _ _ (Left e) fq _ gri _ =
    return (Just e, IntMap.map (const FMError) $ fqFeedsIM gri fq, (0,0), (0,0))
getFeedMasksFromFQ_process l u unreadOnly feedTcsList (Right ids) fq filters gri pprm = do
    let foundFeeds = HM.keys ids
        feedTcs = IntMap.fromList feedTcsList
        emptyPr = defaultPostsRead ("","")
        mkFeedMask f = mkFeedMask' $ IntMap.lookup (feedId f gri) pprm
        mkFeedMask' Nothing = return $ Nothing
        mkFeedMask' (Just (p@(Posts {..}), pr)) = do
            (!comments, !totalComments) <-
                foldM insC (IntMap.empty, 0) cidIdx
            let !fm = restrict $
                    FMFeedMask
                    { fmPostsMask = posts
                    , fmCommentsMask = case HM.lookup bfu (fqFeeds fq) of
                        Just True -> -- expanded
                            Just $ IntMap.filter (not . ReadSet.null) comments
                        _ ->
                            Nothing
                    }
                restrict = maybe id intersectFeedMask filterFM
                !fmUnread = unreadOnlyFeedMask fm (Just pr)
            return $ Just (fid, (fm, fmUnread))
            where bfu = pBlogFeedUrl
                  fid = feedId bfu gri
                  filterFM = IntMap.lookup fid filters
                  filterCM = fmCommentsMask =<< filterFM
                  (tp,tc) = IntMap.findWithDefault (2^60,2^60) fid feedTcs
                  (pids, cids) =
                      HM.lookupDefault (HS.empty, HM.empty) bfu ids
                  mtReadSet mt guids total = ReadSet.fromList
                      [ idx | (idx, mh) <- BA.assocs (mtHeaders mt)
                      , idx < total
                      , mhGuid mh `HS.member` guids ]
                  !posts = mtReadSet pMsgTree pids tp
                  insC acc@(!c, !tc) (idx, pguid, cs, cc)
--                       | unreadOnly
--                       , Just cr <- IntMap.lookup idx (prCommentsRead pr)
--                       , cc <= ReadSet.size cr =
--                           return acc
--                           --  ^ всё прочитано, нет смысла читать комменты
                      | Just fm <- filterFM
                      , not $ case fmCommentsMask fm of
                          Nothing -> ReadSet.member idx (fmPostsMask fm)
                          Just cm -> IntMap.member idx cm
                      =
                          return acc
                          -- все комментарии отфильтрованы, нет смысла их читать
                      | cc <= HS.size cs =
                          -- все комментарии подходят
                          return
                              ( IntMap.insert idx (ReadSet.fromRange 0 (cc-1)) c
                              , tc + cc )
                      | otherwise = do
                          logLS l $ show ("read", bfu, pguid)
                          -- TODO: по-идее, при unreadOnly == False
                          -- можно не читать комменты, т.к. число результатов
                          -- можно и по guids считать?
                          -- хотя не учитываются commentsCount.
                          Comments {..} <- cachedReadComments'
                              $ CommentsKey bfu pguid
                              -- cached, т.к. может быть много фильтров и
                              -- стримов на один и тот же фид
                          let !crs = mtReadSet cMsgTree cs cc
                          return
                              ( IntMap.insert idx crs c
                              , tc + ReadSet.size crs )
                  cidIdx
                      | HM.size cids == 0 = []
                      | otherwise =
                          [ (idx, mhGuid mh, cs, cc)
                          | (idx, mh) <- BA.assocs (mtHeaders pMsgTree)
--                           , if unreadOnly then
--                                 not (ReadSet.member idx (prIgnoredPosts pr))
--                             else
--                                 True
                          , cs <- maybeToList $ HM.lookup (mhGuid mh) cids
                          , let cc = postCommentsCount idx tc p
                          , cc > 0 ]
    t1 <- getUrTime
    masks2 <- IntMap.fromList . catMaybes
        <$> (join $ forkReadPar2 (mapM mkFeedMask) foundFeeds)
    let masks = IntMap.map fst masks2
        masksR =
            IntMap.union
                (if unreadOnly then unreadMasks else masks)
                (IntMap.map emptyFeedMask $ fqFeedsIM gri fq)
                -- пустая маска для фидов, в которых ничего не найдено
        unreadMasks = IntMap.map snd masks2
        idsTotals (HM.elems -> m) =
            ( sum $ map (HS.size . fst) m
            , sum $ map (sum . map HS.size . HM.elems . snd) m )
        masksTotals (IntMap.assocs -> m) =
            ( sum $ map (ReadSet.size . fmPosts . snd) m
            , sum $ map (maybe 0 imTotalComments . cIfExpanded) m )
        cIfExpanded (fid, m)
            | HM.lookup (feedUrl fid gri) (fqFeeds fq) == Just True =
                fmComments m
            | otherwise = Nothing
        total (p,c) = show p ++ "/" ++ show c
        ti = idsTotals ids
        tm = masksTotals masks
        tu = masksTotals unreadMasks
    t2 <- getUrTime
    logLS l $ "process: " ++ (if ti /= tm then total ti ++ " -> " else "")
        ++ total tm ++ "  (" ++ total tu ++ " unread)"
        ++ "  " ++ showMSecs (diffUrTime t2 t1)
    return (Nothing, masksR, tm, tu)
    -- использовать idsTotals неправильно, т.к. не учитываются totalPosts
    -- и может показать больше результатов, чем нужно,
    -- но если их учитывать, то фильтрация непрочитанных станет дольше
    -- из-за readComments.
    -- Теперь, из-за того, что фильтры накладывается потом, а не в запросе,
    -- все равно надо искать всё.
    -- Этого можно избежать, если id-шки тоже индексировать
    -- (можно будет не бегать по guid-ам)


imTotalComments = sum . map ReadSet.size . IntMap.elems

unreadOnlyFeedMask (FMFeedMask {..}) (Just pr) =
    FMFeedMask
    { fmPostsMask = fmPostsMask `ReadSet.difference` prSet pr
                            `ReadSet.difference` prIgnoredPosts pr
    , fmCommentsMask = cs <$> fmCommentsMask
    }
    where cs comments = IntMap.differenceWith
              (nothingIfNull ReadSet.difference)
              (IntMap.filterWithKey
                  (\ pid _ -> not $ ReadSet.member pid (prIgnoredPosts pr))
                  comments)
              (prCommentsRead pr)
unreadOnlyFeedMask fm _ = fm

zipFeedMaskWith f a b = case (a,b) of
    (FMError, _) -> FMError
    (_, FMError) -> FMError
    (FMFeedMask pa ca, FMFeedMask pb cb) -> f pa ca pb cb

intersectFeedMask = zipFeedMaskWith $ \ pa ca pb cb ->
    FMFeedMask
    { fmPostsMask = pa `ReadSet.intersection` pb
    , fmCommentsMask = i ca cb pa pb
    }
    where i (Just a) (Just b) _ _ =
              Just $ intersectionWithMaybe
                  (nothingIfNull ReadSet.intersection) a b
          i (Just a) Nothing  _ pb =
              Just $ IntMap.restrictKeys a (ReadSet.toIntSet pb)
          i Nothing  (Just b) pa _ =
              Just $ IntMap.restrictKeys b (ReadSet.toIntSet pa)
          i Nothing  Nothing  _ _ = Nothing

intersectionWithMaybe :: (a -> a -> Maybe a) -> IntMap a -> IntMap a -> IntMap a
intersectionWithMaybe f =
    IntMap.merge IntMap.dropMissing IntMap.dropMissing (IntMap.zipWithMaybeMatched $ const f)

nothingIfNull f a b = if ReadSet.null rs then Nothing else Just rs
    where rs = f a b

unionFeedMask = zipFeedMaskWith $ \ pa ca pb cb ->
    FMFeedMask
    { fmPostsMask = pa `ReadSet.union` pb
    , fmCommentsMask = u ca cb
    }
    where u (Just a) (Just b) = Just $ IntMap.unionWith ReadSet.union a b
--           u _ _ = Nothing ? так вроде правильнее
          u a        Nothing  = a
          u Nothing  b        = b

-- аналог fixPostsRead
fixFeedMask (Posts {..}) fm@(FMFeedMask {..})
    | lo == 0 = fm -- нечего фиксить, если посты не удалялись
    | otherwise =
        FMFeedMask
        { fmPostsMask = ReadSet.clearTill (lo-1) fmPostsMask
        , fmCommentsMask = fixCs <$> fmCommentsMask
        }
    where (lo', hi) = BA.bounds $ mtHeaders pMsgTree
          lo = min lo' (hi+1) -- для удаленных постов
          fixCs (IntMap.splitLookup lo -> (crLo, crMid, crHi)) = case crMid of
              Nothing -> crHi
              Just m -> IntMap.insert lo m crHi
fixFeedMask _ fm = fm

isEmptyFeedMask _ (FMFeedMask {..}) =
    ReadSet.null fmPostsMask
    && maybe True (all ReadSet.null . IntMap.elems) fmCommentsMask
isEmptyFeedMask d _ = d

emptyFeedMask expanded =
    FMFeedMask
    { fmPostsMask = ReadSet.empty
    , fmCommentsMask = if expanded then Just IntMap.empty else Nothing
    }

searchFeedsQuery :: GRIds -> IntMap (Posts, PostsRead) -> UpdateType
    -> FilterQuery -> Maybe (URL, JSON.Value, Int, Int)
searchFeedsQuery gri pprm ut fq
    | null feedsQueries = Nothing
    | fqNegate fq = ret $ esBool [] query feedsQueries
    | otherwise   = ret $ esBool query [] feedsQueries
    where ret q =
              Just (esUrl ++ "/msg/msg/_search" ++ routing, esFilter q
                   ,IntMap.size fullFeeds, IntMap.size incrementalFeeds)
          query = [obj "query" $ esQueryString $ fqQuery fq]
          feedsQueries =
              map fst $ filter (not . IntMap.null . snd) feedsQueries'
          feedsQueries' = case ut of
              UTFull ->
                  feedsQuery True id feedsMap
              UTChangedOnly cs ->
                  feedsQuery True id $ IntMap.restrictKeys feedsMap cs
              UTIncremental (roundTime -> rt) cs
                  | incremental <- IntMap.filterWithKey
                      (\ fid expanded ->
                          maybe False (hasNewerPosts expanded rt . fst)
                          $ IntMap.lookup fid pprm)
                      $ IntMap.withoutKeys feedsMap cs
                  , full <- IntMap.restrictKeys feedsMap cs
                  ->  feedsQuery False
                          (esGreaterThanDateFilter "dl_time" rt :)
                          incremental
                      ++
                      feedsQuery True id full
          feedsQuery full add fm =
              [q [bfuTerms expanded] expanded
              ,q [bfuTerms collapsed, esBoolField "comment" False] collapsed]
              where (expanded, collapsed) = IntMap.partition id fm
                    q t m = (esMust $ add t, IntMap.map (const full) m)
          feedsMap = fqFeedsIM gri fq
          usedFeeds = IntMap.unions $ map snd feedsQueries'
          (fullFeeds, incrementalFeeds) = IntMap.partition id usedFeeds
          routing
              | [fid] <- IntMap.keys usedFeeds
              = esRouting $ feedUrl fid gri
              | otherwise = ""
          bfuTerms = esTerms "blog_feed_url"
              . sort . feedUrls gri . IntMap.keys
          -- у lucene есть лимит на 1024 boolean операции
          -- выдает "too many boolean clauses",
          -- а вот terms запросто может быть несколько тысяч

hasNewerPosts expanded t (Posts {..}) =
    hi >= lo && if expanded then p || c else p
    where (lo, hi) = BA.bounds hs
          hs = mtHeaders pMsgTree
          p = mhDlTime (hs BA.! hi) >= t
          c = maybe False ((>= t) . fst) (Map.lookupMax pCCVersions)

-- | Только что созданный фид, который может быть еще не проиндексирован,
-- т.к. индекс обновляется каждые 15 секунд.
justCreatedFeed t (Posts {..}) =
    hi >= lo && tLo == tHi && tLo >= t `plusUrTime` (-60)
    where (lo, hi) = BA.bounds hs
          hs = mtHeaders pMsgTree
          tLo = mhDlTime (hs BA.! lo)
          tHi = mhDlTime (hs BA.! hi)

maxDlTime (Posts {..})
    | hi >= lo =
        max (mhDlTime (hs BA.! hi))
            (maybe (UrTime 0 0) fst $ Map.lookupMax pCCVersions)
    | otherwise = UrTime 0 0
    where (lo, hi) = BA.bounds hs
          hs = mtHeaders pMsgTree

esQueryString query =
    JSON.Object $ HM.fromList
        [("query_string", JSON.Object $ HM.fromList
            [("query",
              JSON.String $ fixBadQuery $
              T.replace "\xAD" "" $ -- soft hyphen
              T.replace "link:www." "link:" $
              T.replace "link:http://" "link:" $
              T.replace "link:http\\://" "link:" $
              T.replace "link:https://" "link:" $
              T.replace "link:https\\://" "link:" $
              T.replace "tag:" "tags:" query)
            ,("default_field", JSON.String "std_all")
             --  ^ сохраняем старое поведение
            ,("default_operator", JSON.String "AND")
--            ,("use_dis_max", JSON.Bool False)
            ,("analyze_wildcard", JSON.Bool False)
             --  ^ превращает mening* в men+man
            ,("allow_leading_wildcard", JSON.Bool False)
--            ,("fields", JSON.Array $ V.fromList $ map JSON.String ["subject", "author", "tags", "text"])
--            ,("analyzer", JSON.String "my_analyzer")-- "standard")
              -- analyzer должен быть такой же, как и поле
              -- иначе будут не совпадать токены поска и токены в индексе
             -- Нужны поля и _all с морфологией и без
             -- Если запрос без поля, то ищем в _all:XXX и std_all:XXX
             -- (если потребуется полная совместимость, то только std_all)
             -- если wildcard, то std_all
             -- Если поле, то также ищем в field:XXX OR field.std:XXX
             -- Если wildcard, то только field.std
            ])]

getQueryStrings = hashSetNub . go
    where go (JSON.Object hm)
              | Just (JSON.Object qs) <- HM.lookup "query_string" hm
              , Just (JSON.String q) <- HM.lookup "query" qs =
                  [q]
              | otherwise =
                  concatMap go $ HM.elems hm
          go (JSON.Array (V.toList -> qs)) =
              concatMap go qs
          go _ = []

-- TODO: более полноценный анализ, т.к. в link /? -- это не wildcard в начале
-- слова

-- | Убраем /regex/ и *? в начале слов
--
-- Параметр allow_leading_wildcard=false не убирает звездочки,
-- а возвращает ошибку
fixBadQuery = T.pack . rmStar False . escape . T.unpack
    where escape [] = []
          escape ('/':xs) = '\\':'/': escape xs
          escape ('\\':x:xs) = '\\':x: escape xs
          escape (x:xs)
              | x `elem` quoteChars = '"': escape xs
              | otherwise = x : escape xs
          quoteChars :: String
          quoteChars = "“”„«»‹›〈〉《》「」『』【】〔〕〖〗〘〙〚〛〝〞〟" -- ‘‚’‛
          rmStar _ [] = []
          rmStar True ('*':cs) = '*' : rmStar True (dropWhile (== '*') cs)
          rmStar True ('?':cs) = '?' : rmStar True cs
          rmStar False ('*':cs)
              | reservedOrEnd cs = '*' : ' ' : rmStar False cs
              | otherwise = rmStar False cs
          rmStar False ('?':cs)
              | reservedOrEnd cs = '*' : ' ' : rmStar False cs
              -- меняем ? на *, чтобы не искало токены единичной длины
              | otherwise = rmStar False cs
          rmStar _ ('\\':c:cs) = '\\' : c : rmStar True cs
          rmStar _ (c:cs) = c : rmStar (not (reserved c)) cs
          reserved c =
              c `elem` ("+-=&|><!(){}[]^\"~:\\/" :: String) || isSpace c
          reservedOrEnd [] = True
          reservedOrEnd (c:_) = reserved c

withDecodeSearchResults f r a =
    case decodeSearchResults f r of
        Right x -> a x
        Left e -> return $ Left e

msgToSearchJSON :: Maybe (Key User, LongMsgId, [ItemTag]) -> (Msg -> UrTime)
                -> FeedItem -> JSON.Value
msgToSearchJSON tagged mkPostTime (FeedItem { fiMsg = m@(Msg {..}), .. }) =
    JSON.Object $ HM.fromList $
    [ ("subject", t fiSubjectText)
    , ("tags", t $ T.intercalate " " msgTags)
    , ("author", t msgAuthor)
    , ("text", t fiSearchText)
--    , ("author_", t msgAuthor)
    , ("author_pic", maybe "" t msgAuthorPic)
    , ("author_uri", maybe "" t msgAuthorUri)
    , ("time", esUrTime mTime)
    , ("post_time", esUrTime (mkPostTime m))
    , ("dl_time", esUrTime msgDlTime)
    , ("blog_feed_url", t $ msgKeyBlogFeedUrl msgKey)
    , ("post_guid", maybe "" (t . sbt) $ msgKeyPostGuid msgKey)
    ] ++
    (case tagged of
       Just (user, mid, tags) ->
           [("comment_guid", maybe "" (t . sbt) $ msgKeyCommentGuid msgKey)
           ,("user", t user)
           ,("starred", JSON.Bool (ITStarred `elem` tags))
           ,("user_tags", JSON.Array $ V.fromList [t n | ITTag n <- tags])
           ,("gr_id", t $ T.pack $ show $ midGrItemId $ lmidMsgId $ mid)
           ]
       Nothing ->
           [])
    --  ^ это не нужно, но нужен список тегов, и id пользователя
    -- индекс нужен отдельный, т.к. иначе сообщения будут мешаться
    -- и id не получится прочитать
    -- также routing должен быть по user
    -- ну и скорость поиска будет отличной
    ++
    [ ("item_link", linkArray $ maybeToList msgLink)
    , ("link", linkArray $ maybeToList msgLink ++ map fst fiLinks)
    , ("comment", JSON.Bool $ maybe False (const True) (msgKeyCommentGuid msgKey))
    , ("img", JSON.Bool $ fiHasImgTag || [() | AImage {} <- msgAttachments] /= [])
    , ("gif", JSON.Bool $ fiHasGif ||
                [() | AImage {..} <- msgAttachments, gifUrl aUrl] /= [])
    , ("audio", JSON.Bool $ [() | AAudio {} <- msgAttachments] /= [])
    , ("video", JSON.Bool $ fiHasVideo || any videoAttachment msgAttachments)
    , ("attachment", JSON.Bool (msgAttachments /= []))
    ]
    where linkArray =
              JSON.Array . V.fromList . map t . filter (/= "") . map T.strip
          videoAttachment a = case a of
              AVideo {} -> True
              AVideo2 {} -> True
              AIframe {} -> True
              _ -> False
          mTime = fromMaybe msgDlTime msgTime
          t = JSON.String

esMsgKeyAction routing = esBulkAction routing "msg"

-- removeSearchMsgs l uq msgKeys =
--     esBulkRequest "Deleted" l uq $ map (esMsgKeyAction "delete") msgKeys

saveSearchMsgs l uq feedUrl mkPostTime rootMsg fis = do
    saveSearchMsgs' True (esBulkRequest "Saved") l uq feedUrl mkPostTime rootMsg fis
    withEsUrl2 $ \ u2 ->
        saveSearchMsgs' True (esBulkRequest' u2 "Saved2") l uq feedUrl mkPostTime rootMsg fis
saveSearchMsgs' includeHotLinks bulk l uq feedUrl mkPostTime rootMsg fis = do
    let req fi =
            ([ esMsgKeyAction (msgKeyBlogFeedUrl key) "msg" "index" $
                             msgKeyToJSON key
             , msgToSearchJSON Nothing mkPostTime fi ]
             ++
             f ml
            ,f hl)
            where key = msgKey $ fiMsg fi
                  (hl, ml) = msgToLinkBulk rootMsg fi
                  f :: [a] -> [a]
                  f x | includeHotLinks = x
                      | otherwise = []
        (mmls, concat -> hls0) = unzip $ map req fis
        hls =
            map snd $
            hashSetNub' (fst . fst) $ -- убираем совпадающие id
            reverse $ sortBy (comparing $ snd . fst) hls0
            -- сортируем по времени, чтобы оставить только самый свежий линк
        reqs = concat $ mmls ++ hls
        dups = length hls0 - length hls
        total = length reqs `div` 2
        msg = length fis
        hl = length hls
        ml = total - msg - hl
        showIf 0 _ = ""
        showIf _ t = t
    logLS l $ "Saving " ++ show msg ++ " msgs" ++
              showIf ml (" + " ++ show ml ++ " ml + " ++ show hl ++ " hl = "
                         ++ show total ++ " total") ++
              showIf dups  (" (-" ++ show dups ++ " dups)")
    mapM_ (bulk l) $ groupByN 100 reqs
    -- может сохранять не по 50, если есть hot_link-и более позднего времени
saveSearchTagged :: Logger -> Key User -> [(LongMsgId, [ItemTag])] -> IO ()
saveSearchTagged l user midsAndTags = do
    msgs <- readManyMsgs $ map (lmidMsgKey . fst) midsAndTags
    let req (Just msg, (mid, tags)) =
            [ esMsgKeyAction user "tagged" "index" $ userMidToJSON user mid
            , msgToSearchJSON (Just (user, mid, tags))
                              (\ m -> fromMaybe (msgDlTime m) (msgTime m)) $
                              feedItemFromMsgAndText "" msg (msgText msg)
            ]
        req _ = []
        reqs = concatMap req $ zip msgs midsAndTags
    mapM_ (esBulkRequest "Saved" l) $ groupByN 100 reqs
    withEsUrl2 $ \ u2 ->
        mapM_ (esBulkRequest' u2 "Saved2" l) $ groupByN 100 reqs

deleteSearchTagged :: Logger -> Key User -> [LongMsgId] -> IO ()
deleteSearchTagged l user removed = do
    let acts =
           groupByN 50 $
           map (esBulkAction user "msg" "tagged" "delete" .
                userMidToJSON user) removed
    mapM_ (esBulkRequest "Deleted" l) acts
    withEsUrl2 $ \ u2 ->
        mapM_ (esBulkRequest' u2 "Deleted2" l) acts

esUpdateTaggedItem l msgKey@(MsgKey bfu pg cg) = logTime l "utSearch" $ do
    dr <- postSearchDownloaderL l "esUpdateTaggedItem"
          (esUrl ++ "/msg/tagged/_search")
          $ BL.toStrict $ JSON.encode $ JSON.Object $ HM.fromList
          [("query",
            esFilter $
            esMust [esTerm "blog_feed_url" bfu
                   ,esTerm "post_guid" (mbs pg)
                   ,esTerm "comment_guid" (mbs cg)
                   ])
--             esNFields
--             [("blog_feed_url", JSON.String bfu)
--             ,("post_guid", mbs pg)
--             ,("comment_guid", mbs cg)
--             ])
          ,esTimeout
          ,("size", JSON.Number 3000) -- больше врядли будет
          ]
    case decodeSearchResults (\ id hit -> return $ userMidFromJSON id) dr of
        Right (srTotal, srTook, _, ids) -> do
            forM_ ids $ \ (user, mid) -> do
                gri <- readGRIds' user
                case IntMap.lookup (midGrItemId $ lmidMsgId mid) (griItemTags gri) of
                    Just tags ->
                        saveSearchTagged l user
                            [(mid { lmidMsgKey = msgKey }, tags)]
                            -- обновляем ключ, т.к. у нас кривой escaping
                    Nothing ->
                        logLS l "no tags for item?"
            logLS l $ show (length ids) ++ " user’s tagged items updated"
        Left e ->
            logLT l $ "Can’t find items to update: " <> seErrorMessage e
    where mbs = maybe "" sbt

userDeleteTaggedItems user = withLogger $ \ l -> do
    userDeleteTaggedItems' esSimpleIdsSearch (esBulkRequest "Deleted") user l
    withEsUrl2 $ \ u2 ->
        userDeleteTaggedItems' (esSimpleIdsSearch' u2) (esBulkRequest' u2 "Deleted2") user l
userDeleteTaggedItems' search bulk user l = do
--    esDeleteByQuery l iuq "tagged" user $ esField "user" user
    -- а надо ли? удаление аккаунта вещь редкая, подумаешь, refresh лишний
    ids <- search "msg" "tagged" searchSize
        (Just user) user $
        esFilter $ esTerm "user" user
--    logLS l $ show ids
    mapM_ (bulk l) $
        groupByN 100 $
        map (esBulkAction user "msg" "tagged" "delete" . fst) ids
    when (length ids >= searchSize) $ do
        threadDelay $ 5*1000*1000 -- надо refresh подождать
        userDeleteTaggedItems' search bulk user l
    where searchSize = 10000 -- больше API не позволяет

gifUrl = regexTest "[^?]+\\.gif([?#].*)?$" . T.toLower

-- | Обновленное сообщение и FeedItem, подходящий для поиска
feedItemFromMsgAndText :: URL -> Msg -> T.Text -> FeedItem
feedItemFromMsgAndText baseUri msg text
    = FeedItem
      { fiMsg = msg { msgText = text' }
      , fiHeader = error "msgHeader"
      , fiParent = Nothing
      , fiCommentsPage = Nothing
      , fiFeed = Nothing
      , fiNext = Nothing
      , fiLinks = links
      , fiHasImgTag = imgs /= []
      , fiHasGif = any gifUrl $ imgs <> videos
      , fiHasVideo = videos /= []
      , fiSearchText = searchText
      , fiSubjectText = subjectText
      , fiNoCommentsYet = True
      , fiHubs = []
      }
    where bodyTags =
              fixMessageBody (msgLink msg) baseUri $ tbs text
          (links, imgs, videos, searchText) =
              xmlLinksImgsVideosAndText bodyTags
          subjectText = xmlText $ msgSubject msg
          text'    = T.strip $ renderTagsT bodyTags

-- не пригодилось, пусть полежит тут пока
esNodeIdsToHostNames = do
    DROK n _ <- getSearchDownloader $ esUrl ++ "/_nodes"
    return $ fromMaybe (error "can’t get es nodes?") $ do
        JSON.Object o <- decodeJson n
        JSON.Object n <- HM.lookup "nodes" o
        forM (HM.toList n) $ \ (id, node) -> do
            JSON.Object o <- return node
            JSON.String host <- HM.lookup "hostname" o
            return (id, host)

esIndexShardsLocations indexName = do
    nodesMap <- fmap HM.fromList esNodeIdsToHostNames
    DROK n _ <- getSearchDownloader $ esUrl ++ "/_status"
    let locs = maybe (error "can’t get shard locations") (HM.fromListWith IntSet.union . concatMap concat) $ do
            JSON.Object o <- decodeJson n
            JSON.Object is <- HM.lookup "indices" o
            JSON.Object i <- HM.lookup indexName is
            JSON.Object sh <- HM.lookup "shards" i
            forM (HM.toList sh) $ \ (id, subshards) -> do
                id <- tryReadUnsignedInt id
                JSON.Array (V.toList -> ss) <- return subshards
                forM ss $ \ s -> do
                    JSON.Object s <- return s
                    JSON.Object r <- HM.lookup "routing" s
                    JSON.Bool p <- HM.lookup "primary" r
                    JSON.String nid <- HM.lookup "node" r
                    host <- HM.lookup nid nodesMap
                    return $ if p then [(host, IntSet.singleton id)] else []
    print locs

instance Hashable MsgKey where
    hashWithSalt s (MsgKey a b c) =
        s `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c

printAllFilters = do
    us <- getAllUsers
    fs <- cachedNothingReadManyFilterss us
    let usedBy =
            [ fUser f
            | Just f <- fs, notNull (fFilters f) || notNull (fSmartStreams f) ]
        badQuery (fqQuery -> q) = fixBadQuery q /= q
--     print (length usedBy) -- всего 140 человек
--     mapM_ print usedBy
    forM_ (catMaybes fs) $ \ f -> do
        forM_ (map filterQuery $ fFilters f) $ \ q ->
            when (badQuery q) $
            B.putStrLn $ tbs $ T.concat [fUser f, if fqNegate q then " HIDE: " else " SHOW: ", fqQuery q, "\n", fixBadQuery (fqQuery q)]
        forM_ (fSmartStreams f) $ \ ss@(ssQuery -> q) ->
            when (badQuery q) $
            B.putStrLn $ tbs $ T.concat [fUser f, " SS “", ssName ss, "”: ", fqQuery q, "\n", fixBadQuery (fqQuery q)]

testSSInconsistency = do
    gri <- getGRIds "1"
    fs <- cachedReadFilters' "1"
    let fms = ffmFeedMasks $ fFeedMasks fs
        Just ss = findSmartStream (Just "db") fs
        sfms = ffmFeedMasks $ ssFeedMasks ss
    print $ IntMap.size sfms
    print
        [ (u,fm,ffm)
        | (u,fm) <- IntMap.toList sfms
        , ffm <- maybeToList $ IntMap.lookup u fms
        , intersectFeedMask fm ffm /= fm ]
    u <- cachedReadUser' "1"
    return () --fmap (tail.fst) $ getUpdatedCounts u [("http://planet.haskell.org/rss20.xml",0,0,0,0)]

findSmartStream smartStream fs =
    case smartStream of
       Just n ->
           find ((== n) . ssName) (fSmartStreams fs)
       Nothing ->
           Nothing

fidFeedMask feedMasks fid smartStream fs
    | Just fms <- feedMasks = IntMap.lookup fid fms
    | Just s <- findSmartStream smartStream fs =
        Just $ IntMap.findWithDefault (emptyFeedMask False) fid
            $ ffmFeedMasks $ ssFeedMasks s
    | otherwise =
        IntMap.lookup fid $ ffmFeedMasks $ fFeedMasks fs

lookupSmartStreamFeedMasks name user = do
    fs <- cachedReadFilters' user
    return $ maybe IntMap.empty
        (ffmFeedMasks . ssFeedMasks) (findSmartStream (Just name) fs)

checkSmartStreamError name user = do
    fs <- cachedReadFilters' user
    gri <- cachedReadGRIds' user
    case findSmartStream (Just name) fs of
        Just ss
            | FUTError _ e <- ffmLastUpdated (ssFeedMasks ss) ->
                E.throwIO $ FilterUpdateException e
            | otherwise ->
                checkFilterError (feedIds gri $ HM.keys $ fqFeeds $ ssQuery ss)
                    user
        _ -> return ()

checkFilterError fids user = do
    fs <- cachedReadFilters' user
    let fmAll = fFeedMasks fs
    when (hasError fmAll) $ do
        checkError fmAll
        -- сначала проверяем общую ошибку и только потом ищем ошибки
        -- в отдельных фильтрах
        forM_ (map filterFeedMasks $ fFilters fs) $ \ fm ->
            when (hasError fm) $ checkError fm
    where hasError fm = any (== Just FMError)
              [IntMap.lookup i (ffmFeedMasks fm) | i <- fids]
          checkError fm =
              case ffmLastUpdated fm of
                  FUTError _ e -> E.throw $ FilterUpdateException e
                  _ -> return ()

------------------------------------------------------------------------------
-- Восстановление постов и звездочек в поиске

reindexAllPosts = do
    ps <- fmap (filterSpecial . T.lines) (T.readFile pfn)
        `E.catch` \ (_ :: E.SomeException) -> do
            p <- riakBucketKeys "Posts"
            T.writeFile pfn $ T.unlines p
            return p
    lp <- T.readFile lpfn `E.catch` \ (_ :: E.SomeException) -> return ""
    sem <- MSem.new numThreads
    active <- newMVar []
    withAsync (forever $ save active) $ const $ withLogger $ \ l -> logTime l "Grand Total" $ do
        forM_ (dropWhile (< lp) ps) $ \ p -> do
            MSem.wait sem
            modifyMVar_ active (return . (p:))
            a <- async $ do
                try 5 $ reindexPosts p
                MSem.signal sem
                modifyMVar_ active (return . filter (/= p))
            link a
        replicateM_ numThreads $ MSem.wait sem
    where filterSpecial = filter (not . T.isPrefixOf grBfuPrefix)
          numThreads = 2
          lpfn = "lastPosts.txt"
          pfn = "all_posts.txt"
          save mv = do
              a <- readMVar mv
              when (notNull a) $
                  T.writeFile lpfn $ minimum a
              threadDelay $ 10*1000*1000
          try n act = do
              r <- E.try act
              case r of
                  Right () -> return ()
                  Left (e :: E.SomeException) -> do
                      logS $ "Exception: " ++ show e
                      threadDelay $ 5*1000*1000
                      if n > 0 then
                          try (n-1) act
                      else
                          E.throw e

reindexPosts p = withLogger $ \ l -> logTime l "total" $ do
    logLS l $ "Indexing " ++ T.unpack p
    Posts {..} <- readPosts' p
    let pguids = map mhGuid $ BA.elems $ mtHeaders pMsgTree
        cskeys guid cs =
            keyPrefix Nothing :
            maybe [] (map (keyPrefix . Just . mhGuid)
                      . BA.elems . mtHeaders . cMsgTree) cs
            where keyPrefix = MsgKey p (Just guid)
        sliceRead :: T.Text -> ([a] -> IO [b]) -> [a] -> IO [b]
        sliceRead what f keys =
            logTime l (T.append "read" what) $
            fmap concat $ mapM f $ groupByN 50 keys
            -- Если читать параллельно десятки тысяч сообщений,
            -- риак иногда рвет соединение. По-этому нарезаем кусочками.
    css <- sliceRead "Comm" readManyCommentss [CommentsKey p g | g <- pguids]
    let keys = sort $ concat $ zipWith cskeys pguids css
    msgs <- fmap catMaybes $ sliceRead "Msgs" readManyMsgs keys
    logLS l $ show (length pguids) ++ " posts + "
        ++ show (length keys - length pguids) ++ " comments"
    let hm = HM.fromList [(msgKey m, m) | m <- msgs]
        mkPostTime msg
            | Just m <-
                HM.lookup ((msgKey msg) { msgKeyCommentGuid = Nothing }) hm
            = fromMaybe (msgDlTime m) (msgTime m)
            | otherwise =
              error $ "Can’t find post for " ++ show msg
              -- fromMaybe (msgDlTime msg) (msgTime msg)
        mkfi m = feedItemFromMsgAndText "" m (msgText m)
                 -- baseUri не нужен, т.к. индексируем только текст
    logTime l "indexing" $ mapM_ (saveSearchMsgs' False (esBulkRequest' (fromMaybe (error "no esUrl2 ?") esUrl2) "Saved2") l iuq p mkPostTime (error "rootMsg?")) $ groupByN 50 $ map mkfi msgs

reindexAllTags = mapM_ reindexUserTags =<< getAllUsers

reindexUserTags user = withLogger $ \ l -> logTime l "total" $ do
    GRIds {..} <- readGRIds' user
    let gridToMid =
            IntMap.fromList
            [ (grid, lmidFromGrItemId grid msgKey)
            | items <- Map.elems griTaggedItems
            , (_, grid, msgKey) <- items ]
        midsAndTags =
            IntMap.elems $ IntMap.intersectionWith (,) gridToMid griItemTags
    logLS l $ "Indexing " ++ T.unpack user
    logLS l $ show (length midsAndTags) ++ " tagged items"
    saveSearchTagged l user midsAndTags

clearOldFeedMasks u = do
    f <- readFilters' u
    when (has (fFeedMasks f) || any (has . ssFeedMasks) (fSmartStreams f)) $ do
        logS $ "Fixing " <> T.unpack u
        modifyFilters'_ u $ \ (migrateOldFilters -> f) -> return $
            f { fFeedMasks = fix $ fFeedMasks f
              , fNewSmartStreams = map fixSS $ fSmartStreams f
              }
    where has = not . IntMap.null . ffmOldFeedMasks
          fix f = f { ffmOldFeedMasks = IntMap.empty }
          fixSS s = s { ssFeedMasks = fix $ ssFeedMasks s }

filterStats = do
    fks <- riakBucketKeys "Filters"
    t <- getUrTime
    let paid PTUnknown = False
        paid pt = ptTill pt > t
        qSize fq = HM.size (fqFeeds fq) * T.length (fqQuery fq)
    n <- forM fks $ \ k -> do
        u <- readUser k
        f <- readFilters k
        return $ case (u, f) of
            (Just u, Just (migrateOldFilters -> f))
                | nf <- length $ fFilters f
                , ns <- length $ fSmartStreams f
                , sf <- sum $ map (qSize . filterQuery) $ fFilters f
                , ss <- sum $ map (qSize . ssQuery) $ fSmartStreams f
                , p <- paid $ uvmPaidTill $ uViewMode u
                , ns + nf > 0 ->
                    Just (nf + ns, nf, ns, k, p)
            _ ->
                Nothing
    mapM_ print $ sort $ catMaybes n
