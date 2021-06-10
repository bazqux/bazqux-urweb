{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards, TupleSections,
             BangPatterns, TransformListComp, ScopedTypeVariables, LambdaCase,
             PatternSynonyms, TypeFamilies
#-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module UrCalls
    ( subscriptionsAndSettings
    , subscriptionsAndViewMode
    , subscriptionsAndRenames
    , SubscriptionsAndRenames(..), SubscriptionsAndViewMode(..)
    , SubscriptionsAnd(..)
    , userGetOrdering
    , allTagsSortId, starredSortId, tagsFromGRIds
    , tagsForest, folderForest, smartStreamForest
    , filterForest, filterTagsForest
    , markReqReadCounters
    , ssFeeds
    , lookupCommentsReadSet
    , performBgActions, markBlogReadApi
    , EditItemTag(..), editItemTags
    , userSubscribe, userUnsubscribe, userEditSubscriptionFolders
    , userRenameSubscription, userRenameFolder, userDisableFolder

    , isUserExists
    , discover
    , userDiscoverySubscribe, userRetrySubscription
    , getFeedDetails
    , getTree
    , pageFromFile, addWebpackScripts, webpackStyles
    , escapeXbody
    , getFullText, readMsgAndApplyFixes
    , enablePublicFeed, disablePublicFeed, generateNewPublicFeed

    , runTasks, reloadBrowserPage

    , userAddToPocket, userAuthorizeAndAddToPocket

    , EditTree(..), mkEditTree, inEditTree, editMsgTree
    , fromRelUrl
    , fixFmLinks, feedMsgTime, fmMsgLink, feedMsgToItem
    , pTotalPosts, fixBrokenPostsTimes
    )
    where

import Control.Monad
import Control.Arrow
import Control.Applicative
import Data.List
import Data.Ix
import Data.Ord
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Short as SB
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Generated.DataTypes
import qualified Generated.DataTypes as DT
import Data.Binary
import Lib.UrTime
import qualified Lib.BArray as BA
import qualified Lib.Cache as Cache
import Lib.UnsafeRef
import Lib.Log
import Lib.DnsCache
import Data.Maybe
import URL
import Resolvables
import Riak
import Generated.RiakIO
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Lib.ReadSet as ReadSet
import Lib.ReadSet (ReadSet)
import qualified Data.Aeson as JSON
import Lib.Json
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text.Encoding as T
import qualified Data.Text.ICU.Convert as ICU
import Data.Hashable
import Control.Concurrent.Async
import qualified Control.Concurrent.MSem as MSem
import System.Random
import System.IO.Unsafe
import System.Process
import System.Info
import System.Timeout
import Control.Concurrent
import Network.HTTP.Conduit.Downloader
import qualified Control.Exception as E
import Auth
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as B
import Lib.Hash
import Lib.ReadUtils
import Lib.Regex
import Lib.Stats
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import Data.IORef
import Parser.Custom hiding (post)
import Parser.Types
import Network.BSD
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types as N
import Text.Printf
import Parser.Types (FeedItem(..))
import Discovery
import Search
import Lib.ElasticSearch
import Lib.StringConversion
import PageInfo
import Config
import UsageFlags
import Preprocess
import Lib.FastChar (emptyText)
import Mailer
import Subscriptions
import OPML
import Account
import Session
import Payments (invoiceLink, productInfo)

userSubscribe :: Key User -> TURL -> Maybe T.Text -> [T.Text] -> IO T.Text
userSubscribe key url title folders = do
    usageFlag key UFAddSubscription
    userAddSubscriptions key [
        Subscription
        { sUrl        = nUrl
        , sState      = SSAdded
        , sEditsCount = 0
        , sTitle      = title
        , sFolders    = folders
        }]
--    logT $ T.concat ["User ", key, " added subscription ", url]
    userEvent key "Add subscription" (maskUrlPasswordT url)
    return $ subscriptionUrlHash nUrl
    where nUrl = normalizeTURL url

userDiscoverySubscribe :: Key User -> TURL -> T.Text -> T.Text
                       -> Maybe T.Text -> [T.Text] -> IO T.Text
userDiscoverySubscribe key url country query title folders = do
    usageFlag key UFAddDiscoverySubscription
    mbp <- cachedReadPosts url
    case mbp of
        Just p -> do
            initPostsRead key p
            postsAddSubscriber url key True
        Nothing ->
            return ()
    u <- cachedReadUser' key
    performBgActions key [BGSetSubscriptionViewMode url $
                          (lookupDiscoveryMsgTreeViewMode u url)
                          { mtvmUnreadOnly = True }
                         ]
    userAddSubscriptions key [
        Subscription
        { sUrl        = url
        , sState      = if isJust mbp then SSFeed url -- уже готовая
                        else SSAdded
        , sEditsCount = 0
        , sTitle      = postsTitle =<< mbp
        , sFolders    = folders
        }]
    userEvent key "Add discovery subscription" $
        T.unlines [url, T.pack (show query), country]
    return $ subscriptionUrlHash url

getFeedDetails :: THostName -> Key User -> Key Posts
               -> IO (T.Text, Maybe TURL, Maybe T.Text, MsgTreeViewMode)
getFeedDetails readerHostName user url = do
    u <- cachedReadUser' user
    p <- cachedReadPosts' url
    return ( msgSubject (pRootMessage p)
           , msgLink (pRootMessage p)
           , Just $ faviconStyle' readerHostName $
             fromMaybe url $ msgLink (pRootMessage p)
           , lookupDiscoveryMsgTreeViewMode u url
           )

userRenameSubscription :: Key User -> TURL -> T.Text -> IO ()
userRenameSubscription user u t = do
    usageFlag user UFRenameSubscription
    modifyUser'_ user $ \ user -> return $ user { uSubscriptions =
        map upd $ uSubscriptions user }
    where upd s
            | sUrl s == u =
                s { sTitle = Just t
                  , sEditsCount = sEditsCount s + 1 }
            | otherwise = s

userRenameFolder :: Key User -> T.Text -> T.Text -> IO T.Text
userRenameFolder user f t = do
    usageFlag user UFRenameFolder
    let upd s
            | f `elem` sFolders s =
                s { sFolders = t : filter (`notElem` [f,t]) (sFolders s)
                  , sEditsCount = sEditsCount s + 1 }
            | otherwise = s
        updVM fvm = case HM.lookup f fvm of
            Just vm -> HM.insert t vm $ HM.delete f fvm
            Nothing -> fvm
    modifyUser'_ user $ \ user ->
        return $ user { uSubscriptions = map upd $ uSubscriptions user
                      , uViewMode =
                          (uViewMode user)
                          { uvmFolderViewModes =
                              updVM $ uvmFolderViewModes $ uViewMode user } }
    userModifyPublicFeed user $ \ pfm -> do
        let replace f t pfm =
                case Map.lookup f pfm of
                    Just x -> Map.insertWith (++) t x $ Map.delete f pfm
                    Nothing -> pfm
        return (replace (PFTSmartStream f) (PFTSmartStream t) $
                replace (PFTTag f) (PFTTag t) $
                replace (PFTFolder f) (PFTFolder t) pfm, ())
    ids <- modifyGRIds' user $ \ (moveRootOrdering -> i) -> do
        let s = f
            dest = t
            r = updateGRIds user [] [] $
                i { griLastId = griLastId i + 1
                  , griTaggedItems =
                      case Map.lookup (ITTag s) (griTaggedItems i) of
                          Nothing -> griTaggedItems i
                          Just ts ->
                              Map.alter (Just
                                         . taggedItemsWithGrItemIds
                                         . nubSortTaggedItems
                                         . taggedItemsWithMsgIds
                                         . (ts ++) . fromMaybe [])
                                        (ITTag dest) $
                              Map.delete (ITTag s) (griTaggedItems i)
                  , griItemTags = IntMap.map
                      (\ ts -> if ITTag s `elem` ts then
                                  nub $ map renTag ts
                               else ts)
                      (griItemTags i)
                  , griOrdering =
                      case Map.lookup s (griOrdering i) of
                          Nothing -> griOrdering i
                          Just o ->
                              Map.alter (Just . nub . (o ++) . fromMaybe [])
                                        dest $
                              Map.delete s (griOrdering i)
                  }
        return (r,r)
    tt <- case Map.lookup (ITTag t) (griTaggedItems ids) of
        Just items -> do
            withLogger $ \ l ->
                saveSearchTagged l user $
                    [ ( lmidFromGrItemId grid msgKey
                      , fromMaybe [] $ IntMap.lookup grid (griItemTags ids) )
                    | (_, grid, msgKey) <- items
                    ]
            return True
        Nothing ->
            return False
    ss <- modifyFilters' user $ \ (migrateOldFilters -> fs) ->
        case findSmartStream (Just f) fs of
            Just _ ->
                return
                    ( fs { fVersion = fVersion fs + 1
                         , fNewSmartStreams = map renSS $ fSmartStreams fs
                         }
                    , True )
            _ ->
                return (fs, False)
    return $ T.concat [ if tt then "tag/"
                        else if ss then "smartstream/" else "folder/"
                      , encodeURIComponentT t ]
    where renSS ss
              | ssName ss == f = ss { ssName = t }
              | otherwise = ss
          renTag (ITTag tn) | tn == f = ITTag t
          renTag x = x
          moveRootOrdering gri
              | Just o <- Map.lookup "" (griOrdering gri)
              , Just fi <- HM.lookup f (griFolderIds gri)
              , ti <- HM.lookupDefault (griLastId gri) t (griFolderIds gri) =
                  gri
                  { griLastId = max (griLastId gri) (ti+1)
                  , griOrdering = Map.insert ""
                                  (nub $ map (\i -> if i==fi then ti else i) o)
                                  (griOrdering gri)
                  , griFolderIds = HM.insert t ti (griFolderIds gri)
                  , griFolderNames = IntMap.insert ti t (griFolderNames gri)
                  }
              | otherwise = gri

newtype CmpMsgId = CmpMsgId MsgId
    deriving Eq

instance Ord CmpMsgId where
    compare = comparing $ \ (CmpMsgId (MsgId {..})) ->
        (Down midFeedId, midPostId, midCommentId)
        -- при сортировке начиная с новых, сначала видим новости в самых
        -- первых фидах (см. также cmpTime в feedsForest)

nubSortTaggedItems =
    filterDups IntSet.empty .
    sortBy (comparing $ \ (t,mid,_) -> Down (t,CmpMsgId mid))
    where filterDups _ [] = []
          filterDups s (i@(_,midGrItemId -> iid,_) : is)
          -- почему-то GHC 7.10.3 делает какой-то убер-тормозной код,
          -- если сделать !s и вызывать tagsForest в режиме groupByFeed
              | IntSet.member iid s = filterDups s is
              | otherwise = i : filterDups (IntSet.insert iid s) is

taggedItemsWithMsgIds ts =
    [(t, midFromGrItemId iid, k) | (t, iid, k) <- ts]
taggedItemsWithGrItemIds ts =
    [(t, midGrItemId mid, k) | (t, mid, k) <- ts]

userDisableFolder :: Key User -> T.Text -> IO ()
userDisableFolder user f = do
    let upd s
            | f `elem` sFolders s =
                s { sFolders = filter (/= f) (sFolders s)
                  , sEditsCount = sEditsCount s + 1 }
            | otherwise = s
    modifyUser'_ user $ \ user ->
        return $ user { uSubscriptions = map upd $ uSubscriptions user
                      , uViewMode =
                          (uViewMode user)
                          { uvmFolderViewModes =
                                HM.delete f $ uvmFolderViewModes $
                                uViewMode user } }
    userRemoveTagD user (Just [ITTag f]) 0
    fs <- cachedReadFilters' user
    case findSmartStream (Just f) fs of
        Just _ ->
            deleteSmartStream user f
        Nothing ->
            return ()

userRemoveTagD :: Key User -> Maybe [ItemTag] -> Int -> IO ()
userRemoveTagD user tags0 days = do
    gri <- getGRIds user
    t <- getUrTime
    let maxTime = t `plusUrTime` (-day * days)
        tags = Map.fromList $
            maybe [(t,()) | t@(ITTag _) <- Map.keys (griTaggedItems gri)]
              (map (,())) tags0
        ids = IntMap.fromListWith (\(t1,i) (t2,_) -> (Set.union t1 t2, i))
            [ (grId, (Set.singleton tag, lmidFromGrItemId grId msgKey))
              -- поскольку разные теги могуть быть поставлены в разное время
              -- для каждого сообщения запоминаем какие именно теги надо сносить
            | (tag, ts) <-
                Map.toList $ Map.intersection (griTaggedItems gri) tags
            , (time, grId, msgKey) <- ts
            , time < maxTime
            ]
--    fail $ show ids
    editItemTags' user (map snd $ IntMap.elems ids) $ \ i ->
        i
        { griLastId = griLastId i + 1
        , griTaggedItems =
            Map.differenceWithKey
            (\ tag is _ ->
                 let r = filter
                         (\(_,i,_) -> isNothing $ do
                             (t,_) <- IntMap.lookup i ids
                             guard (Set.member tag t)
                         ) is in
                 if null r then Nothing else Just r)
            (griTaggedItems i) tags
        , griItemTags =
            IntMap.differenceWithKey
            (\ grId ts (tagsToRemove, _) ->
                 let r = filter (`Set.notMember` tagsToRemove) ts in
                 if null r then Nothing else Just r)
            (griItemTags i) ids
        -- , griOrdering = Map.delete s (griOrdering i)
        }

userEditSubscriptionFolders :: Key User -> TURL -> T.Text -> Bool -> IO ()
userEditSubscriptionFolders user u folder add = do
    usageFlag user UFEditSubscriptionFolders
    modifyUser'_ user $ \ user -> return $ user { uSubscriptions =
        map upd $ uSubscriptions user }
    when add $
        modifyGRIds'_ user $ \ gri -> return $
            case Map.lookup folder (griOrdering gri) of
                Just o
                    | Just i <- HM.lookup u (griFeedIds gri)
                    , i `notElem` o ->
                        gri { griOrdering = Map.insert folder (o ++ [i])
                              (griOrdering gri) } -- ставим в конец
                    -- TODO: можно еще подписки через API и импорт
                    -- сразу в папки добавлять, хорошо бы обрабатывать
                _ -> gri
    where upd s
            | sUrl s == u =
                s { sFolders =
                        if add then union [folder] (sFolders s)
                        else filter (/= folder) (sFolders s)
                  , sEditsCount = sEditsCount s + 1 }
            | otherwise = s

userUnsubscribe :: Key User -> [TURL] -> IO ()
userUnsubscribe user urls = do
    usageFlag user UFUnsubscribe
    t <- getUrTime
    ps <- cachedReadManyPostss urls
    u <- cachedReadUser' user
    let urlsSet = HS.fromList urls
    modifyGRIds'_ user $ griUpdateRemovedFeeds t u urls ps
    removeSubscriptionsFromFilters user urls
    modifyUser'_ user $ \ user ->
        let uvm = uViewMode user in
        return $
        user
        { uSubscriptions =
            filter (not . (`HS.member` urlsSet) . sUrl) $ uSubscriptions user
        , uViewMode =
            uvm
            { uvmSubViewModes =
                  HM.difference (uvmSubViewModes uvm) $
                  HM.fromList $ zip urls $ repeat (1, defaultMsgTreeViewMode)
                  -- чистим режимы просмотра, чтобы в тегах/звездочках
                  -- показывать в режиме по-умолчанию.
            }
        }
    forM_ urls $ \ u -> postsAddSubscriber u user False
    userEvent user "Unsubscribe" (T.unlines $ map maskUrlPasswordT urls)

-- isScanning (sirCounters -> c) = c
--     case sState (siSubscription si) of
--     SSScanning _ -> True
--     SSAdded -> True
--     SSFeed _ -> siScannedPercent si /= 100
--     _ -> False
-- isSubscriptionsReady u = do
--     t <- getUrTime
--     (_, s, _) <- subscriptionsAndRenames t u
--     return $ filter isScanning s


type SubscriptionsAnd a
    = (Maybe (T.Text, T.Text, [T.Text]), T.Text, [SubItemRpc]
      ,Maybe (([(Int, FilterQueryRpc)], [(T.Text, FilterQueryRpc)]), T.Text)
      ,a)

type SubscriptionsAndRenames = SubscriptionsAnd [(UrTime, T.Text, T.Text)]
type SubscriptionsAndViewMode = SubscriptionsAnd UserViewMode

subscriptionsAndRenames
    :: THostName -> Bool -> UpdateFilters -> UrTime -> T.Text -> T.Text -> T.Text -> Key User
    -> IO SubscriptionsAndRenames
subscriptionsAndRenames readerHostName bgRefresh updateFilters t h siv fsh u = do
    (xml, siv', sis, fss, uvm) <- subscriptionsAndViewMode readerHostName True True bgRefresh updateFilters h siv fsh u
    gri <- cachedReadGRIds' u
    return (xml, siv', sis, fss,
            preprocessSubUrlRenames $
            takeWhile (\ (rt,_,_) -> rt >= t) $ uvmSubUrlRenames uvm)

preprocessSubUrlRenames =
    map (\(rt,f,t) -> (rt, subscriptionUrlHash f, subscriptionUrlHash t))

testSPerf = do
    t "m" $ modifyGRIds'_ "1" $ \ gri -> do
--         t "e" $ print $ BL.length $ encode gri
-- 1.4MB, get ~60msec, put 80-120msec, encode 30, decode
-- в итоге, у меня drag&drop по 0.3-0.5 секунд кушает на сохранение GRIds
-- поможет зипование
         return $ gri { griLastId = griLastId gri + 1 }
    o <- t "o" $ userGetOrdering uSubscriptions "1"
    ((Just (xml,th,fs), _, sirAll : _, _, _), _, _) <- t "s" $ subscriptionsAndSettings "" True True "1"
--    t "p" $ print sirAll
    t "xml" $ print $ T.length xml
--    print $ T.take 100 xml
    where t n a = withLogger $ \ l -> logTime l n a

userWelcomeState u us gri = do
    (du, ub) <- tryReadLastBackup u
    let (wsHasPrevAccount, wsHasPrevSubs)
            | Just b <- ub = (True, notNull (uSubscriptions $ ubUser b))
            | otherwise = (False, False)
        minT = usFirstSignInTime us
        hasBefore = any (\(t,_,_) -> t < minT)
        wsStarredRestored =
            maybe False hasBefore $
            Map.lookup ITStarred $ griTaggedItems gri
        wsTaggedRestored =
            any hasBefore [l | (ITTag _, l) <-
                                 Map.toList $ griTaggedItems gri]
    return $ WelcomeState {..}

subscriptionsAndSettings readerHostName updateHotLinks includeBps u = do
    uf <- cachedReadUserFilters' u
    ust <- cachedReadUserSettings' u
    us <- cachedReadUserStats' u
    (xml, siv, s, fss, uvm) <-
        subscriptionsAndViewMode readerHostName updateHotLinks includeBps False UFAll "" "" "" u
    gri <- cachedReadGRIds' u
    user <- cachedReadUser' u
    welcomeState <- userWelcomeState u us gri
    let fixUSTEx e =
            e { usteLastWhatsNewTime =
                  max (usFirstSignInTime us) (usteLastWhatsNewTime e)
              , ustePasswordHash = const "" <$> ustePasswordHash e
              , usteAssociatedAccounts =
                  filter (not . isLTFeverApiKey) (usteAssociatedAccounts e)
              }
    return
        ((xml, siv, s, fss, preprocessSubUrlRenames $ take 10 $ uvmSubUrlRenames uvm)
        , ( map snd $ ufFilters uf
          , snd $ uvmOnlyUpdatedSubscriptions uvm
          , modifyUSTEx fixUSTEx $
            ust { ustCountry = ustCountry ust <|>
                               lookup "Country" (usFirstSignInDetails us)
                , ustApiKeys = Nothing -- незачем их клиенту передавать
                })
        , ( welcomeState,
            sortBy (comparing $ \ (t,_,_) -> t)
                $ map paymentInfo $ uPayments user
          , maxPaidSubscriptions u
          , maxFiltersOrSmartStreams u
          )
        )
    where paymentInfo PReserved = (UrTime 0 0, "", "")
          paymentInfo (PFastSpring {..}) =
              (pOrderTime
              ,T.intercalate ", "
               [n | Just (_, n, _) <- map (productInfo pOrderId)
                   $ T.words pOrderType]
              ,invoiceLink pOrderId)

getFiltersAndSmartStreams fs gri prev
    | h == prev = Nothing
    | otherwise = Just (r, h)
    where h = showT $ hash r
          r = (map (\ f -> (filterId f, fqr $ filterQuery f)) $ fFilters fs
              ,map (\ ss -> (ssName ss, fqr $ ssQuery ss)) $ fSmartStreams fs)
          fqr (FilterQuery {..}) =
              FilterQueryRpc
              { fqrQuery = fqQuery
              , fqrNegate = fqNegate
              , fqrFeedGRIds =
                  map (`feedId` gri) $ HM.keys fqFeeds
              }

instance Hashable FilterQueryRpc where
    hashWithSalt s (FilterQueryRpc {..}) =
        s `hashWithSalt` fqrQuery
          `hashWithSalt` fqrNegate
          `hashWithSalt` fqrFeedGRIds

subscriptionUrlHash url =
    T.concat [ "subscription/"
             , encodeURIComponentT $ maskUrlPasswordT' m url ]
    where m t | p /= "" =
                T.append ":" $ encodeURIComponentT $
                T.filter (/= '=') $ base64_sha1T $ T.append url t
                -- заменяем "user:pwd" на ":hash", только если есть пароль,
                -- используем url как соль
              | otherwise = t
              where (_, p) = T.break (== ':') t

-- | Показывает, сколько процентов завершено
activeGRImports :: MVar (HM.HashMap (Key User) Int)
activeGRImports = unsafePerformIO $ newMVar HM.empty
{-# NOINLINE activeGRImports #-}

activeGRImportsCount = fmap HM.size $ readMVar activeGRImports
activeGRImportNames =
    fmap (T.intercalate "<br/>" . sort . HM.keys) $ readMVar activeGRImports
grImportPercent user =
    fmap (HM.lookup user) $ readMVar activeGRImports
setGrImportPercent user !p =
    modifyMVar_ activeGRImports $ \ m -> return (HM.insert user p m)
clearGrImportPercent user =
    modifyMVar_ activeGRImports $ \ m -> return (HM.delete user m)
tagImportPercent u = fromMaybe 100 <$> grImportPercent u

subItemsVersions :: MVar (HM.HashMap (Key User) [(T.Text,(UrTime, [SubItemRpc]))])
subItemsVersions = unsafePerformIO $ newMVar HM.empty
{-# NOINLINE subItemsVersions #-}

userGetFeedsPostsBPS t includeBps bgRefresh u = do
    user <- userCheckSubscriptions u
    let subscriptions = sortBy (comparing sUrl) (uSubscriptions user)
        feeds = catMaybes $ map ssFeed subscriptions
    when bgRefresh $
        clearRecaches (kvCache (undefined :: Posts)) feeds
        --  ^ чтобы не перекешировало лишний раз
        --  по идее, надо в feedsForest после этого recache делать
        --  чтобы пошустрее работало
    bpScanned' <-
        if includeBps then
            forkRead $ cachedNothingReadManyBlogPostsScanneds feeds
        else
            return $ return $ repeat Nothing
    -- где-то на 15% ускоряется на локальной машине
    (feedsPosts, postsReadList) <- readPostsAndFilteredPostsRead user feeds
    bpScanned <- bpScanned'

    let del = HS.fromList $
              map pBlogFeedUrl $ filter deletedPosts $ catMaybes feedsPosts
        mkAdded s
            | sUrl s `HS.member` del = s { sState = SSAdded }
            | otherwise = s
        readyFeeds =
            [ f | (f, bps) <- zip feeds bpScanned
            , maybe True ((==100) . blogPostsScannedPercent t) bps ]
        postsMap = HM.fromList [(pBlogFeedUrl p, p) | Just p <- feedsPosts]
        bigFeed feed
            | Just p <- HM.lookup feed postsMap
            , (a,b) <- BA.bounds $ mtHeaders $ pMsgTree p
            = b-a+1 > 100
            | otherwise = False
        (shortCache, longCache) = partition bigFeed readyFeeds
        -- где-то 2/3 подписок содержат <100 постов
        -- и при этом занимают раза в 4 меньше памяти.
        -- Что-то и памяти меньше от силы на 10%, CPU и число запросов в Riak
        -- не поменялись
        recachePosts = recacheKVs (kvCache (undefined :: Posts)) (Just rmContentHash)
        rmContentHash p@(Posts { pMsgTree = mt }) =
            p { pMsgTree = mt { mtHeaders = BA.amap rmch (mtHeaders mt) } }
            where rmch h = h { mhContentHash = "" }

    if not bgRefresh then do -- было 60 180, в среднем 2 минуты
        recachePosts shortCache 60 180 -- 20 40  -- 3/4 /4
        recachePosts longCache 100 380 -- 1/4 *2
    else
        recachePosts readyFeeds 20 30
        -- не забиваем память постами, если запрос от фонового обновления,
        -- но какое-то время все же держим, folderForest не так тормозил,
        -- если переключатся на вкладку и начнут читать
    recacheKVs (kvCache (undefined :: BlogPostsScanned))
               (Just (\bps -> bps { bpsUrls = Map.empty })) readyFeeds 60 600

    if del /= HS.empty then do
        -- каким-то макаром удалились посты? (параноидальная проверка)
        -- переподписываемся
        logS $ "Existing subscription was removed? "
                 ++ unwords (map show $ HS.toList del)
        modifyUser'_ u $ \ user ->
            return $ user { uSubscriptions = map mkAdded $ uSubscriptions user }
        userGetFeedsPostsBPS t includeBps bgRefresh u
    else
        return (user, subscriptions, postsReadList, bpScanned)

ssFeeds = fqFeeds . ssQuery

-- postsReadMap содержит (Posts, PostsRead) для всех фидов
-- (даже если нет PostsRead)
smartStreamCounters gri user postsReadMap ss =
    smartStreamCounters' user (ssName ss) postsReadMap
        (HM.size $ ssFeeds ss)
        (IntMap.toList $ ffmFeedMasks $ ssFeedMasks ss)
smartStreamCounters' user name postsReadMap numFeeds feedMasks =
    zeroCounters
    { cTotalPosts = sum $ map (\ (_,_,_,fm) -> ReadSet.size $ fmPosts fm) fms
    , cTotalComments = sum $ map imTotalComments tc
    , cReadPosts = sum $ map readPosts fms
    , cReadComments = sum $ map imTotalComments rc
    , cFeed = numFeeds
    , cScannedPercent = 100
    }
    where fms = mapMaybe fixfm feedMasks
          (tc,rc) = unzip $ map totalAndReadComments fms
          ssvm = folderViewMode name user
          expanded vm = mtvmExpandedComments
              $ if mtvmNoOverride ssvm then vm else ssvm
          fixfm x@(fid, fm)
              | Just (p, pr, vm) <- IntMap.lookup fid postsReadMap
                  = Just (p, pr, vm, fixFeedMask p fm)
              | otherwise = Nothing
          readPosts (_, pr, _, fm)
              | ReadSet.size (fmPosts fm) == 0 = 0
              | otherwise =
                  ReadSet.size $
                      fmPosts fm
                      `ReadSet.intersection`
                      (prSet pr `ReadSet.union` prIgnoredPosts pr)
          totalAndReadComments (p, pr, vm, fm)
              | expanded vm =
                  case fmComments fm of
                      Just comments ->
                         (IntMap.difference comments
                              $ readSetIntMap ReadSet.empty $ prIgnoredPosts pr
                         ,IntMap.intersectionWith ReadSet.intersection
                              comments (prCommentsRead pr))
                      Nothing ->
                         let pm = readSetIntMap () $ fmPosts fm
                             i s = IntMap.intersection s pm
                         in
                             (i $ commentsReadSets p pr, i $ prCommentsRead pr)
              | otherwise =
                  (IntMap.empty, IntMap.empty)

taggedItems gri tag =
    case tag of
        Just t -> maybe [] (map (\(_,i,_) -> i))
            $ Map.lookup t (griTaggedItems gri)
        Nothing ->
            [i | (i, ts) <- IntMap.toList $ griItemTags gri
            , ts /= [ITStarred]]

tagCounters gri user postsReadMap sp tag =
    tagCounters' gri user postsReadMap sp tag $ taggedItems gri tag
tagCounters' gri user postsReadMap sp tag items = go 0 0 0 0 items
    where tvm = tagViewMode tag user
          expanded vm = mtvmExpandedComments
              $ if mtvmNoOverride tvm then vm else tvm
          hasTag = case tag of
              Nothing -> not . null
              Just t -> elem t
          postTaggedExpanded fid pid vm
              | expanded vm
              , Just ts <- IntMap.lookup (grItemId fid pid Nothing)
                  $ griItemTags gri
              = hasTag ts
              | otherwise = False
          go tp tc rp rc [] =
              zeroCounters
              { cTotalPosts = tp
              , cTotalComments = tc
              , cReadPosts = rp
              , cReadComments = rc
              , cScannedPercent = sp
              }
          go !tp !tc !rp !rc ((midFromGrItemId -> mid@(MsgId {..})) : is)
              | Just (p, pr, vm) <- IntMap.lookup midFeedId postsReadMap =
                  case midCommentId of
                      Just cid
                          | postTaggedExpanded midFeedId midPostId vm ->
                              -- если среди тегов есть пост этого комментария,
                              -- с развернутыми комментариями, то не добавляем
                              -- комментарий в счетчики
                              go tp tc rp rc is
                          | otherwise ->
                              go tp (tc + 1) rp
                                  (rc + if isCommentRead midPostId p pr cid
                                      then 1 else 0)
                                  is
                      Nothing
                          | expanded vm ->
                              go (tp + 1)
                                  (tc + postCommentsCount midPostId maxBound p)
                                  rp'
                                  (rc + (ReadSet.size
                                       $ lookupCommentsReadSet midPostId
                                       $ prCommentsRead pr))
                                  is
                          | otherwise ->
                              go (tp + 1) tc rp' rc is
                          where rp' = if isPostRead midPostId p pr
                                      then rp+1 else rp
              | otherwise =
                  case midCommentId of
                      Just cid
                          | postTaggedExpanded midFeedId midPostId
                              defaultMsgTreeViewMode -> go tp tc rp rc is
                          | otherwise -> go tp (tc + 1) rp (rc + 1) is
                      Nothing  -> go (tp + 1) tc (rp + 1) rc is


tagReadCountersFromIds gri user tags ids = do
    u <- cachedReadUser' user
    tsp <- tagImportPercent user
    (_, postsReadList) <- readPostsAndFilteredPostsRead u
        $ filter (flip isActiveFeed gri)
        $ feedUrls gri $ IntSet.toList
        $ IntSet.map (midFeedId . midFromGrItemId) ids

    fs <- cachedReadFilters' user
    let !(Counters {..}) = tagCounters' gri u
            (IntMap.fromList $ postsReadList gri fs) tsp tag (IntSet.toList ids)
        tag = listToMaybe =<< tags

    return (tagGRId tag gri
           ,cReadPosts, cReadComments, cTotalPosts, cTotalComments)


tagViewMode :: Maybe ItemTag -> User -> MsgTreeViewMode
tagViewMode t = folderViewMode' dtvm $ case t of
    Just (ITTag t) -> t
    Just ITStarred -> ",SITStarred"
    Nothing -> ",SITAllTags"
    where dtvm = defaultMsgTreeViewMode { mtvmUnreadOnly = False }

tagGRId :: Maybe ItemTag -> GRIds -> Int
tagGRId t gri = case t of
    Just (ITTag t) -> folderId t gri
    Just ITStarred -> starredSortId
    Nothing -> allTagsSortId

tagFromGRId :: Int -> GRIds -> Maybe ItemTag
tagFromGRId i gri
    | i == starredSortId = Just ITStarred
    | i == allTagsSortId = Nothing
    | otherwise = Just $ ITTag $ folderName i gri

tagsFromGRIds :: GRIds -> [Int] -> [ItemTag]
tagsFromGRIds gri = mapMaybe (flip tagFromGRId gri)

-- | updateAllFilters
subscriptionsAndViewMode
    :: THostName -> Bool -> Bool -> Bool -> UpdateFilters
    -> T.Text -> T.Text -> T.Text -> Key User
    -> IO SubscriptionsAndViewMode
subscriptionsAndViewMode readerHostName updateHotLinks includeBps bgRefresh updateFilters titlesHash0 prevSubItemsVersion filtersHash u =
    E.mask $ \ restore -> do
        waitBucketKey bkey
        r <- newEmptyMVar
        forkIO $ do
            u <- E.try $ restore $ run
            putMVar r u
            releaseBucketKey bkey
            -- разблокируем именно в отдельной нити, чтобы фильтры обновились
            -- даже если API прибьет subscriptionsAndViewMode по таймауту
            -- (который равен 28сек, а фильтры могут обновляться 1.5-2 минуты)
            -- и следующие subscriptionsAndViewMode ждали и не перегружали
            -- систему
        s <- takeMVar r
        case s of
            Left (e :: E.SomeException) -> E.throwIO e
            Right Nothing -> fail "Timeout"
            Right (Just r) -> return r
    where bkey = ("subscriptionsAndViewMode", BL.fromStrict $ tbs u)
          run = timeout (10*60*1000*1000) $
              -- на всякий случай оставляем большой таймаут за который
              -- фильтры должны обновиться
              subscriptionsAndViewMode' readerHostName updateHotLinks includeBps bgRefresh updateFilters titlesHash0 prevSubItemsVersion filtersHash u

subscriptionsAndViewMode' readerHostName updateHotLinks includeBps bgRefresh updateFilters titlesHash0 prevSubItemsVersion filtersHash u = do

    t <- getUrTime
    let loopGet retries = do
            when (retries > 5) $
                fail "Too many retries due to edited filters or smart streams"
            fp@(_, _, prl, _) <- userGetFeedsPostsBPS t includeBps bgRefresh u
            fs0 <- cachedReadFilters' u
            gri <- getGRIds u
            let pprm = IntMap.fromList
                    [(i,(p,pr)) | (i,(p,pr,_)) <- prl gri (defaultFilters u)]
            mfs <- updateUserFeedMasks (not bgRefresh && updateHotLinks)
                updateFilters fs0 gri pprm u
            case mfs of
                Nothing -> do
                    logT $ "Filters changed, retrying subscriptionsAndViewMode for " <> u
                    loopGet (retries + 1)
                Just fs -> do
                    when (retries > 0) $
                        logS $ "There were " ++ show retries
                            ++ " retries due to config changes"
                    return (fp, gri, fs)

    ((user@(User {..}), subscriptions, postsReadList', bpScanned), gri, fs)
        <- loopGet 0

    tagSP <- tagImportPercent u
    ust <- cachedReadUserSettings' u
    let startIndex
            | null tags0 = 2
            | otherwise = 3
        postsReadList = postsReadList' gri fs
        postsReadMap = IntMap.fromList postsReadList
        ss = merge (zip [startIndex..] subscriptions)
            (zip postsReadList bpScanned)
        tagCounters' = tagCounters gri user postsReadMap tagSP
        addScanning c
            | tagSP < 100 = c { cScanning = cScanning c + 1 }
            | otherwise = c
        mkAllSir sirs =
            SubItemRpc
            { sirPath          = ""
            , sirIndex         = 0
            , sirTitle         = snd $ sitSymbolAndTitle SITAll
            , sirSIType        = SITAll
            , sirCounters      = addScanning $ sumCounters sirs
            , sirViewMode      = folderViewMode "" user
            , sirParentFolders = []
            , sirDomIds        = []
            , sirFaviconStyle  = Nothing
            , sirGRId          = allItemsSortId
            }
        starredSir =
            SubItemRpc
            { sirPath          = "starred"
            , sirIndex         = 1
            , sirTitle         = "Starred Items"
            , sirSIType        = SITStarred
            , sirCounters      = tagCounters' t
            , sirViewMode      = tagViewMode t user
            , sirParentFolders = []
            , sirDomIds        = []
            , sirFaviconStyle  = Nothing
            , sirGRId          = tagGRId t gri
            }
            where t = Just ITStarred
        allTagsSir =
            SubItemRpc
            { sirPath          = "tags"
            , sirIndex         = 2
            , sirTitle         = "Tags"
            , sirSIType        = SITAllTags
            , sirCounters      = tagCounters' t
            , sirViewMode      = tagViewMode t user
            , sirParentFolders = []
            , sirDomIds        = []
            , sirFaviconStyle  = Nothing
            , sirGRId          = tagGRId t gri
            }
            where t = Nothing
        mkTagSir i tn =
            SubItemRpc
            { sirPath          = T.concat [ "tag/", encodeURIComponentT tn ]
            , sirIndex         = i
            , sirTitle         = tn
            , sirSIType        = SITTag tn
            , sirCounters      = tagCounters' t
            , sirViewMode      = tagViewMode t user
            , sirParentFolders = []
              -- all tags не является папкой, т.к. не отображает сумму значений
            , sirDomIds        = []
            , sirFaviconStyle  = Nothing
            , sirGRId          = tagGRId t gri
            }
            where t = Just $ ITTag tn
        mkSmartStreamSir i ss =
            SubItemRpc
            { sirPath          = T.concat [ "smartstream/"
                                          , encodeURIComponentT t ]
            , sirIndex         = i
            , sirTitle         = t
            , sirSIType        = SITSmartStream t
                $ map sirIndex $ mapMaybe (`HM.lookup` feedUrlToSir)
                $ HM.keys $ ssFeeds ss
            , sirCounters      = smartStreamCounters gri user postsReadMap ss
            , sirViewMode      = folderViewMode t user
            , sirParentFolders = []
            , sirDomIds        = []
            , sirFaviconStyle  = Nothing
            , sirGRId          = folderId t gri
            }
            where t = ssName ss
        mkSSir i s sit c =
            SubItemRpc
            { sirPath          = subscriptionUrlHash $ sUrl s
            , sirIndex         = i
            , sirTitle         = snd $ sitSymbolAndTitle sit
            , sirSIType        = sit
            , sirCounters      = c
            , sirViewMode      = feedViewMode (sUrl s) user
            , sirParentFolders = [0] -- SITAll у нас всегда нулевая
            , sirDomIds        = []
            , sirFaviconStyle  = faviconStyle readerHostName sit
            , sirGRId          = feedId (sUrl s) gri
            }
        mkFolderSir i (f, sirs) =
            SubItemRpc
            { sirPath          = T.concat [ "folder/", encodeURIComponentT f ]
            , sirIndex         = i
            , sirTitle         = f
            , sirSIType        = SITFolder f
            , sirCounters      = sumCounters sirs
            , sirViewMode      = folderViewMode f user
            , sirParentFolders = []
            , sirDomIds        = []
            , sirFaviconStyle  = Nothing
            , sirGRId          = folderId f gri
            }
        merge [] _ = []
        merge ((i,s):ss) ps
            | isJust (ssFeed s)
            , (((_, (p, pr, _)), bps) : ps') <- ps
            , (sit, c, _) <- mkSITFeed gri fs False s p pr t bps
                = mkSSir i s sit c : merge ss ps'
            | otherwise
                = mkSSir i s (SITFeed s Nothing Nothing) -- Nothing Nothing Nothing Nothing Nothing)
                  (mkCounters s 0 0 0 0 100) : merge ss ps
--        ssMap0 = IntMap.fromDistinctAscList (zip [1..] ss)
        (rootSS, folders) = groupByFolder ss
        feedUrlToSir =
            HM.fromList
            [(sUrl fu, s) | s@(SubItemRpc { sirSIType = SITFeed fu _ _ }) <- ss]
        folderIndices = [length ss + startIndex..]
        folderSirs = [mkFolderSir i f | (i, f) <- zip folderIndices folders]
        tagIndices = [length ss + startIndex + length folders..]
        smartStreamIndices =
            [length ss + startIndex + length folders + length tags0..]
        smartStreams0 =
            sortBy (comparing $ fst)
            [(T.toLower $ ssName ss, ss) | ss <- fSmartStreams fs]
        smartStreamSirs =
            map (\ (i,sir) -> sir { sirIndex = i }) $
            zip smartStreamIndices $
            orderSirs "" $
            -- ?можно убрать сортировку, но тогда getUsedTags должен сортировать
            -- ?по domIds
            map (\ (_,ss) -> mkSmartStreamSir 0 ss) smartStreams0
        tags0 =
            map snd $ sort
            [(T.toLower t, t) | ITTag t <- Map.keys (griTaggedItems gri)]
        tagSirs =
            map (\ (i,sir) -> sir { sirIndex = i }) $
            zip tagIndices $
            orderSirs "" $
            -- можно убрать сортировку, но тогда getUsedTags должен сортировать
            -- по domIds
            map (mkTagSir 0) tags0
        order :: T.Text -> [SortSir] -> [SortSir]
        order f sortSirs = reorder (sirGRId . sortSir) gri f sortSirs
        orderSirs :: T.Text -> [SubItemRpc] -> [SubItemRpc]
        orderSirs f = map sortSir . order f . map SortSubItem
        -- без папок
        parentFolders = -- sir -> parents
             foldl' (\ m (i, (_, sirs)) ->
                         foldl' (\ m s -> IntMap.insertWith (++) (sirIndex s)
                                          [i] m) m sirs)
                    IntMap.empty ((2, ("", tagSirs))
                                  : zip folderIndices folders)
        insParents sir
            | Just p <- IntMap.lookup (sirIndex sir) parentFolders
                = sir { sirParentFolders = 0 : p }
            | otherwise = sir
        insDomIds sir
            | Just ids <- IntMap.lookup (sirIndex sir) domIds
                = sir { sirDomIds = ids }
            | otherwise = sir
        draggable = True -- u `elem` ["1", "demo"]
        sirsXml sirs acc = foldl' (flip (sirXml draggable)) acc sirs
        sirXml = sirXml' (ustExactUnreadCounts ust)
        ($>) = flip (.)
        folderXml (fsir, (f,sirs)) =
            sirXml draggable fsir $>
            sirXmlAdd (T.concat [ "<div class='folder "
                                , if mtvmFolderExpanded $ sirViewMode fsir
                                  then "folderExpanded" else "folderCollapsed"
                                , "' id=folder"
                                , T.pack $ show $ sirIndex fsir
                                , ">"]) $>
            sirsXml (orderSirs f sirs) $> sirXmlAdd "</div>"
        sortXml (SortFolder fsir sirs) = folderXml (fsir, (if sirSIType fsir == SITAllTags then "" else sirTitle fsir, sirs))
        sortXml (SortSubItem sir) = sirXml draggable sir
        rootXml acc = foldl' (flip sortXml) acc $
            order "" $
            [SortSubItem starredSir]
            ++ (if null tags0 then [] else [SortFolder allTagsSir tagSirs])
            ++ map SortSubItem smartStreamSirs
            ++ [ SortFolder fsir sirs
               | (fsir, (f,sirs)) <- zip folderSirs folders]
            ++ map SortSubItem rootSS
        (domIds, _, xml) =
            (sirXml False sirAll $> -- sirXml True starredSir $>
--             (if null tags0 then id else folderXml (allTagsSir, ("", tagSirs))) $>
--              sirXmlAdd "<div class='taggedSeparator'></div>" $>
             rootXml)
            (IntMap.empty, 0, [])
        sirAll = mkAllSir ss
        addAllTags = if null tags0 then id else (allTagsSir:)
        sirs = map insDomIds (sirAll : starredSir :
                              addAllTags
                              (tagSirs ++ map insParents ss ++ folderSirs
                               ++ smartStreamSirs))
        domSirs =
            map snd $ sortBy (comparing fst) $
            concatMap (\ s -> map (,s) (sirDomIds s)) sirs
        titlesHash =
            hashData $ T.encodeUtf8 $
            T.concat $ concatMap (\ s -> sirTitle s : map (T.pack . show) (sirParentFolders s)) domSirs
             -- порядок заголовков может не измениться, но может измениться
             -- уровень вложенности
--     TL.writeFile "../css/sublist_test.html" $ TL.unlines
--         [ "<head>"
--         , "<link href='basic.css' media='all' rel='stylesheet' type='text/css' />"
--         , "</head>"
--         , "<body class='noTouch'>"
--         , "<div class='left'> <div class='subscriptions onlyUnreadSubscriptions' id=subs>"
--         , xml
--         , "</div></div>"
--         , "<div id='right' class=right onclick='set()'></div>"
--         , "<script>"
--         , "  var xml = subs.innerHTML;"
--         , "  subs.innerHTML = xml;"
--         , "  right.innerHTML = '';"
--         , "  var start = 0;"
--         , "  function setFeed(x) {}"
--         , "  function set()"
--         , "  {"
--         , "//  console.time('someFunction timer');"
--         , "  start = new Date().getTime();"
--         , "  subs.innerHTML = xml;"
--         , "  setTimeout(function () {"
--         , "  var end = new Date().getTime();"
--         , "  var time = end - start;"
--         , "alert('Execution time: ' + time); }, 0);"
--         , "//  console.timeEnd('someFunction timer')"
--         , "  setTimeout(set, 5000);"
--         , "  }"
--         , "</script>"
--         , ""
--         , "</body>" ]
    let siv = hashData $ BL.toStrict $ encode sirs
        insSirs cached
            | Just (_, sirs0) <- lookup siv cached
            = (siv, (t, sirs0)) : filter ((/= siv) . fst) cached
              -- держим в кеше предыдущую версию, чтобы уменьшить кол-во мусора
            | otherwise
            = (siv, (t, sirs)) : take 10 cached
        fromApi = readerHostName == ""
        sameHash = titlesHash == titlesHash0
        modSirs acc s [] = reverse acc ++ s
        modSirs acc [] _ = reverse acc
        modSirs acc (s:ss) (s0:s0s)
            | s == s0 = modSirs acc ss s0s
            | otherwise = -- do
--                 logS $ show s
--                 logS $ show s0
                modSirs (s:acc) ss s0s
        sortedFolders = map sirTitle $ reorder sirGRId gri "" folderSirs
    sirs0 <- if fromApi then return Nothing else
        -- в случае доступа из API не нужно запоминать предыдущие версии,
        -- а главное, необязательно вычислять все sir-ы, если мы вызвали
        -- subscriptionsAndSettings только для обновления фильтров
        -- и кеширования (а то public feed-ы считают ненужные им данные для UI)
        modifyMVar subItemsVersions $ \ sivs0 -> do
            let expire = t `plusUrTime` (-600)
                sivs =
                    HM.alter (Just . insSirs . fromMaybe []) u
                    $ HM.mapMaybe (\ r ->
                        if any (\ (_,(t0,_)) -> t0 <= expire) r then
                            case filter (\ (_,(t0,_)) -> t0 > expire) r of
                                [] -> Nothing
                                x -> length x `seq` Just x
                        else
                            Just r)
                        sivs0
                prev =
                    fmap snd . lookup prevSubItemsVersion =<< HM.lookup u sivs0
            siv `seq` sivs `seq` return (sivs, prev)
    return ( if sameHash then Nothing
             else Just (T.concat $ reverse xml, titlesHash, sortedFolders)
           , siv
           , if not fromApi && siv == prevSubItemsVersion then
                 []
             else if sameHash then
                 modSirs [] sirs (fromMaybe [] sirs0)
             else
                 sirs
           , getFiltersAndSmartStreams fs gri filtersHash
           , uViewMode)

data SortSir
    = SortSubItem
      { sortSir :: SubItemRpc }
    | SortFolder
      { sortSir :: SubItemRpc
      , sortSirSub :: [SubItemRpc]
      }
sortSirGRId = sirGRId . sortSir

sirXmlAdd !x (!domIds, !i, !acc) = (domIds, i, x:acc)
sirXml' exactUnreads draggable sir (!domIds, !i, !acc) =
    ( IntMap.insertWith (++) (sirIndex sir) [i] domIds
    , i+1
    , cons $ T.concat
      [
--      , if tag then "<span class=tagLI>" else ""
        "<li title='", title, "' class='", cls, "' id='cls", is
      , "' onclick='bq.setFeed(", is, ")'"
      , if draggable then " draggable=true" else ""
      , "><span class=cursor></span>"
      , symbol
      , "<span class=buttonText dir=auto>", title, "</span>"
      , "<span id='uc", is, "' class='unreadCount sp"
      , si $ (cScannedPercent $ sirCounters sir) `div` 5 * 5
      , "'>", unread, "</span></li>"
--      , if tag then "</span>\n" else "\n"
--       , cls, "\n"
--       ,  title, "\t", unread, "\n"
      ]
    )
    where (sym, escapeHtmlT -> title) = sirSymbolAndTitle sir
          cons !t = t : acc
          symbol
              | Just fs <- sirFaviconStyle sir =
                  T.concat [ "<span id='fav", is, "' class='favicon' style='"
                           , fs, "'></span>"]
              | otherwise =
                  T.concat ["<span class='", sym, "'",toggleFolder, "></span>"]
          (cls, unread) = sirClsAndUnread exactUnreads sir
          is = si i
          si = T.pack . show
          toggleFolder
              | SITFolder _ <- sirSIType sir = tf
              | SITAllTags <- sirSIType sir = tf
              | otherwise = ""
          tag = case sirSIType sir of
              SITAllTags -> True
              SITStarred -> True
              SITTag _ -> True
              _ -> False
          tf = T.concat [ " onclick='uw_event=event;bq.toggleFolder("
                        , si i, ");return false;'"]

faviconStyle readerHostName siType
    | SITFeed {..} <- siType
    , grBfuPrefix `T.isPrefixOf` sUrl sitSubscription =
        Just $ faviconStyle' readerHostName "none"
    | SITFeed {..} <- siType
    , url <- fromMaybe (sUrl sitSubscription) sitFeedLink =
        Just $ faviconStyle' readerHostName url
    | otherwise = Nothing

faviconStyle' readerHostName url =
    T.concat ["background-image: url(\"", faviconUrl readerHostName hn, "\")"]
    where hn = fastHostNameFromUrl url

sirClsAndUnread exact sir =
    ( T.unwords [ cls | (cond, cls) <- classes, cond ]
    , if p == 0 && c == 0 then "0"
      else T.append
        (if not exact && p > 500 then "500+"
         else if p /= 0 then T.pack (show p) else "")
        (if c /= 0 then
             T.concat ["/"
                      , if not exact && c > 500 then "500+"
                        else T.pack (show c)]
         else ""))
    where cnt = sirCounters sir
          p = cTotalPosts cnt - cReadPosts cnt
          c | mtvmExpandedComments (sirViewMode sir) =
                cTotalComments cnt - cReadComments cnt
            | otherwise = 0
          folder
              | SITFolder _ <- sirSIType sir = True
              | otherwise = False
          tag = case sirSIType sir of
              SITStarred -> True
              SITAllTags -> True
              SITTag _ -> True
              _ -> False
          folderExpanded = mtvmFolderExpanded $ sirViewMode sir
          classes =
              [ (p==0 && c==0 && (cFeed cnt /= 0 || tag), "allItemsRead")
              , (p==0 && c==0 && cScanning cnt == 0, "noNewItems")
              , (p>0, "hasUnreadPosts")
              , (c>0, "hasUnreadComments")
              , (cScanning cnt /= 0, "subItemScanning")
              , (cError cnt /= 0, "subItemError")
              , (folder && folderExpanded, "folderExpanded")
              , (folder && not folderExpanded, "folderCollapsed")
              , (tag, "tagLI")
              ]

sirSymbolAndTitle = sitSymbolAndTitle . sirSIType
sitSymbolAndTitle st = case st of
    SITFeed { sitSubscription = s } ->
        ("iconFeed"
        ,fromMaybe (rmHttpPrefix $ maskUrlPasswordT $ humanReadableURL $ sUrl s) (sTitle s))
    SITFolder f -> ("iconFolder", f)
    SITSmartStream ss _ -> ("iconSmartStream", ss)
    SITSearch q -> (" ", q)
    SITAll -> ("iconLatest", "Latest")
    SITTag t -> ("iconTag", t)
    SITStarred -> ("iconStarred", "Starred Items")
    SITAllTags -> ("iconFolder", "Tags")

groupByFolder = go [] HM.empty
    where go root folders [] =
              (sortSirs root,
               map (\ (f, sirs) -> (f, sortSirs sirs)) $
               sortBy (comparing $ T.toLower . fst) $ HM.toList folders)
          go root folders (sir:sirs)
             | SITFeed { sitSubscription = s } <- sirSIType sir
             , sFolders s /= []
                 = go root (foldl' (\ fs f -> HM.insertWith (++) f [sir] fs)
                                   folders (sFolders s)) sirs
             | otherwise
                 = go (sir:root) folders sirs
          sortSirs sirs =
              map snd $
              sortBy (comparing fst)
              [(T.toLower (snd $ sirSymbolAndTitle s), s) | s <- sirs]

zeroCounters =
    Counters
    { cReadPosts        = 0
    , cReadComments     = 0
    , cTotalPosts       = 0
    , cTotalComments    = 0
    , cScanning         = 0
    , cScanningComments = 0
    , cError            = 0
    , cFeed             = 0
    , cScannedPercent   = 0
    }

mkCounters subscription rp rc tp tc sp =
    Counters
    { cReadPosts        = rp
    , cReadComments     = rc
    , cTotalPosts       = tp
    , cTotalComments    = tc
    , cScanning         = s
    , cScanningComments = sc
    , cError            = e
    , cFeed             = f
    , cScannedPercent   = sp
    }
    where (s,sc,e,f) =
              case sState subscription of
                  SSFeed _       -> (0, if sp /= 100 then 1 else 0, 0, 1)
                  SSError _      -> (0, 0, 1, 0)
                  SSErrorPath {} -> (0, 0, 1, 0)
                  _              -> (1, 0, 0, 0)

sumCounters = go 0 0 0 0 0 0 0 0 0
    where go rp rc tp tc s sc e f sp [] =
              Counters
              { cReadPosts        = rp
              , cReadComments     = rc
              , cTotalPosts       = tp
              , cTotalComments    = tc
              , cScanning         = s
              , cScanningComments = sc
              , cError            = e
              , cFeed             = f
              , cScannedPercent   = 100
              -- if sc > 0 then sp `div` sc else 100
              --  ^ неправильно считает
              }
          go !rp !rc !tp !tc !s !sc !e !f !sp
                 (SubItemRpc
                  { sirCounters = Counters rp' rc' tp' tc' s' sc' e' f' sp'
                  , sirViewMode = vm
                  } : cs)
             = go (rp+rp')
                  (if mtvmExpandedComments vm then rc+rc' else rc)
                  (tp+tp')
                  (if mtvmExpandedComments vm then tc+tc' else tc)
                  (s+s') (sc+sc') (e+e') (f+f') (sp+sp') cs

pTotalPosts = succ . snd . BA.bounds . mtHeaders . pMsgTree

mkSITFeed gri fs postsOnly s p pr t bps =
    mkSITFeed' gri fs postsOnly s p pr t bps
               (pTotalPosts p)
               (pTotalComments p)
mkSITFeed' gri fs postsOnly s p@(Posts {..}) (PostsRead {..}) t bps totalPosts totalComments =
    ( SITFeed {..}
    , mkCounters s
                 readPosts readComments
                 totalPosts totalComments siScannedPercent
    , SITFeedDetails {..} )
    where sitSubscription = s { sTitle = sTitle s <|> nothingIfEmpty siFeedTitle }
          (lo', hi) = BA.bounds $ mtHeaders pMsgTree
          lo = min lo' (hi+1)
--           totalPosts = hi+1
--           totalComments = pTotalComments
          readPosts = ReadSet.size prSet + ReadSet.size prIgnoredPosts + lo
          readComments
              | postsOnly = 0
              | otherwise = pDeletedComments +
                            prTotalCommentsRead + ignoredComments
              -- если удалятся посты, то за счет ignore total не должен меняться
              -- т.е. то, что при подписке мы обновляем только total, не должно
              -- нам мешать
          ignoredComments =
              sum $ map (commentsCount totalComments) $ IntMap.elems $
              IntMap.intersection pCommentCounts $
              IntMap.fromDistinctAscList $ map (, ()) $
              ReadSet.toList prIgnoredPosts
          siScannedPercent = maybe 100 (blogPostsScannedPercent t) bps
          ch = mtChildrenSet pMsgTree (-1)
          tp (TimeId t i) = MsgTreePoint (-1) t i
          -- пока не учитываем возможную вложенность (дерево при
          -- подписке на страницу жж)
          sitPointAllAsc  = fmap (tp . fst) $ Set.minView ch
          sitPointAllDesc = fmap (tp . fst) $ Set.maxView ch
          (sitPointUnreadDesc, sitPointUnreadAsc,
           sitPointUnreadPostsOnlyDesc, sitPointUnreadPostsOnlyAsc) =
              go Nothing Nothing Nothing Nothing
                 (totalPosts - readPosts) (totalComments - readComments)
                 (Set.foldl (flip (:)) [] ch)
                 -- toDescList, т.к. непрочитанные обычно последние
          go d a dp ap p c [] = (d, a, dp, ap)
          go !d !a !dp !ap !p !c (TimeId t i : tis)
              | i >= totalPosts = go d a dp ap p c tis -- пропускаем
              | p <= 0 && c <= 0 = (d, a, dp, ap)
              | otherwise =
                  go d' a' dp' ap' (p - unreadPosts) (c - unreadComments) tis
              where point = Just $ tp (TimeId t i)
                    (d',a',dp',ap')
                        | unreadPosts > 0 =
                            (d <|> point, point, dp <|> point, point)
                        | unreadComments > 0 =
                            (d <|> point, point, dp, ap)
                        | otherwise = (d,a,dp,ap)
                    unreadPosts
                        | i `ReadSet.member` prSet ||
                          i `ReadSet.member` prIgnoredPosts = 0
                        | otherwise = 1
                    postComments = fromMaybe 0 $ do
                        ccs <- IntMap.lookup i pCommentCounts
                        (cnt,_) <- IntMap.maxView ccs
                        return cnt
                    readComments
                        | postComments == 0 = 0
                        | otherwise =
                            fromMaybe 0 $ fmap ReadSet.size $
                            IntMap.lookup i prCommentsRead
                    unreadComments = postComments - readComments
          siFeedTitle = xmlText $ msgSubject pRootMessage
          sitFeedLink = msgLink pRootMessage


-- дерево и 25 сообщений и 825комментов (1.6Мб несжат/0.35 сжат):
--   coreader.exe 0.2s
--   beam.smp     0.6s
--   real         0.4s
-- т.е. теоретически с 4-х ядер можно 4000 сообщений и 1.75мб трафика в сек
-- по 10 сообщений будет где-то 400 rps и трафика побольше
-- типа 2000 пользователей онлайн

msgLimit (AMNormal {}) = 15
msgLimit (AMDiscovery {}) = 15
-- если делать больше, то в Firefox заметны тормоза при добавлении,
-- хотя он вообще тормозит знатно
msgLimit (AMGRIdsOnly {..}) = amCount

apiModeUI (AMNormal {}) = True
apiModeUI (AMDiscovery {}) = True
apiModeUI (AMGRIdsOnly {..}) = amFromUI

apiModePreprocesSettings = \ case
    AMNormal {..} ->
        PreprocessSettings (Just amHostName) True amAcceptLanguage Nothing
    AMDiscovery {..} ->
        PreprocessSettings (Just amHostName) True amAcceptLanguage Nothing
    AMGRIdsOnly {..} ->
        PreprocessSettings Nothing False "" amMaxMsgTextLength

apiModeMsgLinkParams (AMGRIdsOnly {..}) = amMsgLinkParams
apiModeMsgLinkParams _ = []

readMsgAndApplyFixes hostName acceptLanguage k = do
    m <- readMsg k
    case m of
        Just m ->
            Just <$> fixMessageContent
                (PreprocessSettings (Just hostName) False acceptLanguage Nothing)
                (\ _ _ _ -> "") <$> addAvatar (fixAuthorPic m)
        Nothing ->
            return Nothing

fixMessageAuthor pRootMessage m = fixAuthorPic $
    m { msgAuthor = if msgAuthor m /= "" then msgAuthor m
                    else msgAuthor pRootMessage
      , msgAuthorUri =
          msgAuthorUri m <|>
          emailLink (msgAuthorEmail m) <|>
          do guard (msgAuthor m == "")
             msgAuthorUri pRootMessage <|> msgLink pRootMessage
      , msgAuthorPic =
          msgAuthorPic m <|> msgAuthorPic pRootMessage
      }
fixAuthorPic m
    | Just _ <- msgAuthorPic m = m
    | Just au <- msgAuthorUri m
    , not ("mailto:" `T.isPrefixOf` au) = m
    | msgAuthorEmail m /= "" =
        m { msgAuthorPic = Just $ T.concat
              [ "https://secure.gravatar.com/avatar/"
              , md5 $ T.toLower $ msgAuthorEmail m
              , "?d=404" ] }
    | otherwise = m

addAvatar m
    | isJust $ msgKeyCommentGuid $ msgKey m =
        return m
        -- не пытаемся искать avatar-ки по ссылкам с комментов,
        -- вдруг спам какой.
    | otherwise = do
        p <- cachedReadPosts' (msgKeyBlogFeedUrl $ msgKey m)
        addAvatar' (pRootMessage p) m

addAvatar' rootMsg m
    | msgAuthorPic m == Nothing
    , Just u <- msgAuthorUri m <|> msgLink rootMsg
    = do
        l <- getAvatarFromLinkLazy u
        return $ m { msgAuthorPic = l }
    | otherwise = return m

addMsgLinkParams am m
    | p <- apiModeMsgLinkParams am
    , notNull p
    = m { msgLink = fmap (urlAddParamsT p) $ msgLink m }
    | otherwise = m

unjsMh mh =
    mh
    { mhSubject = xmlText $ mhSubject mh
    , mhAuthor = xmlText $ mhAuthor mh
    }

msgHeaderFromMsg (Msg { msgKey = MsgKey {..}, ..}) =
    mhShortText `seq` MsgHeader {..}
    where mhGuid =
              fromMaybe (tsb msgKeyBlogFeedUrl) $
              msgKeyCommentGuid <|> msgKeyPostGuid
          mhContentHash = ""
          mhAuthor = msgAuthor
          mhAuthorPic = msgAuthorPic
          mhSubject = msgSubject
          mhTime = msgTime
          mhDlTime = msgDlTime
          mhShortText = msgShortText

emailLink "" = Nothing
emailLink e  = Just $ T.append "mailto:" e

data PostOrComment
    = POCPost TURL
    | POCComment TURL SB.ShortByteString Int
    deriving Show

dlTimeOk apiMode mh
    | AMGRIdsOnly {..} <- apiMode =
        maybe True (mhDlTime mh >=) amMinDlTime &&
        maybe True (mhDlTime mh <=) amMaxDlTime &&
        maybe True (fromMaybe (mhDlTime mh) (mhTime mh) <=)
                   amMaxTime
    | otherwise = True

readMsgSem :: MSem.MSem Int
readMsgSem = unsafePerformIO $ MSem.new 8
{-# NOINLINE readMsgSem #-}

preprocessMsgSem :: MSem.MSem Int
preprocessMsgSem = unsafePerformIO $ MSem.new 4
{-# NOINLINE preprocessMsgSem #-}

inFeedMask pid cid fm =
    case (cid, fmComments fm) of
        (Just c, Just cs) ->
            maybe False (ReadSet.member c) $ IntMap.lookup pid cs
        _ ->
            ReadSet.member pid (fmPosts fm)
            -- если не было фильтра по комментариям,
            -- то видны все комментарии отфильтрованных постов

commentsMask pid fm cc =
    case fmComments fm of
        Just cs ->
            IntMap.findWithDefault ReadSet.empty pid cs
        _ ->
            if ReadSet.member pid (fmPosts fm) then
                ReadSet.fromRange 0 (cc-1)
            else
                ReadSet.empty

-- | Пробегается по элементам Set-а из mtChildren[mtpParentId]
loopMt u fs gri apiMode limit root shortTextFunc fullView ascending unreadOnly mkNextReq postOrComment mt isRead isOutOfBounds isTagged mask f point cnt = do
--    print $ order $ mtChildrenSet mt $ mtpParentId point
    go cnt [] Nothing (order $ mtChildrenSet mt $ mtpParentId point)
    where order | ascending =
                    Set.toAscList . snd . Set.split (pointTimeId (-1))
                    -- где-то на 10-20% быстрее при синхронизации
                | otherwise =
                    Set.toDescList . fst . Set.split (pointTimeId 1)
          pointTimeId inc = TimeId (mtpTime point) (mtpId point + inc)
          mLimit = msgLimit apiMode
          go cnt acc next _
             | root && isJust next = return (cnt, reverse acc, next)
               -- для постов не важно общее кол-во, оно известно заранее
          go cnt acc next [] = return (cnt, reverse acc, next)
          go cnt acc next (i@(TimeId time idx) : is)
             | isOutOfBounds idx && not (isTagged idx) = go cnt acc next is
             | AMGRIdsOnly {..} <- apiMode
             , Just minDlTime <- amMinDlTime
             , not ascending
             , mh <- mtHeaders mt BA.! idx
             , mhDlTime mh < minDlTime
             , fromMaybe (mhDlTime mh) (mhTime mh) < minDlTime =
                 --  ^ а ведь mhTime не может быть больше dlTime
                 return (cnt, reverse acc, Nothing)
             | otherwise = do
--                 print i
                 let mh = mtHeaders mt BA.! idx
                     guid = mhGuid mh
                     activeFeed
                         | AMDiscovery {} <- apiMode = True
                         | otherwise = isActiveFeed bfu gri
                     filteredOut
                         | Just fm <- IntMap.lookup fid
                             $ ffmFeedMasks $ fFeedMasks fs
                             = not $ inFeedMask pid cid fm
                         | otherwise = False
                     readLocked =
                         not activeFeed || filteredOut || isOutOfBounds idx
                     read
                         | readLocked = True -- всегда прочитанные
                         | otherwise = isRead idx
                     (pid, cid, mkey) = case postOrComment of
                         POCPost bfu ->
                             (idx, Nothing, MsgKey bfu (Just guid) Nothing)
                         POCComment bfu pg pidx ->
                             (pidx, Just idx, MsgKey bfu (Just pg) (Just guid))
                     bfu = msgKeyBlogFeedUrl mkey
                     fid = feedId bfu gri
                     itemSmartStreams =
                         IntSet.fromList
                         [ folderId (ssName ss) gri | ss <- fSmartStreams fs
                         , isNothing (msgKeyCommentGuid mkey)
                           || expandedInSmartStream u bfu (ssName ss)
                         , let fms = ffmFeedMasks $ ssFeedMasks ss
                         , fm <- maybeToList $ IntMap.lookup fid fms
                         , inFeedMask pid cid fm ]
                     itemTags = IntMap.findWithDefault []
                         (grItemId fid pid cid) (griItemTags gri)
                     itemTagsUI =
                         (if null [() | ITTag _ <- itemTags]
                          then id else (Nothing :))
                         $ map Just itemTags
                     nextPoint = point { mtpTime = time, mtpId = idx }
                     checkMkey
                         | AMGRIdsOnly {..} <- apiMode = do
                             let ref = unsafeRef amContinuation
                             c <- readIORef ref
                             case c of
                                 Just m
                                     | m == mkey && limit nextPoint -> do
--                                         logS "MsgKey found, but need to swith to other feed first"
                                         return False
                                     | m == mkey -> do
--                                         logS "MsgKey found!"
                                         writeIORef ref Nothing
                                         return False
                                     | otherwise ->
                                         return False
                                 Nothing ->
                                     return $
                                         ((amReadOnly && read)
                                          || not amReadOnly) &&
                                         ((HS.size amExcludeTags /= 0 &&
                                          all (not . (`HS.member` amExcludeTags))
                                              itemTags)
                                          || HS.size amExcludeTags == 0) &&
                                         ((HS.size amIncludeTags /= 0 &&
                                          any (`HS.member` amIncludeTags)
                                              itemTags)
                                          || HS.size amIncludeTags == 0)
                         | otherwise = return True
                 checked <- checkMkey
                 let isFull'
                         | unreadOnly = not read
                         | otherwise = True
                     inMask m = ReadSet.member idx m || isTagged idx
                     isFull
                         | Just m <- mask = inMask m && isFull'
                         | otherwise = isFull'
                     searchResult
                         -- маска может быть в фильтре, а может быть в поиске
                         -- надо это отличать
                         -- хотя в фильтре нет subItem с результатами поиска
                         -- и обновляться он не будет, но всё равно некрасиво
                         | Just m <- mask = inMask m
                         | otherwise = False
                     full = checked && dlTimeOk apiMode mh && isFull
                     cnt' = mfcSucc cnt full read itemSmartStreams itemTagsUI
                     nextReq = Just $ mkNextReq nextPoint
                     -- следующая точка -- существующее сообщение
                     retLimit = return (cnt, reverse acc, nextReq)
                 if full && limit nextPoint then
                     retLimit -- не ищем комментарии
                 else if limit nextPoint then do
                     (cnt'', _, _) <- f guid (ascMtp idx)
                                      (cnt' { mfcFull = mLimit - 1 })
                                      -- не более одного комментария
                     if mfcFull cnt'' /= mfcFull cnt' then
                         retLimit -- есть комментарии, пост подходит
                     else
                         go cnt acc next is -- игнорируем пост,
                         -- ищем следующий подходящий
                 else do
                     (cnt'', subForest, subNext) <- f guid (ascMtp idx) cnt'
                        -- вложенные элементы у нас всегда ascending
                     let ignore = mfcFull cnt'' == mfcFull cnt ||
                                  mfcFull cnt' > mLimit
                         msgItem mv =
                             MsgItem
                             { miMsgView      = mv
                             , miMsgKey       = mkey
                             , miMsgId        = MsgId fid pid cid
                             , miRead         = read
                             , miTags         = reverse $ itemTags
                               -- показываем в обратном порядке, чтобы было
                               -- также, как добавляли
                             , miSmartStreams = itemSmartStreams
                             , miReadLocked   = readLocked
                             , miFull         = isFull
                             , miSearchResult = searchResult
                             }
                     m <-
                         if ignore then return $ error "ignored item?"
                         else item idx msgItem mkey full activeFeed
--                      logS $ show mkey
--                      logS $ show cnt ++ " -> " ++ show cnt''
--                      logS $ show subForest
                     let duc :: ((Int -> Int -> Maybe Int) -> a -> a -> a)
                             -> (MFCnt -> a) -> a
                         duc d f = d
                                 (\ a b -> if a /= b then
                                     Just (a - b) else Nothing)
                                 (f cnt'') (f cnt')
                         dc f = f cnt'' - f cnt'
                         mnf = (m, MsgForest
                                     (dc mfcTotal)
                                     (dc mfcUnread)
                                     (dc mfcTotalResults)
                                     (dc mfcUnreadResults)
                                     (duc IntMap.differenceWith
                                         mfcSmartStreamUnreadCounts)
                                     (duc IntMap.differenceWith
                                         mfcSmartStreamUnreadResultCounts)
                                     (duc Map.differenceWith
                                         mfcTagTotalCounts)
                                     (duc Map.differenceWith
                                         mfcTagUnreadCounts)
                                     (duc Map.differenceWith
                                         mfcTagUnreadResultCounts)
                                     subForest
                                     subNext)
                     go cnt''
                        (if ignore then acc else (mnf:acc))
                        (case next of
                           Nothing | mfcFull cnt' > mLimit -> nextReq
                           _ -> next)
                        is
          headers = mtHeaders mt
          checkRemoved True m = m
          checkRemoved _ m
              | AGrOrigin {} : _ <- msgAttachments m = m
              | u <- msgKeyBlogFeedUrl (msgKey m)
              , Just rfs <- griRemovedFeeds gri
              , Just rfi <- HM.lookup u rfs =
                  m { msgAttachments =
                          AGrOrigin u "" (rfiStreamTitle rfi) (rfiHtmlUrl rfi)
                          : msgAttachments m }
              | otherwise = m
          unsafeAsync act
              | AMGRIdsOnly { amCount = c } <- apiMode
              , c > 50 = act
                  -- отключаем параллельность для больших API-запросов
                  -- (/feed/…?n=100, Vienna, синхронизация Feeddler/FeedMe),
                  -- дабы не грузить систему там, где скорость не так важна.
              | otherwise = do
                  -- с unsafeAsync даже на закешированных данных и локальной БД
                  -- работает в 2 раза быстрее (но на 30% больше CPU)
                  a <- async act
                  return $ unsafePerformIO $ wait a
          item idx msgItem mkey full activeFeed
              | AMGRIdsOnly { amFetch = False, .. } <- apiMode = do
                  -- не читаем сообщения
                  return $ msgItem $ MVShort (headers BA.! idx) Nothing
              | otherwise = unsafeAsync $ do
                  -- читаем Msg для всего, не так часто попадаются
                  -- цепочки прочитанных комментариев, зато это позволяет
                  -- упростить клиентский код и не перевычислять
                  -- ссылки/заголовки
                  -- для постов или тегов получаем MsgHeader из Msg
                  let MsgKey bfu p c = mkey
                      progress x = return () -- logS $ show (bfu, p, c) <> " " <> x
                  progress "read"
                  m0 <- MSem.with readMsgSem $
                      (if apiModeUI apiMode then cachedReadMsg'
                       else readMsg')
                       -- читаются один раз при синхронизации, нефиг
                       -- ими память забивать
                      mkey
                  progress "addAvatar"
                  ma <- MSem.with readMsgSem $ addAvatar m0
                  progress "preprocess"
                  let m = checkRemoved activeFeed $
                          addMsgLinkParams apiMode $
                          fixMessageContent
                              (apiModePreprocesSettings apiMode)
                              shortTextFunc $
                          fixAuthorPic ma
                  MSem.with preprocessMsgSem $ m `seq` return ()
                  let !mh = msgHeaderFromMsg m
                      mv | (full && fullView) || not (apiModeUI apiMode) = MVFull m
                         | otherwise =
                             MVShort mh (Just m)
                  progress "done"
                  return $ msgItem mv
--               | otherwise = do
--                   let mh = fsMH full searchResult $
--                            unjsMh (headers BA.! idx)
--                   mv <- case () of
--                       _ | (full) -- && mfcFull cnt <= mLimit
--                             -> do
--                             m <- fmap (fsMsg full searchResult .
--                                        checkRemoved activeFeed . rmDebug .
--                                        addMsgLinkParams apiMode .
--                                        fixMessageContent (apiModePreprocesSettings apiMode) .
--                                        fixAuthorPic) $
--                                  cachedReadMsg' mkey
--                             m <- addAvatar m
--                             return $ if full && fullView then MVFull m
--                                      else MVShort mh (Just m)
-- --                         | full && cnt == mLimit -> return $ MVFull limitMsg
--                         | otherwise -> return $ MVShort mh Nothing
--                   return $ msgItem mv

commentsForest apiMode taggedComment gri crq@(CommentsReq {..}) user smartStream feedMasks (MsgTreeViewMode {..}) cnt = do
  let withComments f = do
          mbc <- cachedReadComments crqKey
          case mbc of
              Nothing -> do
                  logS $ "Comments not found for " ++ show (user, crq)
                  return (cnt, [], Nothing)
              Just c ->
                  f c
  withComments $ \ Comments {..} -> do
    p <- cachedReadPosts' (ckBlogFeedUrl crqKey)
    pr <- cachedNothingReadPostsRead' (user, ckBlogFeedUrl crqKey)
    us <- cachedReadUserSettings' user
    fs <- cachedReadFilters' user
    u <- cachedReadUser' user
    let mask = do
            fm <- fidFeedMask feedMasks (feedId (ckBlogFeedUrl crqKey) gri)
                smartStream fs
            return $ commentsMask crqPostId fm (hi+1)
        (msgTree, isTagged, onlyRoot)
            | Just (t, cid) <- taggedComment =
                (cMsgTree
                 { mtChildren =
                     IntMap.singleton (-1) $ Set.singleton (TimeId t cid)
                 }
                ,(== cid)
                ,True)
            | otherwise =
                (cMsgTree, const False, False)
        isRead = isCommentRead crqPostId p pr
        isOutOfBounds
            | isPostOutOfBounds crqPostId p = const True
            | otherwise = \ idx -> idx < lo || idx > hi
        (lo, min (crqTotalComments-1) -> hi) = BA.bounds $ mtHeaders cMsgTree
        comments =
            loopMt u fs gri apiMode (const False) onlyRoot
                   (shortTextFunc us $ if onlyRoot then mtvmPosts else PVMFull)
                   (if onlyRoot then mtvmPosts == PVMFull else True)
                   True -- ascending
                   mtvmUnreadOnly
                   (\ p -> trComments False smartStream
                           $ crq { crqMsgTreePoint = p })
                   (POCComment (ckBlogFeedUrl crqKey)
                               (ckPostGuid crqKey) crqPostId)
                   msgTree isRead isOutOfBounds isTagged mask $ \ _guid ->
                       if not onlyRoot then comments
                          -- TODO: пробегаться, считать число комментов?
                       else \ point cnt -> return ( cnt, [], Nothing)
    r@(c,_,_) <- comments crqMsgTreePoint cnt
--     logS $ show crq
--     logS $ show cnt
--     logS $ show c
    return r

trComments e Nothing r = TRComments e r
trComments e (Just n) r = TRCommentsS e n r

shortTextFunc us mtvmPosts
    | ustListViewMode us == LVMCompact && mtvmPosts == PVMShort =
        \ shorter s -> firstWords ((if shorter then 190 else 375) - toEnum (T.length s))
    | mtvmPosts == PVMMosaic =
        \ _ _ -> firstParagraph 100
    | mtvmPosts == PVMMagazine =
        \ shorter s -> firstParagraph (if shorter then 100 else 300 - toEnum (T.length s))
    | otherwise =
        \ _ _ -> firstWords 66

expandedInSmartStream u bfu n = mtvmExpandedComments $
    if mtvmNoOverride svm then feedVM else svm
    where svm = folderViewMode n u
          feedVM = feedViewMode bfu u

postsForest taggedPost gri apiMode limit prq@(PostsReq {..}) user smartStream feedMasks viewMode@(MsgTreeViewMode {..}) cnt = do
    let bfu = feedUrl prqFeedId gri
    p@(Posts {..}) <- cachedReadPosts' bfu
    pr <- cachedNothingReadPostsRead' (user, bfu)
    us <- cachedReadUserSettings' user
    fs <- cachedReadFilters' user
    u <- cachedReadUser' user
    let dummy = smartStream :: Maybe T.Text
        (msgTree, isTagged)
            | Just (t, pid, mh) <- taggedPost =
                -- для помеченного поста выдаем только его
                (MsgTree
                 { mtChildren =
                     IntMap.singleton (-1) $ Set.singleton (TimeId t pid)
                 , mtHeaders = BA.listArray (pid, pid) [mh]
                 }
                ,(== pid))
            | otherwise =
                (pMsgTree, const False)
        feedMask = fidFeedMask feedMasks prqFeedId smartStream fs
        filtersFeedMask = fidFeedMask Nothing prqFeedId Nothing fs
        isRead idx = isPostRead idx p pr
        isOutOfBounds idx = idx < lo || idx > hi
        mLimit = msgLimit apiMode
        (lo, min (prqTotalPosts-1) -> hi) = BA.bounds $ mtHeaders pMsgTree
        posts root =
            loopMt u fs gri apiMode limit
                   root (shortTextFunc us mtvmPosts)
                   (mtvmPosts == PVMFull)
                   mtvmAscending mtvmUnreadOnly
                   (\ p -> TRPosts [prq { prqMsgTreePoint = p }])
                   (POCPost bfu)
                   msgTree isRead isOutOfBounds isTagged
                   (fmap fmPosts feedMask) $ \ pguid point cnt ->
                let idx = mtpParentId point in
                case (apiMode, IntMap.lookup idx pCommentCounts) of
                    (apiModeUI -> True, Just ccs) -> do
                        let cc = commentsCount prqTotalComments ccs
                            ckey = CommentsKey bfu pguid
                            mkey = MsgKey bfu (Just pguid) Nothing
                            rs  | ignored = ccRS
                                | otherwise =
                                    lookupCommentsReadSet idx
                                        (prCommentsRead pr)
                            urs -- unread set
                                | ignored = ReadSet.empty
                                | otherwise = ReadSet.difference ccRS rs
                            ccRS = ReadSet.fromRange 0 (cc-1)
                            ignored = ReadSet.member idx (prIgnoredPosts pr)
                            (unreadCC, totalCC, unreadCommentsSet)
                                | Just fm <- filtersFeedMask
                                  -- Skip всегда пропускают все комментарии,
                                  -- а не только результаты поиска/smart stream
                                  -- Используем только маски фильтров
                                , m <- commentsMask idx fm cc
                                , unreadCSet <- ReadSet.difference m rs
                                , ucc <- ReadSet.size unreadCSet
                                =   (ucc
                                    ,ReadSet.size m
                                    ,unreadCSet)
                                | otherwise =
                                    let ucc = cc - ReadSet.size rs in
                                    (ucc
                                    ,cc
                                    ,urs)
                            (totalResultCC, unreadResultSet)
                                | Just fm <- feedMask
                                , m <- commentsMask idx fm cc =
                                    (ReadSet.size m, ReadSet.difference m rs)
                                | otherwise =
                                    (totalCC, unreadCommentsSet)
                            unreadResultCC = ReadSet.size unreadResultSet
                            fullCC =
                                if mtvmUnreadOnly then
                                    unreadResultCC
                                else
                                    totalResultCC
                            ssucs = mkSSC unreadCommentsSet
                            ssurcs = mkSSC unreadResultSet
                            mkSSC set
                                | ReadSet.null set = IntMap.empty
                                | otherwise =
                                    IntMap.fromList
                                    [(folderId (ssName ss) gri, c)
                                    | ss <- fSmartStreams fs
                                    , expandedInSmartStream u bfu (ssName ss)
                                    , fm <- maybeToList
                                        $ IntMap.lookup prqFeedId
                                        $ ffmFeedMasks $ ssFeedMasks ss
                                    , let c = ReadSet.size $
                                              ReadSet.intersection set
                                                  (commentsMask idx fm cc)
                                    , c > 0
                                    ]
                            mkTC' checkCid =
                                Map.fromListWith (+)
                                [(t, 1)
                                | (icid -> Just cid, ts) <- IntMap.toList
                                    $ postCommentsItemTags fid idx gri
                                , checkCid cid
                                , t <-
                                     (if any isITTag ts then (Nothing :)
                                      else id)
                                     $ map Just ts]
                            mkTC set
                                | ReadSet.null set = Map.empty
                                | otherwise = mkTC' (`ReadSet.member` set)
                            ttcs = mkTC' $ const True
                            tucs = mkTC unreadCommentsSet
                            turcs = mkTC unreadResultSet
                            fid = feedId bfu gri
                            icid = midCommentId . midFromGrItemId
                            req =
                                CommentsReq
                                { crqKey = ckey
                                , crqPostId = idx
                                , crqMsgTreePoint = ascMtp (-1)
                                , crqTotalComments = cc
                                }
                            cnt' numFull =
                                mfcPlus cnt
                                    totalCC unreadCC
                                    totalResultCC unreadResultCC
                                    ssucs ssurcs ttcs tucs turcs
                                    numFull
                        if fullCC <= 0 then
                            return (cnt' 0, [], Nothing)
                        else if not mtvmExpandedComments then
                            -- даже в поиске оставим возможность раскрывать
                            -- комментарии
                            return (cnt' 0, []
                                   ,Just $ trComments True smartStream req)
                        else if mfcFull cnt >= mLimit then
                            return (cnt' fullCC, []
                                   ,Just $ trComments False smartStream req)
                        else
                            commentsForest apiMode Nothing gri req user smartStream feedMasks viewMode cnt
                    _ ->
                        posts False point cnt
    posts True prqMsgTreePoint cnt

feedsForest gri apiMode prqs user smartStream feedMasks viewMode@(MsgTreeViewMode {..}) cnt = do
    u <- cachedReadUser' user
    let go cnt acc [] =
            return (cnt, reverse acc, Nothing)
        go cnt acc prqs
            | mfcFull cnt >= msgLimit apiMode =
                return (cnt, reverse acc, Just $ TRPosts prqs)
        go cnt acc [rq] = do -- вырожденный случай с одним запросом
            (cnt', sf, n) <-
                postsForest Nothing gri apiMode
                            (const False) rq user smartStream feedMasks
                            (rqViewMode rq) cnt
            return (cnt', reverse acc ++ sf, n)
        go cnt acc prqs@(rq:nrq:rqs) = do
--            debugReqs prqs
--            print (cnt, (rq:nrq:rqs))
            (cnt', sf, n) <-
                postsForest Nothing gri apiMode
                            (limit rq nrq) rq user smartStream feedMasks
                            (rqViewMode rq) cnt
--            print (cnt', n)
            go cnt' (revAppend sf acc)
                    (insReqs (getRq n) (nrq:rqs))
                    -- (sortReqs (getRq n ++ (nrq : rqs)))
        groupByFeed = mtvmGroupByFeed viewMode
        insReqs a b | groupByFeed = a ++ b
        insReqs [] xs = xs
        insReqs (r:rs) xs = insReqs rs (insertSorted compareReqs r xs)
        getRq (Just (TRPosts rqs)) = rqs
        getRq _ = []
        limit rq nrq
            | groupByFeed = const False
            | mtvmAscending = \ t ->
                cmpTime (rq { prqMsgTreePoint = t }) nrq == GT
            | otherwise     = \ t ->
                cmpTime (rq { prqMsgTreePoint = t }) nrq == LT
            where
        sortReqs
            | groupByFeed = id
            | otherwise = sortBy compareReqs
        cmpTime = comparing
            $ \ PostsReq {..} -> (mtpTime prqMsgTreePoint, Down prqFeedId)
        compareReqs
            | mtvmAscending = cmpTime
            | otherwise = flip cmpTime
        rqViewMode rq =
            if mtvmNoOverride then
                (feedViewMode (feedUrl (prqFeedId rq) gri) u)
                { mtvmAscending = mtvmAscending
                , mtvmUnreadOnly = mtvmUnreadOnly
                -- а вот expanded и postsViewMode берутся по блогу
                }
            else
                viewMode
        debugReqs :: [PostsReq] -> IO ()
        debugReqs rs =
            logS $ unlines $ header : map showReq (take 5 rs)
            where header = "----- " ++ show (rs == sortReqs rs) ++ " "
                           ++ T.unpack (hashData $ BL.toStrict $ encode rs)
        showReq (PostsReq {..}) =
            showUrTime (mtpTime prqMsgTreePoint)
            ++ printf " %3d " (mtpId prqMsgTreePoint)
            ++ drop 2 (dropWhile (/= '/') $ T.unpack $ feedUrl prqFeedId gri)
    go cnt [] $ sortReqs prqs

insertSorted cmp e xs = go xs
    where go [] = [e]
          go (x:xs)
              | cmp x e == LT = x : go xs
              | otherwise = e:x:xs

revAppend [] ys = ys
revAppend (x:xs) ys = revAppend xs (x:ys)

testSSTI = do
    u <- cachedReadUser' "1"
    gri <- getGRIds "1"
    fs <- cachedReadFilters' "1"
    ssTaggedItems u gri fs $ head $ fSmartStreams fs

ssTaggedItems user gri fs ss = do
    checkSmartStreamError (ssName ss) (uId user)
    (_, ppr) <- readPostsAndFilteredPostsRead user urls
    return
        [ ( fromMaybe mhDlTime mhTime
          , MsgId fid pid Nothing
          , MsgKey (feedUrl fid gri) (Just mhGuid) Nothing)
        | ((_, (p, pr, _)), (fid, fm)) <- zip (ppr gri fs) fms
        , let hs = mtHeaders $ pMsgTree p
              (lo,hi) = BA.bounds hs
        , pid <- ReadSet.toList $ fmPosts fm
        , pid >= lo && pid <= hi
        , not (ReadSet.member pid (prSet pr)
               || ReadSet.member pid (prIgnoredPosts pr))
        , let MsgHeader {..} = hs BA.! pid
        ]
    where fms = IntMap.toList $ ffmFeedMasks $ ssFeedMasks ss
          urls = feedUrls gri $ map fst fms

tagsForest' ids maxTag gri apiMode lastMsg tags user _smartStream _masks viewMode@(MsgTreeViewMode {..}) cnt = do
    (taggedItems, _) <- tagsList ids maxTag gri apiMode (tlsFromLastMsg lastMsg)
        tags user viewMode
    u <- cachedReadUser' user
    let collapse vm
            | search = vm -- { mtvmExpandedComments = False }
            | otherwise = vm
        search = isJust ids
        bfuViewMode bfu = collapse $
            (if mtvmNoOverride then
                 (if isActiveFeed bfu gri
                  then
                      feedViewMode bfu u
                  else
                      defaultMsgTreeViewMode
                      -- в тегах для несуществующих фидов всегда режим
                      -- по-умолчанию (т.к. клиент не знает, как их отображать,
                      -- да и нафиг, пускай только в discovery работает)
                 )
                 { mtvmUnreadOnly = mtvmUnreadOnly
--                 , mtvmExpandedComments = False
                 -- а вот postsViewMode берется по блогу
                 }
             else
                 viewMode)
             { mtvmAscending = False
               -- для комментариев, для постов не важно, т.к. они по одному
               -- обрабатываются
             }
        go cnt acc _ [] =
            return (cnt, reverse acc, Nothing)
        go cnt acc last _
            | mfcFull cnt >= msgLimit apiMode =
                return (cnt, reverse acc, Just $ TRTags tags maxTag last)
        go cnt !acc last ((t,mid@(MsgId{..}),mkey) : ts) = do
            (cnt', sf, _) <- case mkey of
                MsgKey bfu (Just pguid) Nothing -> do
                    let pid = midPostId
                        mh = MsgHeader
                             { mhGuid        = pguid
                             , mhContentHash = ""
                             , mhAuthor      = ""
                             , mhAuthorPic   = Nothing
                             , mhSubject     = ""
                             , mhTime        = Just t
                             , mhDlTime      = t
                             , mhShortText   = ""
                             }
                    postsForest
                        (Just (t, pid, mh))
                        gri apiMode
                        (const False)
                        (PostsReq
                         { prqFeedId = feedId bfu gri
                         , prqMsgTreePoint = descMtp (-1)
                         , prqTotalPosts = maxBound
                         , prqTotalComments = maxBound
                         }) user Nothing Nothing
                        (bfuViewMode bfu) cnt
                MsgKey bfu (Just pid) (Just cid) ->
                    commentsForest apiMode
                        (Just (t, fromMaybe (-1) midCommentId))
                        gri
                        (CommentsReq
                         { crqKey = CommentsKey bfu pid
                         , crqPostId = midPostId
                         , crqMsgTreePoint = ascMtp (-1)
                         , crqTotalComments = maxBound
                         }) user Nothing Nothing
                        (bfuViewMode bfu) cnt
                _ -> return (cnt, [], Nothing)
--            print cnt'
            go cnt' (revAppend (map (fixTime t) sf) acc) (Just (t,mid)) ts
        fixTime t (mi, sf) = (mi { miMsgView = fixMvTime t $ miMsgView mi }, sf)
        fixMvTime t (MVFull m) = MVFull $ fixMsgTime t m
        fixMvTime t (MVShort mh cm) =
            MVShort (fixMhTime t mh) (fixMsgTime t <$> cm)
        fixMsgTime t m = m { msgTime = Just t, msgDlTime = t }
        fixMhTime t mh = mh { mhTime = Just t, mhDlTime = t }
    go cnt [] Nothing taggedItems

maxMaxTag = Just (maxBound :: UrTime, MsgId 0 0 Nothing)

data TagsListSlice
    = TLSAbove (UrTime, MsgId)
    | TLSBelow (UrTime, MsgId)
    | TLSAll
    deriving Show

tlsFromLastMsg = maybe TLSAll TLSBelow

tagsList ids maxTag gri apiMode slice tags user viewMode@(MsgTreeViewMode {..}) = do
    fs <- cachedReadFilters' user
    let tagsSet = HS.fromList [t | ITTag t <- fromMaybe [] tags]
        smartStreams =
            filter (\ ss -> HS.member (ssName ss) tagsSet) (fSmartStreams fs)
    u <- cachedReadUser' user
    ssitems <- mapM (ssTaggedItems u gri fs) smartStreams
    ordering <- userGetOrdering uSubscriptions user
    let timeOk
            | AMGRIdsOnly {..} <- apiMode = \ (t,_,_) ->
                maybe True (t >=) amMinDlTime &&
                maybe True (t <=) amMaxDlTime
            | otherwise = \ _ -> True
        (skipTillNext, addDummyLastMsg) = case slice of
            TLSBelow l ->
                (\ ts -> case dropWhile (notLast l) ts of
                    (_:xs) -> xs
                    [] -> []
                ,addLast l)
            TLSAbove l ->
                (takeWhile $ notLast l, addLast l)
            TLSAll -> (id, id)
            -- на случай, если с lastMsg уже сняли пометку
            -- (или пометили прочитанным в режиме unread only), добавляем
            -- его в список taggedItems0, чтобы он оказался в нужном месте
            -- для последующего обрезания списка тегов,
            -- а потом пропускаем посты до/после него
        notLast (_,i) (_,mid,_) = mid /= i
        addLast (t,i) ts = (t, i, MsgKey "" Nothing Nothing) : ts
        allTaggedItems =
            Map.elems $ Map.delete ITStarred $ griTaggedItems gri
        tTime (t,_,_) = t
        tId (_,mid,_) = midGrItemId mid
        tFeedId (_,mid,_) = midFeedId mid
        dropOther = case apiMode of
            AMGRIdsOnly { amFetch = False, .. }
                | ITStarred `notElem` fromMaybe [] tags ->
                -- Reeder очень тормозит при большом кол-ве помеченных
                -- сообщений.
                -- Он запрашивает id-шки каждого тега по отдельности,
                -- в результате чего общее кол-во синхронизируемых помеченных
                -- выходит за предел в n=5000 (или 1000/2500)
                let mask =
                        IntSet.fromList $ map tId $
                        take amCount $ nubSortTaggedItems $
                        taggedItemsWithMsgIds $
                        concat allTaggedItems
                in
                    filter (flip IntSet.member mask . tId)
            _ -> id
        taggedItems =
            filter timeOk $
            skipTillNext $
            sortByFeed tFeedId viewMode gri ordering Nothing $
            (if mtvmAscending then reverse else id)
            taggedItems0
        skipTillMaxTag
            | Just mt <- maxTag
            = dropWhile $ \ (t, mid, _) -> (t, mid) > mt
            | otherwise = const []
        taggedItems0 =
            skipTillMaxTag $
            nubSortTaggedItems $
            addDummyLastMsg $
            dropOther $
            (maybe id (\ids -> filter (flip IntSet.member ids . tId)) ids) $
            concat $
            ssitems ++
            map taggedItemsWithMsgIds
            (maybe
                allTaggedItems
                (map (\ t -> Map.findWithDefault [] t (griTaggedItems gri)))
                tags)
            -- для тегов, чьи имена совпадают со smart stream, надо
            -- получать список (time, iid, msgKey)
            -- и concat-ить его сюда
        maxTag' = case taggedItems0 of
            (t,mid,_):_ -> Just (t,mid)
            [] -> Nothing
    return (taggedItems, maxTag')

-- testTagsForest = do
--     gri <- getGRIds "1"
--     tagsForest Nothing gri AMNormal Nothing (Just [ITStarred]) "1" undefined undefined (defaultMsgTreeViewMode {mtvmEx = MTVMEx {mtvmexFolderExpanded = True, mtvmexGroupByFeed = True, mtvmexReserved1 = False, mtvmexReserved2 = 0}}) (MFCnt 1 0 0)

isPostRead pid p (PostsRead {..}) =
    ReadSet.member pid prIgnoredPosts || ReadSet.member pid prSet
    || isPostOutOfBounds pid p

isPostOutOfBounds pid (Posts {..}) =
    pid < lo || pid > hi
    where (lo, hi) = BA.bounds $ mtHeaders pMsgTree

isCommentRead pid p (PostsRead {..})
    | ReadSet.member pid prIgnoredPosts = const True
    | otherwise = \ cid -> ReadSet.member cid rs || isPostOutOfBounds pid p
    where rs = lookupCommentsReadSet pid prCommentsRead

lookupCommentsReadSet idx prCommentsRead =
    case IntMap.lookup idx prCommentsRead of
        Just cr -> cr
        _ -> ReadSet.empty

ascMtp parent = MsgTreePoint parent (UrTime 0 0) (-1)
descMtp parent = MsgTreePoint parent (UrTime m 0) m
    where m = 2^32
    -- при group by feed и поиске descMtp из TRPosts не заменяются на правильные
    -- даты и post id (т.к. limit отключен и нет первоначального пробегания
    -- по всем фидам) и попадают в браузер, а javascript использует double
    -- для хранения integer и 2^63 парсит неверно (получается больше, чем надо),
    -- в результате UrTime maxBound превращается в UrTime (-1) когда приходит
    -- обратно в хаскелл
-- jsMaxSafeInteger = 2^53-1

mfcPlus (MFCnt {..}) t u tr ur ssu ssur tt tu tur f =
    MFCnt
    { mfcTotal = mfcTotal + t
    , mfcUnread = mfcUnread + u
    , mfcTotalResults = mfcTotalResults + tr
    , mfcUnreadResults = mfcUnreadResults + ur
    , mfcSmartStreamUnreadCounts =
        IntMap.unionWith (+) mfcSmartStreamUnreadCounts ssu
    , mfcSmartStreamUnreadResultCounts =
        IntMap.unionWith (+) mfcSmartStreamUnreadResultCounts ssur
    , mfcTagTotalCounts =
        Map.unionWith (+) mfcTagTotalCounts tt
    , mfcTagUnreadCounts =
        Map.unionWith (+) mfcTagUnreadCounts tu
    , mfcTagUnreadResultCounts =
        Map.unionWith (+) mfcTagUnreadResultCounts tur
    , mfcFull = mfcFull + f
    }
mfcSucc cnt full read ss ts =
    mfcPlus cnt 1 (n $ not read) (n full) (n $ full && not read)
        ssm (if full then ssm else IntMap.empty)
        tsmTotal tsm (if full then tsm else Map.empty) (n full)
    where n True = 1
          n False = 0
          tsmTotal = Map.fromList $ map (,1) ts
          (ssm, tsm)
              | read = (IntMap.empty, Map.empty)
              | otherwise =
                  (IntMap.fromDistinctAscList $ map (,1) $ IntSet.toList ss
                  ,tsmTotal)

data MFCnt
    = MFCnt
      { mfcTotal :: !Int -- ^ mfTotalCount
      , mfcUnread :: !Int -- ^ mfUnreadCount
      , mfcTotalResults :: !Int -- ^ mfTotalResultsCount
      , mfcUnreadResults :: !Int -- ^ mfUnreadResultsCount
      , mfcSmartStreamUnreadCounts :: IntMap Int -- ^ mfSmartStreamUnreadCounts
      , mfcSmartStreamUnreadResultCounts :: IntMap Int -- ^ mfSmartStreamUnreadResultCounts
      , mfcTagTotalCounts :: Map (Maybe ItemTag) Int -- ^ mfTagTotalCounts
      , mfcTagUnreadCounts :: Map (Maybe ItemTag) Int -- ^ mfTagUnreadCounts
      , mfcTagUnreadResultCounts :: Map (Maybe ItemTag) Int -- ^ mfTagUnreadResultCounts
      , mfcFull :: !Int
        -- ^ число видимых сообщений, не равно mfResultsCount, если
        -- mtvmExpandedComments == False
        -- ограничение отображаемых сообщений работает по mfcFull
      }
    deriving Show

userSITFeed u f tp tc = do
    user <- cachedReadUser' u
    let Just s = find ((==) f . sUrl) $ uSubscriptions user
    p <- cachedReadPosts' f
    pr <- cachedNothingReadPostsRead' (u, f)
    fs <- cachedReadFilters' u
    gri <- cachedReadGRIds' u
    return $ case mkSITFeed' gri fs False s p pr (error "t") (error "bps") tp tc of
               (f,_,_) -> f

perfTest = testLatestFeed "1"
testLatestFeed user = do
    ((_, _, sirs, _, _), _, _) <-
        subscriptionsAndSettings "" True True user
    gri <- cachedReadGRIds' user
    let feeds =
            [ ( feedId (sUrl sitSubscription) gri
              , cReadPosts, cReadComments, cTotalPosts, cTotalComments)
            | SubItemRpc
              { sirSIType = SITFeed { .. }
              , sirCounters = Counters { cFeed = 1, .. }, .. } <- sirs]
        mtvm = defaultMsgTreeViewMode { mtvmUnreadOnly = False }
        loopReqs n reqs
            | null reqs || n <= 0 = return ()
            | otherwise = do
                logS "getTree"
                mfs <- getTree (AMNormal "" "") user mtvm reqs
                BL.length (encode mfs) `seq` return ()
                loopReqs (n - 1) (mkReqs mfs reqs)
--    putStrLn . ppShow =<<
    logS "folderForest"
    (_, _, mf) <- folderForest user Nothing (FODFeeds feeds) [] mtvm "" ""
    BL.length (encode mf) `seq` return ()
    loopReqs 300 (mfRequests mf)

getUpdatedCounts u feedsAndTcs viewMode folder smartStream = do
    fs <- cachedReadFilters' (uId u)
    ordering <- userGetOrdering uSubscriptions (uId u)
    gri <- cachedReadGRIds' (uId u)
    let fids = [fid | (fid,_,_,_,_) <- feedsAndTcs]
        feeds = map (flip feedUrl gri) fids
        usedFeeds = IntMap.fromList $ map (,()) fids
        ssfms = ffmFeedMasks . ssFeedMasks
        usedSmartStreams =
            filter (\ ss -> IntMap.size
                       (IntMap.intersection (ssfms ss) usedFeeds) > 0
                       || Just (ssName ss) == smartStream
                       -- выбранный стрим обновляем всегда (обработка ситуации,
                       -- когда в стриме после редактирования не осталось
                       -- результатов поиска)
                   )
--             filter (\ ss -> not $ IntMap.disjoint (ssfms ss) usedFeeds)
                (fSmartStreams fs)
            -- Возможны редкие случаи с выпадением единственного подходящего
            -- старого поста из фида и убирания его FeedMask, а также
            -- убирания FeedMask после редактирования.
            -- Тогда пересечения с ssffms не будет и smart stream не обновится.
            --
            -- Оставляем такое поведение (все равно обновится при выборе
            -- или фоновом обновлении), т.к. использование ssFeeds приведет
            -- к тому, что на каждый чих будут пересчитываться все smart streams
            -- с этими фидами (даже если результатов для заданных фидов нет).
        usedTags0 = foldl' Set.union Set.empty $ map (feedTags gri) fids
        hasStarred = Set.member (Just ITStarred) usedTags0
        (usedTags, usedTagsFeeds)
            | Set.member Nothing usedTags0 =
                (Nothing :
                 map Just
                     ((if hasStarred then id else filter (/= ITStarred))
                      $ Map.keys $ griTaggedItems gri)
                 -- обновляем все теги, если есть хотя бы один тег,
                 -- чтобы счетчики Tags и отдельных тегов не расходились
                ,if hasStarred then
                     IntSet.union starredFeeds tagsFeeds
                 else
                     tagsFeeds)
            | hasStarred = ([Just ITStarred], starredFeeds)
            | otherwise = ([], IntSet.empty)
        starredFeeds = tagFeeds gri (Just ITStarred)
        tagsFeeds    = tagFeeds gri Nothing
        smartStreamFeeds =
            IntSet.fromDistinctAscList $ IntMap.keys $
            foldl' IntMap.union IntMap.empty (map ssfms usedSmartStreams)
        feeds' = feeds <> filter (flip isActiveFeed gri)
            (feedUrls gri $ IntSet.toList
             $ IntSet.union usedTagsFeeds smartStreamFeeds
               `IntSet.difference` IntSet.fromList fids)
    tsp <- tagImportPercent (uId u)
    (_, prl) <- readPostsAndFilteredPostsRead u feeds'
    let postsReadMap = IntMap.fromList postsReadList
        postsReadList = prl gri fs
        sMap = HM.fromList [(sUrl s, s) | s <- uSubscriptions u]
        sitFeeds =
            [mkSITFeed' gri fs False s p pr (error "t") Nothing -- (error "bps")
                 (pTotalPosts p) (pTotalComments p) -- tp tc
             | ((flip feedUrl gri -> f, rp, rc, tp, tc), (_, (p, pr, _)))
                 <- zip feedsAndTcs postsReadList
             , let s = fromMaybe
                       (Subscription
                        { sUrl        = f
                        , sState      = SSFeed f
                        , sEditsCount = 0
                        , sTitle      = Nothing
                        , sFolders    = []
                        }) $
                       -- find ((==) f . sUrl) $ uSubscriptions u
                       HM.lookup f sMap
            ]
        updatedCounts = catMaybes $ zipWith updatedCount sitFeeds feedsAndTcs
        updatedCount (sit, c, _) f@(fn,_,_,_,_) = do
            let f' = (fn, cReadPosts c, cReadComments c, cTotalPosts c, cTotalComments c)
            if f == f' && isActiveFeed (feedUrl fn gri) gri then
                fail "no difference"
            else
                return f'
        ssCounts = map ssc usedSmartStreams
        ssc ss =
            (folderId (ssName ss) gri,
             cReadPosts, cReadComments, cTotalPosts, cTotalComments)
            where Counters {..} = smartStreamCounters gri u postsReadMap ss
        tagCounts = map tc usedTags
        tc tag =
            (tagGRId tag gri,
             cReadPosts, cReadComments, cTotalPosts, cTotalComments)
            where Counters {..} = tagCounters gri u postsReadMap tsp tag
        feedTcs =
            sortByFeed fst viewMode gri ordering folder
                $ zipWith ftc sitFeeds feedsAndTcs
        ftc (sit, c, _) (f, _, _, _, _) =
            (f, (cTotalPosts c, cTotalComments c))
        showId i
            | Just folder <- IntMap.lookup i (griFolderNames gri) =
                folder
            | Just feed <- IntMap.lookup i (griFeedUrls gri) =
                feed
            | otherwise = showT i
--         showC (i,rp,rc,tp,tc) = showId i <> " " <> showT (rp,rc,tp,tc)
--     logT $ T.unlines $
--         [uId u
--         ,"updatedCounts"]
--         <>
--         map showC updatedCounts
--         <>
--         ["ssCounts"]
--         <>
--         map showC ssCounts
--         <>
--         ["tagCounts"]
--         <>
--         map showC tagCounts
    return (updatedCounts <> ssCounts <> tagCounts, sitFeeds, postsReadMap, ordering, feedTcs)

-- | Набор всех фидов, отмеченных этим тегом
tagFeeds :: GRIds -> Maybe ItemTag -> IntSet
tagFeeds gri tag =
    IntSet.filter (\ i -> isActiveFeed (feedUrl i gri) gri)
    $ IntSet.fromList $ map (midFeedId . midFromGrItemId) $ taggedItems gri tag

-- | Набор всех тегов в этом фиде
feedTags :: GRIds -> Int -> Set (Maybe ItemTag)
feedTags gri fid =
    Set.fromList $ concatMap tags $ IntMap.elems $ feedItemTags fid gri
    where tags [] = [] -- такого быть не должно
          tags [ITStarred] = [Just ITStarred]
          tags ts = Nothing : map Just ts


markReqReadCounters user viewMode markReq mids = do
    u <- cachedReadUser' user
    gri <- cachedReadGRIds' user
    let tcsFids = return . IntSet.fromList . map fst
        tagFids = return . tagFeeds gri . tagsToTag
        tagsToTag (Just (t:_)) = Just t
        tagsToTag _ = Nothing
    fids <- case markReq of
        MRPosts tcs ->
            tcsFids tcs
        MRTags tags maxTag ->
            tagFids tags
        MRSmartStream sn tcs ->
            tcsFids tcs
        MRSearchPosts q fmk tcs ->
            tcsFids tcs
        MRSearchTags query idsKey tags maxTag ->
            tagFids tags
        MRSearchSmartStream sn q fmk tcs ->
            tcsFids tcs
    let feedIds = filter (`isActiveFeedId` gri) $ IntSet.toList
            $ fids `IntSet.union` IntSet.fromList (map midFeedId mids)
    (uc, _, _, _, _) <-
        getUpdatedCounts u [(f,0,0,0,-1) | f <- feedIds]
            viewMode Nothing Nothing
    return uc


griApiModeAndTcs u feedsOrDiscovery viewMode hostName acceptLanguage = do
    gri0 <- getGRIds' user (uSubscriptions u)
    gri <- case feedsOrDiscovery of
        FODDiscovery feed -> do
            p <- cachedReadPosts' feed
            initPostsRead user p
            if griLastId (addFeedUrlToGRIds gri0 feed) /= griLastId gri0 then
                modifyGRIds' user $ \ gri ->
                    let i = addFeedUrlToGRIds gri feed in return (i,i)
            else
                return gri0
        _ ->
            return gri0
    case feedsOrDiscovery of
        FODFeeds rc ->
            return
                (gri
                ,AMNormal
                 { amHostName = hostName
                 , amAcceptLanguage = acceptLanguage }
                ,rc)
        FODFeedsApi am feeds ->
            return
                (gri
                ,am
                ,[(feedId f gri, 0, 0, maxBound, maxBound) | f <- feeds])
        FODDiscovery feed -> do
            (updatedCounts, _, _, _, _) <-
                getUpdatedCounts u [(feedId feed gri, 0, 0, -1, -1)]
                    viewMode Nothing Nothing
            return
                (gri
                ,AMDiscovery
                 { amUrl = feed
                 , amHostName = hostName
                 , amAcceptLanguage = acceptLanguage }
                ,updatedCounts)
    where user = uId u

folderForest user folder feedsOrDiscovery readyReqs viewMode hostName acceptLanguage = do
--    logS "folderForest"
    u <- cachedReadUser' user
    (gri, apiMode, modifiedFeeds) <-
        griApiModeAndTcs u feedsOrDiscovery viewMode hostName acceptLanguage
    checkFilterError [i | (i,_,_,_,_) <- modifiedFeeds] user
    (updatedCounts, sitFeeds, _, ordering, feedTcs) <-
        getUpdatedCounts u modifiedFeeds viewMode folder Nothing
    let reqs = catMaybes (zipWith sitReq sitFeeds modifiedFeeds) ++ readyReqs
        point sit s
            | unreadOnly == False && ascending = sitPointAllAsc s
            | unreadOnly == False  = sitPointAllDesc sit
            | expanded == False && ascending =
                sitPointUnreadPostsOnlyAsc s
            | expanded == False =
                sitPointUnreadPostsOnlyDesc s
            | ascending = sitPointUnreadAsc s
            | otherwise = sitPointUnreadDesc s
            where expanded = isExpanded sit
                  ascending = mtvmAscending viewMode
        isExpanded sit
            | AMGRIdsOnly {} <- apiMode = False
            | otherwise =
                mtvmExpandedComments $
                if mtvmNoOverride viewMode then
                    feedViewMode (sUrl $ sitSubscription sit) u
                else
                    viewMode
        readOnly
            | AMGRIdsOnly {..} <- apiMode = amReadOnly
            | otherwise = False
        unreadOnly = mtvmUnreadOnly viewMode
        sitReq (sit, c, sd) (f, rp, rc, tp, tc) = do
            when (unreadOnly && cTotalPosts c == cReadPosts c &&
                  (not (isExpanded sit) || cTotalComments c == cReadComments c))
                (fail "ignore read feed")
            when (readOnly && (cReadPosts c == 0 || cTotalPosts c - cReadPosts c == 500))
                (fail "ignore feed without unreads")
            p <- point sit sd
            return $
                PostsReq
                { prqFeedId = f
                , prqMsgTreePoint = p
                , prqTotalPosts = cTotalPosts c -- tp
                , prqTotalComments = cTotalComments c -- tc
                }
    mf <- userGetMsgForest apiMode user viewMode $
        TRPosts $ sortByFeed prqFeedId viewMode gri ordering folder reqs
--    logS $ show ("folderForest", mfResultsCount mf, mfUnreadCount mf)
    return (MRPosts feedTcs, updatedCounts, mf)

userGetMsgForest :: ApiMode -> Key User -> MsgTreeViewMode -> TreeReq
                 -> IO MsgForest
userGetMsgForest apiMode user viewMode req = do
    r <- getTree apiMode user viewMode [req]
    return $ case r of
        [Just mf] -> mf
        _ ->
            -- при запросе с лимитом числа сообщений равным нулю (из API)
            -- может вернуться [Nothing]
            -- API подразумевает n > 0, при n <= 0 просто ничего не делаем
            -- (нет смысла в подготовке дерева, т.к. continuation в API
            -- идет от последнего выданного элемента, исправлять это смысла
            -- также не вижу, т.к. вызов с n=0 все равно некорректен)
            MsgForest
            { mfTotalCount = 0
            , mfUnreadCount = 0
            , mfTotalResultsCount = 0
            , mfUnreadResultsCount = 0
            , mfSmartStreamUnreadCounts = IntMap.empty
            , mfSmartStreamUnreadResultCounts = IntMap.empty
            , mfTagTotalCounts = Map.empty
            , mfTagUnreadCounts = Map.empty
            , mfTagUnreadResultCounts = Map.empty
            , mfList = []
            , mfNextReq = Nothing }

getTree :: ApiMode -> Key User -> MsgTreeViewMode -> [TreeReq]
            -> IO [Maybe MsgForest]
getTree apiMode user viewMode reqs = do
--    logS $ "getTree: " ++ show viewMode ++ " " ++ show reqs
    case apiMode of
        AMDiscovery {} -> usageFlag user UFDiscoverySubscription
        _ -> return ()
    gri <- getGRIds user

    let feedMasksKey =
            listToMaybe $
            [ (Nothing, trQuery, trFeedMasksKey, trReqs)
            | TRSearchPosts {..} <- reqs] ++
            [ (Just trStreamName, trQuery, trFeedMasksKey, trReqs)
            | TRSearchSmartStream {..} <- reqs]
        smartStream = listToMaybe $
            [ trStreamName | TRSearchSmartStream {..} <- reqs ] ++
            [ trStreamName | TRCommentsS {..} <- reqs ] ++
            [ trStreamName | TRSmartStream {..} <- reqs]

    feedMasks <- case feedMasksKey of
        Just (n,q,k,r) -> do
            mbm <- Cache.lookup k feedMasksCache
            case mbm of
                Just m -> return $ Just m
                Nothing -> do
                    (m,_,_) <- withLogger $ \ l -> do
                        logLS l $ "Restoring masks for " ++ show q
                        u <- cachedReadUser' user
                        getFeedMasksE l u
                            [ ( prqFeedId,
                                (prqTotalPosts, prqTotalComments))
                            | PostsReq {..} <- r
                            ]
                            viewMode n q
                    Cache.insert k m feedMasksCache
                    return $ Just m
        Nothing -> return Nothing
    feedMasks <- case [name | TRSmartStream name _ <- reqs] of
        [name] -> fmap Just $ lookupSmartStreamFeedMasks name user
        _ -> return feedMasks

    let   go acc n reqs
             | n >= msgLimit apiMode =
                 return $ reverse acc ++ map (const Nothing) reqs
          go acc n [] = return $ reverse acc
          go acc n (r:rs) = do
              (mf,n') <- processReq n r
              go (Just mf:acc) n' rs
          retryReq r =
              MsgForest 0 0 0 0 IntMap.empty IntMap.empty
                  Map.empty Map.empty Map.empty [] (Just r)
--           processReq n (TRPosts [pReq]) =
--               forest n $ postsForest id (const True) False (const False) pReq
          processReq n (TRPosts feeds) =
              forest n $ feedsForest gri apiMode feeds
          processReq n (TRSearchPosts {..}) = do
              -- никакой разницы с TRPosts, маски готовятся раньше
              (mf,n') <- forest n $ feedsForest gri apiMode trReqs
              return (mf { mfNextReq = case mfNextReq mf of
                             Just (TRPosts f) ->
                                 Just (TRSearchPosts trQuery trFeedMasksKey f)
                             _ ->
                                 Nothing }, n')
          processReq n (TRSmartStream {..}) = do
              -- никакой разницы с TRPosts, маски готовятся раньше
              (mf,n') <- forest n $ feedsForest gri apiMode trReqs
              return (mf { mfNextReq = case mfNextReq mf of
                             Just (TRPosts f) ->
                                 Just (TRSmartStream trStreamName f)
                             _ ->
                                 Nothing }, n')
          processReq n (TRSearchSmartStream {..}) = do
              -- никакой разницы с TRPosts, маски готовятся раньше
              (mf,n') <- forest n $ feedsForest gri apiMode trReqs
              return (mf { mfNextReq = case mfNextReq mf of
                             Just (TRPosts f) ->
                                 Just (TRSearchSmartStream trStreamName
                                           trQuery trFeedMasksKey f)
                             _ ->
                                 Nothing }, n')
          processReq n (TRTags tags maxTag lastMsg)  =
              forest n $ tagsForest' Nothing maxTag gri apiMode lastMsg tags
          processReq n (TRSearchTags query idsKey tags maxTag lastMsg) = do
              ids <-  getCachedTagsFilterIds idsKey user query tags
              (mf,n') <- forest n $ tagsForest' (Just ids) maxTag gri apiMode lastMsg tags
              return (mf { mfNextReq = case mfNextReq mf of
                             Just (TRTags t mt n) ->
                                 Just (TRSearchTags query idsKey t mt n)
                             _ ->
                                 Nothing }, n')
          processReq n (TRComments _ cReq) =
              forest n $ commentsForest apiMode Nothing gri cReq
          processReq n (TRCommentsS _ _ cReq) =
              forest n $ commentsForest apiMode Nothing gri cReq
--         rqViewMode rq =
--             (feedViewMode (prqBlogFeedUrl rq) u)
--             { mtvmAscending = mtvmAscending
--             , mtvmUnreadOnly = mtvmUnreadOnly
--             -- а вот expanded и postsViewMode берутся по блогу
--             }
          forest n mkf = do
              (cnt, subForest, next) <- mkf user smartStream feedMasks viewMode
                   $ MFCnt 0 0 0 0 IntMap.empty IntMap.empty
                       Map.empty Map.empty Map.empty n
              return (MsgForest
                         (mfcTotal cnt) (mfcUnread cnt)
                         (mfcTotalResults cnt) (mfcUnreadResults cnt)
                         (mfcSmartStreamUnreadCounts cnt)
                         (mfcSmartStreamUnreadResultCounts cnt)
                         (mfcTagTotalCounts cnt)
                         (mfcTagUnreadCounts cnt)
                         (mfcTagUnreadResultCounts cnt)
                         subForest next,
                      mfcFull cnt)
    go [] 0 reqs

-- от того, что стал читать сообщения по одному, а не пачкой,
-- и стал вычитывать commentsRead на 1500 тыс сообщений из фотожабы
-- c 1.2-1.4 сек перешло на 1.6-1.8, что не сильно страшно.

--                       total / coreader.exe / postgres
-- getFeedLinks         50     / 10           / 40                   ~ 50total
-- userSubscriptions   110msec / 30msec / riak 150 + 20 + 20 + 20    ~ 240total

-- причем у нас подписок даже чуть меньше
-- выходит раза в 2 хуже по latency и раз в 5 по общему времени

-- а по запросам сообщений вроде особой разницы нет.

prBfu = snd . prKey

-- | убираем игнор и помечаем все комментарии поста прочитанными
removeIgnore :: MsgId -> Int -> PostsRead -> IO PostsRead
removeIgnore (MsgId {..}) totalComments pr@(PostsRead {..}) = do
    p <- cachedReadPosts' (prBfu pr)
    let cc = postCommentsCount midPostId totalComments p
--    logS $ "cc = " ++ show cc ++ "; totalComments = " ++ show totalComments
    return $
        pr
        { prSet = if isJust midCommentId then ReadSet.insert midPostId prSet
                  -- если пометили комментарий,
                  -- то пост все-таки оставляем прочитанным
                  else prSet
        , prIgnoredPosts = ReadSet.delete midPostId prIgnoredPosts
        , prCommentsRead =
            if cc > 0 then
                IntMap.insert midPostId
                          (ReadSet.fromRange 0 (cc-1))
                          prCommentsRead
            else
                IntMap.delete midPostId prCommentsRead
        , prTotalCommentsRead = prTotalCommentsRead + cc
        }

-- | Bool -- read/undread
--markMsgRead :: _ -> MsgId -> Bool -> Int -> PostsRead -> IO PostsRead
markMsgRead c mid@(MsgId {..}) read totalComments pr =
    if midPostId == -1 || (read && ReadSet.member midPostId (prIgnoredPosts pr)) then return pr
    else do
--    modifyPostsRead'_ (user, msgKeyBlogFeedUrl midMsgKey) $ \
        -- TODO: проверять соотвествие id и guid-ов
        let mark id rs cnt
                | read      =
                    if ReadSet.member id rs then (cnt, rs)
                    else (cnt+1, ReadSet.insert id rs)
                | otherwise =
                    if ReadSet.member id rs then (cnt-1, ReadSet.delete id rs)
                    else (cnt, rs)
        pr <-
            if read == False && ReadSet.member midPostId (prIgnoredPosts pr)
            then
                removeIgnore mid totalComments pr
            else
                return pr
        pr' <- case midCommentId of
            Nothing ->
                return $ pr { prSet = snd $ mark midPostId (prSet pr) 0 }
            Just cid -> do
                let rs = fromMaybe ReadSet.empty $
                         (IntMap.lookup midPostId (prCommentsRead pr))
                    (tcr, rs') = mark cid rs (prTotalCommentsRead pr)
                return $ pr { prTotalCommentsRead = tcr
                            , prCommentsRead = IntMap.insert midPostId
                                               rs' (prCommentsRead pr)
                            }
        incrCounters "pr" c (ReadSet.size (prSet pr') - ReadSet.size (prSet pr))
        incrCounters "cr" c (prTotalCommentsRead pr' - prTotalCommentsRead pr)
        return pr'

data MarkBlogRead
    = MarkBlogRead
      { mbrTotalPosts :: Int
      , mbrTotalComments :: Int
      , mbrMarkPost :: UrTime -> Int -> Bool -- time pid
      , mbrMarkCommentAboveBelow :: Int -> Maybe MarkComment
        -- ^ как помечать комментарии заданного поста.
        -- Сам пост при этом может не трогаться, как в случае mark below от
        -- комментария (или от поста к развернутым комментариями) или в случае
        -- отмечания помеченного звездочкой комментария
      , mbrOverrideTime :: Int -> Maybe Int -> Maybe UrTime
        -- ^ pid cid, для тегов используем время тега, а не поста/комментария
      }

data MarkComment
    = MCAbove { mcCommentId :: Int, mcExclude :: Bool }
    | MCBelow { mcCommentId :: Int }
    | MCSelected { mcCommentIds :: [Int] }


markBlogRead c what feedMask fid blogFeedUrl (MarkBlogRead {..}) olderThan pr = do
    t <- getUrTime
    feedMask <- case feedMask of
        Just _ -> return feedMask
        Nothing -> do
            fs <- cachedReadFilters' $ fst $ prKey pr
            return $ fidFeedMask Nothing fid Nothing fs
    let checkT
            | olderThan == 0 = \ _ _ _ -> True
            | otherwise = \ pid cid mh -> time pid cid mh < minT
            where minT = t `plusUrTime` (-olderThan * day)
        time pid cid (MsgHeader {..}) =
            fromMaybe mhDlTime (mbrOverrideTime pid cid <|> mhTime)
    p@(Posts {..}) <- cachedReadPosts' blogFeedUrl
    let checkPostMask idx
            | Just fm <- feedMask = ReadSet.member idx (fmPosts fm)
            | otherwise = True
        go pr [] = return pr
        go !pr ((idx, mh@(MsgHeader {..})):ps)
            | ReadSet.member idx (prIgnoredPosts pr)
              || idx >= mbrTotalPosts
              || (not markPost && isNothing markComment)
                = go pr ps
            | otherwise = do
                (!tcrPlus, !cr) <- getCR
--                logTL ["Time: ", showT time, "; idx: ", showT idx, "; markPost: ", showT markPost, "; markComment: ", showT markComment]
                go (pr
                    { prSet =
                          if checkT idx Nothing mh
                              && checkPostMask idx
                              && markPost
                          then ReadSet.insert idx (prSet pr)
                          else prSet pr
                    , prTotalCommentsRead = tcrPlus + prTotalCommentsRead pr
                    , prCommentsRead = cr
                    }) ps
            where markPost = mbrMarkPost (time idx Nothing mh) idx
                  markComment = mbrMarkCommentAboveBelow idx
                  cc = fromMaybe 0 $ commentsCount mbrTotalComments
                      <$> IntMap.lookup idx pCommentCounts
                  getCR
                      | cc /= 0
                      , not skipComments
                      , cr <- fromMaybe ReadSet.empty $
                              IntMap.lookup idx (prCommentsRead pr)
                      , cc0 <- ReadSet.size cr
                      , cc > cc0 = do
                          let ckey = CommentsKey blogFeedUrl mhGuid
                          cr' <- case markComment of
                              Just mc -> do
                                  Comments {..} <- cachedReadComments' ckey
                                  return $ gocr cr
                                      [ (c, mtHeaders cMsgTree BA.! c)
                                      | c <- commentIds mc cMsgTree]
                              _ | olderThan > 0 -> do
                                  Comments {..} <- cachedReadComments' ckey
                                  return $ gocr cr $ BA.assocs
                                      $ mtHeaders cMsgTree
                              _ ->
                                  return $ ReadSet.union cMask cr
                          return (ReadSet.size cr' - cc0,
                                  IntMap.insert idx cr' (prCommentsRead pr))
                      | otherwise =
                          return (0, prCommentsRead pr)
                  (skipComments, checkComment, cMask)
                      | Just fm <- feedMask
                      , m <- commentsMask idx fm cc =
                          (ReadSet.null m, (`ReadSet.member` m), m)
                      | otherwise =
                          (False, const True, ReadSet.fromRange 0 (cc-1))
                  gocr cr [] = cr
                  gocr !cr ((cidx, mh) : cs)
                      | checkT idx (Just cidx) mh && checkComment cidx =
                          gocr (ReadSet.insert cidx cr) cs
                      | otherwise = gocr cr cs
                  commentIds mc = case mc of
                      MCAbove cid e ->
                          (if e then filter (/= cid) else id)
                          . commentIdsCid True cid
                      MCBelow cid -> commentIdsCid False cid
                      MCSelected cids -> const cids
                  commentIdsCid above cid cMsgTree =
                      dropWhileEnd (/= cid)
                      $ (if above then id else reverse)
                      $ allComments (-1)
                      $ mtChildren cMsgTree
                  allComments parentId children
                      | Just ch <- IntMap.lookup parentId children
                      = concat
                        [ cid : allComments cid children
                        | TimeId _ cid <- Set.toAscList ch]
                      | otherwise = []
    pr' <- go pr $ BA.assocs $ mtHeaders pMsgTree
    incrCounters "ps" c (ReadSet.size (prSet pr') - ReadSet.size (prSet pr))
    incrCounters "cs" c (prTotalCommentsRead pr' - prTotalCommentsRead pr)
    incrCounters what c 1
    t' <- getUrTime
--    logTL [what, " ", T.pack $ showSecs $ diffUrTime t' t]
    incrCounters ("ms-" <> what) c (round (diffUrTime t' t * 1000))
    return pr'

markMasksRead c what user masks (IntMap.fromList -> mbrs) d = do
    gri <- cachedReadGRIds' user
    -- наоборот, работать по mbrs?
    forM_ (IntMap.toList masks) $ \ (fid, feedMask) ->
        let feed = feedUrl fid gri in
        case IntMap.lookup fid mbrs of
            Just mbr ->
                modifyPostsRead'_ (user, feed) $
                    markBlogRead c what (Just feedMask) fid feed mbr d
            Nothing -> return ()

feedsAndTcsToFeedTcs :: ReadCounters -> HM.HashMap Int (Int, Int)
feedsAndTcsToFeedTcs feedsAndTcs =
    HM.fromList [(f, (tp,tc)) | (f,_,_,tp,tc) <- feedsAndTcs]

markSearchRead c user smartStream query feedMasksKey feedTcs mbrs d = do
    u <- cachedReadUser' user
    mbm <- Cache.lookup feedMasksKey feedMasksCache
    masks <-
        case mbm of
            Just m -> return m
            Nothing -> do
                (masks, _, _) <- withLogger $ \ l ->
                    getFeedMasksE l u feedTcs defaultMsgTreeViewMode
                        smartStream query
                return masks
    markMasksRead c
        (if isJust smartStream then "markSmartStreamSearchRead" else "markSearchRead")
        user masks mbrs d

mkFeedTcs :: ReadCounters -> [(Int, (Int, Int))]
mkFeedTcs feedsAndTcs = [(f, (tp,tc)) | (f,_,_,tp,tc) <- feedsAndTcs]

markSmartStreamRead c user smartStream feedTcs mbrs d = do
    u <- cachedReadUser' user
    fms <- lookupSmartStreamFeedMasks smartStream user
    when (not $ IntMap.null fms) $
        markMasksRead c "markSmartStreamRead" user fms mbrs d

markBlogReadApi user gri apiMode blogFeedUrl = do
    p@(Posts {..}) <- cachedReadPosts' blogFeedUrl
    fs <- cachedReadFilters' user
    let fmCheck
            | Just fm <- fidFeedMask Nothing (feedId blogFeedUrl gri) Nothing fs
            =   (`ReadSet.member` fmPosts fm)
            | otherwise =
                const True
    modifyPostsRead' (user, blogFeedUrl) $ \ pr -> do
        let go rs [] = return rs
            go !rs ((idx, mh):ps)
                | ReadSet.member idx (prIgnoredPosts pr) ||
                  ReadSet.member idx rs ||
                  not (fmCheck idx) ||
                  not (dlTimeOk apiMode mh)
                    = go rs ps
                | otherwise = go (ReadSet.insert idx rs) ps
        rs' <- go (prSet pr) $ BA.assocs $ mtHeaders pMsgTree
        -- TODO: хорошо бы еще проверять число непрочитанных, чтобы не бегать
        -- там, где все уже готово (в вебе мы и так ничего не посылаем,
        -- а тут похуже), но это как-то уж слишком замороченно.
        -- Причем проверять надо до modifyPostsRead,
        -- делая cachedNothingPostsReads (т.к. modify тоже делает чтение).
        -- Хотя с forkReadPar2 стало 1-3 секунды даже на 2k подписок
        return (pr { prSet = rs' }, ReadSet.size rs' - ReadSet.size (prSet pr))

skipComments c (MsgId {..}) totalComments pr =
    if midPostId == -1 || ReadSet.member midPostId (prIgnoredPosts pr) then return pr
    else do
--     modifyPostsRead'_ (user, msgKeyBlogFeedUrl) $ \
        -- TODO: проверять соотвествие id и guid-ов
        p <- cachedReadPosts' (prBfu pr)
        let cc = postCommentsCount midPostId totalComments p
            rs = fromMaybe ReadSet.empty $
                 IntMap.lookup midPostId (prCommentsRead pr)
            mbpguid
                | inRange (BA.bounds $ mtHeaders (pMsgTree p)) midPostId =
                    Just $ mhGuid $ mtHeaders (pMsgTree p) BA.! midPostId
                | otherwise =
                    Nothing
        pr' <- case (midCommentId, mbpguid) of
            (Nothing, _) -> do
                return $ pr { prSet = ReadSet.insert midPostId (prSet pr)
                            , prCommentsRead =
                                if cc > 0 then
                                    IntMap.insert midPostId
                                        (ReadSet.fromRange 0 (cc-1))
                                        (prCommentsRead pr)
                                else
                                    IntMap.delete midPostId (prCommentsRead pr)
                                    -- а надо ли delete?
                            , prTotalCommentsRead =
                                prTotalCommentsRead pr - ReadSet.size rs + cc
                            }
            (Just cid, Just pguid) -> do
                let ckey = CommentsKey (prBfu pr) pguid
                Comments {..} <- cachedReadComments' ckey
                let mark (cnt, rs) id
                        | id >= cc = (cnt, rs) -- пока не видимы пользователю
                        | otherwise =
                            foldl' mark
                            (if ReadSet.member id rs then (cnt, rs)
                             else (cnt+1, ReadSet.insert id rs))
                            (map tiId $ Set.toList $ mtChildrenSet cMsgTree id)
                    (tcr, rs') = mark (prTotalCommentsRead pr, rs) cid
                return $ pr { prTotalCommentsRead = tcr
                            , prCommentsRead = IntMap.insert midPostId
                                               rs' (prCommentsRead pr)
                            }
            _ -> return pr -- такого быть не должно ;)
        incrCounters "ps" c (ReadSet.size (prSet pr') - ReadSet.size (prSet pr))
        incrCounters "cs" c (prTotalCommentsRead pr' - prTotalCommentsRead pr)
        incrCounters "skip" c 1
        return pr'

-- ignorePost :: _ -> MsgId -> Int -> PostsRead -> IO PostsRead
ignorePost c (MsgId {..}) totalComments pr@(PostsRead {..}) =
    if midPostId == -1 || ReadSet.member midPostId prIgnoredPosts then return pr
    else do
--         when (not $ ReadSet.member midPostId (prSet pr)) $
--             incrCounters "ps" c 1
        let cc = maybe 0 ReadSet.size $ IntMap.lookup midPostId prCommentsRead
        incrCounters "pi" c 1
        incrCounters "ci" c cc
        incrCounters "ignore" c 1
        return $
            pr
            { prSet = ReadSet.delete midPostId prSet
            , prIgnoredPosts = ReadSet.insert midPostId prIgnoredPosts
            , prCommentsRead = IntMap.delete midPostId prCommentsRead
            , prTotalCommentsRead = prTotalCommentsRead - cc
            }

markRead :: IORef (Map.Map T.Text Int) -> Key User
    -> MarkReadDirection -> Int -> MsgTreeViewMode -> [(UrTime, MsgId)]
    -> MarkReq -> IO ()
markRead c user dir0 olderThan mtvm uiPosts markReq = do
    gri <- cachedReadGRIds' user
--    logT $ T.unlines [showT dir, showT markReq, showT pid']
    usageFlag user uf
    case markReq of
        MRPosts tcs ->
            markMbrs gri "markBlogRead" $ mbrs tcs
        MRTags tags maxTag ->
            markTags gri Nothing tags maxTag
        MRSmartStream sn tcs ->
            markSmartStreamRead c user sn tcs (mbrs tcs) olderThan
        MRSearchPosts q fmk tcs ->
            markSearchRead c user Nothing q fmk tcs (mbrs tcs) olderThan
        MRSearchTags query idsKey tags maxTag -> do
            ids <- getCachedTagsFilterIds idsKey user query tags
            markTags gri (Just ids) tags maxTag
        MRSearchSmartStream sn q fmk tcs ->
            markSearchRead c user (Just sn) q fmk tcs (mbrs tcs) olderThan
    where dir | mtvmAscending mtvm = case dir0 of
                  MRDAbove p -> MRDBelow p
                  MRDBelow p -> MRDAbove p
                  MRDAll -> MRDAll
              | otherwise = dir0
          uf  | MRDAbove _ <- dir0 = UFMarkReadAbove
              | MRDBelow _ <- dir0 = UFMarkReadBelow
              | olderThan == 0 = UFMarkAllAsRead
              | search = UFMarkSearchAsReadD olderThan
              | otherwise = UFMarkAllAsReadD olderThan
          search = case markReq of
              MRSearchPosts {} -> True
              MRSearchTags {} -> True
              MRSearchSmartStream {} -> True
              _ -> False
          mbrs tcs = case dir0 of
              MRDAll -> map (second markAll) tcs
              MRDAbove (_, (_, MsgId { midFeedId = feedId }), _)
                  | mtvmGroupByFeed mtvm ->
                      markGroupByFeed feedId $ reverse tcs
              MRDBelow (_, (_, MsgId { midFeedId = feedId }), _)
                  | mtvmGroupByFeed mtvm ->
                      markGroupByFeed feedId tcs
              _ ->
                  map markAboveOrBelow tcs
          markGroupByFeed feedId tcs =
              case dropWhile ((/= feedId) . fst) tcs of
                  [] -> []
                  (x:xs) ->
                      markAboveOrBelow x : map (second markAll) xs
          markAll (tp, tc) =
              MarkBlogRead
              { mbrTotalPosts = tp
              , mbrTotalComments = tc
              , mbrMarkPost = \ _ _ -> True
              , mbrMarkCommentAboveBelow = \ _ -> Nothing
              , mbrOverrideTime = \ _ _ -> Nothing
              }
          pid' = case dir0 of
              MRDAbove (_, (False, MsgId { midCommentId = Just cid, ..}), _) ->
                  0 -- в случае above из комментария пост помечается
              _ ->
                  1
          markAboveOrBelow (fid, (tp, tc)) =
              (fid
              ,MarkBlogRead
               { mbrTotalPosts = tp
               , mbrTotalComments = tc
               , mbrMarkPost = case dir of
                  MRDAbove (pTime, (_, MsgId {..}), _)
                      | fid < midFeedId ->
                          \ time _ -> time >= pTime
                      | fid == midFeedId ->
                          \ time pid -> (time, pid) >= (pTime, midPostId+pid')
                      | otherwise ->
                          \ time _ -> time > pTime
                  MRDBelow (pTime, (_, MsgId {..}), _)
                      | fid < midFeedId ->
                          \ time _ -> time < pTime
                      | fid == midFeedId ->
                          \ time pid -> (time, pid) <= (pTime, midPostId-pid')
                      | otherwise ->
                          \ time _ -> time <= pTime
                  _ ->
                      \ _ _ -> True
               , mbrMarkCommentAboveBelow = markComment fid
               , mbrOverrideTime = \ _ _ -> Nothing
               })
          markComment fid =
              case dir0 of
                  MRDAbove (_, (False, MsgId { midCommentId = Just cid, ..}), _)
                      -- учитывая, что комментарии выше могут быть свернуты
                      -- и еще не загружены, начинаем помечать не с первого
                      -- найденного выше в UI, а именно выше текущего
                      -- (а если начинали с поста, то по-умолчанию и так
                      -- пометятся все комментарии предыдущего поста)
                      | fid == midFeedId ->
                          \ pid -> do
                              guard (pid == midPostId)
                              return (MCAbove cid True)
                  MRDBelow (_, _, (False, MsgId { midCommentId = Just cid, ..}))
                      -- помечаем только начиная со следующего видимого в UI
                      -- комментария (чтобы не помечать свернутые комментарии
                      -- поста, заодно не помечаются свернутые поддеревья,
                      -- что спорно, но оставим так)
                      | fid == midFeedId ->
                          \ pid ->
                              guard (pid == midPostId) >> return (MCBelow cid)
                  _ ->
                      \ _ -> Nothing
          markMbrs gri what = parMapM_ $ \ (fid, mbr) ->
              let bfu = feedUrl fid gri in
              modifyPostsRead'_ (user, bfu) $
                  markBlogRead c what Nothing fid bfu mbr olderThan
          parMapM_ f l = concurrently_ (mapM_ f l1) (mapM_ f l2)
              where (l1, l2) = splitAt (length l `div` 2) l
          markTags gri ids tags maxTag = do
              let (point, tls) = case dir0 of
                      MRDAll -> (Nothing, TLSAll)
                      MRDAbove (t, mkRoot -> m, _) -> (Just m, TLSAbove (t, m))
                      MRDBelow (t, mkRoot -> m, _) -> (Just m, TLSBelow (t, m))
                  mkRoot (True, mid) = mid
                  mkRoot (False, mid) = mid { midCommentId = Nothing }
              logS $ show ("lastMsg", tls, tags)
              (ts, _) <- tagsList ids maxTag gri (AMNormal "" "") tls tags user
                  (mtvm { mtvmUnreadOnly = False })
                  -- точно ли мы помечаем комментарии у невидимых прочитанных?
              let mids = uiPosts <> tsMids
                  tsMids = map (\(t,mid,_) -> (t,mid)) ts
                  pointGroup =
                      -- в случае mark below в комментариях добавляем feedId
                      -- поста к группам (т.к. больше в тегах может и не быть
                      -- постов из этого фида и комментарии тогда не пометятся)
                      maybe IntMap.empty
                      (\ m -> IntMap.singleton (midFeedId m)
                          (IntSet.empty, IntMap.empty, Map.empty))
                      point
                  groupped = IntMap.toList
                      $ groupMidsByFeed gri mids `IntMap.union` pointGroup
--               logS "uiPosts"
--               mapM_ (logS . show) uiPosts
--               logS "ts"
--               mapM_ (logS . show) tsMids
--               logS "groupped"
--               mapM_ (logS . show) groupped
              markMbrs gri "markTags" $ map markTagsMbr groupped
          groupMidsByFeed gri mids =
              IntMap.fromListWith
              (\ (pa,ca,ta) (pb,cb,tb) ->
                  (IntSet.union pa pb, IntMap.unionWith IntSet.union ca cb
                  ,Map.union ta tb))
              [(midFeedId, case midCommentId of
                  Just cid ->
                      (IntSet.empty
                      ,IntMap.singleton midPostId (IntSet.singleton cid)
                      ,tmap)
                  Nothing  ->
                      (IntSet.singleton midPostId, IntMap.empty, tmap))
              |(t,MsgId {..}) <- mids
              ,isActiveFeed (feedUrl midFeedId gri) gri
              ,let tmap = Map.singleton (midPostId, midCommentId) t
              ]
          markTagsMbr (fid, (posts, comments, timeMap)) =
              (fid
              ,MarkBlogRead
               { mbrTotalPosts = maxBound
               , mbrTotalComments = maxBound
               , mbrMarkPost = \ _ pid -> IntSet.member pid posts
               , mbrMarkCommentAboveBelow = \ pid ->
                  case IntMap.lookup pid comments of
                      Just cs | not (IntSet.member pid posts) ->
                          Just $ MCSelected $ IntSet.toList cs
                      _ ->
                          markComment fid pid
               , mbrOverrideTime = \ pid cid -> Map.lookup (pid, cid) timeMap
               })

miLongMsgId mi = LongMsgId (miMsgKey mi) (miMsgId mi)

removeTagFromTree :: Key User -> Maybe [ItemTag] -> MsgTreeViewMode -> [TreeReq]
    -> IO ()
removeTagFromTree user tags mtvm reqs = do
    gri <- cachedReadGRIds' user
    iterateTreeReqs "removeTagFromTree" (mark gri) user []
        (mtvm
         { mtvmExpandedComments = False
           -- не заходим в комментарии
         , mtvmNoOverride = False
         })
        reqs
    where mark gri items =
              performBgActions user
                  [BGRemoveTag (miLongMsgId mi) tag
                   -- maxBound используется только при убирании отметки
                   -- ignore, чего у нас не происходит, т.к. мы помечаем
                   -- прочитанным, а не наоборот
                  | mi <- items
                  , tag <- (maybe (filter isITTag) intersect tags) (miTags mi)]

isITTag (ITTag _) = True
isITTag _ = False

mfRequests (MsgForest {..}) =
    concatMap mfRequests (map snd mfList) ++ maybeToList mfNextReq
mkReqs (Just mf : mfs) (_:rs) =
    mfRequests mf ++ mkReqs mfs rs
mkReqs _ rs = rs

iterateTreeReqs :: T.Text -> ([MsgItem] -> IO T.Text)
    -> Key User -> [ItemTag] -> MsgTreeViewMode -> [TreeReq] -> IO ()
iterateTreeReqs what f user tags mtvm reqs0 = withLogger $ \ l -> do
    r <- logTime l what $ loopReqs [] reqs0
    logLT l $ T.unwords r
    where loopReqs acc [] = return $ reverse acc
          loopReqs acc reqs = do
              logTL ["loopReqs ", showT reqs]
              am <- apiMode
              mfs <- getTree am user mtvm reqs
              x <- f $ filter miFull $ concatMap items $ catMaybes mfs
              loopReqs (x:acc) (mkReqs mfs reqs)
          items (MsgForest {..}) =
              concat [mi : items mf | (mi,mf) <- mfList]
          apiMode = newIORef Nothing >>= \ ref -> return $
--               AMNormal "" ""
              AMGRIdsOnly
              { amFetch          = False
              , amCount          = 50000
              , amContinuation   = UnsafeRef ref
              , amMinDlTime      = Nothing
              , amMaxDlTime      = Nothing
              , amMaxTime        = Nothing
              , amExcludeTags    = HS.empty
              , amIncludeTags    = HS.fromList tags
              , amReadOnly       = False
              , amMsgLinkParams  = []
              , amFromUI         = True
              , amMaxMsgTextLength = Nothing
              }


data EditItemTag
    = EITAdd !UrTime
    | EITRemove

-- iis должны подаваться задом наперед, от новых к старым.
editItemTags :: Key User -> [(EditItemTag, ItemTag, LongMsgId)] -> IO ()
editItemTags user [] = return ()
editItemTags user iis =
    editItemTags' user [mid | (_,_,mid) <- iis] modGri
    where modGri ids = foldl' edit ids $ reverse iis -- чтобы в том же порядке
          edit !ids (eit, tag, lmsgId)
              | noChange = ids
              | otherwise =
                  griTaggedItems ids `seq` griLastId ids `seq`
                  ids
                  { griItemTags = IntMap.alter
                    ((\ ts -> if null ts then Nothing else Just ts) .
                     -- nub . -- O(n^2)
                     fi . fromMaybe [])
                    iid (griItemTags ids)
                  , griTaggedItems = Map.alter
                    ((\ is -> if null is then Nothing else Just is) .
                     -- nubTaggedItems . -- O(n^2)
                     ft . fromMaybe [])
                    tag (griTaggedItems ids)
                  , griLastId = griLastId ids + 1
                    -- чтобы версия росла, и Eq быстрее работает
                  }
              where iid = midGrItemId $ lmidMsgId lmsgId
                    exists
                        | Just ts <- IntMap.lookup iid (griItemTags ids)
                        = tag `elem` ts
                        | otherwise = False
                    (noChange, fi, ft) = case eit of
                        EITAdd t ->
                            ( exists
                            , (tag :)
                            , ((t, iid, lmidMsgKey lmsgId) :) )
                        EITRemove ->
                            ( not exists
                            , filter (/= tag)
                            , rmFirst )
                    rmFirst [] = []
                    rmFirst (x@(_,iid',_):xs)
                        | iid == iid' = xs
                        | otherwise   = x : rmFirst xs

editItemTags' :: Key User -> [LongMsgId] -> (GRIds -> GRIds) -> IO ()
editItemTags' user mids modGri = withLogger $ \ l -> logTime l "editItemTags" $ do
    logLS l $ "processing " ++ show (length mids) ++ " tags"
--     logTime l "dummyModify" $ modifyGRIds'_ user $ \ ids ->
--         return $ ids { griLastId = griLastId ids + 1 }
    gri <- -- logTime l "modifyGRIds" $
        modifyGRIds' user $ \ ids -> do
        let r = modGri ids
        return (r,r)
    -- определяем, что добавилось индексируем и редактируем PostsTagged
    -- определяем, что удалилось и удаляем из индекса
    let tagged =
            IntMap.fromList [(midGrItemId $ lmidMsgId mid, mid) | mid <- mids]
        active = tagged `IntMap.intersection` griItemTags gri
        activeTags =
            Set.fromList $ concat $ IntMap.elems $
            griItemTags gri `IntMap.intersection` tagged
        removed = tagged `IntMap.difference` griItemTags gri
        activePosts =
            HM.fromListWith IntMap.union
            [( msgKeyBlogFeedUrl (lmidMsgKey mid)
             , IntMap.singleton
               (midPostId $ lmidMsgId mid)
               (fromMaybe "" $ msgKeyPostGuid $ lmidMsgKey mid) )
            | mid <- IntMap.elems active ]
        setFromMap = HS.fromList . HM.keys
        activeFromDiscovery =
            setFromMap activePosts
            `HS.difference` fromMaybe HS.empty (griActiveFeeds gri)
            `HS.difference` maybe HS.empty setFromMap (griRemovedFeeds gri)
    when (HS.size activeFromDiscovery > 0) $ do
        -- запоминаем названия фидов, в которых поставили звездочку/тег,
        -- но при этом на них не подписаны (т.е. пометили в режиме discovery)
        let xs = HS.toList activeFromDiscovery
        ps <- cachedReadManyPostss xs
        t <- getUrTime
        modifyGRIds'_ user $ \ gri -> return $
            gri
            { griRemovedFeeds = Just $
                HM.fromList
                [( u, RemovedFeedInfo (msgSubject (pRootMessage p))
                        (fromMaybe u $ msgLink (pRootMessage p)) t 0)
                | Just p <- ps
                , let u = pBlogFeedUrl p]
                `HM.union` (fromMaybe HM.empty $ griRemovedFeeds gri) }
    setTagged <- async $ forM_ (HM.toList activePosts) $ \ (bfu, tagged) -> do
        modifyPostsTagged'_ bfu $ \ pt ->
            return $ pt { ptSet = foldl' (flip ReadSet.insert) (ptSet pt)
                                  (IntMap.keys tagged)
                        }
        modifyPostsTaggedGuids'_ bfu $ \ ptg ->
            return $ ptg { ptgGuids = IntMap.union (ptgGuids ptg) tagged
                         }
    deleteSearch <- async $ deleteSearchTagged l user (IntMap.elems removed)
    saveSearch <- async $
        saveSearchTagged l user $ IntMap.elems $
            IntMap.intersectionWith (,) active (griItemTags gri)
    wait saveSearch
    wait setTagged
    wait deleteSearch
    ust <- cachedReadUserSettings' user
    let toPing =
            [ f
            | (pft,fs) <- maybe [] Map.toList (ustPublicFeeds ust)
            , case pft of
                  PFTTag t -> ITTag t `Set.member` activeTags
                  PFTAllTags -> Set.delete ITStarred activeTags /= Set.empty
                  PFTStarred -> ITStarred `Set.member` activeTags
                  _ -> False
            , (f, True, _) <- fs
            ]
    when (notNull toPing) $ void $ forkIO $ mapM_ pingPublicFeed toPing
    return ()

incrCounters :: T.Text -> IORef (Map.Map T.Text Int) -> Int -> IO ()
incrCounters what c !amount =
    atomicModifyIORef' c ((,()) . Map.insertWith (+) what amount)

performBgActions :: Key User -> [BgAction] -> IO T.Text
performBgActions user as = go 0 (return "")
    where go n e
              | n > 4 = e
              | otherwise = do
                  gri <- cachedReadGRIds' user
                  r <- E.try $ performBgActions' user gri as
                  case r of
                      Right ok -> return ok
                      Left (e :: E.SomeException) -> do
                          logS $ "Exception in performBgActions: " ++ show e
                          incrStat "exceptions" 1
                          -- threadDelay 50000
                          -- go (n+1) (E.throwIO e)
                          --  ^ надо было ловить только RiakException,
                          -- а то таймауты отваливались
                          E.throwIO e

performBgActions' :: Key User -> GRIds -> [BgAction] -> IO T.Text
performBgActions' user gri as = do
--    logS $ "performBgActions " ++ show user ++ " " ++ show as
    counters <- newIORef Map.empty
    flush =<< foldM (fold counters) emptyAcc (removeRepetitions as)
    c <- readIORef counters
    return $ T.intercalate " " [T.concat [T.pack (show n), " ", w]
                                | (w, n) <- Map.toList c, n /= 0]
    where emptyAcc = (return, Nothing, [])
          removeRepetitions = map head . group
          -- все BgAction являются идемпотентными, удаляем повторы, которые
          -- могут возникнуть, если, например, нажать Shift+Q (mark above
          -- as read) и держать.
          fold c p@(uvmf, cp, iis) act =
              let e a = a >> return (uvmf, cp, iis)
                  u f = return (uvmf >=> f, cp, iis)
                  us f = e $ updUserSettings user f
                  ordering f =
                      e $ modifyGRIds'_ user $ \ gri ->
                          return $ gri { griOrdering = f gri (griOrdering gri) }
                  tag eit mid t =
                      return (uvmf, cp, (eit, t, mid) : iis)
                  uf = usageFlag user
              in
              case act of
                  BGMarkMsgRead mid r tc -> do
                      when (not r) $ uf UFKeepUnread
                      checkM p mid $ markMsgRead c mid r tc
                  BGSkipComments mid tc -> do
                      uf UFSkip
                      checkM p mid $ skipComments c mid tc
                  BGIgnorePost mid tc -> do
                      uf UFIgnore
                      checkM p mid $ ignorePost c mid tc
                  BGAddTag mid t -> do
                      uf $ if t == ITStarred then UFStar else UFTag
                      time <- getUrTime
                      tag (EITAdd time) mid t
                  BGRemoveTag mid t -> do
                      uf $ if t == ITStarred then UFStar else UFTag
                      tag EITRemove mid t
                  BGRemoveTagD ts d -> do
                      incrCounters (T.pack $ show act) c 1
                      r <- flush p
                      userRemoveTagD user ts d
                      return r
                  BGSetOnlyUpdatedSubscriptions v ->
                      u $ \ uvm -> do
                          incrCounters (if v then "SUpdated" else "SAll") c 1
                          return $
                              uvm { uvmOnlyUpdatedSubscriptions =
                                        (succ $ fst $
                                         uvmOnlyUpdatedSubscriptions uvm, v) }
                  BGSetFolderViewMode f vm ->
                      u $ \ uvm -> do
                          logVM "f" c vm f $ uvmFolderViewModes uvm
                          return $ uvm { uvmFolderViewModes = insVM f vm $
                                         uvmFolderViewModes uvm }
                  BGSetSubscriptionViewMode f vm ->
                      u $ \ uvm -> do
                          logVM "s" c vm f $ uvmSubViewModes uvm
                          return $ uvm { uvmSubViewModes = insVM f vm $
                                         uvmSubViewModes uvm }
                  BGClearAllSubscriptions -> e $ do
                      incrCounters "clearAllSubscriptions" c 1
                      s <- fmap uSubscriptions $ readUser' user
                      userUnsubscribe user $ map sUrl s
                  BGSaveFilterQuery q -> e $ saveFilterQuery user q
                  BGSetScrollMode sm -> us $ \ s -> s { ustScrollMode = sm }
                  BGSetListViewMode m -> us $ \ s -> s { ustListViewMode = m }
                  BGSetMarkReadMode m -> us $ \ s -> s { ustMarkReadMode = m }
                  BGSetUltraCompact m -> us $ \ s -> s { ustUltraCompact = m }
                  BGSetCountry c -> us $ \ s -> s { ustCountry = Just c }
                  BGSetExactUnreadCounts m ->
                      us $ \ s -> s { ustExactUnreadCounts = m }
                  BGDragAndDrop w ia s t -> do
                      uf UFDragAndDrop
                      e $ userDragAndDrop user w ia s t
                  BGSortAllFeedsAndFolders ->
                      ordering $ const $ const Map.empty
                  BGSortFolder f ->
                      ordering $ const $ Map.delete f
                  BGSortTags ->
                      ordering $ \ gri om ->
                          case Map.lookup "" om of
                              Nothing -> om
                              Just o -> Map.insert "" (sortTags gri o) om
                  BGShareAction a -> do
                      uf $ UFShareAction a
                      e $ alterUserStats user $ return . (,())
                          . fmap (incrUserStatsCounters (T.pack $ show a))
                  BGWhatsNewClick t ->
                      e $ uf $ UFWhatsNewClick t
                  BGWhatsNewClose t -> do
                      uf $ UFWhatsNewClose t
                      us $ modifyUSTEx $ \ e -> e { usteLastWhatsNewTime = t }
                  BGMarkRead d o vm ps mr -> do
                      r <- flush p
                      markRead c user d o vm ps mr
                      return r
                  BGRemoveTagFromTree a t vm rs -> do
                      uf $ case (t == Just [ITStarred], a) of
                          (True , True) -> UFUnstarAbove
                          (True , _)    -> UFUnstarBelow
                          (False, True) -> UFUntagAbove
                          (False, _)    -> UFUntagBelow
                      r <- flush p
                      removeTagFromTree user t vm rs
                      return r
          sortTags gri o =
              let tags =
                      [ (i,t) | ITTag t <- Map.keys (griTaggedItems gri)
                      , i <- maybeToList (HM.lookup t (griFolderIds gri)) ]
                  tagsSet = IntSet.fromList $ map fst tags
              in
                  filter (not . (`IntSet.member` tagsSet)) o
                  ++ map fst (sortBy (comparing $ T.toLower . snd) tags)
          checkM p m = checkU p $ feedUrl (midFeedId m) gri
          checkU (uf, Nothing, iis) u f = return (uf, Just (u, f), iis)
          checkU (uf, Just (u0, f0), iis) u f
              | u == u0 = return (uf, Just (u0, f0 >=> f), iis)
              | otherwise = run (u0, f0) >> checkU (uf, Nothing, iis) u f
          flush (u,p,iis) = do
              modUVM u
              maybe (return ()) run p
              editItemTags user iis
              return emptyAcc
          run (bfu, f) = modifyPostsRead'_ (user, bfu) $ \ pr -> do
              -- p <- cachedReadPosts' bfu
              -- fmap (fixPostsRead p) $
              f pr -- проверить -- не пашет, видимо из-за resolve, которому
              -- больше нравится prSet с большим кол-вом прочитанных сообщений
          logVM prefix c vm f ms = do
              when (neq mtvmAscending) $
                  stat $ if mtvmAscending vm then "Asc" else "Desc"
              when (neq mtvmUnreadOnly) $
                  stat $ if mtvmUnreadOnly vm then "UnreadOnly" else "AllItems"
              when (neq mtvmExpandedComments || neq mtvmPosts) $
                  stat $ case (mtvmPosts vm, mtvmExpandedComments vm) of
                      (PVMShort, _) -> "ListView"
                      (PVMFull, True) -> "FullExp"
                      (PVMFull, False) -> "FullColl"
                      (PVMMagazine, _) -> "Magazine"
                      (PVMMosaic, _) -> "Mosaic"
              where vm0 = maybe defaultMsgTreeViewMode snd (HM.lookup f ms)
                    neq :: Eq a => (MsgTreeViewMode -> a) -> Bool
                    neq f = f vm0 /= f vm
                    stat what = incrCounters (T.append prefix what) c 1
          insVM f vm ms =
              HM.unionWith max ms $
              HM.singleton f (1 + maximum (0 : map fst (HM.elems ms)), vm)
          modUVM f =
              modifyUser'_ user $ \ u -> do
                  vm' <- f $ uViewMode u
                  return $ u { uViewMode = vm' }

userDragAndDrop user what insertAfter s t = case (what, insertAfter, s, t) of
    (SITFeed {..}, Nothing, from, Just to) ->
        -- нет insertAfter, значит просто фид в папку затащили
        changeFolders sitSubscription from (Just to)
    (sit, Just insAfter, from, to) -> do
        -- затащили в папку или поменяли порядок в папке
        case sit of
            SITFeed {..} -> changeFolders sitSubscription from to
            _ -> return ()
        o <- userGetOrdering uSubscriptions user
        let oid = fromMaybe "" to
        case lookup oid o of
            Just (_, ordering) ->
                modifyGRIds'_ user $ \ gri -> do
                    let folderId f = HM.lookup f (griFolderIds gri)
                        sitGRId sit = case sit of
                            SITFeed { sitSubscription = s, .. } ->
                                HM.lookup (sUrl s) (griFeedIds gri)
                            SITStarred -> Just starredSortId
                            SITAllTags -> Just allTagsSortId
                            SITFolder f -> folderId f
                            SITSmartStream n _ -> folderId n
                            SITTag t -> folderId t
                            SITAll -> Just allItemsSortId
                            _ -> Nothing
                        tagsIds =
                            IntSet.fromList $ mapMaybe folderId
                            [t | ITTag t <- Map.keys (griTaggedItems gri)]
                        ins fi ia
                            | ia == allItemsSortId
                            , SITTag _ <- sit
                            , o <- filter (/= fi) ordering
                            , Just firstTag <- find (`IntSet.member` tagsIds) o
                            , (a, (_:b)) <- span (/= firstTag) o =
                                a ++ (fi:firstTag:b)
                                -- вставляем перед следующим тегом, а не после
                                -- allTags, чтобы в мобильных приложениях
                                -- теги наверх не убегали
                            | (a, (_:b)) <- span (/= ia) $
                                            filter (/= fi) ordering =
                                a ++ (ia:fi:b)
                            | otherwise = fi : ordering
                        o' | Just fi <- sitGRId sit =
                               nub $ case sitGRId insAfter of
                                   Just ia -> ins fi ia
                                   _ -> fi : ordering -- в начало
                           | otherwise = ordering
                    return $
                        gri { griOrdering = Map.insert oid o' (griOrdering gri) }
            Nothing ->
                return () -- почему-то нет такой папки?
    _ ->
        return ()
    where changeFolders sub from to = do
              let mod s
                      | sUrl s == sUrl sub =
                          s { sEditsCount = sEditsCount s + 1 -- ??
                            , sFolders = nub $
                                maybeToList to ++
                                maybe id (filter . (/=)) from (sFolders s)
                            }
                      | otherwise = s
              userMapSubscriptions mod user

allItemsSortId = 0xffffffff
starredSortId  = 0xfffffffa
allTagsSortId  = 0xffffff00
maxFeedSortId  = 0xfffff000

reorder :: (b -> Int) -> GRIds -> T.Text -> [b] -> [b]
reorder getId gri f is
    | Just sortIds <- Map.lookup f (griOrdering gri) = reorder' getId sortIds is
    | otherwise = is
reorder' :: (b -> Int) -> [Int] -> [b] -> [b]
reorder' getId sortIds is =
    map snd $ sortBy (comparing fst) $ map (\ i -> (o i, i)) is
    where order = IntMap.fromList $ zip sortIds [0..]
          o (getId -> i) =
              maybe (noOrder i) Left $ IntMap.lookup i order
          noOrder i
            | i >= allTagsSortId = Left (0 - i) -- до остальных подписок
            | otherwise = Right i -- после, в порядке добавления подписок

-- переупорядочиваем список фидов (или чего-то, имеющее адрес фида).
-- тот же reorder, но для toplevel получает порядок всех фидов, а не только
-- toplevel и id не чего угодно (папок, фидов, тегов), а только фидов.
-- используется в group by feed
sortByFeed :: (t -> Int) -> MsgTreeViewMode -> GRIds
     -> [(T.Text, (Bool, [Int]))] -> Maybe T.Text -> [t] -> [t]
sortByFeed getId' viewMode gri ordering f items
    | mtvmGroupByFeed viewMode
    , Just o <- order = reorder' getId o items
    | otherwise = items
    where getId (getId' -> i)
              | T.isPrefixOf grBfuPrefix (feedUrl i gri) = maxFeedSortId
                -- импортированное всегда в конце
              | otherwise = i
          order =
              do folder <- f
                 fmap snd $ lookup folder ordering
              <|>
              rootFeedsOrdering gri ordering

rootFeedsOrdering gri ordering = do
    (_,ro) <- lookup "" ordering
    return $ hashSetNub $ concatMap feedIds ro
    where feedIds i
              | Just folder <- IntMap.lookup i (griFolderNames gri) =
                  fromMaybe [] (fmap snd $ lookup folder ordering)
              | Just _feed <- IntMap.lookup i (griFeedUrls gri) =
                  [i]
              | otherwise = []

userGetOrdering :: (User -> [Subscription])
     -> Key User -> IO [(T.Text, (Bool, [Int]))]
userGetOrdering subscriptions user = do
    -- для API выдаются только активные подписки,
    -- это приводит к тому, что при перестановке фидов в мобильном клиенте,
    -- ошибочные фиды сбрасываются вниз, ну и ладно
    u <- cachedReadUser' user
    i <- getGRIds' user (uSubscriptions u)
    fs <- cachedReadFilters' user
    let ins n x = HM.insertWith (flip (++)) n [x]
        fvms = uvmFolderViewModes $ uViewMode u
        folders = sortBy (comparing $ T.toLower . fst) $ HM.toList $
            foldl' (\ f s ->
                        if null (sFolders s) then ins "" s f
                        else foldl' (\ f fn -> ins fn s f) f (sFolders s))
                   (HM.singleton "" []) -- всегда выдаем root folder
                   (subscriptions u)
        foldersSet = HS.fromList $ map fst folders
        smartStreams = sortBy (comparing T.toLower)
            [ ssName s | s <- fSmartStreams fs
            , not (HS.member (ssName s) foldersSet) ]
        tags = sortBy (comparing T.toLower)
            [ t | ITTag t <- Map.keys (griTaggedItems i)
            , not (HS.member t foldersSet) ]
        order = reorder id i
        makeIds ids = map (\ id -> HM.lookupDefault 0 id (ids i))
        folder (name, s) =
            ( name
            , (fromMaybe True $
               fmap (mtvmFolderExpanded . snd) $ HM.lookup name fvms
              ,order name $
                 (if name == "" then
                      \ t ->
                       [starredSortId, allTagsSortId]
                       ++ makeIds griFolderIds smartStreams
                       ++ makeIds griFolderIds (filter (/= "") $ map fst folders)
                       ++ t
                       ++ makeIds griFolderIds tags
                  else id) $
                 makeIds griFeedIds $ map sUrl $
                 sortBy (comparing $ \ s ->
                         T.toLower $ fromMaybe (sUrl s) (sTitle s)) s )
            )
    return $ map folder folders

userMapSubscriptions upd user =
    modifyUser'_ user $ \ user -> return $ user { uSubscriptions =
        map upd $ uSubscriptions user }

userRetrySubscription user u = do
    usageFlag user UFRetrySubscription
    let upd s
            | sUrl s == u =
                s { sState = SSAdded
                  , sEditsCount = sEditsCount s + 1 }
            | otherwise = s
    modifySubscriptionUrlInfo'_ u $ \ sui ->
        return $ sui { suiTime = UrTime 0 0 }
        -- делаем невалидной, чтобы сразу не переподписался
    userMapSubscriptions upd user
    subscribeUrls [u]
    addActiveCheckSubscriptions user

saveFilterQuery :: Key User -> T.Text -> IO ()
saveFilterQuery user query = do
    t <- getUrTime
    modifyUserFilters'_ user $ \ uf ->
        return $ uf { ufFilters = (t, query) :
                                  take 99
                                  (filter ((/=) query . snd) (ufFilters uf)) }

feedMasksCache :: Cache.Cache T.Text (IntMap FeedMask)
feedMasksCache = unsafePerformIO $ Cache.new 300
{-# NOINLINE feedMasksCache #-}

tagsFilterCache :: Cache.Cache T.Text IntSet.IntSet
tagsFilterCache = unsafePerformIO $ Cache.new 300
{-# NOINLINE tagsFilterCache #-}

tagsForest apiMode user tags viewMode = do
    gri <- getGRIds user
    (items, maxTag) <-
        tagsList Nothing maxMaxTag gri apiMode TLSAll tags user viewMode

    let markReq = MRTags tags maxTag

    uc <- markReqReadCounters user viewMode markReq []

    (markReq, uc,) <$> userGetMsgForest apiMode user viewMode
        (TRTags tags maxTag Nothing)

filterTagsForest :: Key User -> T.Text
    -> Maybe [ItemTag] -> MsgTreeViewMode -> THostName -> T.Text
    -> IO (Either T.Text (MarkReq, ReadCounters, FilterResults))
filterTagsForest user query tags viewMode hostName acceptLanguage = withLogger $ \ l -> do
    usageFlag user UFSearchTags
    saveFilterQuery user query
    t0 <- getUrTime

    gri <- getGRIds user

    let idsKey = T.append (T.pack $ showUrTime t0)
                          (hashData (BL.toStrict $ encode (query, tags)))
        apiMode = AMNormal hostName acceptLanguage
        tl i = tagsList i maxMaxTag gri apiMode TLSAll tags user viewMode

    ids <- getTagsFilterIds l user query tags
    case ids of
        Left e -> return $ Left e
        Right ids -> do
            Cache.insert idsKey ids tagsFilterCache

            (_, rp, rc, tp, tc) <- tagReadCountersFromIds gri user tags ids
            (_, maxTag) <- tl (Just ids)

            frMsgForest <-
                userGetMsgForest apiMode user viewMode
                    $ TRSearchTags query idsKey tags maxTag Nothing
            t2 <- getUrTime
            let frTookReal = truncate $ 1000 * (t2 `diffUrTime` t0)
                frTook = frTookReal
                frTotalPosts = tp
                frTotalComments = tc
                frUnreadPosts = tp - rp
                frUnreadComments = tc - rc
                markReq =
                    MRSearchTags
                    { mrQuery = query
                    , mrIdsKey = idsKey
                    , mrTags = tags
                    , mrMaxTag = maxTag
                    }
            uc <- markReqReadCounters user viewMode markReq []
            return $ Right (markReq, uc, FilterResults {..})

getCachedTagsFilterIds idsKey user query tags = do
    mbi <- Cache.lookup idsKey tagsFilterCache
    case mbi of
        Just i -> return i
        Nothing -> do
            i <- withLogger $ \ l -> do
                logLS l $ "Restoring ids for " ++ show query
                getTagsFilterIdsE l user query tags
            Cache.insert idsKey i tagsFilterCache
            return i

type ReadCounters = [(Int, Int, Int, Int, Int)]

filterForest :: Key User -> Maybe T.Text -> T.Text -> Maybe T.Text
             -> FeedsOrDiscovery -> MsgTreeViewMode -> THostName -> T.Text
             -> IO (Either T.Text (MarkReq, ReadCounters, FilterResults))
filterForest user name query folder feedsOrDiscovery viewMode hostName acceptLanguage = withLogger $ \ l -> do
    u <- cachedReadUser' user
    forM_ name $ \ ss -> checkSmartStreamError ss user
    (gri, apiMode, feedsAndTcs) <-
        griApiModeAndTcs u feedsOrDiscovery viewMode hostName acceptLanguage
    logLTL l ["Search for “", query, "” by ", user, " ("
             ,showT (length feedsAndTcs), " feeds)"]
    usageFlag user UFSearch
    saveFilterQuery user query
    t0 <- getUrTime
    getuc <- forkRead $ getUpdatedCounts u feedsAndTcs viewMode folder Nothing
    let feedMasksKey = T.append (T.pack $ showUrTime t0)
                                (hashData (BL.toStrict $ encode
                                                 (name, query, feedsAndTcs)))
    fms <- getFeedMasks l u (mkFeedTcs feedsAndTcs) viewMode name query
    case fms of
        Left e -> return $ Left e
        Right ( feedMasks
              , (frTotalPosts, frTotalComments)
              , (frUnreadPosts, frUnreadComments) ) -> do
            Cache.insert feedMasksKey feedMasks feedMasksCache

            let reqs = postsReqsFromFeedMasks feedsAndTcs feedMasks viewMode
            (updatedCounts, _, _, ordering, feedTcs) <- getuc
            frMsgForest <-
                userGetMsgForest apiMode user viewMode
                    $ (maybe TRSearchPosts TRSearchSmartStream name)
                        query feedMasksKey
                    $ sortByFeed prqFeedId viewMode gri ordering folder reqs
            t1 <- getUrTime
            let frTookReal = truncate $ 1000 * (t1 `diffUrTime` t0)
                frTook = frTookReal
                markReq =
                    (maybe MRSearchPosts MRSearchSmartStream name)
                        query feedMasksKey feedTcs
            -- print (srTook, srTookReal)
            return $ Right (markReq, updatedCounts, FilterResults {..})

smartStreamForest :: ApiMode -> Key User -> T.Text
                  -> ReadCounters -> MsgTreeViewMode
                  -> IO (MarkReq, ReadCounters, MsgForest)
smartStreamForest apiMode user name feedsAndTcs viewMode = withLogger $ \ l -> logTime l "smartStreamForest" $ do
    u <- cachedReadUser' user
    checkSmartStreamError name user
    gri <- getGRIds' user (uSubscriptions u)
    (updatedCounts, _, postsReadMap, ordering, feedTcs) <- logTime l "getuc"
        $ getUpdatedCounts u feedsAndTcs viewMode Nothing (Just name)
    feedMasks0 <- lookupSmartStreamFeedMasks name user
    let feedMasks
            | mtvmUnreadOnly viewMode =
                IntMap.filterWithKey hasUnread feedMasks0
            | otherwise = feedMasks0
        hasUnread fid fm =
            cTotalPosts c /= cReadPosts c || cTotalComments c /= cReadComments c
            where c = smartStreamCounters' u name postsReadMap 0 [(fid,fm)]
        reqs = postsReqsFromFeedMasks feedsAndTcs feedMasks viewMode
    msgForest <- logTime l "getTree" $
        userGetMsgForest apiMode user viewMode $
            TRSmartStream name $
            sortByFeed prqFeedId viewMode gri ordering Nothing reqs
--    (updatedCounts, _) <- logTime l "getuc" getuc
    logLS l $ show (IntMap.size feedMasks0, IntMap.size feedMasks)
    return (MRSmartStream name feedTcs, updatedCounts, msgForest)

postsReqsFromFeedMasks feedsAndTcs feedMasks viewMode =
    [PostsReq
     { prqFeedId        = fid
     , prqMsgTreePoint  =
         if mtvmAscending viewMode then ascMtp (-1) else descMtp (-1)
     , prqTotalPosts    = tp
     , prqTotalComments = tc
     }
    | (fid,_,_,tp,tc) <- feedsAndTcs
    , IntMap.member fid feedMasks
    ]

discover readerHostName user country query = do
    usageFlag user UFSearchSubscriptions
    searchSubscriptions readerHostName user country query

htmlFileDataCache :: MVar (HM.HashMap T.Text T.Text)
htmlFileDataCache = unsafePerformIO $ newMVar HM.empty
{-# NOINLINE htmlFileDataCache #-}

htmlFileData fn = modifyMVar htmlFileDataCache $ \ c ->
    case HM.lookup fn c of
        Just r -> return (c, r)
        Nothing -> do
            dat <- B.readFile $ T.unpack fn
            let r = renderTagsT $ filter (not . empty) $ parseTagsT dat
                empty (TagText t) = T.all isSpace t
                empty _ = False
            return (HM.insert fn r c, r)

pageFromFile = htmlFileData

addWebpackScripts appJs = do
    s <- htmlFileData "dist/assets/scripts.html"
    return $ s <> appJs
webpackStyles = htmlFileData "dist/assets/styles.html"

testUnreadCounts = do
    let user = "1"
        feed = "http://blogs.yandex.ru/search.rss?text=haskell&ft=all&server=vkontakte.ru&x_server=on&holdres=mark"
               -- "http://feeds2.feedburner.com/rsdn/decl"
    Posts {..} <- readPosts' feed
    PostsRead {..} <- readPostsRead' (user, feed)
    let count cc =
            case IntMap.maxView cc of
                Just (c, _) -> c
                Nothing -> 0
        pccs = IntMap.map count pCommentCounts
        rccs = IntMap.map ReadSet.size prCommentsRead
        unread = IntMap.filter (/=0) $ IntMap.intersectionWith (-) pccs rccs
        unreadP = IntMap.difference pccs rccs
        overread = IntMap.difference rccs pccs
        checkP n [] = Nothing
        checkP n (n' : ccs)
            | n < n' = checkP n' ccs
            | otherwise = Just (n,n')
        nonP = IntMap.filter isJust $
               IntMap.map (checkP 0 . IntMap.elems) pCommentCounts
    print ("vCount", Map.size pCCVersions)
    print ("unread", unread)
    print ("unreadP", unreadP)
    print ("overread", overread)
    print ("pccs==rccs", pccs==rccs)
    print ("pTotalComments", pTotalComments)
    print ("totComments", sum $ IntMap.elems pccs)
    print ("nonP", nonP)

-- thankYouForBuying :: T.Text -> IO T.Text
-- thankYouForBuying t = do
--     mb <- parseQueryString t

isUserExists :: Key User -> IO Bool
isUserExists = fmap isJust . cachedReadUserStats

_clearPostsRead regex user = do
    u <- readUser' user
    mapM_ deletePostsRead
        [defaultPostsRead (user,sUrl)
        | Subscription { sState = SSFeed {}, .. } <- uSubscriptions u
        , rt regex sUrl
        ]

fixUserSubscriptionFolders uid opmlFileName = do
    ss <- fmap opmlSubscriptionsS $ B.readFile opmlFileName
    forM_ ss $ \ s -> forM_ (sFolders s) $ \ f ->
        userEditSubscriptionFolders uid (sUrl s) f True

------------------------------------------------------------------------------
-- Tasks

runTasks = do
    runEkgServer "localhost" readerEkgPort
    forkIO usageFlagsSaver

    hn <- getHostName
    when (hn == "a.bazqux.com") $ do
        forkIO $ forever $ do
            try "expired users achiver" archiveExpiredUsers
            threadDelay (3600*1000*1000)
        runMailer
        forkIO $ clearExpiredSessionsLoop
        forkIO $ clearExpiredTokensLoop
        return ()

reloadBrowserPage = when (os == "darwin") $ void $ forkIO $
    void $ system "./reload_browser_page"

clearExpiredSessionsLoop =
   clearExpiredValuesLoop "Session" sessionExpire (clearSession' "expired")
clearExpiredTokensLoop =
   clearExpiredValuesLoop "EmailVerificationToken" evtkExpire clearEmailVerificationToken'

clearExpiredValuesLoop bucket expire remove = loop HM.empty
    where loop c = do
              c' <- try' HM.empty ("expired " <> bucket <> " cleaner") $
                  clearExpiredValues bucket expire remove c
              threadDelay (3600*1000*1000)
              loop c'

clearExpiredValues :: (KV v, Key v ~ T.Text)
    => String -> (v -> UrTime) -> (v -> IO a) -> HM.HashMap T.Text v
    -> IO (HM.HashMap T.Text v)
clearExpiredValues bucket expire remove cache = do
    -- идея: если в сессии первые несколько букв -- время,
    -- то можно их удалять без чтения (и, возможно даже, получать список
    -- только таких сессий из riak).
    -- С другой стороны, время логина в ID сессии -- раскрытие личных данных.
    ss <- riakBucketKeys bucket
    t <- getUrTime
    let go c n [] = do
            logS $ bucket ++ ": cleared " ++ show n ++ "/" ++ show (length ss)
                ++ " (" ++ show (HM.size c) ++ " cached)"
            return c
        go !c !n (key:ss)
            | Just cs <- HM.lookup key c =
              if expire cs < t then do
                  remove cs
                  go (HM.delete key c) (n+1) ss
              else
                  go c n ss
            | otherwise = do
                mbs <- readKV key
                case mbs of
                    Just s
                        | expire s < t -> do
                            remove s
                            go c (n+1) ss
                        | otherwise ->
                            go (HM.insert key s c) n ss
                    Nothing ->
                        go c n ss
    go (HM.intersection cache $ HM.fromList $ map (,()) ss)
       --  ^ чистим сессии, которые были удалены ридером
       0 ss

------------------------------------------------------------------------------
-- Full Text

getFullText user cachedOnly hostName acceptLanguage mkey = do
    usageFlag user UFReadability
    m <- cachedReadMsg mkey
    case m of
        Just msg
            | isJust $ msgKeyCommentGuid mkey ->
                case links $ tags msg of
                    l:_ ->
                        fetch True l
                    _ ->
                        err "No links found in this comment to get full text"
            | hn == "reddit.com"
            , Just l <- redditLink $ tags msg ->
                fetch False l
                -- у link post уже есть заголовок
            | Just p <- apiPostKind ->
                case externalLink $ tags msg of
                    Just l ->
                        fetch True l
                    Nothing ->
                        err $ T.concat
                        ["No external links found in this ", p, " to get full text."]
            | Just l <- msgLink msg ->
                fetch False l
            | otherwise ->
                err "Post has no link."
        Nothing ->
            err "Can’t find message?"
    where fetch = fetchFullTextCached mkey cachedOnly hostName acceptLanguage
          bfu = msgKeyBlogFeedUrl mkey
          hn = strip $ hostNameFromUrlT bfu
          stripPrefix p x = fromMaybe x $ T.stripPrefix p x
          strip = stripPrefix "m." . stripPrefix "www."
          err = return . Left
          tags =
              parseTagsT . tbs . msgText .
              fixMessageContent
                  (PreprocessSettings
                      (Just hostName) (not cachedOnly) acceptLanguage Nothing)
                  -- cachedOnly == True в Google Translate
                  (\ _ _ _ -> "")
          apiPostKind = case hn of
              "twitter.com" -> Just "tweet"
              "facebook.com" -> Just "Facebook post"
              "plus.google.com" -> Just "Google+ post"
              "vk.com" -> Just "VK post"
              "reddit.com" -> Just "Reddit post"
              "instagram.com" -> Just "Instagram post"
              "t.me" -> Just "Telegram post"
              _ -> Nothing
          redditLink [] = Nothing
          redditLink (TagOpen "a" (lookup "href" -> Just l) :
                      TagText "[link]" : TagClose "a" : _) = Just l
          redditLink (_ : ts) = redditLink ts
          externalLink ts =
              listToMaybe $
              filter (\ l -> strip (hostNameFromUrlT l) /= hn) $
              take 1 (mapMaybe shareLink ts) ++
              links ts
          shareLink (TagOpen "a" as)
              | Just "bqrShareLink" <- lookup "class" as
              = lookup "href" as
          shareLink _ = Nothing
          -- ссылки с непустым текстом внутри (картинки-ссылки игнорируем)
          links [] = []
          links (TagOpen "a" (lookup "href" -> Just l) : ts) =
              checkText l ts
          links (_:ts) = links ts
          checkText _ [] = []
          checkText l tags@(t:ts) = case t of
              TagOpen "a" _ -> links tags
              TagClose "a"  -> links tags
              TagText t | not (emptyText t) -> l : links ts
              _ -> checkText l ts

fetchFullTextCached mkey cachedOnly hostName acceptLanguage includeTitle url = do
    mbft <- cachedReadFullTextCache url
    t <- getUrTime
    let fix (Left e)
            | includeTitle = Left $ T.concat
                ["Error getting full text from\n", url, "\n\n", e]
            | otherwise = Left e
        fix (Right (title, text))
            | includeTitle =
                Right $ fixText $
                renderTagsT
                [TagOpen "h1" [("class", "fullTextSubject")],
                 TagOpen "a" [("href", url)],
                 TagText title,
                 TagClose "a",
                 TagClose "h1"
                ]
                `T.append`
                text
            | otherwise = Right $ fixText text
        fixText t =
            msgText $
            fixMessageContent
                (PreprocessSettings
                    (Just hostName) (not cachedOnly) acceptLanguage Nothing)
                (\ _ _ _ -> "") $
            (defaultMsg $ MsgKey "" Nothing Nothing) { msgText = t }
    fmap fix $ case mbft of
        Just ft
            | FTTitleAndText ti te <- ftcText ft
            , cachedOnly || t `diffUrTime` ftcTime ft < day ->
                return $ Right (ti, te)
            | FTError e <- ftcText ft
            , t `diffUrTime` ftcTime ft < 3600 || cachedOnly -> return $ Left e
              -- не делаем повторные запросы слишком часто
        _ | cachedOnly ->
            return $ Left "No cached full text found"
          | otherwise -> do
            r <- getFullTextFromFiveFilters $ T.unpack url
            mergeWriteFullTextCache $
                FullTextCache
                { ftcUrl       = url
                , ftcText      = case r of
                    Left e -> FTError e
                    Right (ti, te) -> FTTitleAndText ti te
                , ftcTime      = t
                , ftcReserved1 = False
                , ftcReserved2 = False
                }
            return r

pattern (::.) :: Char -> B.ByteString -> B.ByteString
pattern x ::. xs <- (B.uncons -> Just (x, xs))

infixr 5 ::.

ensureUtf8Html (RawDownloadResult {..})
    | Just ct <- lookup "Content-Type" rdrHeaders
    , Just cs <- contentTypeCharset ct
    = convertToUtf8 cs rdrBody
    | Just cs <- htmlCharset rdrBody
    = convertToUtf8 cs rdrBody
    | otherwise
    = rdrBody

-- | https://html.spec.whatwg.org/multipage/urls-and-fetching.html#extracting-character-encodings-from-meta-elements
contentTypeCharset = go . asciiToLowerB
    where go t = case B.breakSubstring "charset" t of
              (_, cs)
                  | B.null cs -> Nothing
                  | otherwise -> eq $ dropWS $ B.drop 7 cs
          eq = \ case
              '=' ::. ts -> charset $ dropWS ts
              t -> go t
          charset = \ case
              '\'' ::. ts -> takeTill '\'' ts
              '"'  ::. ts -> takeTill '"' ts
              t@(_ ::. _) ->
                  getHtmlEncoding $ B.takeWhile (\ c -> not (ws c || c == ';')) t
              _ -> Nothing
          dropWS = B.dropWhile ws
          ws = asciiWhitespace
          takeTill q ts
              | B.null tl = Nothing
              | otherwise = getHtmlEncoding r
              where (r, tl) = B.span (/= q) ts

-- | https://html.spec.whatwg.org/multipage/parsing.html#prescan-a-byte-stream-to-determine-its-encoding
htmlCharset = fmap fix . go . parseTags . B.take 1024
    where go [] = Nothing
          go (TagOpen "meta" as : ts)
              | Just s <- check False False Nothing $ hashSetNub' fst as
              = Just s
          go (_ : ts) = go ts
          fix "utf-16be" = "utf-8"
          fix "utf-16le" = "utf-8"
          fix "utf-16" = "utf-8"
          fix "x-user-defined" = "windows-1252"
          fix e = e
          check gotPragma needPragma s = \ case
              []  | (needPragma && gotPragma) || not needPragma -> s
                  | otherwise -> Nothing
              ((n,v) : as)
                  | n == "http-equiv"
                    && asciiToLowerB (stripB v) == "content-type" ->
                      check True needPragma s as
                  | n == "content"
                  , Just c <- contentTypeCharset v
                  , Nothing <- s
                  -> check gotPragma True (Just c) as
                  | n == "charset"
                  , Just c <- getHtmlEncoding v
                  -> check gotPragma False (Just c) as
                  | otherwise
                  -> check gotPragma needPragma s as

-- | https://encoding.spec.whatwg.org/#concept-encoding-get
-- надо фильтровать поддерживаемые HTML кодировки
getHtmlEncoding (asciiToLowerB . stripB -> e)
    | B.null e = Nothing
    | otherwise = Just e

convertToUtf8 enc s
    | B.map toLower enc /= "utf-8" =
        unsafePerformIO $
            (do -- print enc
                c <- ICU.open (B.unpack enc) Nothing
                return $! T.encodeUtf8 $ ICU.toUnicode c s)
            `E.catch`
            \ (_ :: E.SomeException) -> return s
    | otherwise = s

getFullTextFromFiveFilters url0 = withLogger $ \ l -> do
  let hn = hostNameFromUrl fiveFiltersUrl
  withAsync (resolveA dnsCache hn) $ \ adns -> do
--    putStrLn apiUrl
    t <- logTime l "download" $ withTimeout $
        rateLimitedDownloadWithRedirects l False 5 $ T.pack url0
    mbip <- wait adns
    case (t, mbip) of
        (Left e, _) ->
            err ["Error downloading article:\n", e]
        (_, Left e) ->
            err ["Can’t resolve Five Filters script domain:\n", e]
        (Right (_, rdr), _)
            | Just (T.strip . bst -> ct) <-
                lookup "Content-Type" (rdrHeaders rdr)
            , not (T.isPrefixOf "text/" ct) ->
                err ["Error extracting text:\n"
                    ,"Article links to file (Content-Type: "
                    ,ct, ") instead of a web page."]
        (Right (url, rdr), Right ip) -> do
            let apiUrl = fiveFiltersUrl <> "/extract.php?lang=0&xss=0&url=" <>
                    encodeURIComponent (T.unpack url)
                form = tbs $ "inputhtml="
                    <> encodeURIComponentT (bst $ ensureUtf8Html rdr)
            dr <- logTime l "five filters" $
                postUrlEncoded searchDownloader apiUrl (Just ip) form

            case dr of
                DROK dat _ -> case decodeJson dat of
                    Just (JSON.Object o)
                        | Just (JSON.String c) <- HM.lookup "content" o
                        , c <- renderTagsT $
                            fixMessageBody (Just url) (T.unpack url) $ tbs c
                        , T.strip c /= "" ->
                            return $ Right
                                (case HM.lookup "title" o of
                                     Just (JSON.String t) -> t
                                     _ -> ""
                                ,c)
                        | otherwise ->
                            err ["No text extracted"]
                    Just o ->
                        err ["Unknown JSON format:\n", T.take 100 $ bst dat ]
                    Nothing -> err ["Error decoding JSON."]
                DRError e -> err ["Can’t get data from Five Filters script:\n", T.pack e]
                DRRedirect _ -> err ["redirect?"]
                DRNotModified -> err ["not modified?"]
    where withTimeout x = do
              r <- timeout (15*1000*1000) x
              case r of
                  Just r -> return r
                  Nothing -> err ["Timeout"]
          err = return . Left . T.concat

getFullTextFromDiffbot url = do
    dr <- withLogger $ \ l -> logTime l "diffbot" $
          download searchDownloader
          ("http://www.diffbot.com/api/article?token=" ++ diffbotToken ++ "&html&url=" ++ url)
          Nothing []
--    print dr
    let err = return . Left
        notEmpty (TagText t) = T.any (not . isSpace) t
        notEmpty _ = False
        -- неправильно, могут быть картинки и видео,
        -- но без текста
    case dr of
        DROK dat _ -> case decodeJson dat of
            Just (JSON.Object o)
                | Just (JSON.String e) <- HM.lookup "error" o ->
                    err e
                | Just (JSON.String "") <- HM.lookup "text" o ->
                    err "No text extracted"
                | Just (JSON.String h) <- HM.lookup "html" o ->
                    if any notEmpty $ parseTagsT $ T.encodeUtf8 h then
                        return $ Right $ renderTagsT $
                            fixMessageBody (Just $ T.pack url) url $ tbs h
                    else
                        err "No text extracted"
            Just _ -> err "Invalid JSON from Diffbot?"
            Nothing -> err "Can’t decode JSON from Diffbot?"
        DRError e -> return $ Left $ T.concat ["Can’t get data from Diffbot:<br/>", T.pack e]

        _ -> return $ Left "Error getting full article text"

------------------------------------------------------------------------------
-- Media tests.


normalizeSwfUrl u
    | Just v <- youtubeVideoId u
        = T.append "https://www.youtube.com/embed/" v
    | Just v <- vimeoVideoId u
        = T.append "https://player.vimeo.com/video/" v
    | otherwise = u

imgToSwfUrl u
    | [[_,_,v]] <- rg "youtube\\.com/vi/([0-9a-zA-Z_\\-]+)" u
        = Just $ T.append "https://www.youtube.com/embed/" v
    | otherwise = Nothing

-- | Исправление относительных URL, mime и reverse в enclosures/media/images.
-- Заменяет url на origEnclosureLink. Приводит все youtube/vimeo url
-- к стандартному виду для iframe.
-- Устанавливаем mime на основе расширений
--   бывает media image
--   http://g.etfv.co/http://pewinternet.org
--   или http://www.montrealgazette.com/7677238.bin
--   и это действительно image но таких случаев мало,
--   пускай будут как приложения.
--   Зато бывают pdf/mp3 как image и jpg как video
--   Еще бывают кривые mime у enclosure:
--     mpeg -> .mp3
--     image/pjpeg -> image/jpeg
--     application/octet-stream иногда может быть
--     .png, .cpp
--     application/x-shockwave-flash
--      обычно ссылка на youtube или vimeo
--      или swf
--     video/mpeg часто бывает .mp4
--     video/flv -- непонятно, играет его safari
--     или нет
--     audio/x-m4a -- бывает mp3
--     стоит всегда проверять mime расширения
--     и оставлять, если определится audio/video?
--     Image(84) -> jpg
--     Array -> mp3
--     image/peg -- это .jpg
fixEnclosures baseUri fm =
    fm
    { fmImages = reverse $ map relUri $ fmImages fm
    , fmEnclosures =
        reverse [(u, fixContentsMime u a) | (relUri -> u, a) <- fmEnclosures fm]
    , fmMedia = fixMedia $ fmMedia fm
    }
    where relUri u =
              normalizeSwfUrl $
              T.pack $ relUriNoNormalize
              (T.unpack $ fromMaybe u $ lookup u $ fmEnclosureLinks fm)
              baseUri
          fixMedia m =
              Media
              { mThumbnails =
                  reverse
                  [(relUri u, a) | (u, a) <- mThumbnails m]
              , mContents =
                  reverse
                  [(u, fixContentsMime u a, fixMedia ch)
                   | (relUri -> u, a, ch) <- mContents m]
              , mGroups = reverse $ map fixMedia $ mGroups m
              , mTitle = mTitle m
              , mDescription = mDescription m
              }
              -- иногда есть картика подкаста и она же дублируется в enclosures
          fixContentsMime u a =
                add "medium" (T.takeWhile (/= '/') typ) $
                add "type" typ a
                where add n v a = case lookup n a of
                          Just v0
                              | v0 == v -> a
                              | otherwise ->
                                  (n,v) : ("orig_" `T.append` n, v0) :
                                  filter ((/= n) . fst) a
                          Nothing ->
                              (n,v) : a
                      look n = case lookup n a of
                                 Just "" -> Nothing
                                 r -> r
                      typ = fixMime u $ fromMaybe "" $
                            (do m <- look "medium"
                                t <- look "type"
                                guard (m /= "" && m `T.isPrefixOf` t)
                                return t)
                            <|>
                            (fmap (`T.append` "/") $ look "medium")
                            <|> look "type"
                            <|>
                            (do look "width"
                                look "height"
                                -- у Engadget вместо media:thumbnail
                                -- идет media:content, без medium/type,
                                -- но с width и height и url не содержит
                                -- расширения.
                                -- Тут может быть и видео, но, как минимум, это
                                -- точно не файловый attachment.
                                return "image/")
          fixMime u t
              | isJust $ youtubeVideoId u <|> vimeoVideoId u =
                  "application/x-shockwave-flash"
              | otherwise =
                  mimeByUrl' t u

-- | alNum "Part-1.mp3" -> [Right "part",Left 1,Right "mp",Left 3]
alNum t
    | t == "" = []
    | al /= "" = Right (T.toLower al) : alNum alx
    | num /= "" = Left (readUnsignedInteger num) : alNum numx
    | otherwise = alNum ox
    where (al, alx) = T.span isAlpha t
          (num, numx) = T.span digit t
          (other, ox) = T.span (\ x -> not (isAlpha x || digit x)) t
          digit x = x >= '0' && x <= '9'
sortAlNumBy f l = map snd $ sortBy (comparing fst) [(alNum (f x), x) | x <- l]

getAttachments imgLinks videoLinks baseUri rootFM fm0 =
    imageAttachment ++ -- sortAlNumBy fileName  не сортируем, нафиг
    attachments
--    ++ (if attachments /= [] then error (show videoLinks) else [])
    where fm = fixEnclosures baseUri fm0
          m = fmMedia fm
          imageUrl = listToMaybe $ map fst imageAts
--           dbg x
--               | any (("youtube" `T.isInfixOf`) . fst) x = error (show x)
--               | otherwise = x
          fileName =
              decodeURIComponentT .
              snd . T.breakOnEnd "/" .
              T.takeWhile (`notElem` ("?#" :: [Char])) . aUrl
          (imageAts, otherAts) =
              -- списки
              -- (url, (type, attributes, childMedia))
              partition (\(_,(t,_,_)) -> "image" `T.isPrefixOf` t) $
              -- слиты дублирующиеся приложения
              map (\ (u, (i,tac)) -> (u,tac)) $
              sortBy (comparing $ \ (_, (i,_)) -> i) $
              HM.toList $ HM.fromListWith combine $
              zipWith (\i (u,tac) -> (u, (i,tac))) [0..] $
              -- убрано то, что дублируется в посте
              filter (not . badOrDuplicateUrl) $
              -- img.youtube превращены в youtube.com/embed/…
              map fixYoutubeImg $
              -- слитые enclosure/contents/thumbnails/images
              [(u, (t, a, emptyM))
               | (u,a) <- fmEnclosures fm, ("type", t) <- a] ++
              [(u, (t, a, ch))
               | (u, a, ch) <- mContents m ++ concatMap chooseDefault
                               (mGroups m)
               , ("type", t) <- a] ++
              [(u, ("image", a, emptyM))
               | (u, a) <- topThumbnail m ++ concatMap topThumbnail
                           (mGroups m)] ++
              [(u, ("image", [], emptyM)) | u <- fmImages fm]
          emptyM = Media [] [] [] Nothing Nothing
          fixYoutubeImg x@(u, (t,a,ch))
              | Just u' <- imgToSwfUrl u =
                  (u', ("application/x-shockwave-flash", a, ch))
              | otherwise = x
          combine (i,(t1,a1,chm1)) (_,(t2,a2,chm2)) =
              (i, ( if "image" `T.isPrefixOf` t2 then t2 else t1
                  -- если вдруг появился thumbnail/image, то правим content
                  , a1 ++ a2, combineMedia chm1 chm2))
          combineMedia (Media t1 c1 g1 tt1 d1) (Media t2 c2 g2 tt2 d2) =
              Media (t1 ++ t2) (c1 ++ c2) (g1 ++ g2)
                  (tt1 <|> tt2) (d1 <|> d2)
          -- TODO: стоит еще имя файла проверять без ?…
          -- также должен учитывать img в постах -- уже учитывает, т.к. удаляет
          badOrDuplicateUrl (u, _) =
              HS.member u rootLinks ||
              "http://assets.libsyn.com/item" `T.isPrefixOf` u
              -- libsyn выдает media:thumbnail с разным номером, но
              -- одинаковые и огромного размера
--              u == "" || isToplevelUrl (T.unpack u) ||
--              (u /= "" && T.last u == '/')
-- пожалуй toplevel ссылки тоже стоит оставить, для чего-то их все-таки
-- приложили
--              u == link || u == clink -- иногда ссылки прикладывают
-- иногда ссылка может быть .mp4 (Их нравы), так что нафиг.
              where link = fromMaybe "" $ lookup LKLink $ fmLinks fm
                    clink= fromMaybe "" $ lookup LKCommentsHtml $ fmLinks fm
                    rootLinks =
                        HS.fromList $
                        [relUri u | (LKAuthorPic, u) <- fmLinks rootFM] ++
                        imgLinks ++ map normalizeSwfUrl videoLinks
                    relUri u = T.pack $ relUriNoNormalize (T.unpack u) baseUri
          topThumbnail m = topImage snd $ mThumbnails m
          topImage attrs is =
              take 1 $ map snd $ sortBy (comparing $ Down . fst)
              [((lookInt "width" (attrs i), lookInt "height" (attrs i)), i)
               | i <- is]
          imageAttachment
              | imgLinks == [] && videoLinks == [] && not hasVideo
              , (reverse -> (a:_), _) <- foldl toAttachment ([], True) imageAts
              = [a]
              | otherwise = []
          (reverse -> attachments, hasVideo) =
              foldl toAttachment ([], False) otherAts
          toAttachment (as, hv) (u, (t, a, chM))
              | "image" `T.isPrefixOf` t = (AImage u w h title : as, hv)
              | "audio" `T.isPrefixOf` t = (AAudio u t s d title : as, hv)
              | "video" `T.isPrefixOf` t =
                  ((if isJust (lookup "loop" a) then
                        AVideo2 u t s d title w h poster True
                    else
                        AVideo  u t s d title w h poster) : as, True)
              | t == "application/x-shockwave-flash" =
                  ( AIframe u (renderTagsT (embedIframe u)) d title : as
                  , True)
              | otherwise = (AOther u t s : as, hv)
              where w = lookInt "width" a
                    h = lookInt "height" a
                    s = lookInt "filesize" a <|> lookInt "length" a
                    d = readDuration =<< (lookup "duration" a <|> fmDuration fm)
                    title = lookup "title" a <|> mTitle chM
                    poster
                        | hv = Nothing
                        | otherwise =
                            lookup "poster" a
                            <|> fst <$> listToMaybe (topThumbnail chM)
                            <|> (\(u,_,_) -> u) <$> (find imageMedia $ mContents chM)
                            <|> imageUrl
          chooseDefault m
              | Just c <-
                  find (\(u,_,_) -> HS.member u encLinks) (mContents m)
                  -- если элемент группы дублирует enclosure, то выбираем его
                  -- дабы избежать дублирования и получить больше деталей
                  <|> findA ("isdefault", "true")
                  <|> findA ("type", "application/x-shockwave-flash")
                  = [g c]
              | c:_ <- mContents m
              , imageMedia c = map g $ topImage (\(_,a,_) -> a) (mContents m)
              | c:_ <- mContents m = [g c]
              | otherwise = []
              where findA pair = find (\(_,a,_) -> pair `elem` a) $ mContents m
                    g (u,a,ch) = (u,a, combineMedia ch m)
                    -- чтобы у видео выбирался thumbnail из группы
                    encLinks = HS.fromList $ map fst $ fmEnclosures fm
          lookInt n a = lookup n a >>= tryReadUnsignedInt
          imageMedia (_,a,_)
              | Just t <- lookup "type" a = "image" `T.isPrefixOf` t
              | otherwise = False

-- testAttachments url = do
--     PRFeed baseUri rootFM fms <- parseUrlT url
--     let attachments fm =
--             (getAttachments i e baseUri rootFM fm
--             ,tryLookup [LKAuthorPic])
--             where (_,i,e,_) = xmlLinksImgsVideosAndText $
--                             fixMessageBody baseUri $
--                             T.encodeUtf8 $ text fm
--                   tryLookup' base lks = lookupFmLinks base lks links
--                   tryLookup = tryLookup' baseUri
--                   links = fixFmLinks fm
--         text fm
--             | fmBody fm == "" = fmSummary fm
--             | otherwise = fmBody fm
--     return $ map attachments fms

------------------------------------------------------------------------------
-- Редактирование деревьев постов

-- | Убирает элементы, расположенные до тех, что уже есть в дереве.
-- Нужно для обработки твиттера, у которого мы сейчас вытаскиваем 200 элементов,
-- а не 20, как в начале, чтобы не появилось 180 "новых" элементов.
stripOlderItems toFeedItem et fis
    | null d = fis
    | otherwise = d
    where d = dropWhile (not . (`guidInEditTree` et) . toFeedItem) fis

editMsgTree mbPid checkHash stripOlder fromHub isPost modTime l u uq mkPostTime rootMsg mt toFeedItem is
    | not isPost && mtSize mt > 10000 = do
       logLS l $ "too many comments (" ++ show (mtSize mt)
                 ++ ") ignoring new messages"
       return ([], mt, mkEditTree checkHash mt, True)
    | otherwise = do
--     logLS l $ "editMsgTree: received \n"
--             ++ unlines (map (\ (toFeedItem -> fi) -> show (msgSubject (fiMsg fi), msgKeyPostGuid (msgKey $ fiMsg fi), mhContentHash (fiHeader fi))) is)
       let et = mkEditTree checkHash mt
       go False [] et $
           if stripOlder then stripOlderItems toFeedItem et is else is
    where go sameGuidFound acc et [] = do
            let subj fi
                    | msgSubject (fiMsg fi) /= "" = msgSubject (fiMsg fi)
                    | otherwise = mhShortText (fiHeader fi)
                newFis = reverse acc
                freshFis = map (fst . fst) $ filter (not . snd) newFis
                updatedFis = map fst $ filter snd newFis
--            logLT l $ T.unlines $ "editMsgTree: saving\n" : map subj newFis
            incrStat "messages" (length freshFis)
            incrStat "updatedMessages" (length updatedFis)
            incrStat (if isPost then "posts" else "comments") (length freshFis)
            incrStat (if isPost then "updatedPosts" else "updatedComments")
                     (length updatedFis)
            when fromHub $
                incrStat "hubMessages" (length freshFis)
            when (notNull newFis) $ do
                let mTime
                        | "livejournal.com" `T.isInfixOf` u = mkPostTime
                          -- TODO: как-то очень криво, но пока не соображу,
                          -- где лучше подставлять postTime в качестве
                          -- времени сообщения для deleted comment-ов
                          -- (дабы не пересканировать из-за них пост,
                          -- как будто от свежих комментариев)
                        | otherwise = \ Msg {..} -> fromMaybe msgDlTime msgTime
                when (notNull freshFis) $
                    modifyIORef modTime $
                        max (maximum $ map (mTime . fiMsg) freshFis)
                let k mk = (msgKeyCommentGuid mk, msgKeyPostGuid mk)
                    sortFis = sortBy (comparing $ k . msgKey . fiMsg)
--                     newFis' =
--                         sortBy (comparing $ k . msgKey . fiMsg) newFis
                -- предсортировка, дабы сохранялись хорошо
--                mapM_ (logLS l . show . fiMsg) newFis'
                when (notNull updatedFis) $ do
                    logTime l "uMsgs   " $ do
                        forM_ updatedFis $ \ (fi, pid) ->
                            updateMsg l (fiMsg fi) pid
                        -- writeManyMsgs не обновляют существующие сообщения
                    logLS l $ show (length updatedFis) ++ " messages updated"
                when (notNull freshFis) $
                    logTime l "wMsgs   " $
                        writeManyMsgs (map fiMsg $ sortFis freshFis)
                logTime l "wSearch " $
                    saveSearchMsgs l uq u mkPostTime rootMsg (map (fst . fst) newFis)
--            writeManySearchKVs msgToSearchJSON newMsgs
            let links = [( Just $ mhGuid $ fiHeader fi
                         , feed, pp
                         , fromMaybe (mhDlTime $ fiHeader fi)
                                     (mhTime $ fiHeader fi)
                         , fiNoCommentsYet fi
                         , Nothing
                         )
                         | ((fi,_), updated) <- newFis
                         , not updated -- только для новых сообщений
                           -- по-идее, может обновиться link, но не стоит
                           -- из-за него пересканировать комментарии
                         , Just (feed, pp) <-
                             [if isPost then
                                  do cp <- fiCommentsPage fi
                                     f <- fiFeed fi
                                     return (cp, [PuCommentsFeed f])
                                  <|>
                                  fmap (,[]) (fiCommentsPage fi <|>
                                              fiFeed fi)
                              else fmap (,[]) $ fiFeed fi]]
            return (links, unEditTree et, et, sameGuidFound)
          go !sameGuidFound acc !et ((toFeedItem -> fi):is)
             | Just (et', updated, fi', idx) <- addFeedItem fromHub isPost et fi
                 = go (sameGuidFound || updated) (((fi',fromMaybe idx mbPid),updated):acc) et' is
             | otherwise = go (sameGuidFound || guidInEditTree fi et) acc et is

testUpdate =
    withLogger $ \ l ->
    esUpdateTaggedItem l $ MsgKey "http://beta.bazqux.com/js/hs.xml"
                           (Just "426227:4867632:34575963") Nothing

updateMsg l msg pid = do
    modifyMsg'_ (msgKey msg) $ \ _ -> return msg
    pt <- fmap ptSet $ readPostsTagged' (msgKeyBlogFeedUrl $ msgKey msg)
    when (pid `ReadSet.member` pt) $
        esUpdateTaggedItem l (msgKey msg)

fixBrokenPostsTimes l posts
    | et <- mkEditTree False (pMsgTree posts)
    , Just root <- IntMap.lookup (-1) (etChildren et)
    , toFix <-
        [ (idx, time, mh) | TimeId time idx <- Set.toAscList root
        , mh <- maybeToList $ IntMap.lookup idx $ etHeaders et
        , fromMaybe (mhDlTime mh) (mhTime mh) /= time
        ]
    , notNull toFix = do
        logLS l $ "Fixing " ++ show (length toFix) ++ " broken posts times"
        msgs <- readManyMsgs
            [ MsgKey (pBlogFeedUrl posts) (Just (mhGuid mh)) Nothing
            | (_,_,mh) <- toFix ]
        let fix et (Nothing, _) = return et
            fix !et (Just msg, (idx, time, mh)) = do
               incrStat "brokenPostsTimesFixed" 1
               updateMsg l (msg { msgTime = Just time }) idx
               return $ et { etHeaders = IntMap.insert idx
                                         (mh { mhTime = Just time
                                             , mhDlTime = msgDlTime msg }) $
                                         etHeaders et }
        et' <- foldM fix et $ zip msgs toFix
        return $ posts { pMsgTree = unEditTree et' }
    | otherwise =
        return posts


data EditTree
    = EditTree
      { etHeaders :: IntMap MsgHeader
      , etContentHash :: HM.HashMap SB.ShortByteString Int
      , etChildren :: IntMap (Set TimeId)
        -- guid -> (parent, orderTime, idx)
      , etGuids :: HM.HashMap SB.ShortByteString (Int, UrTime, Int)
      , etBoundsLo :: Int
      , etBoundsHi :: Int
      , etCheckContentHash :: Bool
      }
    deriving Show

mkEditTree etCheckContentHash (MsgTree {..}) = EditTree {..}
    where hs = mtHeaders
          (etBoundsLo, max (etBoundsLo-1) -> etBoundsHi) = BA.bounds hs
          etHeaders = IntMap.fromDistinctAscList $ BA.assocs hs
          etContentHash =
              HM.fromList [(mhContentHash h, idx) | (idx,h) <- BA.assocs hs]
          etChildren = mtChildren
          etGuids =
              HM.fromList
              [ (mhGuid (hs BA.! idx), (parent, time, idx))
              | (parent, chSet) <- IntMap.toList mtChildren
              , TimeId time idx <- Set.toList chSet ]

unEditTree (EditTree {..}) =
    MsgTree
    (BA.listArray (etBoundsLo, etBoundsHi) (IntMap.elems etHeaders))
    etChildren

inEditTree (FeedItem { fiHeader = h@(MsgHeader {..}), ..}) et@(EditTree {..}) =
    mhGuid `HM.member` etGuids ||
    (etCheckContentHash && mhContentHash `HM.member` etContentHash)
guidInEditTree (FeedItem { fiHeader = h, ..}) et@(EditTree {..}) =
    mhGuid h `HM.member` etGuids
guidUrlVariantInEditTree (FeedItem { fiHeader = h, ..}) et@(EditTree {..})
    = any (\ u -> tsb u `HM.member` etGuids) $ guidUrlVariants $ sbt $ mhGuid h

-- | Возможные варианты одинаковых адресов
-- для одного поста могут прийти
-- (http|https) :// (www.|) host (:443|) /path
-- https://blog.google/post
-- http://blog.google:443/post
-- http://www.blog.google:443/post
guidUrlVariants u
    | [[_, _, host, oport, path]] <- regexGet "https?://(www\\.)?([^:/]+)(:[0-9]+)?(.*)" u = do
        protocol <- ["https://", "http://"]
        www <- ["www.", ""]
        port <- if oport `elem` [":443", ""] then [":443", ""] else [oport]
        let r = T.concat [protocol, www, host, port, path]
        guard (r /= u)
        return r
    | otherwise = []

-- теперь для постов всегда читаются сообщения
stripHeader False mh = mh
stripHeader True mh = -- срезает 50-60% данных постов
    mh
    { mhAuthor = ""
    , mhAuthorPic = Nothing
    , mhSubject = ""
    , mhShortText = ""
    }
isHeaderStripped (MsgHeader {..}) =
    mhSubject == "" && mhShortText == "" &&
    mhAuthor == "" && mhAuthorPic == Nothing

addFeedItem fromHub isPost
            et@(EditTree {..})
            fi@(FeedItem { fiHeader = (stripHeader isPost -> h@(MsgHeader {..}))
                         , ..})
    | not fromHub -- что-то неохота обновлять посты из хаба
    , not (notUpdating (msgKeyBlogFeedUrl $ msgKey fiMsg)
           || maybe False notUpdating (msgLink fiMsg))
    , Just (_,_,existingIdx) <- HM.lookup mhGuid etGuids
    , Just existingH <- IntMap.lookup existingIdx etHeaders
    , mhContentHash /= DT.mhContentHash existingH
    = update existingIdx existingH
    | fi `inEditTree` et || fi `guidUrlVariantInEditTree` et = Nothing
    | Just pg <- fiParent
    , Just (_, _, pId) <- HM.lookup pg etGuids = insert pId
    | Nothing <- fiParent = insert (-1)
    | otherwise = Nothing
    where notUpdating u =
              isPost && ("livejournal.com" `T.isInfixOf` u ||
                         "habr.com" `T.isInfixOf` u)
              --  ^ их мы ручками парсим и обновляем
              -- есть еще hosted ЖЖ типа varlamov, но будем надеяться,
              -- что он редко редактирует посты ;)
              -- хотя хорошо бы проверять, что content hash msg из базы
              -- не соответствует тому, что в заголовке и тогда не трогать его,
              -- т.к. он был обновлен из внешнего источника
          idx = etBoundsHi + 1
          update ei eh = Just
              (et
              { etHeaders = IntMap.insert ei (fixHeaderTime h eh) etHeaders
              , etContentHash = HM.insert mhContentHash ei $
                                HM.delete (DT.mhContentHash eh) etContentHash
              }, True, fi { fiHeader = fixHeaderTime (fiHeader fi) eh
                          , fiMsg =
                                fiMsg
                                { msgDlTime = DT.mhDlTime eh
                                , msgTime = DT.mhTime eh
                                }
                          -- восстанавливаем оригинальное время скачивания,
                          -- чтобы у сообщений без msgTime
                          -- не отображалось новое время.
                          }, ei)
          fixHeaderTime h eh =
              h
              { mhDlTime = DT.mhDlTime eh
              , mhTime = DT.mhTime eh
              -- очень важно Time тоже исправлять, т.к. оно делается не меньшим,
              -- чем последнее время скачивания и у обновленных сообщений оно
              -- становится кривым.
              }
          insert p = Just
              (EditTree
              { etHeaders = IntMap.insert idx h etHeaders
              , etContentHash = HM.insert mhContentHash idx etContentHash
              , etChildren =
                  IntMap.insert p (Set.insert (TimeId time idx) chSet)
                  etChildren
              , etGuids = HM.insert mhGuid (p, time, idx) etGuids
              , etBoundsLo = etBoundsLo
              , etBoundsHi = idx
              , etCheckContentHash = etCheckContentHash
              }, False, fi, idx)
              where chSet = mtChildrenSet (MsgTree (error "MsgTree") etChildren) p
                    time
                       | Just t <- mhTime = t
                       | otherwise = mhDlTime
--                        | Set.null chSet = UrTime 0 0
--                        | (t, _) <- Set.findMax chSet = t
                         --  ^ пытаемся сохранить порядок, если у сообщения
                         -- нет времени (deleted comment)

-- rootFM / rootMsg можно создавать прямо в парсере
-- также понадобится фикс для post (msg/mhSubject), minTime и mkKey
-- и еще parent

feedMsgToItem :: Bool -> FeedMsg -> Msg -> UrTime -> UrTime -> URL -> (SB.ShortByteString -> MsgKey) -> Maybe SB.ShortByteString -> FeedMsg
              -> FeedItem
feedMsgToItem post rootFM rootMsg msgDlTime minTime baseUri mkKey fiParent fm@(FeedMsg {..}) =
    FeedItem {..}
    where fiMsg = fixMessageAuthor rootMsg $ Msg {..}
          fiHeader = MsgHeader {..}
          fiNoCommentsYet = False -- fmCommentsCount == Just 0
          fiHasVideo = videos /= []
          fiHasImgTag = imgs /= []
          fiHasGif = any gifUrl imgs
          -- TODO: учитывать lastBuildDate, иначе lj:reply-count=0
          -- может быть сильно протухшим
          (tsb -> mhGuid)
              | T.length guid > 1000 =
                  --  ^ бывают кривые фиды, с текстом вместо guid
                  hashData $ T.encodeUtf8 guid
              | guid /= "" = guid
              | Just l <- msgLink
              , l /= "" && not fmLinkNotUnique = normalizeMsgLink l
              | otherwise = idHash
          idHash = hashData $ B.concat
              $ map (tbs . T.strip) [msgAuthor, msgSubject, msgText]
          mhContentHash =
              tsb $ hashData $ BL.toStrict $ encode
              (msgAttachments
              ,msgAuthor, msgAuthorUri, msgAuthorEmail, msgAuthorPic
              ,normalizeMsgLink <$> msgLink, msgSubject, msgTags, msgText)
              -- все поля, кроме Key, Time, DlTime, ShortText и Debug
          mhAuthor = msgAuthor
          mhAuthorPic = let Msg {..} = fiMsg in msgAuthorPic
          mhSubject = msgSubject
          mhTime = msgTime
          mhDlTime = msgDlTime
          mhShortText = ""
          guid = fix fmGuid
          msgKey = mkKey mhGuid
          msgAttachments =
              getAttachments imgs videos baseLink rootFM fm
          msgAuthor  = T.take 200 $ fixAuthor $ xmlText fmAuthor
          msgAuthorUri = tryLookup [LKAuthor]
          msgAuthorEmail = fix fmAuthorEmail
          msgAuthorPic = tryLookup [LKAuthorPic]
          msgSubject
              | not post && subjectRepeatsText fiSubjectText fiSearchText = ""
              | otherwise = T.take 1000 $ fix $ xmlText fmSubject
          msgTime =
              fmap (\ t -> min msgDlTime
                           (if t >= minTime then t else msgDlTime)) $
              -- если пост внезапно позже предыдущих ставим ему время скачивания
              feedMsgTime fm
          msgText    = T.take 1000000 $ fix $ renderTagsT bodyTags
          msgTags    = filter (/= "") $ nub $  reverse $ map (stripTagPrefix . fix . xmlText) fmTags
          msgShortText = ""
          msgShorterText = ""
          msgLink = tryLookup msgLinkLKs <|> guidLink
          guidLink
              | T.length guid < 1000 &&
                regexTest "https?://[^.]+\\.[^.]+" guid = Just guid
              | otherwise = Nothing
          stripTagPrefix t
              | Just x <- T.stripPrefix "tag:" t = x
              | Just x <- T.stripPrefix "cat:@" t = x
              | Just x <- T.stripPrefix "cat:" t = x
              | otherwise = t
          baseLink =
              fromMaybe baseUri $
              (<|>) (fmap T.unpack $ tryLookup' baseUri [LKBaseUri]) $
              fmap (relUriNoNormalize "/" . T.unpack) $
              --  ^ берем ссылку на корень
              -- img src=nonRelative.png ссылается от корня, а не от поста
              -- а вот при наличии xml:base в entry уже от него
              -- TODO: по-правильному надо смотреть link в rootMsg,
              -- и ссылку source в atom для агрегаторов.
              tryLookup [LKOrigLink, LKLink]
              <|>
              Generated.DataTypes.msgLink rootMsg
          fiCommentsPage  =
              fmap (T.takeWhile (/= '#')) $
              tryLookup [LKApiFeed, LKOrigLink, LKCommentsHtml, LKLink]
              -- комментарии к постам ищем везде
          fiFeed = tryLookup [LKApiFeed, LKFeed]
          fiNext = tryLookup [LKNext]
          fiHubs =
              [fromRelUrl baseUri u | (LKHub, u) <- links]
--               ++
--               if "livejournal.com/" `isInfixOf` baseUri
--                  && not (".html" `isInfixOf` baseUri) then
--                   ["http://pubsubhubbub.appspot.com/"]
--               else []
          fixMB = fixMessageBody msgLink baseLink . tbs
          bodyTags = fixMB text
          (fiLinks, imgs, videos, fiSearchText) =
              xmlLinksImgsVideosAndText bodyTags
          (_, _, _, fiSubjectText) =
              xmlLinksImgsVideosAndText $ fixMB fmSubject
          text
              | fmBody == "" && fmSummary == "" =
                  maximumBy (comparing T.length) $ "":mediaDescriptions fmMedia
              | fmBody == "" = fmSummary
              | otherwise    = fmBody
          mediaDescriptions m =
              maybeToList (mDescription m) ++
              concatMap mediaDescriptions (reverse $ mGroups m)
          baseUri' = maybe baseUri T.unpack $ tryLookup' baseUri [LKBaseUri]
          tryLookup' base lks = lookupFmLinks base lks links
          tryLookup = tryLookup' baseUri'
          links = fixFmLinks fm
--           badFeed fi =
--               -- убираем относительные url-ы
--               -- …#comment-1234
--               '#' `elem` fi
--               -- и статусы твиттера ;)
--               || regexTest "http://twitter.com/.+/statuses/[0-9]+" (T.pack fi)
--           absLinks = filter (not . badFeed . snd) links
          -- убираем некорректные utf последовательности, дабы не было глюков
          -- с "Fail: URI Error" из-за decodeURIComponent
          fix = T.strip -- toText

fixFmLinks fm =
    [(k, T.unpack l)
    | (k,l) <- reverse (fmLinks fm)
    , T.length l < 1000
    -- бывают кривые фиды с текстом вместо link
    ]
fromRelUrl baseUri r = T.strip $ T.pack $ relUri r baseUri
-- | Пытаемся найти линки по порядку, причем сначала HTTP(S) ссылки,
-- а потом любые (ftp, magnet и т.д.)
lookupFmLinks baseUri lks links = go Nothing lks
    where go r [] = r
          go r (x:xs)
             | Just l <- fromRelUrl baseUri `fmap` lookup x links
             = if isHttp l then
                   Just l
               else
                   go (r <|> Just l) xs
             | otherwise = go r xs
msgLinkLKs = [LKOrigLink, LKLink, LKCommentsHtml]
fmMsgLink baseUri fm =
    lookupFmLinks baseUri' msgLinkLKs links
    where links = fixFmLinks fm
          baseUri' = maybe baseUri T.unpack $ lookupFmLinks baseUri [LKBaseUri] links

fixAuthorPrefix p f a
    | p `T.isPrefixOf` a && ")" `T.isSuffixOf` a =
        T.take (T.length a - T.length p - 1) $ T.drop (T.length p) a
    | otherwise = f a
fixAuthor = T.strip .
    (fixAuthorPrefix "noreply@blogger.com (" $
     fixAuthorPrefix "null@example.com (" $
     fixAuthorPrefix "nobody@flickr.com (" id) . T.strip

subjectRepeatsText subj text = strip subj `T.isPrefixOf` strip text
    where strip = T.concat . T.split (\ c -> isSpace c || c == '.' || c == '&')

feedMsgTime (FeedMsg {..}) =
    fmPublishedTime <|> fmUpdatedTime

-- | Используется для errorPage, в которой может быть не UTF-8 текст
-- (Cannot unurlify <AB>cd)
escapeXbody :: B.ByteString -> T.Text
escapeXbody = escapeHtmlT . bst

type PublicFeedInfo = [(T.Text, Bool, Maybe T.Text)]

userModifyPublicFeed u f = modifyUserSettings' u $ \ ust -> do
    let pfm = fromMaybe Map.empty $ ustPublicFeeds ust
        e pfm' = ust { ustEditsCount = ustEditsCount ust + 1
                     , ustPublicFeeds = Just pfm' }
    (pfm', r) <- f pfm
    return (if pfm' /= pfm then e pfm' else ust, r)

enablePublicFeed :: PublicFeedType -> Key User -> IO PublicFeedInfo
enablePublicFeed t u = do
    usageFlag u UFEnablePublicFeed
    userModifyPublicFeed u $ \ pfm ->
        case Map.lookup t pfm of
            Just fs@(_:_) -> do
                let x = [(f, True, d) | (f,_,d) <- fs]
                return (Map.insert t x pfm, x)
            _ -> do
                i <- new
                let x = [(i, True, Nothing)]
                return (Map.insert t x pfm, x)
    where new = do
              rnd <- replicateM 10 randomIO :: IO [Word8]
              let i = T.decodeUtf8 $ Base16.encode $
                      B.pack $ map (toEnum . fromEnum) rnd
              pf <- readPublicFeed i
              case pf of
                  Just _ -> new -- уже есть, генерим новую
                  Nothing ->
                      modifyPublicFeed' i $ \ pf ->
                          return (pf { pfUser = Just u }, i)

disablePublicFeed :: PublicFeedType -> Key User -> IO PublicFeedInfo
disablePublicFeed t u = do
    usageFlag u UFDisablePublicFeed
    userModifyPublicFeed u $ \ pfm ->
        case Map.lookup t pfm of
            Just fs@(_:_) -> do
                let x = [(f, False, d) | (f,_,d) <- fs]
                return (Map.insert t x pfm, x)
            _ ->
                return (pfm, [])

generateNewPublicFeed :: PublicFeedType -> Key User -> IO PublicFeedInfo
generateNewPublicFeed t u = do
    usageFlag u UFGenerateNewPublicFeed
    userModifyPublicFeed u $ \ pfm ->
        case Map.lookup t pfm of
            Just fs -> do
                forM_ fs $ \ (f,_,_) -> deletePublicFeed $ defaultPublicFeed f
                return (Map.delete t pfm, [])
            _ ->
                return (pfm, [])
    enablePublicFeed t u

pingPublicFeed feed = do
    postUrlEncoded
        searchDownloader "http://pubsubhubbub.appspot.com/publish" Nothing
        $ B.concat
            [ "hub.mode=publish"
            , "&hub.url=https%3A%2F%2Fbazqux.com%2Ffeed%2F"
            , tbs feed ]
    return ()

------------------------------------------------------------------------------
-- Pocket

userAddToPocket user host redirectUri link title = do
    usageFlag user $ UFShareAction SAPocket
    us <- cachedReadUserSettings' user
    case ustApiKeys us of
        Just k | Just (token,_) <- akPocket k -> do
            r <- pocketAdd token link title
            case r of
                Nothing -> return OEROK
                Just e
                    | "HTTP 401 " `T.isInfixOf` e -> redirect
                    | otherwise -> return $ OERError e
        _ -> redirect
    where redirect = do
              c <- pocketGetRequestCode $ T.concat
                  ["https://", host, redirectUri]
              case c of
                  Left e -> return $ OERError $ T.pack e
                  Right (code, redir) -> do
                      updApiKeys user $ \ k ->
                          k { akPocketRequest = Just (code, link, title) }
                      return $ OERRedirect redir


userAuthorizeAndAddToPocket user = do
    us <- cachedReadUserSettings' user
    case ustApiKeys us of
        Just k | Just (code, link, title) <- akPocketRequest k -> do
            (token, username) <- pocketAuthorize code
            updApiKeys user $ \ k ->
                k { akPocket = Just (token, username)
                  , akPocketRequest = Nothing }
            r <- pocketAdd token link title
            case r of
                Nothing -> return ()
                Just e -> fail $ T.unpack e
        Just k | Just (_, user) <- akPocket k -> do
            fail $ "Already authorized as " ++ T.unpack user
        _ ->
            fail "No authorization request pending"

pocketRequest path params = withReaderDL $ \ d -> do
    downloadG
        (\ req -> return $
                   req { C.requestHeaders =
                           [ ( "Content-Type"
                             , "application/json; charset=UTF-8" )
                           , ( "X-Accept", "application/json" ) ]
                       , C.method = N.methodPost
                       , C.requestBody = C.RequestBodyLBS $ JSON.encode $
                           JSON.Object $ HM.fromList $
                           [ ("consumer_key", pocketConsumerKey) ]
                           ++ [ (n, JSON.String v) | (n,v) <- params ]
                       })
        d ("https://getpocket.com" ++ path) Nothing []

pocketGetRequestCode redirectUri = do
    dr <- pocketRequest "/v3/oauth/request" [("redirect_uri", redirectUri)]
    case dr of
        DROK r _ ->
            case decodeJson r of
                Just (JSON.Object o)
                    | Just (JSON.String code) <- HM.lookup "code" o ->
                         return $ Right (code, T.concat ["https://getpocket.com/auth/authorize?request_token=", code, "&redirect_uri=", encodeURIComponentT redirectUri])
                _ -> return $ Left $ "Can’t get Pocket code from JSON" ++ show r
        DRError e ->
            return $ Left $ "Can’t get Pocket code: " ++ e
        _ ->
            return $ Left "Can’t get Pocket code"

pocketAuthorize code = do
    dr <- pocketRequest "/v3/oauth/authorize" [("code", code)]
    case dr of
        DROK r _ ->
            case decodeJson r of
                Just (JSON.Object o)
                    | Just (JSON.String token) <- HM.lookup "access_token" o
                    , Just (JSON.String user) <- HM.lookup "username" o
                    -> return (token, user)
                _ -> error $ "Can’t get Pocket token from JSON" ++ show r
        DRError e ->
            error $ "Can’t authorize to Pocket: " ++ e
        _ ->
            error "Can’t authorize to Pocket"

pocketAdd token url title = do
    dr <- pocketRequest "/v3/add" $ [("access_token", token), ("url", url)]
                  ++ if title /= "" then [("title", title)] else []
    case dr of
        DROK r _ ->
            case decodeJson r of
                Just (JSON.Object o)
                    | Just (JSON.Number 1) <- HM.lookup "status" o ->
                        return Nothing
                _ ->
                    return $ Just $ T.concat ["Invalid Pocket JSON:\n",
                                              bst r]
        DRError e ->
            return $ Just $ T.append "Add to Pocket failed: " (T.pack e)
        _ ->
            return $ Just "Add to Pocket failed"

------------------------------------------------------------------------------
-- Всякие одноразовые утилиты

-- fixClearedFeeds = do
--     urls <- fmap T.lines $ T.readFile "bad_urls.txt"
--     mapM_ fixClearedFeed urls

fixClearedFeed feed = modifyPosts'_ feed $ \ p -> do
     print feed
--     p <- readPosts' feed
     ptg <- readPostsTaggedGuids' feed
     let (maxId, maxGuid) =
            case IntMap.maxViewWithKey (ptgGuids ptg) of
                Just (m,_) -> m
                _ -> (1000000, "")
--         Just (curId, _) =
         postsCount = succ . snd . BA.bounds . mtHeaders
         totalPosts = maxId + 10000 -- 13876
         mt = pMsgTree p
         mth = mtHeaders mt
         diff =
             case find ((== maxGuid) . mhGuid . snd) $ BA.assocs mth of
                 Just (curId,_) -> maxId - curId
                 _ -> 1000000
--             totalPosts - postsCount mt
         (lo0,hi0) = BA.bounds mth
     print (diff, maxId, postsCount mt)
--      print curId
     return $ p { pMsgTree = MsgTree (BA.listArray (lo0+diff,hi0+diff) $ BA.elems mth)
                                     (IntMap.map (Set.map (\(TimeId t i) -> TimeId t (i+diff))) (mtChildren mt)) }

--    where feed =
              -- "http://feeds.boingboing.net/boingboing/iBag"
              -- "http://salon.com.feedsportal.com/c/35105/f/648624/index.rss"
              -- "http://feeds.feedburner.com/uproxx/warmingglow"

-- checkNegativeUnreads = do
--     runEkgServer "localhost" 12351
--     users <- getAllUsers
--     mapM_ checkNegativeUnreads' users
checkNegativeUnreads' user = do
    logT user
    ((_, _, sirs, _, _), _, _) <- subscriptionsAndSettings "" False False user
    let urls = [u | s <- sirs,
                let c = sirCounters s,
                cFeed c == 1 && (cTotalPosts c - cReadPosts c < 0),
                u <- case sirSIType s of
                       SITFeed s _ _ -> [sUrl s]
                       _ -> []]
    when (notNull urls) $ logT "FOUND:"
    mapM_ logT urls
    T.appendFile "negative_unreads_urls.txt" $
        T.concat $ map (`T.append` "\n") urls

checkFeedsScanning = do
--    urls <- fmap T.lines $ T.readFile "/home/volodya/err.txt"
    let urls = map snd deadFeeds
    forM_ urls $ \ u -> do
        p <- readPosts u
        when (isJust p) $ do
            uts <- readUrlToScan u
            case uts of
                Just (UrlToScan {..})
                    | null utsParentPaths -> print ("no pp", u)
                    | ParentPath u [] `notElem` utsParentPaths ->
                        print ("no blog pp",u)
                    | utsNextScanTime == UrTime 0 0 -> print ("nst==0", u)
                    | otherwise -> do
                        sl <- readScanList' utsNextScanTime
                        case lookup u (slUrls sl) of
                            Just _ -> return ()
                            _ -> print ("not in scan list", u)
                Nothing -> print ("no UTS", u)

fixDeadFeeds = do
--     forM_ urls $ \ u -> atomicModifyUrlToScan'_ u $ \ uts -> do
--         return $ uts { utsParentPaths = nub $ ParentPath u [] : utsParentPaths uts }
    saveSubscriptionUrls urls
    where urls = map snd deadFeeds

deadFeeds = []

cleanContentHash feed guid = modifyPosts'_ feed $ \ p -> do
    let mt = pMsgTree p
        p' = p { pMsgTree = mt { mtHeaders = BA.amap fix $ mtHeaders mt } }
        fix mh
            | mhGuid mh == guid = mh { mhContentHash = "" }
            | otherwise = mh
    when (p /= p') $ do
--        print p'
        print "found"
    return p'

clearSensitiveMsgs =
    forM_ ids $ \ i -> do
        let key = MsgKey feed (Just i) Nothing
        m <- readMsg key
        when (isJust m) $ do
            putStrLn $ "wiping " ++ show i
            modifyMsg' key $ \ m ->
                return (m { msgSubject = "-", msgText = "", msgAuthor = "" }, ())
    where feed = "feedUrl"
          ids =
              [ "articleId"
              ]
