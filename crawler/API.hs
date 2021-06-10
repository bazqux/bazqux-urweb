{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards, TupleSections,
             BangPatterns, TransformListComp, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-imports #-}
module API
    ( api0user
    , apiDataToJSON, apiDataToXML
    , vimeoThumbnail
    , apiProxy
    , processFeed
    , processFever
    , api0_atom
    , api0_userinfo
    , api0_preference_list
    , api0_friend_list
    , api0_preference_stream_list
    , api0_preference_stream_set
    , api0_tag_list
    , api0_subscription_list
    , api0_subscription_quickadd
    , api0_subscription_edit
    , api0_unread_count
    , api0_stream_items_ids
    , api0_stream_items_contents_atom
    , api0_stream_items_contents_json
    , api0_stream_contents
    , api0_edit_tag
    , api0_rename_tag
    , api0_disable_tag
    , api0_mark_all_as_read
    )
    where

import Text.Printf
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Data.List
import Data.Hashable
import Data.Ord
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Short as SB
import qualified Data.Text as T
import Generated.DataTypes
import Lib.UrTime
import qualified Lib.BArray as BA
import Lib.UnsafeRef
import Lib.Log
import Lib.Regex
import Lib.StringConversion
import Data.Maybe
import URL
import Resolvables
import Generated.RiakIO
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Array
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Text as JSON
import Lib.Json
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text.Lazy.Encoding as TL
import GHC.Stack
import Lib.ReadUtils
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import Data.IORef
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import qualified Lib.ReadSet as ReadSet
import System.IO.Unsafe
import UrCalls
import Preprocess (fixMessageContent, thumbnailWidth, PreprocessSettings(..), proxyUrl, ProxyType(..))
import Riak
import Discovery
import Search
import Config
import PageInfo (rateLimitedDownload)
import Subscriptions (userCheckSubscriptions)

------------------------------------------------------------------------------
-- Google Reader API

data APIData
    = APIObject [(T.Text, APIData)]
    | APIList [APIData]
    | APIString T.Text
    | APINumber Bool Integer
    | APIDouble Double
    | APIBool Bool
    deriving (Show, Eq)

apiObj = APIObject
apiObj1 n v = APIObject [(n,v)]
apiList = APIList
apiStr = APIString
apiInt = APINumber
apiTrue = APIBool True
apiBool = APIBool
apiBoolStr b = APIString $ if b then "true" else "false"

apiDataToXML d = renderTagsT $ go [] d []
    where go a (APIObject x) n =
              TagOpen "object" a : foldr one (TagClose "object" : n) x
              where one (k,v) n = go [("name", k)] v n
          go a (APIList x) n =
              TagOpen "list" a : foldr (go []) (TagClose "list" : n) x
          go a (APIString s) n =
              TagOpen "string" a : TagText s : TagClose "string" : n
          go a (APINumber _ i) n =
              TagOpen "number" a : TagText (T.pack $ show i) : TagClose "number" : n
          go a (APIDouble i) n =
              TagOpen "number" a : TagText (T.pack $ show i) : TagClose "number" : n
          go a (APIBool b) n =
              TagOpen "boolean" a : TagText (if b then "true" else "false") : TagClose "boolean" : n

apiDataToJSON = TL.encodeUtf8 . toLazyText . go
    where go (APIObject []) = "{}"
          go (APIObject (x:xs)) =
              singleton '{' <> one x <> foldr f (singleton '}') xs
              where f a z = singleton ',' <> one a <> z
                    one (k,v) = singleton '"' <> fromText k <> singleton '"'
                                <> singleton ':' <> go v
          go (APIList []) = "[]"
          go (APIList (x:xs)) =
              singleton '[' <> go x <> foldr f (singleton ']') xs
              where f a z = singleton ',' <> go a <> z
          go (APIString s) = JSON.encodeToTextBuilder (JSON.String s)
          go (APINumber False i) = decimal i
          go (APINumber True i) = singleton '"' <> decimal i <> singleton '"'
          go (APIDouble d) = realFloat d
          go (APIBool True) = fromText "true"
          go (APIBool False) = fromText "false"

api0user user = "01234567890123456789"
api0userPrefix user = T.concat ["user/", api0user user, "/"]
                      -- если в метке короткий идентификатор пользователя
                      -- Mr.Reader перестает видеть папки
api0labelT user label = T.concat [api0userPrefix user, "label/", label]
api0label user label = apiStr $ api0labelT user label
api0getFolder state
    | [[_,f]] <- regexGet "user/.*/label/(.*)" state = Just f
    | otherwise = Nothing
api0tagT user label = T.concat [api0userPrefix user, "tag/", label]
-- /tag/ нестандартное расширение, чтобы получить содержимое тега,
-- даже если есть папка с таким же именем
api0getTag state
    | [[_,f]] <- regexGet "user/[^/]*/tag/(.*)" state = Just f
    | otherwise = Nothing
api0itemTag user ITStarred =
    api0userPrefix user `T.append` "state/com.google/starred"
api0itemTag user (ITTag t) = api0labelT user t
api0stateTag folders state
    | "state/com.google/starred" `T.isInfixOf` state = Just ITStarred
    | Just f <- api0getTag state = Just $ ITTag f
    | Just f <- api0getFolder state
    , not (f `HS.member` folders) = Just $ ITTag f
    | otherwise = Nothing

api0_userinfo user = return $ apiObj
    [ ("userId", apiStr $ api0user user)
    , ("userName", apiStr user)
    , ("userProfileId", apiStr $ api0user user)
    , ("userEmail", apiStr user)
    , ("isBloggerUser", apiTrue)
    , ("signupTimeSec", apiInt False 1234567890)
    , ("isMultiLoginEnabled", apiTrue) ]

sortIdStr = T.pack . printf "%08X"
apiSortId = apiStr . sortIdStr

-- | /subscription/list
-- http://stackoverflow.com/questions/4428117/google-reader-api-sortid-and-firstitemmsec
-- sortid для ручной сортировки (игнорируем)
-- firstitemmsec для фильтрации старых сообщений (которые уже нельзя пометить
-- непрочитанными), также игнорируем.
api0_subscription_list user = do
    subscriptionsAndSettings "" False False user
    -- не checkSubscriptions, чтобы закешировало, без
    u <- cachedReadUser' user -- userCheckSubscriptions user
    let s = uFeedSubscriptions u
    ids <- getGRIds' user s
    ps <- cachedReadManyPostss [u | Subscription { sState = SSFeed u, ..} <- s]
    let htmlUrls = HM.fromList
                   [(pBlogFeedUrl p, msgLink $ pRootMessage p) | Just p <- ps]
        sub (Subscription {..}) =
            apiObj [ ("id", apiStr $ "feed/" `T.append` sUrl)
                   , ("title", apiStr $ fromMaybe sUrl sTitle)
                   , ("categories", apiList $ map cat sFolders)
                   , ("sortid"
                     , apiSortId $ HM.lookupDefault 0 sUrl $ griFeedIds ids)
                   , ("firstitemmsec", apiInt True 1234567890000)
                   , ("htmlUrl"
                     , apiStr $ fromMaybe sUrl $ join $ HM.lookup sUrl htmlUrls)
                   ]
        cat name = apiObj [ ("id", api0label user name)
                          , ("label", apiStr name) ]
    return $ apiObj1 "subscriptions" $ apiList $ map sub s

isFeed (sState -> SSFeed {..}) = True
isFeed _ = False

uFeedSubscriptions = filter isFeed . uSubscriptions

api0_preference_list user =
    return $ apiObj1 "prefs" $ apiList
        [
--          apiObj [( "id", apiStr "lhn-prefs")
--                 ,( "value"
--                 , apiStr "{\"subscriptions\":{\"ssa\":\"true\"}}")
--                  -- ssa = Sort Subscriptions Alphabetically
--                 ]
        ]
api0_friend_list user =
    return $ apiObj1 "friends" $ apiList []

api0_preference_stream_list user = do
    o <- userGetOrdering uFeedSubscriptions user
    return $ apiObj1 "streamprefs" $ apiObj $ map pref o
    where pref (folder, (expanded, filter (< allTagsSortId) -> ordering)) =
            ( if folder == "" then
                  api0userPrefix user `T.append` "state/com.google/root"
              else api0labelT user folder
            , apiList $
              (if folder /= "" then
                   [apiObj
                    [("id", apiStr "is-expanded")
                    ,("value", apiBoolStr expanded)]]
               else []) ++
              [apiObj
               [("id", apiStr "subscription-ordering")
               ,("value", apiStr $ T.concat $ map sortIdStr ordering)]])

api0_unread_count fromBrowser user = do
    (_, _, s, _, _ ) <-
        subscriptionsAndViewMode "" False False fromBrowser UFAll "" "" "" user
    return $ apiObj
        [ ("max", apiInt False 500)
        , ("unreadcounts", apiList $ map cnt $ filter unread s)
        , ("bq_total_unreads", apiInt False (toEnum $ count $ head s)) ]
    where count (sirCounters -> c) = cTotalPosts c - cReadPosts c
          unread s = count s > 0 && cFeed (sirCounters s) > 0
          -- только фиды/папки
          cnt s = apiObj
              [("id", apiStr $
                case sirSIType s of
                    SITAll -> "user/-/state/com.google/reading-list"
                    SITFolder f -> "user/-/label/" `T.append` f
                    SITSmartStream ss _ -> "user/-/label/" `T.append` ss
                    SITTag t -> "tag?" -- "user/-/label/" `T.append` t
                    SITAllTags -> "allTags?"
                    SITStarred -> "starred?"
                    SITFeed {sitSubscription = s, ..} ->
                        "feed/" `T.append` sUrl s
                    SITSearch {} -> "search?")
              ,("count", apiInt False $ toEnum $ count s)
              ,("newestItemTimestampUsec", apiInt True $ urTimeUsec $
                case sirSIType s of
                    SITFeed {sitPointAllDesc = Just mtp, ..} -> mtpTime mtp
                    _ -> UrTime 0 0) ]
                --  ^ Reeder вроде не использует

api0_tag_list user = do
    u <- userCheckSubscriptions user
    i <- getGRIds' user (uFeedSubscriptions u)
    fs <- cachedReadFilters' user
    return $ apiObj1 "tags" $ apiList $
        [state starredSortId "com.google/starred"
        ,state 0xffffffbc "com.google/broadcast"] ++
        (map (tag True i) $ sortBy (comparing T.toLower) $
         map ssName (fSmartStreams fs)) ++
        (map (tag False i) $ sortBy (comparing T.toLower) $
         nub $
         [t | ITTag t <- Map.keys (griTaggedItems i)] ++
         concatMap sFolders (uFeedSubscriptions u)) ++
        [state 0xffffffbf "com.blogger/blogger-following"]
    where tag ss i name =
              apiObj $ [ ("id", api0label user name)
                       , ("sortid", apiSortId $
                          HM.lookupDefault 0 name (griFolderIds i)) ]
                       ++ if ss then [ ("smartstream", apiTrue) ] else []
          state i s = apiObj [ ("id", apiStr $ T.concat [api0userPrefix user
                                                        ,"state/", s])
                             , ("sortid", apiSortId i) ]

-- {"id":"-3587166568350259386","directStreamIds":[],"timestampUsec":"1368470951385679"}
-- com.google/broadcast -- share
-- com.google/created   -- notes

data APIFeedInfo
    = APIFeedInfo
      { afiId :: T.Text
      , afiTitle :: T.Text
      , afiAlternate :: Maybe TURL
      , afiUpdated :: UrTime
      }
    deriving Show

getAfi user feeds = do
    t <- getUrTime
    u <- cachedReadUser' user
    let findFeedTitle feed
            | user == emptyUserForExternalFeedsAccess =
                return $ (\s -> if s /= "" then s else feed) . msgSubject
            | otherwise = do
                Subscription {..} <- find ((== feed) . sUrl) (uSubscriptions u)
                return $ const $ fromMaybe sUrl sTitle
    case feeds of
        feed:_
            | Just title <- findFeedTitle feed -> do
                p <- cachedReadPosts' feed
                return $
                    APIFeedInfo
                    { afiId = T.concat [ "feed/", feed ]
                    , afiTitle = title $ pRootMessage p
                    , afiAlternate = msgLink $ pRootMessage p
                    , afiUpdated = t
                    }
        _ -> return $
            APIFeedInfo
            { afiId = T.concat [ api0userPrefix user
                               , "state/com.google/reading-list"]
                               -- могут быть и звездочки
            , afiTitle = "Reading list"
            , afiAlternate = Nothing
            , afiUpdated = t
            }

api0_msgForestPreprocess fetch qs user = do
    u <- cachedReadUser' user
    let s = uSubscriptions u
        folders = HS.fromList [ f | Subscription { sState = SSFeed {}, .. } <- s
                              , f <- sFolders]
        folderFeeds f =
            [sUrl | Subscription { sState = SSFeed {}, .. } <- s
            , f `elem` sFolders]
        getParams p = [v | (pn,v) <- qs, pn == p]
        xts = getParams "xt"
        its = getParams "it"
        subs = getParams "s"
        diff f = sort $ HS.toList $ f subs `HS.difference` f xts
        feeds =
            sort $ HS.toList $
            feedsSet True subs `HS.difference` feedsSet False xts
        tags  = diff tagsSet
        feedsSet sub = HS.fromList . concatMap (stateFeeds sub)
        tagsSet = HS.fromList . mapMaybe (api0stateTag folders)
        stateFeeds sub state
            | readingListFeed state || (sub && readItemsState state) =
                [sUrl | Subscription { sState = SSFeed {}, .. } <- s]
            | Just f <- api0getFolder state =
                folderFeeds f
                -- в вызове ids google reader игнорирует метки
                -- только в contents
            | "feed/" `T.isPrefixOf` state =
                [T.drop 5 state]
            | otherwise = []
               -- "state/com.google/starred"
        n = fromMaybe 20 $ do
              ns <- lookup "n" qs
              n <- tryReadUnsignedInt ns
              return $ if fetch then min 1000 n else min 50000 n
        readingListFeed state =
            "state/com.google/reading-list" `T.isInfixOf` state
        readItemsState state =
            "state/com.google/read" `T.isSuffixOf` state
        readOnly = any readItemsState subs
        rl = any readingListFeed subs
        unreadOnly = any ("state/com.google/read" `T.isSuffixOf`) xts
        ascending = ("r","o") `elem` qs
        minDlTime = do
            ts <- lookup "ot" qs
            t <- tryReadUnsignedInt ts
            return $ UrTime (t - 180) 0 -- добавляем минуту на кеширование
        maxDlTime =
            return . flip UrTime 0 =<< tryReadUnsignedInt =<< lookup "nt" qs
        maxTime = tryReadUsecUrTime =<< lookup "ts" qs
            -- для mark-all-as-read
        getMKey id = do
            (_, is) <- getGRItemIds False user [("i", id)]
            return $ listToMaybe $ map iiMsgKey is
        mCont
            | Just c <- lookup "c" qs
            , c /= "null" = getMKey c
            | otherwise = return Nothing
        msgLinkParams =
            -- https://bazqux.com/feed/29b2ed31f7e4f13ab863?_utm_source=asdf&_utm_campaign=asdfasdf&n=1000
            -- https://bazqux.com/feed/29b2ed31f7e4f13ab863?add_utm_source=asdf&add_utm_campaign=asdfasdf&n=1000
            -- лучше просто "_", а не "add_"
            [(p,v) | (T.stripPrefix "_" -> Just p, v) <- qs]
        maxMsgTextLength
            | Just l <- lookup "max_text_length" qs = tryReadUnsignedInt l
            | otherwise = Nothing
    cont <- newIORef =<< mCont
    let am = AMGRIdsOnly fetch n
                (UnsafeRef cont) minDlTime maxDlTime maxTime
                (tagsSet xts) (tagsSet its) readOnly
                msgLinkParams False
                maxMsgTextLength
            -- для запроса всех прочитанных читаем всю кучу и потом ограничиваем
        mv = defaultMsgTreeViewMode
             { mtvmUnreadOnly = unreadOnly
             , mtvmAscending = ascending }
    return (tags, feeds, am, mv, rl, s, n)

api0_msgForest fetch qs user = do
    (tags, feeds, am, mv, rl, s, n) <- api0_msgForestPreprocess fetch qs user
--    print maxTime
    fs <- cachedReadFilters' user
    i <- getGRIds' user (s ++ [Subscription f (SSFeed f) 0 Nothing [] | f <- feeds])
    let third (_,_,mf) = mf
    mf <- third <$>
        if ("s","all-tags") `elem` qs then
            tagsForest am user Nothing mv
        else
            case tags of
                [] ->
                    folderForest user Nothing (FODFeedsApi am feeds) [] mv "" ""
                [ITTag n]
                    | Just ss <- find ((== n) . ssName) (fSmartStreams fs) ->
                    smartStreamForest am user n
                        [(feedId f i, 0, 0, maxBound, maxBound)
                        | f <- HM.keys $ ssFeeds ss]
                        (mv { mtvmUnreadOnly = fromProcessFeed `notElem` qs })
                        -- Синхронизируем только непрочитанные,
                        -- чтобы в мобильных приложениях теги появлялись
                        -- только при наличии новых элементов smartStream.
                        -- В public feed выдаем всё.
                _ ->
                    tagsForest am user (Just tags) mv
        -- не позволяем одновременно запрашивать теги и фиды
    afi <- getAfi user (if rl then [] else feeds)
    let feeds' = HS.fromList $ map (miBfu . fst) $ mfList mf
        cont
            | l <- mfList mf
            , length l == n && n > 0
            , (mi,_) <- last l = Just $ T.pack $ show $ midGrItemId $ miMsgId mi
            | otherwise = Nothing
    return (i, rl, afi, Just am, HS.toList feeds', mf, cont)

test_ids = withLogger $ \ l -> do
    r <- logTime l "ids" $ api0_stream_items_ids [("n","20000"), ("s","user/-/state/com.google/starred")] "1"
    logTime l "toJSON" $ print $ BL.length $ apiDataToJSON r

warnOT (Just (AMGRIdsOnly {..}))
    | isJust amMinDlTime || isJust amMaxDlTime =
        [("ot_warning", apiStr "Please, do not use the “ot” parameter. See https://github.com/bazqux/bazqux-api#the-right-way-to-sync")]
warnOT _ = []

api0_stream_items_ids qs user = do
    (i, _, _, am, _, mf, c) <- api0_msgForest False qs user
    let includeFeed = ("includeAllDirectStreamIds", "true") `elem` qs
        ascending = ("r","o") `elem` qs

--    withLogger $ \ l -> logTime l "length" $ print $ length $ mfList mf

    return $ apiObj $ warnOT am ++ [("itemRefs", apiList $
--         map snd $
--         (if ascending then
--              sortBy (comparing fst)
--          else
--              sortBy (comparing $ Down . fst))
        [apiObj [("id", apiInt True $ toEnum ii)
                ,("directStreamIds", apiList $
                  (map (apiStr . api0itemTag user) $ miTagsAndSmartStreams i mi)
                  ++
                  if includeFeed then [apiStr $ "feed/" `T.append` bfu] else [])
                ,("timestampUsec", usecStr time)]
        | (mi@(MsgItem {..}), _) <- mfList mf
        , let bfu = miBfu mi
        , let time = case miMsgView of
                  MVFull m -> fromMaybe (msgDlTime m) (msgTime m)
                  MVShort h _ -> fromMaybe (mhDlTime h) (mhTime h)
        , let ii = midGrItemId miMsgId
        ])]
        ++ maybe [] (\c -> [("continuation", apiStr c)]) c


miTagsAndSmartStreams gri (MsgItem {..}) =
    miTags <> map (ITTag . (`folderName` gri)) (IntSet.toList miSmartStreams)

miBfu = msgKeyBlogFeedUrl . miMsgKey

api0_msgs qs user = do
    u <- cachedReadUser' user
    (i, rl, afi, am, feeds, mf, c) <- api0_msgForest True qs user
    ps <- cachedReadManyPostss feeds
    let pm = HM.fromList $ zip feeds ps
        titles = HM.fromList [ (sUrl, fromMaybe sUrl sTitle)
                             | Subscription {..} <- uSubscriptions u]
    return (i, rl, afi, am, c,
        [ (GRItemId
           { iiMsgId = miMsgId
           , iiMsgKey = miMsgKey
           , iiStreamTitle =
               HM.lookupDefault (msgSubject $ pRootMessage p) bfu titles
           , iiHtmlUrl = msgLink (pRootMessage p)
           , iiRead = miRead
           }, msg, miTagsAndSmartStreams i mi)
        | (mi@(MsgItem { miMsgView = MVFull msg, .. }), _) <- mfList mf
        , let bfu = msgKeyBlogFeedUrl $ msgKey msg
        , p <- maybeToList $ join $ HM.lookup bfu pm
        ])
api0_json_msgs i user path (APIFeedInfo {..}) am c m =
    return $ apiObj $
        warnOT am <>
        [("direction", apiStr "ltr")
        ,("id", apiStr afiId)
        ,("title", apiStr afiTitle)
        ,("description", apiStr afiTitle)
        ,("self", apiObj1 "href" (apiStr path))]
        ++ maybe [] (\c -> [("continuation", apiStr c)]) c
        ++ maybe [] (\ h -> [("alternate",
                              apiObj [ ("href", apiStr h)
                                     , ("type", apiStr "text/html")])])
           afiAlternate
        ++
        [("updated", secInt False afiUpdated)
        ,("items", apiList $ map (api0_json_msg i user) m)]
api0_atom_msgs i user path publicFeed qs rl (APIFeedInfo {..}) am c m =
    return $ T.concat $
        (header : warning : feedInfo : map (api0_atom_msg publicFeed i rl user) m)
        ++ [footer]
    where header = "<?xml version=\"1.0\"?><feed xmlns:media=\"http://search.yahoo.com/mrss/\" xmlns:gr=\"http://www.google.com/schemas/reader/atom/\" xmlns:idx=\"urn:atom-extension:indexing\" xmlns=\"http://www.w3.org/2005/Atom\" idx:index=\"no\" gr:dir=\"ltr\"><!--\nContent-type: Preventing XSRF in IE.\n\n--><generator uri=\"https://bazqux.com\">BazQux Reader</generator>"
          warning
              | [(w, APIString s)] <- warnOT am =
                  T.concat ["\n<!-- ", w, ": ", s, " -->\n"]
              | otherwise = ""
          footer = "</feed>"
          isFeed = "/feed/" `T.isPrefixOf` path
          title
              | Just pf <- publicFeed =
                  (if noBranding then id else (<> " from BazQux Reader")) $
                  case pf of
                      PFTAll -> "Latest"
                      PFTStarred -> "Starred items"
                      PFTFolder f -> f
                      PFTTag t -> t
                      PFTAllTags -> "Tagged items"
                      PFTSmartStream s -> s
              | otherwise = afiTitle
          -- pfTitle = case
          rtFeed = case publicFeed of
              Just PFTStarred -> True
              Just PFTAllTags -> True
              Just (PFTTag _) -> True
              _ -> False
          noBranding =
              isJust $ lookup "nobranding" qs <|> lookup "no_branding" qs
          alternate
              | Just pf <- publicFeed =
                  if noBranding then Nothing else Just $
                  T.append "https://bazqux.com" $ case pf of
                      PFTAll -> ""
                      PFTStarred -> "/i/starred"
                      PFTFolder f -> T.append "/i/folder/" (encodeURIComponentT f)
                      PFTTag t -> T.append "/i/tag/" (encodeURIComponentT t)
                      PFTAllTags -> "/i/tags"
                      PFTSmartStream s ->
                          T.append "/i/smartstream/" (encodeURIComponentT s)
              | otherwise = afiAlternate
          feedInfo = renderTagsT $
              TagOpen "id" [] :
              TagText (T.append "tag:google.com,2005:reader/" afiId) :
              TagClose "id" :
              TagOpen "title" [] : TagText title : TagClose "title" :
              TagOpen "subtitle" [("type","html")] : TagText title :
              TagClose "subtitle" :
              TagOpen "link" [("rel", "self"), ("href", path)] :
              TagClose "link" :
              (if rtFeed && T.length path == T.length "https://bazqux.com/feed/01234567890123456789" then
                   [ TagOpen "link"
                     [ ("rel", "hub")
                     , ("href", "http://pubsubhubbub.appspot.com/")]
                   , TagClose "link"]
              else [])
              ++
              maybe [] (\c -> [TagOpen "gr:continuation" [], TagText c
                              ,TagClose "gr:continuation"]) c
              ++
              (maybe [] (\ h ->
                             [ TagOpen "link" [ ("rel", "alternate")
                                              , ("href", h)
                                              , ("type", "text/html") ]
                             , TagClose "link" ])
               alternate
               ++
               [ TagOpen "updated" []
               , TagText (T.pack $ formatUrTimeRfc3399 afiUpdated)
               , TagClose "updated" ])
-- <gr:continuation>CLqphK3Km7cC</gr:continuation>

api0_stream_contents path qs user = do
    (i, _, afi, am, c, m) <- api0_msgs qs user
    api0_json_msgs i user path afi am c m
api0_atom path publicFeed qs user = do
--    logS $ show ("api0_atom", path, qs, user)
    (i, rl, afi, am, c, m) <- api0_msgs qs user
    api0_atom_msgs i user path publicFeed qs rl afi am c m
processFeed feed path qs = do
    pf <- readPublicFeed' feed
    case pfUser pf of
        Just u -> do
            ust <- readUserSettings' u
            let fs = [ t
                     | (t,fs) <-
                         Map.toList (fromMaybe Map.empty $ ustPublicFeeds ust)
                     , (f,True,_) <- fs
                     , f == feed]
            case fs of
                (t:_) -> processFeed' t feed qs path u
                _ -> return Nothing
        _ -> return Nothing
processFeed' t feed qs path user = do
    f <- cachedReadFilters' user
    gri <- cachedReadGRIds' user
    u <- cachedReadUser' user
    let needFilters = case t of
            PFTAll -> not $ IntMap.null fm
            PFTFolder fn ->
                any (`IntMap.member` fm)
                [feedId (sUrl s) gri
                | s <- uFeedSubscriptions u
                , fn `elem` sFolders s]
            PFTSmartStream _ -> True
            _ -> False -- теги/звездочки от фильтров не зависят
        fm = ffmFeedMasks $ fFeedMasks f
    when needFilters $
        void $ subscriptionsAndSettings "" False False user
        -- обновляем фильтры
    Just <$> api0_atom path (Just t) (("s", s t) : fromProcessFeed : qs) user
    where s PFTAll = "user/-/state/com.google/reading-list"
          s PFTStarred = api0itemTag user ITStarred
          s (PFTTag t) = api0tagT user t
          s (PFTFolder t) = api0labelT user t
          s (PFTSmartStream t) = api0tagT user t
          s PFTAllTags = "all-tags"

fromProcessFeed = ("fromProcessFeed", "true")

msecStr' = T.pack . show . urTimeMsec
msecStr = apiStr . msecStr'
usecStr = apiInt True . urTimeUsec
secInt s = apiInt s . toEnum . urtSeconds
-- /reader/api/0/stream/items/contents?i=-4884920866890957123&output=atom
--                                                                   ^
-- есть подозрение, что строками в json и number в xml передается время
-- хотя id-шки тоже.
googleTag = "tag:google.com,2005:reader/item/"

data GRItemId
    = GRItemId
      { iiMsgId :: MsgId
      , iiMsgKey :: MsgKey
      , iiStreamTitle :: T.Text
      , iiHtmlUrl :: Maybe TURL
      , iiRead :: Bool
      }
    deriving Show

iiLongMsgId (GRItemId {..}) = LongMsgId iiMsgKey iiMsgId

getGRItemIds getRead user qs = do
    u <- cachedReadUser' user
    gri <- getGRIds' user (uSubscriptions u)
    let feeds =
            [fromJustE ("Can’t find feed #" ++ show id) $
             IntMap.lookup id (griFeedUrls gri) | id <- usedFeedIds]
    feedsPosts' <- forkRead $ cachedReadManyPostss feeds
    postsRead' <- forkRead $
        if getRead
        then cachedNothingReadManyPostsReads (map (user,) feeds)
        else return []
    p <- feedsPosts'
    pr <- postsRead'
    -- тут же надо читать PostsRead чтобы выдавать флаги, прочитано/непрочитано
    let pm = IntMap.fromList $ zip usedFeedIds p
        prm = IntMap.fromList $ zip usedFeedIds pr
        titles = HM.fromList [ (sUrl, fromMaybe sUrl sTitle)
                             | Subscription {..} <- uSubscriptions u]
        emptyId (feedId, postId, cid) =
            GRItemId
            { iiMsgId = MsgId feedId postId cid
            , iiMsgKey = MsgKey "" Nothing Nothing
            , iiStreamTitle = ""
            , iiHtmlUrl = Nothing
            , iiRead = True
            }
        r i@(feedId, postId, cid)
            | Just (Just p) <- IntMap.lookup feedId pm
            , bfu <- pBlogFeedUrl p
            , h <- mtHeaders $ pMsgTree p = do
                mbpGuid <-
                    if inRange (BA.bounds h) postId then
                        return $ Just $ mhGuid $ h BA.! postId
                    else do
                        ptg <- cachedReadPostsTaggedGuids' bfu
                        return $ IntMap.lookup postId $ ptgGuids ptg
                (cGuid, cOk) <- case (cid, mbpGuid) of
                    (Just c, Just pGuid) -> do
                        ci <- cachedReadComments' (CommentsKey bfu pGuid)
                        let ch = mtHeaders $ cMsgTree ci
                        if inRange (BA.bounds ch) c then
                            return (Just $ mhGuid (ch BA.! c), True)
                        else
                            return (Nothing, False)
                    _ ->
                        return (Nothing, True)
                return $ case (mbpGuid, cOk) of
                    (Just pGuid, True) ->
                        GRItemId
                        { iiMsgId = MsgId feedId postId cid
                        , iiMsgKey = MsgKey bfu (Just pGuid) cGuid
                        , iiStreamTitle =
                            HM.lookupDefault (msgSubject $ pRootMessage p)
                                             bfu titles
                        , iiHtmlUrl = msgLink (pRootMessage p)
                        , iiRead =
                            not (isActiveFeed bfu gri)
                            ||
                            fromMaybe False (do
                            Just pr <- IntMap.lookup feedId prm
                            return $ case cid of
                                Nothing ->
                                    ReadSet.member postId (prSet pr) ||
                                    ReadSet.member postId (prIgnoredPosts pr)
                                Just cid ->
                                    ReadSet.member postId (prIgnoredPosts pr) ||
                                    ReadSet.member cid (lookupCommentsReadSet
                                                        postId
                                                        (prCommentsRead pr)))
                        }
                    _ ->
                        emptyId i
            | otherwise =
                return $ emptyId i
    afi <- getAfi user feeds
    iis <- mapM r ids
--    logS $ show (qs, iis)
    return (afi, iis)
    where ids = [readId i | ("i", i) <- qs]
          usedFeedIds =
              IntSet.toList $ IntSet.fromList $ map (\(f,_,_) -> f) ids
          readId = splitGrItemId . readGRId

readGRId i
    | googleTag `T.isPrefixOf` i =
        read ("0x" ++ T.unpack (T.drop (T.length googleTag) i))
--     | T.length i == 16
--       -- Reeder в режиме Google Reader отправляет hex без префикса
--     , Just (c, _) <- T.uncons i
--     , c == '0' || c == 'f' || c == '8' =
--         read ("0x" ++ T.unpack i)
    | otherwise =
        maybe (error $ "invalid item id: " <> T.unpack i) fromEnum
            $ tryReadInteger i

googleItemId (GRItemId {..}) =
    T.concat [ googleTag
             , T.pack $ printf "%016x" $ midGrItemId iiMsgId ]

-- Reeder for Mac делает запросы по 250 штук
-- Mr. Reader по 50
api0_msgsByItemIds qs user = do
    (afi, take 10000 -> iis) <- getGRItemIds True user qs
    msgs <- map (fixMessageContent (PreprocessSettings Nothing False "" Nothing) (\ _ _ _ -> "") <$>)
        <$> readManyMsgs (map iiMsgKey iis)
            -- не cached, т.к. читаются один раз
    i <- getGRIds user
    let msg (ii, fromMaybe (defaultMsg $ iiMsgKey ii) -> m) =
            (ii, m, IntMap.findWithDefault [] (midGrItemId $ iiMsgId ii)
                    (griItemTags i))
    return (i, afi, map msg $ zip iis msgs)
api0_stream_items_contents_json path qs user = do
    (i, afi, m) <- api0_msgsByItemIds qs user
    api0_json_msgs i user path afi Nothing Nothing m
api0_stream_items_contents_atom path qs user = do
    (i, afi, m) <- api0_msgsByItemIds qs user
    api0_atom_msgs i user path Nothing qs True afi Nothing Nothing m

msgEnclosures publicFeed = go . msgAttachments
    where go [] = []
          go (AImage {} : as) = go as
          go (AIframe {} : as) = go as
          go (AAudio {..} : as) =
              t aUrl aMime aFileSize
              (mbia "duration" aDuration ++ mba "title" aTitle) : go as
          go (AVideo {..} : as) =
              t aUrl aMime aFileSize
              (mbia "duration" aDuration ++ mba "title" aTitle
               ++ mbia "width" aWidth ++ mbia "height" aHeight
               ++ mba "poster" aPoster) : go as
          go (AVideo2 {..} : as) =
              t aUrl aMime aFileSize
              (mbia "duration" aDuration ++ mba "title" aTitle
               ++ mbia "width" aWidth ++ mbia "height" aHeight
               ++ mba "poster" aPoster ++ (if aLoop then [("loop","")] else []))
               : go as
          go (AOther {..} : as) = t aUrl aMime aFileSize [] : go as
          go (AGrOrigin {} : as) = go as
          go (AThumbnail {} : as) = go as
          t u m f ext =
              [("href", u), ("type", m)] ++ mbia "length" f ++
              (if publicFeed then [] else ext)
              -- duration и остальные параметры являются нестандартными
              -- и не проходит валидация публичного фида
              -- помещаю их в media:content,
              -- который есть только у публичных фидов
          mba a v = maybe [] (\ x -> [(a, x)]) v
          mbia a (Just v) | v > 0 = [(a, T.pack $ show v)]
          mbia _ _ = []

msgMediaRssContents :: Msg -> [Tag T.Text]
msgMediaRssContents = concat . go . msgAttachments
    where go [] = []
          go (AImage {} : as) = go as
          go (AIframe {} : as) = go as
          go (AAudio {..} : as) =
              t aUrl aMime aFileSize
              (mbia "duration" aDuration)
              (mbTitle aTitle) : go as
          go (AVideo {..} : as) =
              t aUrl aMime aFileSize
              (mbia "duration" aDuration
               ++ mbia "width" aWidth ++ mbia "height" aHeight)
              (mbTitle aTitle ++ mbPoster aPoster) : go as
          go (AVideo2 {..} : as) = go (AVideo {..} : as)
          go (AOther {..} : as) =
              -- t aUrl aMime aFileSize :
              -- не включаем не audio/video в mediaRss
              go as
          go (AGrOrigin {} : as) = go as
          go (AThumbnail {} : as) = go as
          t u m f attrs sub =
              [TagOpen "media:content"
               ([("url", u), ("type", m)] ++ mbia "fileSize" f ++ attrs)]
              ++ sub
              ++ [TagClose "media:content"]
          mba a v = maybe [] (\ x -> [(a, x)]) v
          mbia a (Just v) | v > 0 = [(a, T.pack $ show v)]
          mbia _ _ = []
          mbTitle Nothing = []
          mbTitle (Just t) =
              [TagOpen "media:title" [("type", "plain")]
              ,TagText t, TagClose "media:title"]
          mbPoster Nothing = []
          mbPoster (Just u) =
              [TagOpen "media:thumbnail" [("url", u)]
              ,TagClose "media:thumbnail"]

api0_json_msg i user (ii@(GRItemId {..}), m@(Msg {..}), tags) =
    apiObj $
    (if readLocked then (("isReadStateLocked", apiTrue):) else id)
    [("crawlTimeMsec", msecStr msgDlTime)
    ,("timestampUsec", usecStr t)
    ,("id", apiStr $ googleItemId ii)
    ,("categories", apiList $ map apiStr $
      (if iiRead then [state "read"] else []) ++
      [state "reading-list"
      ,state "fresh"] ++ map (api0itemTag user) tags ++
      msgTags)
    ,("published", secInt False t)
    ,("updated", secInt False t)
    ,("canonical"
     , apiList [apiObj1 "href" (apiStr l)
                | l <- maybeToList msgLink ])]
    ++ maybe [] (\ l -> [("alternate"
                         , apiList [apiObj [ ("href", apiStr l)
                                           , ("type", apiStr "text/html")]])])
             msgLink
    ++ enclosures ++
    [("summary", apiObj [("direction", apiStr "ltr")
                        ,("content", apiStr msgText)])
    ,("title", apiStr (renderTagsT [TagText msgSubject])) --msgSubject)
    ,("author", apiStr msgAuthor)
    -- "likingUsers":[],"comments":[],"annotations":[],
    -- "enclosure":[{"href":"http://feeds.wired.com/~r/wired/software/~5/pQZCl3FuSlA/0513_archos_1200-200x100.jpg","type":"image/jpeg","length":"48000"}]
    ,("origin"
     , apiObj [("streamId", apiStr streamId)
              ,("title", apiStr streamTitle)
              ,("htmlUrl", apiStr htmlUrl)
               --  ^ Reeder берет иконку отсюда
              ])]
    where (streamId, streamTitle, htmlUrl, readLocked) = api0msgStream m ii
          t = fromMaybe msgDlTime msgTime
          state s = T.concat [api0userPrefix user, "state/com.google/", s]
          enclosures
              | es@(_:_) <- msgEnclosures False m =
                  [("enclosure", apiList
                    [apiObj [(n, apiStr v) | (n,v) <- e] | e <- es])]
              | otherwise = []

api0msgStream (Msg {..}) (GRItemId {..})
    | AGrOrigin {..} : _ <- msgAttachments =
        ( "feed/https://bazqux.com/fakefeed/" `T.append` aFeed
          -- видимо, надо несуществующий фид сюда. А то Mr.Reader пытается
          -- пометить непрочитанными пост
          -- и Reeder показвает этот пост, как непрочитанный
          -- Reeder не показывает непрочитанные для "gr:1"
          -- может сделать его https://bazqux.com/gri/<user> ?
          --
          -- Может вообще всем выводить этот stream как есть?
        , aStreamTitle `T.append`
          (if aGuid /= "" then " (imported)" else " (unsubscribed)")
        , aHtmlUrl
        , True )
    | otherwise =
        ( "feed/" `T.append` msgKeyBlogFeedUrl msgKey
        , iiStreamTitle
        , fromMaybe (msgKeyBlogFeedUrl msgKey) iiHtmlUrl
        , False )

api0itemTags user i ii =
    IntMap.findWithDefault [] (midGrItemId (iiMsgId ii)) $
    griItemTags i

api0_atom_msg publicFeed i rl user (ii@(GRItemId {..}), m@(Msg {..}), tags) = renderTagsT $
    TagOpen "entry" ((if readLocked then (("gr:is-read-state-locked", "true"):)
                      else id)
                     [("gr:crawl-timestamp-msec"
                      , msecStr' (fromMaybe msgDlTime msgTime)
                       -- msgDlTime <- Feeddler показывает это время
                     )]) :
    TagOpen "id" [("gr:original-id", maybe "" sbt (msgKeyPostGuid msgKey))] :
        TagText (googleItemId ii) :
    TagClose "id" :
    ((if isJust publicFeed then [] else
          (if iiRead then state "read" else []) ++
          (if rl then state "reading-list" ++ state "fresh" else []) ++
          concatMap itemTag tags) ++
     -- почему-то эти теги только если reading-list запрашивается
     concatMap tag msgTags ++
    (TagOpen "title" [("type", "html")] :
             TagText (renderTagsT [TagText msgSubject]) : TagClose "title" :
     TagOpen "published" [] : TagText time : TagClose "published" :
     TagOpen "updated" [] : TagText time : TagClose "updated" :
-- почему-то NNW показывает теги и не синхронизирует прочитанность
    (maybe [] (flip link []) msgLink ++
    concatMap enclosure (msgEnclosures (isJust publicFeed) m) ++
    (if isJust publicFeed then msgMediaRssContents m else []) ++
    (TagOpen "summary" [("type","html")] :
        TagText msgText :
    TagClose "summary" :
    TagOpen "author" [] : TagOpen "name" [] : -- gr:unknown-author="true"
        TagText msgAuthor :
    TagClose "name" : TagClose "author" :
    TagOpen "source" [("gr:stream-id", streamId)] :
    TagOpen "id" [] :
        TagText ("tag:google.com,2005:reader/" `T.append` streamId) :
    TagClose "id" :
    TagOpen "title" [("type","html")] :
        TagText streamTitle :
    TagClose "title" :
    link htmlUrl
    [TagClose "source", TagClose "entry"]))))
    where (streamId, streamTitle, htmlUrl, readLocked) = api0msgStream m ii
          link href xs =
              TagOpen "link" [("rel","alternate")
                             ,("href", href)
                             ,("type","text/html")] : TagClose "link" : xs
          state' t s =
              [TagOpen "category"
              [("term", t)
              ,("scheme", "http://www.google.com/reader/")
              ,("label", s)], TagClose "category"]
          state s = state' (T.concat [api0userPrefix user, "state/com.google/", s]) s
          itemTag ITStarred = state' (api0itemTag user ITStarred) "starred"
          itemTag (ITTag t) = state' (api0itemTag user $ ITTag t) t
          tag t =
              [TagOpen "category" [("term", t)], TagClose "category"]
          enclosure a = [ TagOpen "link" (("rel", "enclosure"):a)
                        , TagClose "link" ]
-- <category term="user/03119542330395810734/state/com.google/reading-list" scheme="http://www.google.com/reader/" label="reading-list"/>
          time = T.pack $ formatUrTimeRfc3399 $ fromMaybe msgDlTime msgTime

api0_edit_tag log qs user = do
    let notEmpty ii = msgKeyBlogFeedUrl (iiMsgKey ii) /= ""
    (_, take 10000 . filter notEmpty -> iis) <-
        getGRItemIds False user $ sort qs
    time <- getUrTime
    -- сортируем, чтобы bgactions больше пачки набирал
    let bgMarkMsgRead read (GRItemId {..}) =
            BGMarkMsgRead iiMsgId read maxBound
        markRead read = do
            r <- performBgActions user $ map (bgMarkMsgRead read) iis
            writeIORef log r
            return []
        readState s = "state/com.google/read" `T.isSuffixOf` s
        starredState s = "state/com.google/starred" `T.isSuffixOf` s
        edit act tag =
            return $ zip3 (repeat act) (repeat tag) (map iiLongMsgId iis)
        add = edit (EITAdd time)
        remove = edit EITRemove
    as <- forM [t | ("a",t) <- qs] $ \ t -> case t of
        (readState -> True) -> markRead True
        (starredState -> True) -> add ITStarred
        (api0getFolder -> Just f) -> add (ITTag f)
        _ -> return []
    rs <- forM [t | ("r",t) <- qs] $ \ t -> case t of
        (readState -> True) -> markRead False
        (starredState -> True) -> remove ITStarred
        (api0getFolder -> Just f) -> remove (ITTag f)
        _ -> return []
    let edits = concat $ as ++ rs
    editItemTags user $ reverse edits

api0subscribe user q title folders = do
    t <- getUrTime
    modifySubscriptionUrlInfo'_ s $ \ sui ->
        return $ sui { suiTime = case suiKind sui of
                                   SUKError _ -> UrTime 0 0
                                   SUKErrorPath {} -> UrTime 0 0
                                   -- в случае ошибок сносим их для обновления,
                                   -- имитация retrySubscription
                                   _ -> suiTime sui
                     }
    userSubscribe user s title folders
    check t
    where s = normalizeTURL q
          check t = do
              t' <- getUrTime
              if t' > t `plusUrTime` 25 then
                  api0subscribeError q
              else do
                  u <- userCheckSubscriptions user
                  let findRen [] = s
                      findRen ((rt, from, to):rs)
                          | rt < t    = s
                          | from == s = to
                          | otherwise = findRen rs
                      s' = findRen $ uvmSubUrlRenames $ uViewMode u
                      err = do
                          modifyUser'_ user $ \ u -> return $
                              u { uSubscriptions =
                                      filter ((/= s') . sUrl) $
                                      uSubscriptions u }
                              -- сносим кривую подписку
                              -- TODO: по идее нельзя ее сносить, если уже была
                          postsAddSubscriber s' user False
                          api0subscribeError q
                  case fmap sState $ find ((== s') . sUrl) $ uSubscriptions u of
                      Just (SSFeed u)  -> ok u
                      Just (SSError _) -> err
                      Just (SSErrorPath {}) -> err
                      _ -> do
                          threadDelay 500000
                          check t
          ok feed = do
              return $ apiObj [ ("query", apiStr q)
                              , ("numResults", apiInt False 1)
                              , ("streamId", apiStr $ T.append "feed/" feed) ]

api0subscribeError s =
    return $ apiObj [ ("query", apiStr s)
                    , ("numResults", apiInt False 0) ]

api0_subscription_quickadd qs user
    | Just q <- lookup "quickadd" qs
    , q /= ""   = api0subscribe user q Nothing []
    | otherwise = api0subscribeError ""

api0_subscription_edit qs user
    | Just s <- lookup "s" qs
    , "feed/" `T.isPrefixOf` s
    , feed <- T.drop 5 s = case lookup "ac" qs of
        Just "subscribe" -> do
            let getFolder n ((== n) -> True, api0getFolder -> Just f) = [f]
                getFolder _ _ = []
            r <- api0subscribe user feed (lookup "t" qs)
                 (sort $ nub
                  (concatMap (getFolder "a") qs \\
                   concatMap (getFolder "r") qs))
            case r of
                APIObject [_,_] -> return False -- нет streamId
                _ -> return True
        Just "unsubscribe" -> do
            userUnsubscribe user [feed]
            return True
        Just "edit" -> do
            let mb t f = maybe (return ()) f (lookup t qs)
                edit t add = forM_ [f | (c,f) <- qs, c == t] $ \ f ->
                    case api0getFolder f of
                         Just folder ->
                              userEditSubscriptionFolders user feed folder add
                         _ -> return ()
            mb "t" $ userRenameSubscription user feed
            edit "a" True
            edit "r" False
            return True
        _ -> return False
    | otherwise = return False

api0_rename_tag qs user
    | Just s <- api0getFolder =<< lookup "s" qs
    , Just dest <- api0getFolder =<< lookup "dest" qs =
       void $ userRenameFolder user s dest
    | otherwise = return ()

api0_disable_tag qs user
    | Just s <- api0getFolder =<< lookup "s" qs =
       void $ userDisableFolder user s
    | otherwise = return ()

test_mark = withLogger $ \ l -> do
--    (_, _, s, _) <- subscriptionsAndViewMode' False False "" "" [] user
    -- userCheckSubscriptions user
    u <- readUser' user
--     forM_ (map sUrl $ uSubscriptions u) $ \ f ->
--         deletePostsRead $ defaultPostsRead (user, f)
--     print =<< logTime l "dumbMark" (performBgActions user
--           [BGMarkBlogRead u maxBound maxBound | Subscription { sState = SSFeed u, ..} <- uSubscriptions u])
    replicateM_ 2 $ do
        log <- newIORef ""
        logTime l "mark" $ api0_mark_all_as_read log [("s","user/-/state/com.google/reading-list")] user
        print =<< readIORef log
    where user = "perf_test"

api0_mark_all_as_read log qs user = do
    (tags, feeds, am, mv, rl, s, n) <- api0_msgForestPreprocess False qs user
    if tags /= [] then
        go 0 -- с тегами ничего особо не попишешь
    else do
--        print $ length feeds
        gri <- cachedReadGRIds' user
        m <- join $ forkReadPar2 (mapM (markBlogReadApi user gri am)) feeds
        writeIORef log $
            T.concat [ti (sum m), " ps ", ti (length feeds), " markBlogRead"]
    where ti = T.pack . show
          go n = do
              let l n a = a -- withLogger $ \ l -> logTime l n a
              (_,_,_,_,_,mf,_) <- l "msgForest" $ api0_msgForest False mfqs user
--              print mf
              if not $ null $ mfList mf then do
                  let actions =
                          map (\ mid -> BGMarkMsgRead mid True maxBound) $
                          sortBy (comparing midFeedId) $
                          map (miMsgId . fst) $ mfList mf
                  l "length" $ print $ length actions
                  l "bg" $ performBgActions user actions
                  go (n + length actions)
              else
                  writeIORef log $
                      T.concat [ti n, " ps ", " 1 markBlogRead"]
          mfqs =
              ("xt", "user/_/state/com.google/read") :
              ("n","50000") : qs

api0_preference_stream_set qs user
    | Just s <- lookup "s" qs
    , Just "subscription-ordering" <- lookup "k" qs
    , Just v <- lookup "v" qs
    , T.length v <= maxPaidSubscriptions user * 8 * 2 --  *2 для тегов
    , Just sortIds <- getSortIds [] v = do
        u <- cachedReadUser' user
        i <- getGRIds' user (uSubscriptions u)
        olist <- userGetOrdering uFeedSubscriptions user
        let name sortId =
                IntMap.lookup sortId (griFolderNames i) <|>
                IntMap.lookup sortId (griFeedUrls i)
            folder
                | "state/com.google/root" `T.isSuffixOf` s = Just ""
                | Just f <- api0getFolder s = Just f
                | otherwise = Nothing
            names = map name sortIds
            sortIdsSet = IntSet.fromList (starredSortId:allTagsSortId:sortIds)
            findPrev i o = go $ drop 1 $ dropWhile (/= i) $ reverse o
                where go [] = Nothing
                      go (x:xs)
                          | x `IntSet.member` sortIdsSet = Just x
                          | otherwise = go xs
            ins Nothing i o = i:o
            ins (Just a) i o = go o
                where go [] = []
                      go (x:xs) | x == a = x:i:xs
                                | otherwise = x : go xs
            fix "" sortIds
                | Just (_,o) <- lookup "" olist =
                    case (findPrev starredSortId o, findPrev allTagsSortId o) of
                        (Just s, t) | s == allTagsSortId ->
                            ins (Just s) starredSortId $
                            ins t allTagsSortId sortIds
                            -- если звездочки после тегов, то вставляем сначала
                            -- теги
                        (s,t) ->
                            ins t allTagsSortId $ ins s starredSortId sortIds
            fix _ sortIds = sortIds
        case folder of
            Just f | all isJust names -> do -- нужна ли проверка isJust?

                modifyGRIds'_ user $ \ i ->
                    return $ i { griLastId = griLastId i + 1
                               , griOrdering = Map.insert f (fix f sortIds) $
                                               griOrdering i }
            _ -> return ()
--        logS $ show $
    | otherwise = return ()
    where
          getSortIds acc "" = Just $ reverse acc
          getSortIds acc v
              | (n,vs) <- T.splitAt 8 v
              , T.length n == 8
              , [((sortId :: Int), "")] <- reads ("0x" ++ T.unpack n) =
                  getSortIds (sortId : acc) vs
              | otherwise = Nothing

-- api0_preference_set
-- Drag-and-drop
-- k:lhn-prefs
-- v:{"selectors":{"ism":"false","suc":"false"},"friends":{"ism":"true","suc":"true","saf":"true"},"subscriptions":{"ism":"false","sas":"true","suc":"true","ssa":"false"},"recommendations":{"ism":"true"}}
-- ssa=false
--
-- Unread only
-- k:lhn-prefs
-- v:{"selectors":{"ism":"false","suc":"false"},"friends":{"ism":"true","suc":"true","saf":"true"},"subscriptions":{"ism":"false","sas":"false","suc":"true","ssa":"false"},"recommendations":{"ism":"true"}}
-- sas=false
--
-- Hide unread counts
-- k:lhn-prefs
-- v:{"selectors":{"ism":"false","suc":"false"},"friends":{"ism":"true","suc":"true","saf":"true"},"subscriptions":{"ism":"false","sas":"false","suc":"false","ssa":"false"},"recommendations":{"ism":"true"}}
-- suc=false
--
-- Hide favicons
-- k:custom-favicons-enabled
-- v:false
--
-- List view
-- is-card-view
-- v:false


------------------------------------------------------------------------------
-- Fever API

processFever log qs user = do
    processFeverMark log qs user
    feedsAndGroups <-
        if req "feeds" || req "groups" then
            feverFeedsAndGroups user
        else
            return []
    unread_item_ids <-
        ifReq "unread_item_ids" $ fever_item_ids "unread"
    saved_item_ids <-
        ifReq "saved_item_ids" $ fever_item_ids "saved"
    items <-
        ifReq "items" $ fever_items qs
    links <-
        ifReq "links" $ fever_links qs
    return $ apiObj $
        [("api_version", apiInt False 3)
        ,("auth", apiInt False 1)]
        ++ (if req "favicons" then
                [("favicons", apiList [apiObj [("id", apiInt False 1)
                                              ,("data", apiStr emptyGif)]])]
            else [])
        ++ unread_item_ids
        ++ saved_item_ids
        ++ items
        ++ feedsAndGroups
        ++ links
    where req x = isJust $ lookup x qs
          emptyGif = "image/gif;base64,R0lGODlhAQABAIAAAObm5gAAACH5BAEAAAAALAAAAAABAAEAAAICRAEAOw=="
          ifReq x act = if req x then act user else return []

feverFeedsAndGroups user = do
    subscriptionsAndSettings "" True False user
    -- не checkSubscriptions, чтобы закешировало
    u <- cachedReadUser' user -- userCheckSubscriptions user
    let s = uFeedSubscriptions u
    ids <- getGRIds' user s
    ps <- cachedReadManyPostss [u | Subscription { sState = SSFeed u, ..} <- s]
    UrTime sec _ <- getUrTime
    let htmlUrls = HM.fromList
                   [(pBlogFeedUrl p, msgLink $ pRootMessage p) | Just p <- ps]
        feed (Subscription {..}) =
            apiObj [ ("id"
                     , apiInt False $ toEnum $
                       HM.lookupDefault 0 sUrl $ griFeedIds ids)
                   , ("favicon_id", apiInt False 1)
                   , ("title", apiStr $ fromMaybe sUrl sTitle)
                   , ("url", apiStr sUrl)
                   , ("site_url"
                     , apiStr $ fromMaybe sUrl $ join $ HM.lookup sUrl htmlUrls)
                   , ("is_spark", apiInt False 0)
                   , ("last_updated_on_time", apiInt False $ toEnum sec)
                   ]
        groups =
            map (\ t -> (HM.lookupDefault 0 t $ griFolderIds ids, t)) $
            sortBy (comparing T.toLower) $
            HS.toList $ HS.fromList $ concatMap sFolders s
        group (id, title) =
            apiObj [("id", apiInt False $ toEnum id)
                   ,("title", apiStr title)]
        feed_groups =
            [apiObj [("group_id", apiInt False $ toEnum gid)
                    ,("feed_ids", apiStr $ T.intercalate "," $
                                 map (T.pack . show) (IntSet.toList fids))]
             | (gid, fids) <-
                 IntMap.toList $ IntMap.fromListWith IntSet.union $
                 [( HM.lookupDefault 0 f $ griFolderIds ids
                  , IntSet.singleton $ HM.lookupDefault 0 sUrl $ griFeedIds ids)
                  | Subscription {..} <- s
                  , f <- sFolders
                 ]
            ]
    -- Press при обновлении спрашивает unread_item_ids только после since_ids
    return
        [("feeds", apiList $ map feed s)
        ,("groups", apiList $ map group groups)
        ,("feeds_groups", apiList feed_groups)]

fever_ids what user = do
    (i, _, _, _, _, mf, _) <- api0_msgForest False qs user

    return
        [ (mvTime (miMsgView mi), midGrItemId $ miMsgId mi)
        | (mi, _) <- mfList mf
        , what /= "saved" ||
          isActiveFeed (msgKeyBlogFeedUrl $ miMsgKey mi) i
          -- только звездочки из существующих фидов,
          -- т.к. Reeder/ReadKit звездочки из удаленных фидов не понимают
        ]
    where mvTime (MVFull m) = fromMaybe (msgDlTime m) (msgTime m)
          mvTime (MVShort mh _) = fromMaybe (mhDlTime mh) (mhTime mh)
          qs  | what == "unread" =
                  [("s", "user/-/state/com.google/reading-list")
                  ,("xt", "user/-/state/com.google/read")
                  ,("n", "10000")]
              | what == "saved" =
                  [("s", "user/-/state/com.google/starred")
                  ,("n", "10000")]
              | Just stream <- T.stripPrefix "smartstream" what =
                  [("s", api0tagT "-" stream)
                  ,("xt", "user/-/state/com.google/read")
                  ,("n", "10000")]
              | otherwise =
                  [("s", "user/-/state/com.google/reading-list")
                  ,("n", "10000")]

refreshFeverIds unread user = do
    fis0 <- cachedReadFeverIds' user
    t <- getUrTime
    if fiLastRefresh fis0 > Just (t `plusUrTime` (-60)) then
        return fis0
    else do
        ids <- unread
        all <- fever_ids "all" user
        saved <- fever_ids "saved" user
        let add fis ufis ugis [] =
                fis { fiFeverIds = fiFeverIds fis `IntMap.intersection` ugis
                    , fiGRIds = fiGRIds fis `IntMap.intersection` ufis }
            add !fis !ufis !ugis ((_,gi):is)
                | Just fi <- IntMap.lookup gi (fiFeverIds fis) =
                    add fis (IntMap.insert fi 1 ufis) (IntMap.insert gi 1 ugis)
                        is
                | otherwise =
                    let fi = succ $ fiMaxId fis
                        !fifis = IntMap.insert gi fi (fiFeverIds fis)
                        !figis = IntMap.insert fi gi (fiGRIds fis)
                    in
                    add (fis { fiMaxId = fi
                             , fiFeverIds = fifis, fiGRIds = figis })
                        (IntMap.insert fi 1 ufis) (IntMap.insert gi 1 ugis) is
        modifyFeverIds' user $ \ fis -> do
            let r = add (fis { fiLastRefresh = Just t })
                        IntMap.empty IntMap.empty $
                    sort $ ids
                        ++ takeWhile (> (t `plusUrTime` (-30*day), 0)) all
                        ++ saved
            return (r,r)

fever_item_ids what user = do
    ids <- fever_ids what user

    fis <- if what == "unread" then refreshFeverIds (return ids) user
           else cachedReadFeverIds' user

    return
        [(T.append what "_item_ids",
          apiStr $ T.intercalate "," $ map (T.pack . show) $
          sortBy (comparing Down) -- сначала самые свежие
          [ fi
          | (_,id) <- ids
          , fi <- maybeToList $ IntMap.lookup id (fiFeverIds fis) ]
         )]

fromJustE :: HasCallStack => String -> Maybe a -> a
fromJustE _ (Just x) = x
fromJustE e Nothing  = error e

fever_items qs user = do
    fis <- refreshFeverIds (fever_ids "unread" user) user
    let qs' | Just (tryReadUnsignedInt -> Just i) <- lookup "since_id" qs =
                since $ IntMap.toAscList $ snd $ IntMap.split i (fiGRIds fis)
            | Just (tryReadUnsignedInt -> Just 0) <- lookup "max_id" qs =
                reverse $ since $ IntMap.toDescList $ fiGRIds fis
            | Just (tryReadUnsignedInt -> Just i) <- lookup "max_id" qs =
                reverse $ since $ IntMap.toDescList $ fst $ IntMap.split i (fiGRIds fis)
            | Just ids <- lookup "with_ids" qs
            , ids /= "0" =
                -- баг FieryFeeds -- делает лишний запрос
                -- http://bazqux.com/?api&items&with_ids=0
                fisToQS $ map (fromJustE "with_ids must contain comma-separated list of integers" . tryReadUnsignedInt) $
                filter (/= "") $ T.split (== ',') ids
            | otherwise = []
        since = fisToQS . filter (/= 0) . map fst . take 50
                -- нулевую id вообще нельзя, т.к. max_id пойдет от минимального
                -- и опять с нуля
        fisToQS is =
            [ ("i"
              , T.pack $ show $ fromJustE ("Can’t find id=" ++ show fi) $
                IntMap.lookup fi (fiGRIds fis))
            | fi <- is ]
    (_, _, m) <- api0_msgsByItemIds qs' user
    return
        [("total_items", apiInt False 10000)
        ,("items", apiList $ map (fever_item fis) m)]

fever_item fis (ii@(GRItemId {..}), m@(Msg {..}), tags) =
    apiObj $
    [("id", apiInt False $ toEnum $ fromMaybe 0 $
          IntMap.lookup (midGrItemId iiMsgId) (fiFeverIds fis))
    ,("feed_id", apiInt False $ toEnum $ midFeedId iiMsgId)
    ,("title", apiStr (renderTagsT [TagText msgSubject])) --msgSubject)
    ,("author", apiStr msgAuthor)
    ,("html", apiStr msgText)
    ,("url", apiStr $ fromMaybe "" msgLink)
    ,("is_saved", apiInt False $ if ITStarred `elem` tags then 1 else 0)
    ,("is_read", apiInt False $ if iiRead then 1 else 0)
    ,("created_on_time", secInt False $ fromMaybe msgDlTime msgTime)
    ]

fever_links (lookup "page" -> Just page) user | page /= "1" =
    return [("links", apiList [])]
fever_links qs user = do
    f <- cachedReadFilters' user
    gri <- cachedReadGRIds' user
    links <- forM (fSmartStreams f) $ \ ss -> do
        let name = ssName ss
        [(_,ids)] <- fever_item_ids (T.append "smartstream" name) user
        return $ if ids == apiStr "" then Nothing else Just $ apiObj $
            [("id", apiInt False $ toEnum $ fromMaybe 0 $
                  HM.lookup name $ griFolderIds gri)
            ,("feed_id", apiInt False 1)
            ,("item_id", apiInt False 1)
            ,("temperature", APIDouble 98.0)
            ,("is_item", apiInt False 0)
            ,("is_local", apiInt False 0)
            ,("is_saved", apiInt False 0)
            ,("title", apiStr name)
            ,("url", apiStr $ T.append "https://bazqux.com/i/smartstream/" (encodeURIComponentT name))
            ,("item_ids", ids)]

    return [("links", apiList $ catMaybes links)]


processFeverMark log qs user
    | Just (tryReadUnsignedInt -> Just id) <- lookup "id" qs
    , Just as <- lookup "as" qs
    , Just mark <- lookup "mark" qs = do
        i <- getGRIds user
        fis <- cachedReadFeverIds' user
        case (mark, as) of
            ("item", _)
                | Just fi <- IntMap.lookup id (fiGRIds fis)
                , Just c <- cmd as ->
                    api0_edit_tag log [("i", T.pack $ show fi), c] user
            ("feed", "read")
                | Just f <- IntMap.lookup id (griFeedUrls i) ->
                    markAll $ "feed/" `T.append` f
            ("group", "read")
                | id == 0 || id == -1 ->
                    markAll "user/-/state/com.google/reading-list"
                | Just f <- IntMap.lookup id (griFolderNames i) ->
                    markAll $ "user/-/label/" `T.append` f
            _ -> return ()
    | otherwise = return ()
    where markAll what = api0_mark_all_as_read log (("s", what):qs') user
          qs' | Just b <- lookup "before" qs = [("nt", b)]
              | otherwise = []
          cmd "read"    = Just ("a", "state/com.google/read")
          cmd "unread"  = Just ("r", "state/com.google/read")
          cmd "saved"   = Just ("a", "state/com.google/starred")
          cmd "unsaved" = Just ("r", "state/com.google/starred")
          cmd _         = Nothing

------------------------------------------------------------------------------
-- Vimeo thumbnail

data DataCache k v
    = DataCache
      { dcMap :: !(HM.HashMap k v)
      , dcQueue :: !(Map.Map UrTime [k])
      }
    deriving Show

emptyDataCache = DataCache HM.empty Map.empty

cached :: (Hashable k, Eq k) => MVar (DataCache k v) -> (k -> IO (Double, v))
    -> k -> IO v
cached cv f k = do
    t <- getUrTime
    join $ modifyMVar cv $ \ (clear t -> c) -> case HM.lookup k (dcMap c) of
        Just r -> return (c, return r)
        Nothing -> return (c, do
            (dt, r) <- f k
            when (dt > 0) $ do
                t <- getUrTime
                modifyMVar_ cv $ \ (clear t -> c) -> return $
                    DataCache (HM.insert k r $ dcMap c)
                        (Map.insertWith (++) (plusUrTime t dt)[k]  (dcQueue c))
            return r)
    where clear t dc = DataCache m r
              where (l,c,r) = Map.splitLookup t (dcQueue dc)
                    m = foldl' (flip HM.delete) (dcMap dc)
                        (concat (maybeToList c ++ Map.elems l))

vimeoThumbnailCache = unsafePerformIO $ newMVar emptyDataCache
{-# NOINLINE vimeoThumbnailCache #-}

apiProxyCache = unsafePerformIO $ newMVar emptyDataCache
{-# NOINLINE apiProxyCache #-}

-- '<img src="https://i.vimeocdn.com/video/'+m[2]+'_240.jpg"';
-- id картинки не совпадает с id video
vimeoThumbnail :: THostName -> T.Text -> IO (Either T.Text TURL)
vimeoThumbnail h i = cached vimeoThumbnailCache vimeoThumbnail' (h, i)

vimeoThumbnail' :: (THostName, T.Text) -> IO (Double, Either T.Text TURL)
vimeoThumbnail' (h, videoId) = do
    r <- rateLimitedDownload False False ("https://vimeo.com/api/oembed.json?url=https%3A//vimeo.com/" <> videoId)
    case r of
        Right d
            | Just (JSON.Object o) <- decodeJson d
            , Just (JSON.String tu) <- HM.lookup "thumbnail_url" o
            -> return (day, Right $ proxyUrl PTThumbnail h $
                       setVimeoCdnWidth thumbnailWidth tu)
            | otherwise -> err "Invalid JSON"
        Left e -> err e
    where err e = return (hour, Left e)

-- | vimeocdn умеет сжимать картинки по ширине
setVimeoCdnWidth width u
    | [[_,prefix]] <- regexGet setVimeoCdnWidthRE u =
        T.concat [prefix, showT width, ".jpg"]
    | otherwise = u
setVimeoCdnWidthRE = "(https://i\\.vimeocdn\\.com/video/.*_)[0-9x]+\\.jpg"


apiProxy h p = cached apiProxyCache apiProxy' (h, p)
apiProxy' (hostName, p) = case postProcessAndUrl of
    Just (pp, u) ->
        either ((hour,) . Left) ((day,) . Right . SB.toShort . pp)
            <$> rateLimitedDownload False False u
    Nothing ->
        return (0, Left "Bad request")
    where postProcessAndUrl = case p of
              ["youtube_video_details", ids] ->
                  Just (fixYoutube, T.concat ["https://www.googleapis.com/youtube/v3/videos?part=id,snippet&fields=items(id,snippet(title,thumbnails))&maxResults=50&", googleApiKey, "&id=", ids])
              ["vimeo_video_details", vid] ->
                  Just (fixVimeo, "https://vimeo.com/api/oembed.json?url=https%3A//vimeo.com/" <> vid)
              _ -> Nothing
          fixVimeo d
              | Just (JSON.Object o) <- decodeJson d
              , Just (JSON.String tu) <- HM.lookup "thumbnail_url" o
              = BL.toStrict $ JSON.encode $ JSON.Object $
                HM.filterWithKey (\ c _ -> c `elem`
                    ["thumbnail_url", "title", "author_name", "author_url"]) $
                HM.insert "thumbnail_url"
                    (JSON.String $ proxy $ setVimeoCdnWidth 1920 tu) o
                -- иногда бывают мелкие thumbnail_url
              | otherwise = d
          proxy = proxyUrl PTContentStyle hostName
          fixYoutube d
              | Just o <- decodeJson d =
                  BL.toStrict $ JSON.encode $ go o
              | otherwise = d
              where go (JSON.Object o) = JSON.Object $ HM.mapWithKey
                        (\ k o -> case (k, o) of
                            ("url", JSON.String u) -> JSON.String $ proxy u
                            (_, v) -> go v)
                        o
                    go (JSON.Array a) = JSON.Array $ V.map go a
                    go x = x
--     // у этого API есть rate limit на 250 обращений в 15 минут,
--     // также оно не кешируется, и запрос 10 видео идет 500мсек,
--     // в то время как oembed pipeline-ится и общее время может быть 250мсек
--     // (и каждый oembed кешируется отдельно).
--     // Аватарки показывать не получится (их в oembed нет)
-- 'https://api.vimeo.com/videos?per_page=25&access_token=68bfe99786f4382d226c88651b69120d&fields=uri,name,pictures.sizes,user.name,user.link,user.pictures.sizes&uris='+ids.map(function (i) {return "/videos/"+i}).join(","),
