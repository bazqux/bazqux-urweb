{-# LANGUAGE ViewPatterns, RecordWildCards, ScopedTypeVariables #-}
-- | Импорт из Google Reader
module Import
    ( importFromGoogleReaderCallback
    , importStarredAndTaggedItemsFromGoogleReaderCallback
    , importFromGoogleReaderGetForwardUrl
    , importFromGoogleReaderCallback_
    ) where

import Control.Monad
import Data.List
import Data.Ord
import Data.Binary
import qualified Data.ByteString.Short as SB
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Generated.DataTypes
import Lib.UrTime
import qualified Lib.BArray as BA
import Lib.Log
import Lib.StringConversion
import Data.Maybe
import URL
import Resolvables
import Generated.RiakIO
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Text.Encoding as T

import Control.Concurrent
import Network.HTTP.Conduit.Downloader
import qualified Network.HTTP.Types as N
import qualified Network.HTTP.Conduit as C
import qualified Control.Exception as E
import Auth
import qualified Data.ByteString.Char8 as B
import Lib.ReadUtils
import Lib.ElasticSearch

import Text.HTML.TagSoup.Fast
import System.Timeout
import Data.IORef
import Parser (parse)
import Parser.Custom (groupByN, defaultFeedMsg)
import Parser.Types
import Preprocess (xmlLinksImgsVideosAndText)
import qualified Lib.ReadSet as ReadSet
import UrCalls
import qualified Control.Concurrent.MSem as MSem
import Riak
import Parser.Types (FeedItem(..))
import Search
import API

------------------------------------------------------------------------------
-- Импорт подписок

importFromGoogleReaderGetForwardUrl :: TURL -> TURL -> IO TURL
importFromGoogleReaderGetForwardUrl host cb = return $
    T.concat ["https://accounts.google.com/o/oauth2/auth?scope=https%3A%2F%2Fwww.google.com%2Freader%2Fapi&response_type=code&client_id=", googleOAuthClientId, "&redirect_uri=https%3A%2F%2F", host, escapeQueryValueT cb] -- "%2FimportingFromGoogleReader"]

importFromGoogleReaderCallback_ :: T.Text -> T.Text -> T.Text -> IO (T.Text, [Subscription])
importFromGoogleReaderCallback_ host cb =
    googleCallback' True "Google Reader subscriptions"
        host (T.unpack cb)
--        "/importingFromGoogleReader"
        "https://www.google.com/reader/api/0/subscription/list?output=json&"
        (\ t d -> (t, decodeGReaderFeeds d))

decodeGReaderFeeds :: BL.ByteString -> [Subscription]
decodeGReaderFeeds bs = fromMaybe [] $ do
    JSON.Object root <- JSON.decode bs
    JSON.Array ss <- HM.lookup "subscriptions" root
    return $ catMaybes $ flip map (V.toList ss) $ \ s -> do
        JSON.Object s <- return s
        let look k m = do
              JSON.String r <- HM.lookup k m
              return r
        id <- look "id" s
        guard ("feed/" `T.isPrefixOf` id)
        let sUrl = T.drop 5 id
            sState = SSAdded
            sEditsCount = 0
        (Just -> sTitle) <- look "title" s
        JSON.Array cs <- HM.lookup "categories" s
        let sFolders = catMaybes $ flip map (V.toList cs) $ \ c -> do
                JSON.Object c <- return c
                look "label" c
        return $ Subscription {..}

testSubscriptions =
    fmap decodeGReaderFeeds $
         BL.readFile "greader_feeds.js"

test = do
    s <- testSubscriptions
    B.putStrLn $ B.pack $ ppShow s

------------------------------------------------------------------------------
-- Импорт Starred items

importFromGoogleReaderCallback :: T.Text -> T.Text -> T.Text -> T.Text -> IO ()
importFromGoogleReaderCallback host cb qs user = do
    (token, s) <- Auth.importFromGoogleReaderCallback_ host cb qs
    importGoogleReaderSubscriptions user s
    tagImportPercent <- grImportPercent user
    when (isNothing tagImportPercent) $ do
        setGrImportPercent user 1
        void $ forkIO $ importStarredAndTaggedItems True user token (Just s)

importStarredAndTaggedItemsFromGoogleReaderCallback ::
    T.Text -> T.Text -> T.Text -> T.Text -> IO ()
importStarredAndTaggedItemsFromGoogleReaderCallback host cb qs user = do
    tagImportPercent <- grImportPercent user
    when (isNothing tagImportPercent) $ do
        setGrImportPercent user 0
        (token, s) <- Auth.importFromGoogleReaderCallback_ host cb qs
        setGrImportPercent user 1
        void $ forkIO $ importStarredAndTaggedItems True user token (Just s)
--    importGoogleReaderSubscriptions s

testImport =
    importStarredAndTaggedItems True "1" token Nothing
    where token = "ya29.AHES6ZRukJNcSEJcv-YHvZKAuGFiGpstgROufibWPxybK6GRsw"

importStarredAndTaggedItems importToIds user token s =
    (importStarredAndTaggedItems' importToIds user token s
    `E.catch`
    \ (e :: E.SomeException) ->
        logT (T.concat [ "Import of starred items failed for ", user, ": "
                       , T.pack $ show e]))
    `E.finally`
    clearGrImportPercent user

importStarredAndTaggedItems' importToIds user token mbSubscriptions =
    withLogger $ \ l -> logTime l "total" $ do
    logLT l $ T.concat ["Importing starred items for ", user]
    let mkUrl u =
            T.concat
                 [ "https://www.google.com/reader/api/0/"
                 , u
                 , if isJust $ T.find (== '?') u then "&" else "?"
                 , "access_token=", token ]
        d = iuqDownloader iuq
        getUrl' what f u = logTime l (T.pack $ "get " ++ what) $ do
--            print $ mkUrl u
            let err reason =
                    fail $ "Can’t download " ++ what ++ ": " ++ reason
            r <- withMVar (iuqDownloadMVar iuq) $ \ () -> f $ T.unpack $ mkUrl u
            case r of
                DROK dat _ -> return $ BL.fromStrict dat
                DRError e -> err e
                DRNotModified -> err "Not modified?"
                DRRedirect r -> err $ "Redirect to " ++ r ++ " ???"
        getUrl what = getUrl' what $ \ url -> download d url Nothing []
        postUrl what u dat = flip (getUrl' what) u $ \ url ->
            postUrlEncoded d url Nothing dat
        tagsList dat = do
            JSON.Object root <- JSON.decode dat
            JSON.Array tags <- HM.lookup "tags" root
            let tag t = do
                    JSON.Object o <- return t
                    JSON.String s <- HM.lookup "id" o
                    api0getFolder s
            return $ catMaybes $ map tag $ V.toList tags
        idsPrefix =
            "stream/items/ids?output=json&n=10000\
            \&includeAllDirectStreamIds=true"
            -- merge не надо, т.к. он тогда всего 10k вернет, а не с каждого
            -- stream-а
        unionTags t1 t2 = nubBy (\a b -> fst a == fst b) t1 ++ t2
        idsMap folderSet dat = do
            JSON.Object root <- JSON.decode dat
            JSON.Array itemRefs <- HM.lookup "itemRefs" root
            let tag t d = do
                    JSON.String s <- return d
                    tag <- api0stateTag folderSet s
                    return (tag, t)
                id i = do
                    JSON.Object o <- return i
                    JSON.String id <- HM.lookup "id" o
                    JSON.Array d <- HM.lookup "directStreamIds" o
                    JSON.String ts <- HM.lookup "timestampUsec" o
                    t <- tryReadUsecUrTime ts
                    return (fromEnum $ readInteger id,
                            mapMaybe (tag t) $ V.toList d)
            return $ IntMap.fromListWith unionTags $ mapMaybe id $
                V.toList itemRefs

    s <- maybe
        (fmap decodeGReaderFeeds
         (getUrl "subscriptions list" "subscription/list?output=json"))
        return
        mbSubscriptions
    setGrImportPercent user 2
    folderAndTagSet <-
        fmap (HS.fromList . fromMaybe [] . tagsList) $
        getUrl "tags list" "tag/list?output=json"
    setGrImportPercent user 3
    let folderSet = HS.fromList $ concatMap sFolders s
        subTitles =
            HM.fromList
            [ (sUrl, title) | Subscription {..} <- s, Just title <- [sTitle] ]
        tags = sort $ HS.toList $ HS.difference folderAndTagSet folderSet
        streams = map (T.append "&s=" . escapeQueryValueT) $
            "user/-/state/com.google/starred" :
            map (T.append "user/-/label/") tags
        maxUrlLen = 2000 - T.length (mkUrl idsPrefix) -- 2068
        groupStreams acc n [] = [acc]
        groupStreams acc n (s:ss)
            | n' < maxUrlLen = groupStreams (s:acc) n' ss
            | T.length s >= maxUrlLen = groupStreams acc n ss
              --  ^ параноидальный случай против зацикливания
            | otherwise = acc : groupStreams [] 0 (s:ss)
            where n' = n + T.length s
        streamUrls =
            map (T.concat . (idsPrefix :) . reverse) $
            groupStreams [] 0 streams
        bfu = grBfu user
    idsList <- forM streamUrls $ \ u ->
        fmap (idsMap folderSet) $ getUrl "ids list" u
    setGrImportPercent user 4
    posts0 <- cachedReadPosts' bfu
    let existingIds = IntMap.fromList $
            [ (readGRId $ sbt $ mhGuid mh, [])
            | mh <- BA.elems $ mtHeaders $ pMsgTree posts0 ]
        ids =
            IntMap.filter (not . null) $
            foldl (IntMap.unionWith unionTags) IntMap.empty $ catMaybes idsList
        maxIdsPerRequest = 250
        newIds = ids `IntMap.difference` existingIds
        nNew = IntMap.size newIds
        nIds = IntMap.size ids
        updPercent n = do
--            threadDelay 500000
            setGrImportPercent user $
                min 99 (100 * (4 + n) `div` (4 + nNew + nIds))
        batches =
            groupByN maxIdsPerRequest $ -- drop 3 $ -- take 1 $ -- map fst $
            sortBy (comparing $ Down . maximum . map snd . snd) $
            IntMap.toList newIds
        postContents ids =
            postUrl "contents"
                    "stream/items/contents?output=atom"
                    -- likes=false&comments=false"
                    -- mediaRss=true -- приводит к internal server error
                    (T.encodeUtf8 $ T.intercalate "&" $ map (T.append "i=" . T.pack . show . fst) ids)
        grTag t
            | Just _ <- api0getFolder t = True
            | "/com.google/" `T.isInfixOf` t = True
              -- бывает еще /user/12082553810243295800/source/com.google/link
              -- не только state
            | otherwise = False
        fixFM fm =
            fm { fmGuid = T.pack $ show $ readGRId $ fmGuid fm
               , fmAuthor =
                   if fmAuthor fm == "(author unknown)"
                   then "" else fmAuthor fm
               , fmSubject =
                   if fmSubject fm == "(no subject)" -- не уверен, что так
                   then "" else fmSubject fm
               , fmTags = filter (not . grTag) (fmTags fm)
               }
        fixStreamId = T.replace "tag:google.com,2005:reader/feed/" ""
        fixTitle sid t
            | Just title <- HM.lookup sid subTitles = fixTitle "" title
            | otherwise = text
            where (_, _, _, text) = xmlLinksImgsVideosAndText $ parseTagsT $ tbs t
        -- почему-то GR escape-ит &quot; и пр. в заголовке
        -- обрабатывает секунду, если все уже есть и две-три, если нет
        -- и это в ghci
        handleBatches :: Int -> ([a] -> IO ()) -> [[a]] -> IO ()
        handleBatches n act [] = return ()
        handleBatches n act (b:bs) = do
            MSem.with (iuqImportBatchSem iuq) $ do
                r <- timeout 20000000 $ act b
                when (isNothing r) $ logLS l "batch timeout??"
                threadDelay 100000 -- чуть разгружаем
            let n' = n + length b
            updPercent n'
            handleBatches n' act bs
        handleBatch b = logTime l "batch   " $ do
            c <- postContents b
            t <- getUrTime
            case parse t "" (BL.toStrict c) of
                PRFeed _ _ (map fixFM -> fms) -> do
                    let rootMsg =
                            (defaultMsg (MsgKey bfu Nothing Nothing))
                            { msgSubject =
                                  user `T.append`
                                  "’s Google Reader starred and tagged items"
                            }
                        addOrigin fm fi
                            | Just (streamId, title, htmlUrl) <- fmSource fm
                            , origGuid <- fromMaybe "" $
                                lookup LKGROriginalId (fmLinks fm)
                                --  ^ может быть пустой
                            , sid <- fixStreamId streamId =
                                fi { fiMsg = (fiMsg fi) { msgAttachments =
                                     AGrOrigin sid
                                               (tsb origGuid)
                                               (fixTitle sid title)
                                               htmlUrl :
                                     (msgAttachments $ fiMsg fi) } }
                            | otherwise = fi
                        toFeedItem (parent, fm) =
                            addOrigin fm $
                            feedMsgToItem True defaultFeedMsg rootMsg t (UrTime 0 0) ""
                            (\ g -> MsgKey bfu (Just g) Nothing)
                            parent fm
                    modTime <- newIORef t
                    cnt <- modifyPosts' bfu $ \ posts -> do
                        (_, mt', _, _) <-
                            editMsgTree Nothing False False True modTime
                                        l bfu iuq
                                        (fromMaybe t . msgTime)
                                        rootMsg
                                        (pMsgTree posts)
                                        toFeedItem
                                        (zip (repeat Nothing) fms)
                        return (posts { pRootMessage = rootMsg
                                      , pMsgTree = mt'
                                      }
                               , snd $ BA.bounds $ mtHeaders mt')
                    modifyPostsRead'_ (user, bfu) $ \ pr ->
                        return $ pr { prSet = ReadSet.fromRange 0 cnt }
                r ->
                    logLS l $ "handleBatch parse error: " ++ show r
--    print (batches)
    writeImportData user ".starred.bin" $ BL.toStrict $ encode ids
    logLS l $ "Total items: " ++ show (IntMap.size ids)
    logLS l $ "Total tags : " ++ show (length tags) ++
         if notNull tags then " " ++ show tags else ""
    logLS l $ "To sync    : " ++ show (IntMap.size newIds)
    handleBatches 0 handleBatch batches
    when importToIds $ importToGRIds (handleBatches nNew) False user ids

--    getUrl "token"

testImportGRIds = do
    c <- B.readFile "../user_imports/??%40gmail.com.starred.bin"
    importToGRIds mapM_ True "??@gmail.com" (decode $ BL.fromStrict c)

-- importToGRIds :: ([[Int]] -> IO ()) ->
--                  Bool -> T.Text -> IntMap [(ItemTag, UrTime)] -> IO ()
importToGRIds handleBatches test user ids0 = do
    u <- cachedReadUser' userTo
    gri <- getGRIds' userTo (grSubscription user : uSubscriptions u)
    posts0 <- cachedReadPosts' bfu
    let indexes = IntMap.fromList $
            [ (readGRId $ sbt $ mhGuid mh, idx)
            | (idx, mh) <- BA.assocs $ mtHeaders $ pMsgTree posts0 ]
        ids = ids0 `IntMap.intersection` indexes
        Just grFeedId = HM.lookup (grBfu user) (griFeedIds gri)
        lo ITStarred = ITStarred
        lo (ITTag t) = ITTag $ T.toLower t
        tags = sortBy (comparing $ \ (time, tag, _) -> (time, lo tag))
            [ (time, tag,
               MsgId (MsgKey bfu (Just $ SB.pack $ map B.c2w $ show id) Nothing)
                     grFeedId postId Nothing)
            | (id, tags) <- IntMap.assocs ids
            , (tag, time) <- tags
            , Just postId <- [IntMap.lookup id indexes] ]
        add (time, t, k) = (EITAdd time, t, k)
    handleBatches (editItemTags userTo . map add) $ groupByN 250 tags
    modifyGRIds'_ userTo $ \ i ->
        return $ i { griLastId = griLastId i + 1
                   , griTaggedItems =
                         Map.map (reverse . sort) (griTaggedItems i)
                   , griGRTagsImported = True
                   }
        -- сортируем, т.к. могут быть уже добавленные звездочки
    where userTo | test      = "1"
                 | otherwise = user
          bfu = grBfu user

subscribeGR user =
    modifyUser'_ "1" $ \ u ->
        return $ resolve u $
            u { uSubscriptions = grSubscription user : uSubscriptions u }

grSubscription user =
    Subscription
    { sUrl        = bfu
    , sState      = SSFeed bfu
    , sEditsCount = 0
    , sTitle      = Just user
    , sFolders    = []
    }
    where bfu = grBfu user
