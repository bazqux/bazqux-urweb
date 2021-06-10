{-# LANGUAGE ViewPatterns, RecordWildCards, OverloadedStrings, BangPatterns,
             LambdaCase #-}
-- | Вспомогательные ф-ии для работы с elasticsearch
module Lib.ElasticSearch
    ( esBulkRequest, esBulkRequest', esBulkAction, esBulkAction'
    , UQ(..), ImportUrlQueue(..), iuq
    , esField, esBool, esBoolField
    , esTimeout, esTerm, esTerms, esIntTerms
    , esGreaterThanDateFilter, esLessThanDateFilter, esUrTime
    , esFilter, esMust, esShould
    , esRouting, esMaybeRouting, esHasRouting
    , searchDownloader
    , postSearchDownloader, postSearchDownloaderL, getSearchDownloader
    , decodeSearchResults
    , obj, obj', arr
    )
    where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Aeson.Types
import Lib.Json
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Network.HTTP.Conduit.Downloader
import qualified Control.Concurrent.MSem as MSem
import Lib.Log
import Lib.UrTime
import Lib.StringConversion
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Maybe
import Control.Concurrent
import System.IO.Unsafe
import Control.Monad
import Control.Applicative
import URL
import Config (botDownloaderSettings)
import Text.Printf
import Config
import Generated.DataTypes

--     , esBulkAction, esBulkRequest, esDeleteByQuery
--     , esField, esTwoFields, esNFields, esRouting, esMaybeRouting
--     , iuq, ImportUrlQueue(..), UQ(..), decodeSearchResults

-- | Копия пары необходимых полей из UrlQueue
class UQ a where
    uqDownloader_ :: a -> Downloader
    uqCompleteBPS_ :: a -> MVar (HM.HashMap T.Text UrTime)
    uqGetUrTime :: a -> IO UrTime

data ImportUrlQueue
    = ImportUrlQueue
      { iuqDownloader :: Downloader
      , iuqESRequestSem :: MSem.MSem Int
      , iuqESBulkRequestSem :: MSem.MSem Int
      , iuqDownloadMVar :: MVar ()
        -- ^ не позволяем более одного скачивания с гугля
      , iuqImportBatchSem :: MSem.MSem Int
      , iuqCompleteBPS :: MVar (HM.HashMap T.Text UrTime)
      , iuqCurTime :: MVar UrTime
        -- для синхронизации с основной очередью
        -- TODO: переименовать в StubUrlQueue что-ли
        -- или вообще WorkerGlobals
        -- она не только в импорте, но и в ParseServer-ах используется
      }

iuq = unsafePerformIO $ do
    iuqDownloader <- newDownloader botDownloaderSettings
    iuqESRequestSem <- MSem.new 4
    -- было 4, но при большом кол-ве smart stream подтормаживает
    -- было 16, но, кажется это много
    iuqESBulkRequestSem <- MSem.new 1
    iuqImportBatchSem <- MSem.new 1
    iuqDownloadMVar <- newMVar ()
    iuqCompleteBPS <- newMVar HM.empty
    iuqCurTime <- newMVar (UrTime 0 0)
    return $ ImportUrlQueue {..}
{-# NOINLINE iuq #-}

instance UQ ImportUrlQueue where
    uqDownloader_ = iuqDownloader
    uqCompleteBPS_ = iuqCompleteBPS
    uqGetUrTime = readMVar . iuqCurTime

searchDownloader :: Downloader
searchDownloader =
    unsafePerformIO $ newDownloader (botDownloaderSettings { dsMaxDownloadSize = 256*1024*1024, dsTimeout = 50 })
{-# NOINLINE searchDownloader #-}

esTimeout :: (T.Text, JSON.Value)
esTimeout = ("timeout", JSON.String "45s")

rawPost url dat =
    rawDownload (return . postRequest dat) searchDownloader url Nothing []

postSearchDownloader url dat = do
--    B.putStrLn dat
    MSem.with (iuqESRequestSem iuq) $ rawPost url dat
postSearchDownloaderL l name url dat = do
--    B.putStrLn dat
    t0 <- getUrTime
    MSem.with (iuqESRequestSem iuq) $ do
        t <- getUrTime
        r <- rawPost url dat
        tw <- getUrTime
        let d a b = printf "%4d ms" (round (diffUrTime a b * 1000) :: Int)
                --showSecs (diffUrTime a b)
        logLS l $ name ++ ": " ++ d tw t0
                  ++ "  (" ++ d t t0 ++ " + " ++ d tw t ++ ")"
        return r
getSearchDownloader url = do
--    B.putStrLn dat
    MSem.with (iuqESRequestSem iuq) $ download searchDownloader url Nothing []

esBulkAction = esBulkAction' []
esBulkAction' opts routing index typ action key =
    JSON.Object $ HM.singleton action $ JSON.Object $ HM.fromList $
        map (\ (a,b) -> (a, JSON.String b))
                [ ("_index", index)
                , ("_type",  typ)
                , ("_id", key)
                , ("_routing", routing)
--                 , ("_percolate", T.concat ["blog_feed_url:\"", feedUrl, "\""])
                ]
        ++ opts

-- esBulkRequest action l r = mapM_ (esBulkRequest' action l) (groupByN 100 r)
--  ^ неправильно, некоторые действия идут по 2 строки, некоторые по одной
esBulkRequest  = esBulkRequest' esUrl
esBulkRequest' eu action l [] = return ()
esBulkRequest' eu action l reqs = do
--    logLT l "esBulkRequest' pre"
--    print reqs
    let requests = BL.toStrict $ BL.unlines $ map JSON.encode (reqs :: [JSON.Value])

--    logLS l $ "esBulkRequest' " ++ show reqs
    r <- seq requests $
--         countProcess "ESRequests" (uqSearchCount uq) $
         MSem.with (iuqESBulkRequestSem iuq) $
         postSearchDownloader (eu ++ "/_bulk") requests
--    logLT l "esBulkRequest' post"
    case fst r of
        DROK r _
            | Just (took, items) <-
                do JSON.Object root <- decodeJson r
                   JSON.Number (truncate -> took) <- HM.lookup "took" root
                   JSON.Array items <- HM.lookup "items" root
                   return (took, items)
                ->
                logLS l $ action ++ " "
                        ++ show (length $ filter ok $ V.toList items)
                        ++ " search msgs in " ++ show took ++ "ms"
        _ ->
            logLS l $ "Error in ES request: " ++ show r
    where ok i = isJust $ do
              JSON.Object o <- return i
              JSON.Object index <-
                  HM.lookup "index" o
                  <|> HM.lookup "delete" o
                  <|> HM.lookup "update" o
              let oldES = do -- pre 1.0
                      JSON.Bool True <- HM.lookup "ok" index
                      return ()
                  newES = do
                      JSON.Number s <- HM.lookup "status" index
                      guard (s == 201 -- index
                             || s == 200 -- delete
                            )
              oldES <|> newES
--    BL.putStrLn requests

decodeSearchResults decodeR = \ case
    (DROK d _, _) -> decodeSearchResults' decodeR d
    (DRError e, Just rdr) ->
        Left $ either
            (const $ SESystemError
                 $ T.concat [T.pack e, "\n\n", bst $ rdrBody rdr])
            SESyntaxError
            (decodeError $ rdrBody rdr)
    (DRError e, _) -> Left $ SESystemError $ T.pack e
    (e, _) -> Left $ SESystemError $ showT e
decodeSearchResults' decodeR r = maybe (Left $ SESystemError "Can’t parse search results JSON") Right $ do
    -- (0,0,Nothing,[])
    JSON.Object root <- decodeJson r
    JSON.Number (truncate -> took) <- HM.lookup "took" root
    JSON.Object hits <- HM.lookup "hits" root
    JSON.Number (truncate -> total) <- HM.lookup "total" hits
    JSON.Array hitsArray <- HM.lookup "hits" hits
    let scrollId = do
            JSON.String i <- HM.lookup "_scroll_id" root
            return i
        dec acc [] = return $ reverse acc
        dec !acc (h:xs) = do
            JSON.Object hit <- return h
            JSON.String id <- HM.lookup "_id" hit
            !r <- decodeR id hit
            dec (r:acc) xs
    ids <- dec [] $ V.toList hitsArray
    -- forM на длинных списках кажись не очень
    return (total, took, scrollId, ids)

jsonParser :: (JSON.Value -> JSON.Parser a) -> B.ByteString -> Either String a
jsonParser f dat = do
    x <- eitherDecodeJson dat
    JSON.parseEither f x

decodeError :: B.ByteString -> Either String T.Text
decodeError = jsonParser p
    where p = withObject "root object" $ \ r -> do
              e <- r .: "error"
              fs <- e .: "failed_shards"
              flip (withArray "failed shards") fs
                  $ withHead
                  $ withObject "shard"
                  $ (.: "reason") >=>
                    (\x -> (x .: "caused_by") <|> return x) >=> (.: "reason")
                      >=> withText "error reason"
                          (return . rmPrefix . T.strip . T.take 1000)
          withHead f v = case V.toList v of
              (x:_) -> f x
              [] -> fail "Empty array"
          rmPrefix e = fromMaybe e $ T.stripPrefix "parse_exception: " e

obj n v = obj' [(n, v)]
obj' = JSON.Object . HM.fromList
arr = JSON.Array . V.fromList

esTwoFields f1 v1 f2 v2 = esNFields [(f1,v1),(f2,v2)]
esNFields fs =
    esMust
       [obj "term" (obj f v) | (f,v) <- fs]

esTerm = esField
esField f v =
    obj "term" (obj f (JSON.String v))
esBoolField f v =
    obj "term" (obj f (JSON.Bool v))
esTerms f v = obj "terms" $ obj f $ arr $ map JSON.String v
esIntTerms f v = obj "terms" $ obj f $ arr $ map JSON.Number v
esFilter x =
    obj "constant_score" $ obj "filter" x
-- http://www.elasticsearch.org/blog/all-about-elasticsearch-filter-bitsets/
-- and/or/not не использует bitset-ы
-- и подходит только для geo, script и numeric_range (не range), которые
-- тоже не используют bitset-ы
-- для всего остального нужен bool
esBool must mustNot should =
    obj "bool" $ obj' $
        a "must" must $
        a "must_not" mustNot $
        a "should" should []
    where a _ [] x = x
          a n q x = (n, arr q) : x

wrapXs f [x] = x
wrapXs f xs = f xs

esMust = -- obj "and" . arr
    wrapXs $ obj "bool" . obj "must" . arr
esShould = -- obj "or" . arr
    wrapXs $ obj "bool" . obj "should" . arr
esMustNot = -- obj "not"
    obj "bool" . obj "must_not" . arr
esQueryFilter = obj "query" . obj "query_string" . obj "query" . JSON.String
esLessThanDateFilter f t = obj "range" $ obj f $ obj "lt" (esUrTime t)
esGreaterThanDateFilter f t = obj "range" $ obj f $ obj "gt" (esUrTime t)
-- esUrTime = JSON.String . formatUrTime "%Y-%m-%e %k:%M:%S" -- YYYY-MM-dd HH:mm:ss
esUrTime = JSON.String . T.pack . showUrTime . rmUSec
    where rmUSec (UrTime s _) = UrTime s 0

esHasRouting u
    | Right (lookup "routing" -> Just r) <- urlQueryStringUtf8Only $ T.pack u
    = not $ T.null r
    | otherwise = False
esRouting r
    | Nothing <- T.find (== ',') r -- запятая используется для нескольких route
    , r /= "" =
         "?routing=" ++ encodeURIComponent (T.unpack r)
    | otherwise = ""
esMaybeRouting = maybe "" esRouting

-- esDeleteByQuery l typ routing query =
--     esDeleteByQuery' l "msg" typ (Just routing) query
-- esDeleteByQuery' l idx typ routing query = do
--     let
-- --         term = JSON.Object $ HM.fromList
-- --             [("blog_feed_url", JSON.String bfu)
-- --             ,("post_guid", JSON.String pguid)]
-- --         qjson = JSON.Object $ HM.fromList
-- --             [("term", term)]
--         q = BL.toStrict $ JSON.encode query

--     r <- seq (B.length q) $ MSem.with (iuqESRequestSem iuq) $
-- --         countProcess "ESRequests" (uqSearchCount uq) $
--          downloadG
--              (\ rq -> return $ rq { C.method = N.methodDelete
--                                   , C.requestBody = C.RequestBodyBS q })
--              (iuqDownloader iuq)
--              (esUrl ++ "/" ++ idx ++ "/" ++ typ ++ "/_query" ++
--               esMaybeRouting routing)
--              Nothing []
--     case r of
--         DROK r _
--             | Just () <-
--                 do JSON.Object root <- unsafeDecodeJson r
--                    JSON.Bool True <- HM.lookup "ok" root
--                    let o name node = do
--                          JSON.Object obj <- HM.lookup name node
--                          return obj
--                    shards <- o "_indices" root >>= o (T.pack idx) >>= o "_shards"
--                    JSON.Number 0 <- HM.lookup "failed" shards
--                    return ()
--                 ->
--                 -- logLS l "Deleted post from ES"
--                 return ()
--         _ ->
--             logLS l $ "Error in ES delete " ++ typ ++ ": " ++ show r
