{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns,
             TypeFamilies, FlexibleContexts, FlexibleInstances,
             BangPatterns, TupleSections, ScopedTypeVariables,
             DeriveDataTypeable #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- | Работа с Riak
module Riak
    ( KV(..), Resolvable(..), TextKey(..), textKeyList, key, Bucket
    , initRiak
    , newCache, cachedReadKV, cachedReadManyKVs
    , cachedNothingReadKV, cachedNothingReadManyKVs
    , recacheKVs
    , readKV, mergeWriteKV, readManyKVs, writeManyKVs
    , modifyKV, modifyKV_
    , deleteKV
--    , lookupCache
    , riakPool, riakPool2, riakPool3
    , blockBucketKey
    , forkRead, forkReadPar2
    ) where

import Control.Arrow (first)
import Data.List (foldl')
import Data.Typeable
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Short as SB
import qualified Network.Riak.Types as R
import Network.Riak.Value.Resolvable (Resolvable(..))
import Network.Riak.Protocol.GetResponse (GetResponse(..))
import qualified Network.Riak.Value.Resolvable as R
import qualified Network.Riak.Connection as R
import qualified Network.Riak.Connection.Pool as R
import qualified Network.Riak.Content as R
import qualified Network.Riak.Escape as R
import qualified Network.Riak.Value as R (fromContent)
import qualified Network.Riak.Response as R (unescapeLinks)
--import qualified Network.Riak.Types.Internal as R
import qualified Network.Riak.Protocol.GetRequest as GetRequest
import qualified Network.Riak as R (delete)
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import qualified Control.Exception as E
import Data.Binary hiding (get, put)
import Control.Monad
import Control.Concurrent
import System.IO.Unsafe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Lib.Stats
--import URL
import Lib.Merge
import Lib.UrTime
import Data.Maybe
import Blaze.ByteString.Builder
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base64 as Base64
import Data.Char
import Data.Array
import Codec.Compression.Zlib.Raw as Deflate
import Codec.Compression.GZip as GZip
import System.Random
import System.Remote.Monitoring (getGauge)
import qualified System.Remote.Gauge as Gauge
import Lib.Log (logS, withLogger, logTime)
import Lib.BinaryInstances ()
import System.Mem
import GHC.Stats

defragment :: BL.ByteString -> BL.ByteString
defragment = toLazyByteString . fromLazyByteString

type Bucket = BL.ByteString

deflateCompress =
    Deflate.compressWith $
    Deflate.defaultCompressParams
    { compressLevel = Deflate.bestCompression -- bestSpeed
    , compressMemoryLevel = Deflate.maxMemoryLevel }

-- | key-value пара, которую можно сохранять/загружать
class ( Resolvable a, Binary a, Show a
      , Binary (Key a), Ord (Key a), Eq a, TextKey (Key a) )
  => KV a where
    type Key a
    kvBucket  :: a -> Bucket
    kvKey     :: a -> Key a

    kvVersion :: a -> Int
    kvVersion _ = 0

    kvDecodeVersion :: Int -> BL.ByteString -> a
    kvDecodeVersion 0 = decode -- . Deflate.decompress . deflateCompress
    kvDecodeVersion n = error "migration is not yet implemented"

    kvCache :: a -> Cache a
    kvPool :: a -> R.Pool

class TextKey a where
    textKey :: a -> T.Text

instance TextKey String where
    textKey = T.pack
instance TextKey T.Text where
    textKey = id
instance TextKey B.ByteString where
    textKey = T.decodeUtf8With (\ _ -> fmap B.w2c)
instance TextKey BL.ByteString where
    textKey = textKey . B.concat . BL.toChunks
instance TextKey a => TextKey (Maybe a) where
    textKey Nothing = ""
    textKey (Just a) = textKey a
instance (TextKey a, TextKey b) => TextKey (a,b) where
    textKey (a,b) = escList [textKey a, textKey b]

textKeyList :: TextKey a => [a] -> T.Text
textKeyList = escList . map textKey

escList = T.unlines . map escape
    where escape s | isJust $ T.find ('\n' ==) s =
                       T.replace "\n" "\\n" s
                       -- T.pack $ esc $ T.unpack s
                   | otherwise = s
--           esc [] = []
--           esc ('\n':xs) = '\\':'n':esc xs
--           esc (x:xs) = x : esc xs


instance KV (String, String) where
    type Key (String, String) = String
    kvBucket _  = "testKeys"
    kvKey (k,_) = k
    kvCache k = ssCache
    kvPool _ = riakPool
ssCache :: Cache (String, String)
ssCache = unsafePerformIO $ newCache 2 3600 (1024*1024)
{-# NOINLINE ssCache #-}
instance Resolvable (String, String) where
    resolve a b = minimum [a,b]

instance KV a => KV [a] where
    type Key [a] = BL.ByteString
    kvBucket = kvBucket . head
    kvKey = key . kvKey . head
    kvCache = error "kvCache [a] ???"
    kvPool _ = riakPool
instance (KV a, Resolvable a) => Resolvable [a] where
    resolve a b = -- trace ("resolving " ++ show a ++ " and " ++ show b) $
                  unionByWith kvKey resolve a b

-- | Riak, оказывается, зачем-то escape-ит внутри ключи, отчего просто
-- ограничить длину ключа 255 и добавить SHA1 нельзя, надо считать,
-- сколько набралось escaped символов и получившуюся длину escaped-ключа
-- На LevelDB укорачивание ключа и хранение нескольких значений уже не нужно,
-- но для совместимости остается.
key :: TextKey a => a -> BL.ByteString
key k = check 0 0 0
    where ek = T.encodeUtf8 $ textKey k
          ekLen = B.length ek
          b64Len = 27
          -- длина base64 от SHA1 равна 28, но в конце '=',
          -- который Riak тоже escape-ит
          limitLen = 255-27
          ch i accLen si n = check (i+1) (accLen+n) $
              if accLen > limitLen then si else si+1
              -- длина escaped-ключа ограничена 255, но для вставки SHA1
              -- надо запоминать, когда ключ перевалил за 255-len(SHA1)
          check !i !accLen !si
              | accLen > 255 =
                  BL.fromChunks
                      [ B.take (si-1) ek
                      , B.take b64Len $
                        B.map fix $ Base64.encode $ SHA1.hash ek
                      ]
              | i == ekLen = BL.fromChunks [ek]
              | escaped (B.index ek i) = ch i accLen si 3
              | otherwise = ch i accLen si 1
          fix '+' = '_'
          fix '/' = '_'
          fix c   = c

escaped c = esc ! fromEnum c
    where esc = listArray (0,255) [chr i `notElem` ok | i <- [0..255]]
          ok = " !$'()*,-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"

testEscaping = fmap concat $ forM [0..255] $ \ c ->
    do (readKV (toEnum c : replicate (255 - 24 - 1) '1')
           :: IO (Maybe (String, String)))
       return [chr c]
     `E.catch` \ (e :: E.SomeException) -> return []

------------------------------------------------------------------------------
-- Кеш значений

data QueueItem
    = QueueItem
      { qiKey   :: !T.Text
      , qiSize  :: {-# UNPACK #-} !Int
--      , qiTime  :: !Int -- !UrTime
      }
    deriving Show

data CacheItem a
    = CacheItem
      { ciValue :: a
      , ciVClock :: !R.VClock
      , ciCheckTime :: !UrTime
      }
    deriving Show

data Cache_ a
    = Cache_
      { cQueue :: !(IntMap QueueItem) -- !(Queue QueueItem)
      , cMap   :: !(HM.HashMap T.Text (Maybe (CacheItem a), Bool, Int))
                  -- (item, recached, cQueue index)
      , cSize  :: !Int
      , cCheckTime :: !Int
      , cMaxTime :: !Int
      , cMaxSize :: !Int
      , cRecached :: !(HM.HashMap SB.ShortByteString Recache)
      }
    deriving Show

data Recache
    = Recache
      { recacheExpireTime :: {-# UNPACK #-} !UrTime
      , recacheA :: {-# UNPACK #-} !Int
      , recacheB :: {-# UNPACK #-} !Int
      }
    deriving Show

type Cache a = MVar (Cache_ a)

newCache :: Int -> Int -> Int -> IO (Cache a)
newCache cCheckTime cMaxTime cMaxSize =
    newMVar $ Cache_ {..}
    where cQueue = IntMap.empty
          cMap   = HM.empty
          cSize  = 0
          cRecached = HM.empty

ensureCache = ensureCache' True
ensureCache' flip t size !c
    | Just ((qt,qi), q') <- IntMap.minViewWithKey (cQueue c)
    , qt < urTimeNsec t = rm qi q'
      -- сначала всегда удаляем по времени
    | cMaxSize c - cSize c < size
    , Just (qi, q') <-
        (if flip then IntMap.minView else IntMap.maxView) (cQueue c) = rm qi q'
      -- а потом по размеру, причем с двух сторон
    | otherwise = c
    where rm qi q' =
              ensureCache' (not flip) t size $
                          c { cQueue = q'
                            , cMap = HM.delete (qiKey qi) (cMap c)
                            , cSize = cSize c - qiSize qi
                            }

removeFromCache :: KV a => a -> IO ()
removeFromCache x =
    modifyMVar_ (kvCache x) $ \ c -> do
       case HM.lookup k (cMap c) of
            Just (_, _, i) | Just qi <- IntMap.lookup i (cQueue c) ->
                return $ c { cQueue = IntMap.delete i $ cQueue c
                           , cMap = HM.delete k (cMap c)
                           , cSize = cSize c - qiSize qi
                           , cRecached = HM.delete (tsb k) (cRecached c)
                           }
            _ -> return c
    where k = textKey $ kvKey x

tsb = SB.toShort . T.encodeUtf8
{-# INLINE tsb #-}

lookupCache :: forall a . KV a => a -> [Key a] -> IO [Maybe (Maybe a)]
lookupCache x ks = do
    t <- getUrTime
    kcis0 <- modifyMVar (kvCache x) $ \ c -> do
        let c' = ensureCache t 0 c -- для подчистки старых значений
        return (c', [(k, HM.lookup (textKey k) (cMap c')) | k <- ks])

    let checkKci (k, Just (_, True, _)) = Nothing -- не проверяем recached
        checkKci (k, Just (Just ci, _, _))
            | ciCheckTime ci < t = Just (k, ci)
        checkKci _ = Nothing
        fixKci (k, Just (Just ci, _, _)) = (k, Just (Just ci))
        fixKci (k, Just (Nothing, _, _)) = (k, Just Nothing)
        fixKci (k, Nothing) = (k, Nothing)
        kcis :: [(Key a, Maybe (Maybe (CacheItem a)))]
        kcis = map fixKci kcis0
        checkKeys = [ (key k, Just $ ciVClock ci)
                    | Just (k, ci) <- map checkKci kcis0]
        ret = return . map (fmap (fmap ciValue) . snd)
    if null checkKeys then
        ret kcis
    else do
        gs <- withConnection x $ \ c ->
              vclockGetMany c (kvBucket x) checkKeys R.Default
--         logS $ "Checking:\n"
--             ++ unlines (map (\ ((k,_), r) ->
--                                  (if isJust r then "  * " else "  x ")
--                                  ++ BL.unpack k) $ zip checkKeys gs)
        let merge uacc acc kcis [] = (uacc, reverse acc ++ map fixKci kcis)
            merge uacc acc [] _ = error "lookupCache: can't happend"
            merge uacc acc (kci : kcis) (g:gs)
                | Just (k, ci) <- checkKci kci =
                    let ci' = case g of
                            Just (wkv, vc)
                                | Just l <- wKV wkv
                                , (Just x, _) <- findKey k l ->
                                    ci { ciValue = x, ciVClock = vc }
                            _ -> ci
                    in
                        merge ((k, Just (ciValue ci', ciVClock ci')) : uacc)
                              ((k, Just $ Just ci') : acc)
                              kcis gs
                | otherwise =
                    merge uacc (fixKci kci:acc) kcis (g:gs)
            (upd, kcis') = merge [] [] kcis0 gs
        updateCache (kvCache x) upd
        ret kcis'

insertCache :: KV a => Cache a -> [(Key a, Maybe (a, R.VClock), Int)] -> IO ()
insertCache c xs =
    modifyMVar_ c $ \ cOrig -> do
        t <- getUrTime
        let cnt what x = withEkgServer (return ()) $ \ s -> do
                g <- getGauge (T.decodeUtf8 $ BL.toStrict $
                               BL.append (cacheName c) what) s
                Gauge.set g x
            ins [] !c = do
                cnt "_size" (cSize c)
                cnt "_count" (HM.size $ cMap c)
                return c
            ins ((kvk,x,size):xs) !c = case HM.lookup k (cMap c) of
                Just (_,rc, i) -> ins xs $
                    c { cMap = HM.insert k (x', rc, i) (cMap c) }
                Nothing -> do
                    let (plus, rnd, recached) =
                            case HM.lookup (tsb k) (cRecached c) of
                                Just (Recache expireT a b) | expireT > t ->
                                    -- учитываем ранее перекешированные значения
                                    (a, (b-a)*10^9, True)
                                _ ->
                                    (cMaxTime c, 100000, False)
--                     logT $ T.concat [ "ins ", T.pack (show (plus, rnd))
--                                     , " ", textKey k ]
                    (i,q') <- insertInQueue rnd k size
                          (t `plusUrTime` plus) (cQueue c)
                    ins xs $
                        c { cQueue = q'
                          , cSize = cSize c + size
                          , cMap = HM.insert k (x', recached, i) (cMap c)
                          , cRecached =
                              if recached then cRecached c else
                                  HM.delete (tsb k) (cRecached c)
                          }
                where k = textKey kvk
                      x' = fmap (mkCacheItem c t) x
        ins xs $ ensureCache t (sum [s | (_,_,s) <- xs]) cOrig

-- | rnd -- диапазон в наносекундах
insertInQueue rnd k size t q = do
    t' <- findFreeKey 0
    return (t', IntMap.insert t' (QueueItem k size) q)
    where ns = urTimeNsec t
          findFreeKey add = do
              t' <- randomRIO (ns+add, ns+add+rnd)
              if not (IntMap.member t' q) then
                  return t'
              else
                  findFreeKey (add + 1000)

-- | Обновляет значение в кеше, но не трогает очередь/size,
-- т.к. не вижу простого и эффективного способа это сделать.
-- TODO: теперь очередь -- это IntMap, так что должно быть можно
-- К тому же меняется в основном инфа о прочитанности и пользователях,
-- вряд ли их размер будет сильно скакать.
updateCache :: KV a => Cache a -> [(Key a, Maybe (a, R.VClock))] -> IO ()
updateCache c [] = return ()
updateCache c xs = modifyMVar_ c $ \ c -> do
    t <- getUrTime
    let upd [] !c = c
        upd ((kvk, x):xs) !c = case HM.lookup k (cMap c) of
            Just (_,rc,i) ->
                upd xs $ c { cMap = HM.insert k
                                    (fmap (mkCacheItem c t) x, rc, i) (cMap c) }
            Nothing -> upd xs c -- не обновляем значение, если его нет в кеше
            where k = textKey kvk
    return $! upd xs c

cacheName :: KV a => Cache a -> BL.ByteString
cacheName c = cacheName' c undefined
    where cacheName' :: KV a => Cache a -> a -> BL.ByteString
          cacheName' _ kv = kvBucket kv

-- | Переместить значения дальше в кеше, со случайным диапазоном времени.
-- Дальнейшие cachedRead будут автоматически увеличивать время кеширования
-- заданного значения.
recacheKVs :: KV a => Cache a -> [Key a] -> Int -> Int -> IO ()
recacheKVs cache [] a b = return ()
recacheKVs cache keys a b = modifyMVar_ cache $ \ c -> do
--     logS $ show ("recacheKVs", cacheName cache, map textKey keys)
    t <- getUrTime
    let upd [] !c = return c
        upd ((textKey -> k):ks) !c = case HM.lookup k (cMap c) of
            Just (mbci, False, i) | Just qi <- IntMap.lookup i (cQueue c) -> do
                let q = IntMap.delete i $ cQueue c
                (i', q') <- insertInQueue ((b-a)*10^9) k (qiSize qi)
                            (t `plusUrTime` a) q
                upd ks $ c { cQueue = q'
                           , cMap = HM.insert k
                                    (fmap moveCheckTime mbci, True, i')
                                    (cMap c)
                           , cRecached =
                               HM.insert (tsb k)
                                   (Recache (t `plusUrTime` 36000) a b) $
                                   cRecached c
                           }
            _ ->
                upd ks c
                -- не обновляем значение, если его нет в кеше или уже recached
            where moveCheckTime ci =
                      ci { ciCheckTime = ciCheckTime ci `plusUrTime` b }

    upd keys c

mkCacheItem c t (x,vc) =
    CacheItem
    { ciValue = x, ciVClock = vc, ciCheckTime = t `plusUrTime` cCheckTime c }

------------------------------------------------------------------------------
-- Работа с Riak

-- | Для автоматического IsContent и подсчета длины данных
data WrapKV a
    = WrapKV
      { wKV :: Maybe a
        -- может быть и Nothing, т.к. riak может успешно прочесть удаленное
        -- значение, которое будет пустой строкой
      , wSize :: !Int
      , wData :: BL.ByteString
      }
    deriving Show

-- instance Functor WrapKV where
--     fmap f (WrapKV x) = WrapKV $ f x

wrapKV wKV_ = WrapKV {..}
    where wKV = Just wKV_
          wData = defragment $ encode (kvVersion wKV_, wKV_)
          wSize = fromEnum $ BL.length wData
wrapKV' Nothing = WrapKV Nothing 0 ""
wrapKV' (Just kv) = wrapKV kv

instance KV a => Resolvable (WrapKV a) where
    resolve a@(WrapKV _ _ _) (WrapKV Nothing _ _) = a
    resolve (WrapKV Nothing _ _) b@(WrapKV _ _ _) = b
    resolve (WrapKV (Just a) _ _) (WrapKV (Just b) _ _) =
--         trace ("RESOLVE " ++ BL.unpack (kvBucket a)) $
        wrapKV (resolve a b)
instance KV a => R.IsContent (WrapKV a) where
    toContent = R.binary . wData
    parseContent r = wKV `seq` return (WrapKV {..})
        where wData = -- defragment $
                      R.value r
              wSize = fromEnum $ BL.length wData
              wKV | wSize == 0 || R.deleted r == Just True = Nothing
                  | otherwise = Just $!
--                     trace ("chunks " ++ show (length $ BL.toChunks wData)) $
-- не больше 5 чанков, в основном один
--                     trace ("decoding " ++ show (kvBucket wKV)
--                            ++ " " ++ show wData ++ " " ++ show r) $
                    kvDecodeVersion (decode $ BL.take 8 wData) (BL.drop 8 wData)

instance Resolvable R.Content where
    resolve = error "unresolved R.Content?"

data WrapJSON
    = WrapJSON
      { wjV :: JSON.Value
      , wjContent :: R.Content
      }
    deriving Show

wrapJSON j = WrapJSON j (R.toContent j)

instance Resolvable WrapJSON where
    resolve a _ = a
instance R.IsContent WrapJSON where
    toContent = wjContent
    parseContent c = fmap (flip WrapJSON c) $ R.parseContent c

-- Riak-соединение (и, в будущем, очереди/кеши) должно быть в глобальной
-- переменной, т.к. дергание riak-а будет идти из ur/web (а не в какой-то
-- монаде). UrCalls.init будет инитить эту переменную.
-- Действия -- все без VClock (работают на merge/resolve).
--   readKV, mergeWriteKV (через modify + resolve), writeKV (без resolve),
--   readManyKVs (сообщения/списки постов/прочитанности),
--   writeManyKVs (тоже без resolve, для сообщений)
-- для каждого типа данных эти ф-ии будут продублированы с заменой KV на
-- название типа данных и будут доступны из urweb.

riakPool = unsafePerformIO $ R.create (R.defaultClient { R.host = "riak", R.port = "8081" }) 1 300 64
{-# NOINLINE riakPool #-}
riakPool2 = unsafePerformIO $ R.create (R.defaultClient { R.host = "riak2", R.port = "8081" }) 1 300 64
{-# NOINLINE riakPool2 #-}
riakPool3 = unsafePerformIO $ R.create (R.defaultClient { R.host = "riak3", R.port = "8081" }) 1 300 64
{-# NOINLINE riakPool3 #-}

withConnection k = R.withConnection (kvPool k)

initRiak :: Monad m => m ()
initRiak = riakPool `seq` riakPool2 `seq` return ()

test = withConnection ("" :: String, "" :: String) $ \ c -> do
    let x = ("asdf" :: String, "qwer" :: String)
        x2= ("asdf" :: String, "qwer2" :: String)
    put_ c (kvBucket x) (key x) Nothing (wrapKV x)
    Just (_, vc) <- get c (kvBucket x) (key x) :: IO (Maybe (WrapKV (String, String), R.VClock))
    print =<< (vclockGet c (kvBucket x) (key x) (Just vc) R.Default :: IO (Maybe (WrapKV (String, String), R.VClock)))
    put_ c (kvBucket x) (key x) Nothing (wrapKV x2)
    print =<< (vclockGet c (kvBucket x) (key x) (Just vc) R.Default :: IO (Maybe (WrapKV (String, String), R.VClock)))

sKey b k = T.decodeUtf8 $ B.concat $ BL.toChunks b ++ [k]

statRead :: KV a => Bucket -> Maybe (WrapKV a, R.VClock) -> IO ()
statRead b (Just (w, _)) = do
    incrStat (sKey b "_rc") 1
    incrStat (sKey b "_rs") (wSize w)
statRead b Nothing =
    incrStat (sKey b "_rcf") 1

statWrite b s = do
    incrStat (sKey b "_wc") 1
    incrStat (sKey b "_ws") s

-- checkEmpty b k w = print (b, k, w)
-- checkEmpty b k (Just (w,vc)) | wSize w == 0 =
--     print ("Empty value read", b, k, vc)
checkEmpty _ _ _ = return ()

get c b k = do
    r <- vclockGet c b k Nothing R.Default
    checkEmpty b k r
    statRead b r
    return r
getMany c b k = do
    rs <- vclockGetMany c b (map (,Nothing) k) R.Default
    mapM_ (\ (k,r) -> checkEmpty b k r >> statRead b r) (zip k rs)
    return rs

data RiakException
    = PutException BL.ByteString BL.ByteString E.SomeException
    | DeleteException BL.ByteString BL.ByteString E.SomeException
    | PutManyException BL.ByteString E.SomeException
    deriving (Show, Typeable)

instance E.Exception RiakException

tryRiak :: (E.SomeException -> RiakException) -> IO a -> IO a
tryRiak e act = go 0
    where go n = do
              r <- E.try act
              case r of
                  Left exn -> do
                      logS $ "Riak Exception: " ++ show (e exn)
                      incrStat "exceptions" 1
                      if n < 2 then do
                          r <- randomRIO (10000, 50000)
                          threadDelay r
                          go (n+1)
                      else
                          E.throwIO $ e exn
                  Right r ->
                      return r

put_ c b k vc w = do
--     logS $ show ("put_", b, k)
    statWrite b (wSize w)
    tryRiak (PutException b k) $ R.put_ c b k vc w R.Default R.Default
put c b k vc w = do
--     logS $ show ("put", b, k)
    statWrite b (wSize w)
    tryRiak (PutException b k) $ R.put c b k vc w R.Default R.Default
putMany_ c b ws = do
--     logS $ show ("putMany", b, length ws)
    mapM_ (\ (_,_,w) -> statWrite b (wSize w)) ws
    tryRiak (PutManyException b) $ R.putMany_ c b ws R.Default R.Default
-- putManyJSON_ c b ws = do
--     mapM_ (\ (_,_,w) ->
--                statWrite b (fromEnum $ BL.length $ R.value $ wjContent w)) ws
--     R.putMany_ c b ws R.Default R.Default
delete c b k w = do
    incrStat (sKey b "_dc") 1
    incrStat (sKey b "_ds") (wSize w)
    tryRiak (DeleteException b k) $ R.delete c b k R.Default

deleteKV :: KV a => a -> IO ()
deleteKV kv = do
    removeFromCache kv
    E.bracket (waitRead (kvBucket kv, k)) releaseRead $ \ _ -> withConnection kv $ \ c -> do
        a0 <- get c (kvBucket kv) k
        case a0 of
            Just (wkv0, vclock)
                | Just l <- wKV wkv0
                , (_, other) <- findKeyKv kv l ->
                    if null other then
                        delete c (kvBucket kv) k wkv0
                    else do
                        let !wkv = wrapKV other
                        put_ c (kvBucket kv) k (Just vclock) wkv
            _ ->
                return () -- нечего удалять
    where k = key $ kvKey kv

findKeyKv :: KV a => a -> [a] -> (Maybe a, [a])
findKeyKv kv l = findKey (kvKey kv) l

findKey :: KV a => Key a -> [a] -> (Maybe a, [a])
findKey k l = -- trace (show l) $
              go [] l
    where go acc [] = (Nothing, l)
          go acc (x:xs)
              | kvKey x == k = (Just x, reverse acc ++ xs)
              | otherwise    = go (x:acc) xs

readKV :: KV a => Key a -> IO (Maybe a)
readKV = readKV' undefined

-- cached modify нет, т.к. для modify все равно нужен vclock
-- Кеширует Nothing (не делает повторных чтений в случае отсутствия значения)
cachedNothingReadKV :: KV a => Key a -> IO (Maybe a)
cachedNothingReadKV = cachedNothingReadKV' undefined

cachedNothingReadKV' :: KV a => a -> Key a -> IO (Maybe a)
cachedNothingReadKV' x k = do
    cr <- lookupCache x [k]
    case cr of
        [Just x] -> return x
        _ -> do
            r <- withConnection x $ \ c -> get c (kvBucket x) (key k)
            fmap head $ unWrapInsCache x [(k,r)]

cachedReadKV :: KV a => Key a -> IO (Maybe a)
cachedReadKV = cachedReadKV' undefined

cachedReadKV' :: KV a => a -> Key a -> IO (Maybe a)
cachedReadKV' x k = do
    cr <- lookupCache x [k]
    case cr of
        [Just x@(Just _)] -> return x
        _ -> do
            r <- withConnection x $ \ c -> get c (kvBucket x) (key k)
            fmap head $ unWrapInsCache x [(k,r)]

unWrapInsCache :: KV a
               => a -> [(Key a, Maybe (WrapKV [a], R.VClock))] -> IO [Maybe a]
unWrapInsCache x ks = go ks [] []
    where go [] r c = do
            insertCache (kvCache x) c
            return $ reverse r
          go ((k, Just (w@(WrapKV (Just l) size _), vclock)) : ks) !r !c
            | Just x <- fst $ findKey k l =
                        go ks (Just x : r) ((k, Just (x, vclock),size) : c)
          go ((k, _) : ks) r c =
              go ks (Nothing : r) ((k, Nothing, T.length $ textKey k) : c)

readKV' :: KV a => a -> Key a -> IO (Maybe a)
readKV' x k = withConnection x $ \ c -> do
    r <- fmap (unWrapVClock . (k,)) $ get c (kvBucket x) (key k)
    updateCache (kvCache x) [(k,r)]
    return $ fmap fst r

unWrapVClock (k, (Just (WrapKV (Just l) _ _, vclock))) =
    fmap (,vclock) $ fst $ findKey k l
unWrapVClock _ = Nothing

modifyKV :: KV a => Key a -> (Maybe a -> IO (a,b)) -> IO b
modifyKV = modifyKV' undefined

modifyKV_ :: KV a => Key a -> (Maybe a -> IO a) -> IO ()
modifyKV_ k f = modifyKV k (fmap (,()) . f)

-- -- | Возвращает отрезольвенное значение
-- modifyKV_r :: (KV a, RiakMonad m) => Key a -> (Maybe a -> IO a) -> m a
-- modifyKV_r = modifyKV_r' undefined

-- | Храним набор всех модифицирующихся на данный момент значений,
-- дабы не менять одно и то же одновременно. Чтобы было меньше resolve-ов.
-- А то частенько в процессе сохранения одних комментариев блога успевают
-- выкачаться другие и Posts резольвятся.
modifyMVars :: MVar (HashMap (BL.ByteString, BL.ByteString) [MVar ()])
modifyMVars = unsafePerformIO $ newMVar HM.empty
{-# NOINLINE modifyMVars #-}

blockBucket "Posts" = True
blockBucket "BlogPostsScanned" = True
blockBucket "Comments" = True
blockBucket "ScanList" = True
blockBucket "PostsRead" = True
blockBucket "User" = True
blockBucket "Session" = True
blockBucket "UserStats" = True
blockBucket "MailQueue" = True
blockBucket "UserFilters" = True
blockBucket "GRIds" = True
blockBucket "MobileLogin" = True
blockBucket "PostsTagged" = True
blockBucket "API" = True
blockBucket "FeverIds" = True
blockBucket _ = False

waitRead key@(blockBucket -> True, _) = do
    join $ modifyMVar modifyMVars $ \ m -> case HM.lookup key m of
        Nothing ->
            return (HM.insert key [] m, return ())
        Just xs -> do
            wait <- newEmptyMVar
            return (HM.insert key (wait:xs) m, takeMVar wait)
    return key
waitRead k = return k

releaseRead key@(blockBucket -> True, _) =
    modifyMVar_ modifyMVars $ \m -> case HM.lookup key m of
        Nothing ->
            fail $ "releaseRead: not found ??? " ++ show key
        Just [] -> -- никто больше не ждет
            return $ HM.delete key m
        Just (w:ws) -> do
            putMVar w () -- освобождаем первого в очереди
            return $ HM.insert key ws m
releaseRead _ = return ()

blockBucketKey :: BL.ByteString -> BL.ByteString -> IO a -> IO a
blockBucketKey bucket key act =
    E.bracket (waitRead (bucket, key)) releaseRead $ \ _ -> act

modifyKV' :: KV a => a -> Key a -> (Maybe a -> IO (a,b)) -> IO b
modifyKV' x k f =
    E.bracket (waitRead (kvBucket x, key k)) releaseRead $ \ _ -> do
        a0 <- withConnection x $ \ c -> get c (kvBucket x) (key k)
        let kvs = wKV . fst =<< a0
            (ax, other) = case kvs of
                             Nothing -> (Nothing, [])
                             Just l -> findKey k l
        (a, r) <- f ax
        when (Just a /= ax) $ do
            --  ^ не уверен, что это хорошо, хотя уменьшает загрузку харда
            let !wkv = wrapKV (a : other)
--             liftM (fromMaybe (error $ "null wKV in modifyKV_ "
--                               ++ show (kvBucket x)
--                               ++ " " ++ show (encode k)) . wKV . fst)
            (wkw', vc') <- withConnection x $ \ c ->
--                withLogger $ \ l -> logTime l "put" $
                put c (kvBucket x) (key k) (fmap snd a0) wkv
            updateCache (kvCache x) [(k, fmap (, vc') (fst . findKey k =<< wKV wkw'))]
            -- не круто, что надо повторно вычитывать значение,
            -- за то так можно vclock получить
        return r


-- | Если значение уже есть, то делаем resolve.
mergeWriteKV :: KV a => a -> IO ()
mergeWriteKV x = modifyKV_ (kvKey x) (return . maybe x (resolve x))

readManyKVs :: KV a => [Key a] -> IO [Maybe a]
readManyKVs [] = return []
readManyKVs ks = readManyKVs' undefined ks

readManyKVs' :: KV a => a -> [Key a] -> IO [Maybe a]
readManyKVs' x ks = withConnection x $ \ c -> do
    r <- fmap (map unWrapVClock . zip ks) $ getMany c (kvBucket x) $ map key ks
    updateCache (kvCache x) (zip ks r)
    return $ map (fmap fst) r

cachedNothingReadManyKVs :: KV a => [Key a] -> IO [Maybe a]
cachedNothingReadManyKVs [] = return []
cachedNothingReadManyKVs ks = cachedNothingReadManyKVs' undefined ks

cachedNothingReadManyKVs' :: KV a => a -> [Key a] -> IO [Maybe a]
cachedNothingReadManyKVs' x ks = do
    cr <- lookupCache x ks
    let toread = map fst $ filter (isNothing . snd) $ zip ks cr
    if null toread then
        return $ map fromJust cr
    else withConnection x $ \ c -> do
        r <- getMany c (kvBucket x) $ map key toread
        rs <- unWrapInsCache x $ zip toread r
        let go [] rs = rs
            go (Just c : crs) rs = c : go crs rs
            go (Nothing : crs) (r:rs) = r : go crs rs
            go (Nothing : _) [] = error "go (Nothing : _) [] ?"
        return $ go cr rs

cachedReadManyKVs :: KV a => [Key a] -> IO [Maybe a]
cachedReadManyKVs = cachedReadManyKVs' undefined

cachedReadManyKVs' :: KV a => a -> [Key a] -> IO [Maybe a]
cachedReadManyKVs' x ks = do
    cr <- lookupCache x ks
    let toread = map fst $ filter (nn . snd) $ zip ks cr
        nn (Just (Just _)) = False
        nn _ = True

    if null toread then
        return $ map fromJust cr
    else withConnection x $ \ c -> do
        r <- getMany c (kvBucket x) $ map key toread
        rs <- unWrapInsCache x $ zip toread r
        let go [] rs = rs
            go (Just c@(Just _) : crs) rs = c : go crs rs
            go (_ : crs) (r:rs) = r : go crs rs
            go (_ : _) [] = error "go (_ : _) []"
        return $ go cr rs

-- | Не перезаписывает значение, если с таким ключом уже есть.
-- Это по документации хаскельного пакета riak.
-- На деле, занимается resolving-ом (возможно, из-за last_write_wins = false)
writeManyKVs :: KV a => [a] -> IO ()
writeManyKVs xs = withConnection (head xs) $ \ c ->
--     fmap (map $ fromMaybe (error "writeManyKVs null?")
--                   . wKV . fst) $
    putMany_ c (kvBucket $ head xs)
        [(key $ kvKey x, Nothing, wrapKV [x]) | x <- xs]
        -- есть проблема, что у нас может для двух сообщений появиться
        -- одинаковый ключ. С другой стороны, вероятность этого мала.
        -- Плюс, глюк будет неприятный (левое сообщение),
        -- но не фатальный (перепутавшиеся url-ы), т.к. используется
        -- только для сообщений

-- В секунду (все в кеше):
--   1k readKV (один ключ)
--   250-300 writeKV/mergeWriteKV/readKV(разные ключи)/
--           readManyKVs(не сильно быстрее отдельных KV)
--   изменение R.Default на One/All почти ничего не меняет
--   У Льва Валкина в сравнении riak/voldemort были цифры в 10 раз больше,
--   но там было 10 ниток (и 30 ниток, но без особых изменений)

-- Вроде как при put без VClock ничего не должно сохраняться
-- (сейчас сохраняется видимо из-за флага "кто последний, тот и папа"),
-- Так что write-ы будут через modify.
-- А вот writeMany (который для сообщений), как раз можно и без modify,
-- т.к. там, если сообщение есть, то ничего трогать не надо
-- (хотя тоже можно обновлять, проверяя через getMany и хеш, не поменялось ли
--  чего, хотя хеш будет в дереве, так что можно отдельно modify для обновления
--  и отдельно putMany для новых сообщений).


-- грязно натыренный из сорцов riak код

vclockGet :: (R.IsContent a, R.Resolvable a) =>
          R.Connection -> R.Bucket -> R.Key -> Maybe R.VClock -> R.R -> IO (Maybe (a, R.VClock))
vclockGet conn bucket key vclock r = do
--     when (bucket /= "Msg") $ logS $ show ("vclockGet", bucket, key, vclock)
    (\ r -> return . first resolveMany =<< getResp =<< r) `fmap`
        R.exchangeMaybe conn (getReq bucket key vclock r)
{-# INLINE vclockGet #-}

vclockGetMany :: (R.IsContent a, R.Resolvable a) => R.Connection -> R.Bucket -> [(R.Key, Maybe R.VClock)] -> R.R
        -> IO [Maybe (a, R.VClock)]
vclockGetMany conn b ks r = do
--     logS $ show ("vclockGetMany", b, length ks, ks)
    R.pipe (\ c -> do
              r <- R.recvMaybeResponse c
              case return . first resolveMany =<< getResp =<< r of
                  Just (!v,!vc) -> return $ Just (v,vc)
                  Nothing -> return Nothing
           ) conn (map (\(k,c) -> getReq b k c r) ks)

getReq :: R.Bucket -> R.Key -> Maybe R.VClock -> R.R -> GetRequest.GetRequest
getReq bucket key vclock r =
    GetRequest.GetRequest
    { GetRequest.bucket = R.escape bucket
    , GetRequest.key = R.escape key
    , GetRequest.r = fromQuorum r
    , GetRequest.pr = Nothing
    , GetRequest.basic_quorum = Nothing
    , GetRequest.notfound_ok = Nothing
    , GetRequest.if_modified = fmap R.fromVClock vclock
    , GetRequest.head        = Nothing
    , GetRequest.deletedvclock = Nothing
    , GetRequest.timeout = Nothing
    }

resolveMany' :: (Resolvable a) => a -> [a] -> a
resolveMany' = foldl' resolve
{-# INLINE resolveMany' #-}

resolveMany :: (Resolvable a) => [a] -> a
resolveMany (a:as) = resolveMany' a as
resolveMany _      = error "resolveMany: empty list"
{-# INLINE resolveMany #-}

-- из Network.Riak.Types.Internal
fromQuorum :: R.Quorum -> Maybe Word32
fromQuorum R.Default = Just 4294967291
fromQuorum R.One     = Just 4294967294
fromQuorum R.Quorum  = Just 4294967293
fromQuorum R.All     = Just 4294967292
{-# INLINE fromQuorum #-}

-- из Network.Riak.Value

getResp :: (R.IsContent c) => GetResponse -> Maybe ([c], R.VClock)
getResp (GetResponse content (Just s) _) = Just (convert content, R.VClock s)
getResp _ = Nothing

convert :: R.IsContent v => Seq.Seq R.Content -> [v]
convert = go [] [] . toList
    where go cs vs (x:xs) = case R.fromContent y of
                              Just v -> go cs (v:vs) xs
                              _      -> go (y:cs) vs xs
              where y = R.unescapeLinks x
          go [] vs _      = reverse vs
          go cs _  _      = error $ "Network.Riak.Value.convert: " ++
                            show (length cs) ++ " values failed conversion: " ++
                            show cs
-- typeError :: String -> String -> String -> a
-- typeError modu func msg = E.throw (R.TypeException modu func msg)

forkRead act = do
    rv <- newEmptyMVar
    forkIO $ do
        r <- fmap Right act `E.catch` \ (e :: E.SomeException) ->
             return (Left e)
        putMVar rv r
    return $ do
        r <- takeMVar rv
        either E.throwIO return r

forkReadPar2 f list = do
    -- еще где-то 15% срезает времени обновления списка подписок
    r1 <- forkRead $ f l1
    r2 <- forkRead $ f l2
    return $ liftM2 (++) r1 r2
    where (l1, l2) = splitAt (length list `div` 2) list

testDeflate = do
    rnd <- fmap (encode . B.pack) $ replicateM 30000 randomIO
    replicateM_ 100000 $ do
        rnd' <- fmap BL.pack $ replicateM 1 randomIO
        (B.length $ decode $ GZip.decompress $ GZip.compress $ BL.concat [rnd, rnd'])
            `seq` return ()
    performGC