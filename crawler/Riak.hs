{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns,
             TypeFamilies, FlexibleContexts, FlexibleInstances,
             BangPatterns, TupleSections, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- | Работа с Riak
module Riak
    ( KV(..), Resolvable(..), TextKey(..), textKeyList, key, Bucket
    , newCache, cachedReadKV, cachedReadManyKVs
    , cachedNothingReadKV, cachedNothingReadManyKVs
    , recacheKVs, clearRecaches
    , readKV, mergeWriteKV, readManyKVs, writeManyKVs
    , modifyKV, modifyKV_, alterKV
    , deleteKV
    , riakPool
    , blockBucketKey, waitBucketKey, releaseBucketKey
    , forkRead, forkReadPar2
    , riakBucketKeys, riakBucketKeys', getAllUsers
    ) where

import Control.Arrow (first)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.DeepSeq
import Data.Array
import Data.Binary hiding (get, put)
import Data.Bits
import Data.List (sort)
import Data.Char
import Data.Either
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import Data.IntMap (IntMap)
import Data.List (foldl')
import Data.Maybe
import Data.Typeable
import qualified Data.ByteString.Base64 as Base64
import Lib.BinaryInstances ()
import Lib.DnsCache (resolveHostEntry, showHostAddress)
import Lib.Hash
import Lib.Log (logS)
import Lib.Merge
import Lib.Stats
import Lib.StringConversion
import Lib.UrTime
import Network.Riak.Protocol.GetResponse (GetResponse(..))
import Network.Riak.Value.Resolvable (Resolvable(..))
import System.IO.Unsafe
import System.Random
import qualified Codec.RawZlib as RawZlib
import qualified Control.Exception as E
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Short as SB
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Riak as R (delete)
import qualified Network.Riak.Connection as R
import qualified Network.Riak.Connection.Pool as R
import qualified Network.Riak.Content as R
import qualified Network.Riak.Escape as R
import qualified Network.Riak.Protocol.GetRequest as GetRequest
import qualified Network.Riak.Response as R (unescapeLinks)
import qualified Network.Riak.Types as R
import qualified Network.Riak.Value as R (fromContent)
import qualified Network.Riak.Value.Resolvable as R
import qualified System.Remote.Gauge as Gauge
import qualified Data.Aeson as JSON
import Lib.Json
import qualified Data.Vector as V
import System.Timeout
import Network.HTTP.Conduit.Downloader
import URL

type Bucket = BL.ByteString

-- | key-value пара, которую можно сохранять/загружать
class ( Resolvable a, Binary a, Show a
      , Binary (Key a), Ord (Key a), Eq a, TextKey (Key a) )
  => KV a where
    type Key a
    kvBucket  :: a -> Bucket
    kvKey     :: a -> Key a

    kvVersion :: a -> Int
    kvVersion _ = 0

    kvCache :: a -> Cache a

kvDecodeVersion :: KV a => Int -> BL.ByteString -> (Int, a)
kvDecodeVersion v s
    | testBit v deflateBit =
        let d = RawZlib.decompress $ mkOneChunk s
        in
            (fromEnum $ BL.length d, decode d)
    | v == 0 = (fromEnum $ BL.length s, decode s)
    | otherwise = error "migration is not yet implemented"

mkOneChunk :: BL.ByteString -> BL.ByteString
mkOneChunk = BL.fromStrict . BL.toStrict

deflateBit = 63

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
                       -- а если в тексте есть \\n ?
                   | otherwise = s


data DebugKV = DebugKV { dkvBucket, dkvKey, _dkvDump :: B.ByteString }
    deriving (Show, Eq, Ord)
instance Binary DebugKV where
    get = do
--         (_::Int) <- Binary.get
--         (keySize::Int) <- Binary.get
--        key <- Binary.get
        r <- Binary.getRemainingLazyByteString
        return $ DebugKV "" "" (BL.toStrict r)
    put _ = error "Don’t put DebugKV into Riak!"

instance KV DebugKV where
    type Key DebugKV = B.ByteString
    kvBucket = BL.fromStrict . dkvBucket
    kvKey = dkvKey
    kvCache _ = dkvCache
instance NFData DebugKV where
    rnf (DebugKV a b c) = rnf a `seq` rnf b `seq` rnf c `seq` ()
instance Resolvable DebugKV where
    resolve a b = minimum [a,b]
dkvCache :: Cache DebugKV
dkvCache = unsafePerformIO $ newCache 0 0 0
{-# NOINLINE dkvCache #-}

instance KV (String, String) where
    type Key (String, String) = String
    kvBucket _  = "testKeys"
    kvKey (k,_) = k
    kvCache _ = ssCache
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
                      , B.take b64Len $ B.map fix $ base64_sha1 ek
                      ]
              | i == ekLen = BL.fromChunks [ek]
              | escaped (B.index ek i) = ch i accLen si 3
              | otherwise = ch i accLen si 1
          fix '+' = '_'
          fix '/' = '_'
          fix c   = c

escaped c = esc ! fromEnum c
    where esc = listArray (0,255) [chr i `notElem` ok | i <- [0..255]]
          ok = " !$'()*,-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz" :: [Char]

------------------------------------------------------------------------------
-- Кеш значений

data QueueItem
    = QueueItem
      { qiKey   :: !SB.ShortByteString
      , qiSize  :: {-# UNPACK #-} !Int
      }

data CacheItem a
    = CacheItem
      { ciValue :: !a
      , ciVClock :: !SB.ShortByteString -- !R.VClock
      , ciCheckTime :: !UrTime
      }

blVClock = R.VClock . BL.fromStrict . SB.fromShort
sbVClock (R.VClock c) = SB.toShort $ BL.toStrict c

changeValue :: KV a => Maybe (a -> a) -> Maybe (CacheItem a) -> Maybe (CacheItem a)
changeValue (Just f) (Just c) = do
    Just $ c { ciValue = f $ ciValue c }
changeValue _ c = c

data Cache_ a
    = Cache_
      { cQueue :: !(IntMap QueueItem) -- !(Queue QueueItem)
      , cMap   :: !(HM.HashMap SB.ShortByteString (Maybe (CacheItem a), Bool, Int))
                  -- (item, recached, cQueue index)
      , cSize  :: !Int
      , cCheckTime :: !Int
      , cMaxTime :: !Int
      , cMaxSize :: !Int
      , cRecached :: !(HM.HashMap SB.ShortByteString (Recache a))
      }

data Recache a
    = Recache
      { _recacheExpireTime :: {-# UNPACK #-} !UrTime
      , _recacheA :: {-# UNPACK #-} !Int
      , _recacheB :: {-# UNPACK #-} !Int
      , _recacheFunc :: !(Maybe (a -> a))
      }

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
    , qt < fromEnum (urTimeNsec t) = rm qi q'
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
                           , cRecached = HM.delete k (cRecached c)
                           }
            _ -> return c
    where k = tsb $ textKey $ kvKey x

lookupCache :: forall a . KV a => a -> [Key a] -> IO [Maybe (Maybe a)]
lookupCache x ks = do
    t <- getUrTime
    kcis0 <- modifyMVar (kvCache x) $ \ c -> do
        let c' = ensureCache t 0 c -- для подчистки старых значений
        return (c', [(k, HM.lookup (tsb $ textKey k) (cMap c')) | k <- ks])

    let checkKci (_, Just (_, True, _)) = Nothing -- не проверяем recached
        checkKci (k, Just (Just ci, _, _))
            | ciCheckTime ci < t = Just (k, ci)
        checkKci _ = Nothing
        fixKci (k, Just (Just ci, _, _)) = (k, Just (Just ci))
        fixKci (k, Just (Nothing, _, _)) = (k, Just Nothing)
        fixKci (k, Nothing) = (k, Nothing)
        kcis :: [(Key a, Maybe (Maybe (CacheItem a)))]
        kcis = map fixKci kcis0
        checkKeys = [ (key k, Just $ blVClock $ ciVClock ci)
                    | Just (k, ci) <- map checkKci kcis0]
        ret = return . map (fmap (fmap ciValue) . snd)
    if null checkKeys then
        ret kcis
    else do
        gs <- vclockGetMany (kvBucket x) checkKeys R.Default
--         logS $ "Checking:\n"
--             ++ unlines (map (\ ((k,_), r) ->
--                                  (if isJust r then "  * " else "  x ")
--                                  ++ BL.unpack k) $ zip checkKeys gs)
        let merge uacc acc kcis [] = return (uacc, reverse acc ++ map fixKci kcis)
            merge _    _   [] _ = fail "lookupCache: can’t happend"
            merge uacc acc (kci : kcis) (g:gs)
                | Just (k, ci) <- checkKci kci = do
                    (x,ci') <- case g of
                         Just (wkv, vc)
                             | Just l <- wKV wkv
                             , (Just x, _) <- findKey k l ->
                                 return (x, cacheItem 0 (ciCheckTime ci) (x,vc))
                         _ ->
                             return (ciValue ci, ci)
                    merge ((k, Just (x, blVClock $ ciVClock ci')) : uacc)
                          ((k, Just $ Just ci') : acc)
                          kcis gs
                | otherwise =
                    merge uacc (fixKci kci:acc) kcis (g:gs)
        (upd, kcis') <- merge [] [] kcis0 gs
        updateCache (kvCache x) upd
        ret kcis'

insertCache :: KV a => Cache a -> [(Key a, Maybe (a, R.VClock), Int)] -> IO ()
insertCache c xs =
    modifyMVar_ c $ \ cOrig -> do
        t <- getUrTime
        let cnt what x = withEkgServer (return ()) $ do
                g <- getGauge (T.decodeUtf8 $ BL.toStrict $
                               BL.concat ["riak.cache.", cacheName c, what])
                Gauge.set g (toEnum x)
            ins [] !c = do
                cnt ".size" (cSize c)
                cnt ".count" (HM.size $ cMap c)
                return c
            ins ((kvk,x,size):xs) !c = do
              let x' = cacheItem (cCheckTime c) t <$> x
              case HM.lookup k (cMap c) of
                Just (_,rc, i) -> ins xs $
                    c { cMap = HM.insert k (x', rc, i) (cMap c) }
                Nothing -> do
                    let (plus, rnd, recached, f) =
                            case HM.lookup k (cRecached c) of
                                Just (Recache expireT a b f) | expireT > t ->
                                    -- учитываем ранее перекешированные значения
                                    (a, (b-a)*10^9, True, f)
                                _ ->
                                    (cMaxTime c, 100000, False, Nothing)
--                     logT $ T.concat [ "ins ", T.pack (show (plus, rnd))
--                                     , " ", textKey k ]
                    (i,q') <- insertInQueue rnd k size
                          (t `plusUrTime` plus) (cQueue c)
                    ins xs $
                        c { cQueue = q'
                          , cSize = cSize c + size
                          , cMap = HM.insert k
                              (changeValue f x', recached, i) (cMap c)
                          , cRecached =
                              if recached then cRecached c else
                                  HM.delete k (cRecached c)
                          }
                where k = tsb $ textKey kvk
        ins xs $ ensureCache t (sum [s | (_,_,s) <- xs]) cOrig

-- | rnd -- диапазон в наносекундах
insertInQueue rnd k size t q = do
    t' <- findFreeKey 0
    return (t', IntMap.insert t' (QueueItem k size) q)
    where ns = fromEnum $ urTimeNsec t
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
updateCache _ [] = return ()
updateCache c xs = modifyMVar_ c $ \ c -> do
    t <- getUrTime
    let upd [] !c = return $ c
        upd ((kvk, x):xs) !c = case HM.lookup k (cMap c) of
            Just (_,rc,i) ->
                upd xs $ c { cMap = HM.insert k
                   (cacheItem (cCheckTime c) t <$> x, rc, i)
                   (cMap c) }
            _ -> upd xs c -- не обновляем значение, если его нет в кеше
            where k = tsb $ textKey kvk
    upd xs c

cacheName :: KV a => Cache a -> BL.ByteString
cacheName c = cacheName' c undefined
    where cacheName' :: KV a => Cache a -> a -> BL.ByteString
          cacheName' _ kv = kvBucket kv

clearRecaches :: KV a => Cache a -> [Key a] -> IO ()
clearRecaches _     [] = return ()
clearRecaches cache keys = modifyMVar_ cache $ \ c ->
    return $ c { cRecached = foldl' (flip HM.delete) (cRecached c) $
                             map (tsb . textKey) keys }

-- | Переместить значения дальше в кеше, со случайным диапазоном времени.
-- Дальнейшие cachedRead будут автоматически увеличивать время кеширования
-- заданного значения.
recacheKVs :: KV a => Cache a -> Maybe (a -> a) -> [Key a] -> Int -> Int -> IO ()
recacheKVs _     _ []   _ _ = return ()
recacheKVs cache f keys a b = modifyMVar_ cache $ \ c -> do
--     logS $ show ("recacheKVs", cacheName cache, map textKey keys)
    t <- getUrTime
    let upd [] !c = return c
        upd ((tsb . textKey -> k):ks) !c = case HM.lookup k (cMap c) of
            Just (mbci, False, i) | Just qi <- IntMap.lookup i (cQueue c) -> do
                let q = IntMap.delete i $ cQueue c
                (i', q') <- insertInQueue ((b-a)*10^9) k (qiSize qi)
                            (t `plusUrTime` a) q
                upd ks $ c { cQueue = q'
                           , cMap = HM.insert k
                               (moveCheckTime <$> changeValue f mbci, True, i')
                               (cMap c)
                           , cRecached =
                               HM.insert k
                                   (Recache (t `plusUrTime` 600) a b f) $
                                   cRecached c
                           }
            _ ->
                upd ks c
                -- не обновляем значение, если его нет в кеше или уже recached
            where moveCheckTime ci =
                      ci { ciCheckTime = ciCheckTime ci `plusUrTime` b }

    upd keys c

cacheItem ct t (x,vc) =
    CacheItem { ciValue = x, ciVClock = sbVClock vc, ciCheckTime = t `plusUrTime` ct }

------------------------------------------------------------------------------
-- Работа с Riak

-- | Для автоматического IsContent и подсчета длины данных
data WrapKV a
    = WrapKV
      { wKV :: Maybe a
        -- может быть и Nothing, т.к. riak может успешно прочесть удаленное
        -- значение, которое будет пустой строкой
      , wSize :: !Int
      , wCompressedSize :: !Int
      , wData :: BL.ByteString
      }
    deriving Show

-- instance Functor WrapKV where
--     fmap f (WrapKV x) = WrapKV $ f x

wrapKV :: KV a => a -> WrapKV a
wrapKV wKV_ = WrapKV {..}
    where wKV = Just wKV_
          wData0 = encode wKV_
          (mkOneChunk -> wData)
              | BL.length wData0 < 5000 =
                  BL.append (encode v) wData0
              | otherwise =
                  let c = RawZlib.compress (mkOneChunk wData0) in
                  -- deflateCompress выдает блоки по 16kB
--                  trace ("compressed " ++ show (BL.length c * 100 `div` BL.length wData0) ++ "% (of " ++ show (BL.length wData0) ++ ") " ++ show (kvBucket wKV_, textKey (kvKey wKV_))) $
                  BL.append (encode $ setBit v deflateBit) c
          v = kvVersion wKV_
          wSize = fromEnum $ BL.length wData0
          wCompressedSize = fromEnum $ BL.length wData

instance KV a => Resolvable (WrapKV a) where
    resolve a@(WrapKV {}) (WrapKV Nothing _ _ _) = a
    resolve (WrapKV Nothing _ _ _) b@(WrapKV {}) = b
    resolve (WrapKV (Just a) _ _ _) (WrapKV (Just b) _ _ _) =
--         trace ("RESOLVE " ++ BL.unpack (kvBucket a)) $
        wrapKV (resolve a b)
instance KV a => R.IsContent (WrapKV a) where
    toContent = R.binary . wData
    parseContent r = return (WrapKV {..})
        where wData = -- mkOneChunk $
                      R.value r
              wCompressedSize = fromEnum $ BL.length wData
              (wSize, wKV)
                  | wCompressedSize == 0 || R.deleted r == Just True =
                      (0, Nothing)
                  | otherwise =
                      let (s, v) = kvDecodeVersion (decode $ BL.take 8 wData) (BL.drop 8 wData) in
                      (s, Just v)
--                     trace ("chunks " ++ show (length $ BL.toChunks wData)) $
-- не больше 5 чанков, в основном один
--                     trace ("decoding " ++ show (kvBucket wKV)
--                            ++ " " ++ show wData ++ " " ++ show r) $


instance Resolvable R.Content where
    resolve = error "unresolved R.Content?"

-- data WrapJSON
--     = WrapJSON
--       { wjV :: JSON.Value
--       , wjContent :: R.Content
--       }
--     deriving Show

-- wrapJSON j = WrapJSON j (R.toContent j)

-- instance Resolvable WrapJSON where
--     resolve a _ = a
-- instance R.IsContent WrapJSON where
--     toContent = wjContent
--     parseContent c = fmap (flip WrapJSON c) $ R.parseContent c

-- Riak-соединение (и, в будущем, очереди/кеши) должно быть в глобальной
-- переменной, т.к. дергание riak-а будет идти из ur/web (а не в какой-то
-- монаде). UrCalls.init будет инитить эту переменную.
-- Действия -- все без VClock (работают на merge/resolve).
--   readKV, mergeWriteKV (через modify + resolve), writeKV (без resolve),
--   readManyKVs (сообщения/списки постов/прочитанности),
--   writeManyKVs (тоже без resolve, для сообщений)
-- для каждого типа данных эти ф-ии будут продублированы с заменой KV на
-- название типа данных и будут доступны из urweb.

riakPool = unsafePerformIO $ do
    ha <- resolveHostEntry "riak"
    R.create (R.defaultClient { R.host = showHostAddress ha, R.port = "8081" })
             1 300 64
{-# NOINLINE riakPool #-}

_test = do
    let x = ("asdf" :: String, "qwer" :: String)
        x2= ("asdf" :: String, "qwer2" :: String)
    put_ (kvBucket x) (key x) Nothing (wrapKV x)
    Just (_, vc) <- get (kvBucket x) (key x) :: IO (Maybe (WrapKV (String, String), R.VClock))
    print =<< (vclockGet (kvBucket x) (key x) (Just vc) R.Default :: IO (Maybe (WrapKV (String, String), R.VClock)))
    put_ (kvBucket x) (key x) Nothing (wrapKV x2)
    print =<< (vclockGet (kvBucket x) (key x) (Just vc) R.Default :: IO (Maybe (WrapKV (String, String), R.VClock)))

sKey b k = T.decodeUtf8 $ B.concat $ ["riak.io."] ++ BL.toChunks b ++ [k]

statRead :: KV a => Bucket -> Maybe (WrapKV a, R.VClock) -> IO ()
statRead b (Just (w, _)) = do
    incrStat (sKey b ".rc") 1
    incrStat (sKey b ".rs") (wCompressedSize w)
statRead b Nothing =
    incrStat (sKey b ".rcf") 1

statWrite b s = do
    incrStat (sKey b ".wc") 1
    incrStat (sKey b ".ws") s

-- checkEmpty b k w = print (b, k, w)
-- checkEmpty b k (Just (w,vc)) | wCompressedSize w == 0 =
--     print ("Empty value read", b, k, vc)
checkEmpty _ _ _ = return ()

get b k = do
    r <- vclockGet b k Nothing R.Default
    checkEmpty b k r
    statRead b r
    return r
getMany b k = do
    rs <- vclockGetMany b (map (,Nothing) k) R.Default
    mapM_ (\ (k,r) -> checkEmpty b k r >> statRead b r) (zip k rs)
    return rs

data RiakException
    = PutException BL.ByteString BL.ByteString E.SomeException
    | PutManyException BL.ByteString E.SomeException
    | DeleteException BL.ByteString BL.ByteString E.SomeException
    | GetException BL.ByteString BL.ByteString E.SomeException
    | GetManyException BL.ByteString [BL.ByteString] E.SomeException
    deriving (Show, Typeable)

instance E.Exception RiakException

tryRiak e act = tryRiak' 3 e act

tryRiak' :: Int -> (E.SomeException -> RiakException)
    -> (R.Connection -> IO a) -> IO a
tryRiak' tries e act = go 1
    where go n = do
              r <- E.try $ R.withConnection riakPool act >>= E.evaluate
              case r of
                  Left exn -> do
                      logS $ "Riak Exception: " ++ show (e exn)
                      incrStat "exceptions" 1
                      if n < tries then do
                          r <- randomRIO (10000, 50000)
                          threadDelay r
                          go (n+1)
                      else
                          E.throwIO $ e exn
                  Right r ->
                      return r

put_ b k vc w = do
--     logS $ show ("put_", b, k)
    statWrite b (wCompressedSize w)
    tryRiak (PutException b k) $ \ c -> R.put_ c b k vc w R.Default R.Default
put b k vc w = do
--     logS $ show ("put", b, k)
    statWrite b (wCompressedSize w)
    tryRiak (PutException b k) $ \ c -> R.put c b k vc w R.Default R.Default
putMany_ b ws = do
--     logS $ show ("putMany", b, length ws)
    mapM_ (\ (_,_,w) -> statWrite b (wCompressedSize w)) ws
    tryRiak (PutManyException b) $ \ c -> R.putMany_ c b ws R.Default R.Default
-- putManyJSON_ c b ws = do
--     mapM_ (\ (_,_,w) ->
--                statWrite b (fromEnum $ BL.length $ R.value $ wjContent w)) ws
--     R.putMany_ c b ws R.Default R.Default
delete b k w = do
    incrStat (sKey b ".dc") 1
    incrStat (sKey b ".ds") (wCompressedSize w)
    tryRiak (DeleteException b k) $ \ c -> R.delete c b k R.Default

deleteKV :: KV a => a -> IO ()
deleteKV kv = do
    removeFromCache kv
    blockBucketKey (kvBucket kv) k $ do
        a0 <- get (kvBucket kv) k
        deleteKV' kv k a0
    where k = key $ kvKey kv

deleteKV' :: KV a => a -> BL.ByteString -> Maybe (WrapKV [a], R.VClock) -> IO ()
deleteKV' kv k a0 = do
    case a0 of
        Just (wkv0, vclock)
            | Just l <- wKV wkv0
            , (_, other) <- findKey (kvKey kv) l ->
                if null other then
                    delete (kvBucket kv) k wkv0
                else do
                    let !wkv = wrapKV other
                    put_ (kvBucket kv) k (Just vclock) wkv
        _ ->
            return () -- нечего удалять

findKey :: KV a => Key a -> [a] -> (Maybe a, [a])
findKey k l = -- trace (show l) $
              go [] l
    where go _   [] = (Nothing, l)
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
            r <- get (kvBucket x) (key k)
            fmap head $ unWrapInsCache x [(k,r)]

cachedReadKV :: KV a => Key a -> IO (Maybe a)
cachedReadKV = cachedReadKV' undefined

cachedReadKV' :: KV a => a -> Key a -> IO (Maybe a)
cachedReadKV' x k = do
    cr <- lookupCache x [k]
    case cr of
        [Just x@(Just _)] -> return x
        _ -> do
            r <- get (kvBucket x) (key k)
            fmap head $ unWrapInsCache x [(k,r)]

unWrapInsCache :: KV a
               => a -> [(Key a, Maybe (WrapKV [a], R.VClock))] -> IO [Maybe a]
unWrapInsCache x ks = go ks [] []
    where go [] r c = do
            insertCache (kvCache x) c
            return $ reverse r
          go ((k, Just (WrapKV (Just l) size _ _, vclock)) : ks) !r !c
            | Just x <- fst $ findKey k l =
                        go ks (Just x : r) ((k, Just (x, vclock),size) : c)
          go ((k, _) : ks) r c =
              go ks (Nothing : r) ((k, Nothing, T.length $ textKey k) : c)

readKV' :: KV a => a -> Key a -> IO (Maybe a)
readKV' x k = do
    r <- fmap (unWrapVClock . (k,)) $ get (kvBucket x) (key k)
    updateCache (kvCache x) [(k,r)]
    return $ fmap fst r

unWrapVClock (k, (Just (WrapKV (Just l) _ _ _, vclock))) =
    fmap (,vclock) $ fst $ findKey k l
unWrapVClock _ = Nothing

alterKV :: KV a => Key a -> (Maybe a -> IO (Maybe a,b)) -> IO b
alterKV = alterKV' undefined

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

blockedBuckets = HS.fromList
    [ "Posts"
    , "BlogPostsScanned"
    , "Comments"
    , "ScanList"
    , "PostsRead"
    , "User"
    , "Session"
    , "UserStats"
    , "MailQueue"
    , "UserFilters"
    , "Filters"
    , "GRIds"
    , "PostsTagged"
    , "API"
    , "FeverIds"
    , "PageInfo"
    , "userHotLinks"
    , "blockableUrlToScan"
    , "subscriptionsAndViewMode"
    ]
blockBucket b = HS.member b blockedBuckets

-- | Использовать с осторожностью, не забудьте вызывать releaseBucketKey
waitBucketKey key@(blockBucket -> True, _) = E.mask_ $ do
    join $ modifyMVar modifyMVars $ \ m -> case HM.lookup key m of
        Nothing ->
            return (HM.insert key [] m, return ())
        Just xs -> do
            wait <- newEmptyMVar
            let w = takeMVar wait `E.onException` modifyMVar_ modifyMVars rm
                rm m = case HM.lookup key m of
                    Nothing ->
                        fail $ "waitBucketKey: not found ??? " ++ show key
                    Just [] -> do -- никто больше не ждет
--                         logS "waitBucketKey timeout: removing key"
                        return $ HM.delete key m
                    Just rm@(filter (/= wait) -> rm')
                        | rm /= rm' -> do
--                             logS "waitBucketKey timeout: removing mvar"
                            return $ HM.insert key rm' m
                        | otherwise -> do
--                             logS "waitBucketKey timeout: signaling mvar"
                            -- возможен вариант, что параллельный releaseBucketKey
                            -- уже успел сделать putMVar нашей wait, тогда
                            -- освобождаем следующего в очереди
                            putMVar (head rm) ()
                            return $ HM.insert key (tail rm) m
            return (HM.insert key (wait:xs) m, w)
    return key
waitBucketKey k = return k

releaseBucketKey key@(blockBucket -> True, _) =
    modifyMVarMasked_ modifyMVars $ \m -> case HM.lookup key m of
        Nothing ->
            fail $ "releaseBucketKey: not found ??? " ++ show key
        Just [] -> do -- никто больше не ждет
--             logS "releaseBucketKey timeout: removing key"
            return $ HM.delete key m
        Just (w:ws) -> do
--             logS "releaseBucketKey timeout: signaling mvar"
            putMVar w () -- освобождаем первого в очереди
            return $ HM.insert key ws m
releaseBucketKey _ = return ()

blockBucketKey :: BL.ByteString -> BL.ByteString -> IO a -> IO a
blockBucketKey bucket key act =
    E.bracket (waitBucketKey (bucket, key)) releaseBucketKey $ \ _ -> act

modifyKV' :: KV a => a -> Key a -> (Maybe a -> IO (a,b)) -> IO b
modifyKV' x k f =
    alterKV' x k (\ v -> first Just <$> f v)

alterKV' :: KV a => a -> Key a -> (Maybe a -> IO (Maybe a,b)) -> IO b
alterKV' x k f =
    blockBucketKey (kvBucket x) (key k) $ do -- withLogger $ \ l -> do
        let logT :: String -> IO a -> IO a
            logT _ act = act -- logTime l (T.pack $ n ++ " " ++ show (kvBucket x, textKey k)) act
        a0 <- logT "get" $ get (kvBucket x) (key k)
        updateCache (kvCache x) [(k, unWrapVClock (k,a0))]
        let kvs = wKV . fst =<< a0
            (ax, other) = case kvs of
                             Nothing -> (Nothing, [])
                             Just l -> findKey k l
        (a, r) <- f ax
        case a of
            Just a | Just a /= ax -> do
                --   ^ не уверен, что это хорошо, хотя уменьшает загрузку харда
                wkv <- logT "wrapKV" $
                   let !wkv = wrapKV (a : other) in return wkv
--                 liftM (fromMaybe (error $ "null wKV in modifyKV_ "
--                                   ++ show (kvBucket x)
--                                   ++ " " ++ show (encode k)) . wKV . fst)
                (wkw', vc') <- logT "put" $
                    put (kvBucket x) (key k) (fmap snd a0) wkv
                updateCache (kvCache x) [(k, fmap (, vc') (fst . findKey k =<< wKV wkw'))]
                -- не круто, что надо повторно вычитывать значение,
                -- зато так можно vclock получить
            Nothing | Just kv <- ax -> do
                removeFromCache kv
                deleteKV' kv (key k) a0
            _ ->
                return ()
        return r


-- | Если значение уже есть, то делаем resolve.
mergeWriteKV :: KV a => a -> IO ()
mergeWriteKV x = modifyKV_ (kvKey x) (return . maybe x (resolve x))

readManyKVs :: KV a => [Key a] -> IO [Maybe a]
readManyKVs [] = return []
readManyKVs ks = readManyKVs' undefined ks

readManyKVs' :: KV a => a -> [Key a] -> IO [Maybe a]
readManyKVs' x ks = do
    r <- fmap (map unWrapVClock . zip ks) $ getMany (kvBucket x) $ map key ks
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
    else do
        r <- getMany (kvBucket x) $ map key toread
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
    else do
        r <- getMany (kvBucket x) $ map key toread
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
writeManyKVs xs =
--     fmap (map $ fromMaybe (error "writeManyKVs null?")
--                   . wKV . fst) $
    putMany_ (kvBucket $ head xs)
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
          R.Bucket -> R.Key -> Maybe R.VClock -> R.R -> IO (Maybe (a, R.VClock))
vclockGet bucket key vclock r = tryRiak (GetException bucket key) $ \ conn -> do
--    when (bucket /= "Msg") $ logS $ show ("vclockGet", bucket, key, vclock)
    (\ r -> return . first resolveMany =<< getResp =<< r) `fmap`
        R.exchangeMaybe conn (getReq bucket key vclock r)
{-# INLINE vclockGet #-}

vclockGetMany :: (R.IsContent a, R.Resolvable a) => R.Bucket -> [(R.Key, Maybe R.VClock)] -> R.R
        -> IO [Maybe (a, R.VClock)]
vclockGetMany b ks r = tryRiak (GetManyException b (map fst ks)) $ \ conn -> do
--     logS $ show ("vclockGetMany", b, length ks, ks)
    r <- R.pipe (\ c -> E.try $ E.evaluate =<< do
              r <- R.recvMaybeResponse c
              case return . first resolveMany =<< getResp =<< r of
                  Just (!v,!vc) -> return $ Just (v,vc)
                  Nothing -> return Nothing
           ) conn (map (\(k,c) -> getReq b k c r) ks)
    case lefts r of
        x:_ -> E.throwIO (x :: E.SomeException)
        [] -> return $ rights r

-- recv: does not exists (Connection refused) -- локальный bind9 не стоял
getReq :: R.Bucket -> R.Key -> Maybe R.VClock -> R.R -> GetRequest.GetRequest
getReq bucket key vclock r =
    GetRequest.GetRequest
    { GetRequest.bucket = R.escape bucket
    , GetRequest.key = R.escape key
    , GetRequest.r = fromQuorum r
    , GetRequest.pr = Just 2
      -- вылезает {pr_val_unsatisfied,2,1}
      -- попробуем после AAE? --> помогло, ни одного exception
      -- Бывает даже {r_val_unsatisfied,2,1} -- видимо из-за notfound_ok
      -- было всего несколько раз в начале, потом не повторялось
      -- После миграции иногда выскакивает pr_val_unsatisfied
    , GetRequest.basic_quorum = Just True -- wait for 2 nodes, instead of 3
      -- on not found, does it needed with PR=2?
    , GetRequest.notfound_ok = Just False -- maybe not necessary with PR=2
    , GetRequest.if_modified = fmap R.fromVClock vclock
    , GetRequest.head        = Nothing
    , GetRequest.deletedvclock = Nothing -- Just True
      -- если True, то в getResp приходит GetResponse с VClock,
      -- но без содержимого (что логично), и потом в resolveMany оказывается
      -- пустой список. Надо менять обработку, чтобы был не Maybe (a, VClock),
      -- а (Maybe a, Maybe VClock)
      -- или даже Found (a, VClock) | NotFound | Deleted VClock
      -- Работалет без этого уже несколько лет, так что ничего не трогаем ;)
-- When using protocol buffers, make certain that deletedvclock in your object request is set to true in order to receive any tombstone vector clock.
    , GetRequest.timeout = Nothing
    , GetRequest.sloppy_quorum = Nothing
    , GetRequest.n_val = Nothing
    , GetRequest.type' = Nothing
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

forkRead act = do
    a <- async act
    return $ wait a
    --  ^ есть ощущение, что из-за этого виснет ghci 7.8.3

forkReadPar2 f list = do
    -- еще где-то 15% срезает времени обновления списка подписок
    r1 <- forkRead $ f l1
    r2 <- forkRead $ f l2
    return $ liftM2 (++) r1 r2
    where (l1, l2) = splitAt (length list `div` 2) list

riakBucketKeys bucket = go [] ""
    where go acc cont = do
              (r, cont') <- riakBucketKeys' bucket 10000 cont
              case cont' of
                  Just c -> go (r:acc) c
                  Nothing ->
                      return $ sort $ concat (r:acc)

riakBucketKeys' bucket n cont = do
    d <- urlGetContents
        ("http://127.0.0.1:8098/buckets/" ++ bucket
         ++ "/index/$bucket/_?max_results=" ++ show n ++ cont)
    case decodeJson d of
        Just (JSON.Object o)
            | Just (JSON.Array a) <- HM.lookup "keys" o -> do
                let r = sort $ filter (/= "") $ map key $ V.toList a
                length r `seq` case HM.lookup "continuation" o of
                    Just (JSON.String c) -> do
                        -- print $ Base64.decode $ tbs c
                        return (r, Just $ "&continuation="
                                   ++ T.unpack (encodeURIComponentT c))
                    Nothing ->
                        return (r, Nothing)
                    _ ->
                        fail "continuation is not a string?"
        _ -> fail "No keys?"
    where key (JSON.String u) = decodeURIComponentT u
          key j = error $ "Not a string key? " ++ show j

-- curl 'localhost:8098/buckets/User/index/$bucket/_' >all_users.js
getAllUsers = riakBucketKeys "User"


testBlockBucketDeadLock = do
    mapConcurrently_ id
        [block'   900000
        ,block
        ,timeout  500000 block >>= logS . show
--        ,timeout  750000 block >>= logS . show
--         ,block
--        ,timeout 1000000 block >>= logS . show
--         ,block
        ]
    where block' n = blockBucketKey "API" "1"
              $ logS "block {... " >> threadDelay n >> logS "}"
          block = block' 2000000
