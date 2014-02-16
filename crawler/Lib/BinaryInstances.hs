module Lib.BinaryInstances where

import Data.List
import Data.Ord
import Data.Hashable
import Data.Binary
import Data.Binary.Get (getByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Short as SB
import qualified Data.ByteString.Short.Internal as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Set as Set

emptyText = T.empty
{-# NOINLINE emptyText #-}
emptySB = SB.empty
{-# NOINLINE emptySB #-}

instance (Hashable a, Ord a) => Ord (HS.HashSet a) where
    compare a b =
        compare (Set.fromList $ HS.toList a) (Set.fromList $ HS.toList b)

instance (Hashable a, Ord a, Ord b) => Ord (HM.HashMap a b) where
    compare a b =
        compare (Map.fromList $ HM.toList a) (Map.fromList $ HM.toList b)

instance (Ord a, Hashable a, Binary a) => Binary (HS.HashSet a) where
    put = put . sort . HS.toList
    get = fmap HS.fromList get

instance (Ord a, Hashable a, Binary a, Binary b) => Binary (HM.HashMap a b) where
    put = put . sortBy (comparing fst) . HM.toList
    get = fmap HM.fromList get

instance Binary T.Text where
    put t = put $ T.encodeUtf8 t
    get = do
        sz <- get
        if sz <= 0 then return emptyText else do
            bs <- getByteString sz
            let t = T.decodeUtf8With (\ _ -> fmap B.w2c) bs
            T.length t `seq` return t

instance Binary SB.ShortByteString where
    put t = put $ SB.fromShort t
    get = do
        sz <- get
        if sz <= 0 then return emptySB else do
            bs <- getByteString sz
            let sb = SB.toShort bs
            SB.length sb `seq` return sb

instance Hashable SB.ShortByteString where
    hashWithSalt salt sb@(SB.SBS arr) =
        hashByteArrayWithSalt arr 0 (SB.length sb) salt
