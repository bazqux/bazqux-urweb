{-# LANGUAGE BangPatterns #-}
module Lib.BinaryInstances (JunkText(..)) where

import Data.List
import Data.Ord
import Data.Hashable
import Data.Binary
import qualified Data.ByteString.Internal as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.String

-- emptyText = T.empty
-- {-# NOINLINE emptyText #-}
-- emptySB = SB.empty
-- {-# NOINLINE emptySB #-}

instance (Ord a, Hashable a, Binary a) => Binary (HS.HashSet a) where
    put = put . sort . HS.toList
    get = fmap HS.fromList get

instance (Ord a, Hashable a, Binary a, Binary b) => Binary (HM.HashMap a b) where
    put = put . sortBy (comparing fst) . HM.toList
    get = fmap HM.fromList get

newtype JunkText = JunkText { unJunkText :: T.Text }

instance IsString JunkText where
    fromString = JunkText . T.pack

instance Binary JunkText where
    put (JunkText t) = put $ T.encodeUtf8 t
    get = do
        bs <- get
        let !t = JunkText $ T.decodeUtf8With (\ _ -> fmap B.w2c) bs
        return t

-- text-1.2.1.0 включает Binary, хотя и без Short и emptyText и B.w2c,
-- только decodeUtf8
-- instance Binary T.Text where
--     put t = put $ T.encodeUtf8 t
--     get = do
--         sz <- get
--         if sz <= 0 then return emptyText else do
--             bs <- getByteString sz
--             let !sb = SB.toShort bs
--             let ~t = T.decodeUtf8With (\ _ -> fmap B.w2c) $ SB.fromShort sb
-- --            let !t = T.decodeUtf8With (\ _ -> fmap B.w2c) bs
--             return t

-- sizeOfWord = sizeOf (undefined :: Int)
-- roundToWordBoundary n = (n + sizeOfWord - 1) `div` sizeOfWord * sizeOfWord
-- sizeOfText n = 6 * sizeOfWord + roundToWordBoundary (2*n)
-- sizeOfShort n = 4 * sizeOfWord + roundToWordBoundary n
-- -- ShortByteString всегда меньше Text, даже если два слова на Thunk добавить,
-- -- А вот ByteString до 20-28-36 байт (0-1-2 доп. слова в Thunk) большe
-- sizeOfByteString n = 9 * sizeOfWord + roundToWordBoundary n

-- instance Binary SB.ShortByteString where
--     put t = put $ SB.fromShort t
--     get = do
--         sz <- get
--         if sz <= 0 then return emptySB else do
--             bs <- getByteString sz
--             let sb = SB.toShort bs
--             SB.length sb `seq` return sb

-- instance Hashable SB.ShortByteString where
--     hashWithSalt salt sb@(SB.SBS arr) =
--         hashByteArrayWithSalt arr 0 (SB.length sb) salt
