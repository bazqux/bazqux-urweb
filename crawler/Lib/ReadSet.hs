{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}
-- | Хранение индексов прочитанных сообщений (аналог IntSet,
-- но со сжатием последовательностей в интервалы).
--
-- Не использую Diet (for fat sets), т.к. нет балансировки.
module Lib.ReadSet
    ( ReadSet, empty, fromRange, size, member, insert, delete
    , readTill, clearTill
    , toList, fromList, union
    ) where

import Control.Monad
import qualified Lib.Set as Set
import Data.Binary

data ReadSet
    = ReadSet
      { size :: !Int
      , set  :: Set.Set I
      }
    deriving (Show, Read)

data I = I {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    deriving (Show, Read)

instance Binary I where
    put (I a b) = put a >> put b
    get = liftM2 I get get
instance Binary ReadSet where
    put (ReadSet a b) = put a >> put b
    get = liftM2 ReadSet get get
instance (Ord a, Binary a) => Binary (Set.Set a) where
    put s = put (Set.size s) >> mapM_ put (Set.toAscList s)
    get   = liftM Set.fromDistinctAscList get

instance Eq ReadSet where
    ReadSet sz1 s1 == ReadSet sz2 s2 =
        sz1 == sz2 && i2List s1 == i2List s2
instance Ord ReadSet where
    compare (ReadSet sz1 s1) (ReadSet sz2 s2) =
        compare (i2List s1) (i2List s2) -- не самая правильная штука

i2 (I a b) = (a,b)
i2List = map i2 . Set.toAscList

instance Eq I where
    a == b = compare a b == EQ

instance Ord I where
    compare (I a b) (I c d)
        | b < c = LT
        | d < a = GT
        | otherwise = EQ

empty = ReadSet 0 Set.empty

member :: Int -> ReadSet -> Bool
member i (ReadSet _ s) = Set.member (I i i) s

insert :: Int -> ReadSet -> ReadSet
insert i rs@(ReadSet t s)
    | member i rs = rs
    | otherwise = ReadSet (t+1) $ case (Set.maxView l, Set.minView r) of
        (Nothing, Nothing) -> Set.singleton $ I i i
        (Just (I a b, l'), Nothing)
            | b + 1 == i -> ins (I a i) l'
            | otherwise  -> ins (I i i) s
        (Nothing, Just (I c d, r'))
            | i + 1 == c -> ins (I i d) r'
            | otherwise  -> ins (I i i) s
        (Just (I a b, l'), Just (I c d, r'))
            | b + 2 == c -> ins (I a d) $ del (I a b) $ del (I c d) s
            | b + 1 == i -> ins (I a i) $ del (I a b) s
            | i + 1 == c -> ins (I i d) $ del (I c d) s
            | otherwise  -> ins (I i i) s
    where (l,r) = Set.split (I i i) s
          ins = Set.insert
          del = Set.delete

-- | Помечает прочитанными все сообщения от нуля до заданного
readTill :: Int -> ReadSet -> ReadSet
readTill e rs@(ReadSet t s)
    | e < 0 = rs
    | otherwise = case Set.minView s of
        Nothing -> fromRange 0 e
        Just (I a b, s')
            | a == 0 && b == e -> rs
              -- без изменений
            | e < a-1 -> ReadSet (t + e+1) (Set.insert (I 0 e) s)
              -- не пересекаются
            | e <= b -> ReadSet (t - (b-a+1) + b+1) (Set.insert (I 0 b) s')
              -- объединяются
            | otherwise -> readTill e (ReadSet (t - (b-a+1)) s')
              -- e дальше первого интервала

-- | Убирает все прочитанные сообщения от нуля до заданного
clearTill :: Int -> ReadSet -> ReadSet
clearTill e rs@(ReadSet t s)
    | e < 0 = rs
    | otherwise = case Set.minView s of
        Nothing -> rs
        Just (I a b, s')
            | b <= e -> clearTill e $ ReadSet (t - (b-a+1)) s'
              -- e дальше первого интервала
            | a > e -> rs
              -- интервал дальше e
            | otherwise -> ReadSet (t - (e-a+1)) (Set.insert (I (e+1) b) s')
              -- пересечение

delete :: Int -> ReadSet -> ReadSet
delete i rs@(ReadSet t s) = case Set.splitLookup (I i i) s of
    (_, Just (I a b), _)
        | a == b -> r id
        | a == i -> r (Set.insert (I (i+1) b))
        | b == i -> r (Set.insert (I a (i-1)))
        | otherwise -> r (Set.insert (I (i+1) b) . Set.insert (I a (i-1)))
        where r f = ReadSet (t-1) (f $ Set.delete (I a b) s)
    _ -> rs

-- | [a..b] inclusive
fromRange a b
    | b < a = empty
    | otherwise = ReadSet (b-a+1) (Set.singleton $ I a b)

test = -- readTill 5 $
       delete 6 $ delete 1 $ foldl (flip insert) empty [1,4,1,7,3,6, 1,2,6]

toList = concatMap iToList . Set.toList . set
    where iToList (I a b) = [a..b]

fromList = go empty
    where go rs [] = rs
          go !rs (x:xs) = go (insert x rs) xs

-- неоптимально, но должно работать
union a b = fromList $ toList a ++ toList b
