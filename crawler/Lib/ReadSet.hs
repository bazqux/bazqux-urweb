{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, ViewPatterns #-}
-- | Хранение индексов прочитанных сообщений (аналог IntSet,
-- но со сжатием последовательностей в интервалы).
--
-- Не использую Diet (for fat sets), т.к. нет балансировки.
module Lib.ReadSet
    ( ReadSet, empty, null, fromRange, size, member, insert, delete
    , readTill, clearTill
    , toList, fromList, union, intersection, difference, maxIndex
    , toIntSet, fromIntSet
    ) where

import Prelude hiding (null)
import Control.Monad
import Control.DeepSeq
import qualified Data.Set as Set
import Data.Binary
import qualified Data.IntSet as IntSet
-- import Test.QuickCheck

data ReadSet
    = ReadSet
      { size :: !Int
      , set  :: Set.Set I
      }
    deriving (Show)--, Read)

data I = I {-# UNPACK #-} !Int {-# UNPACK #-} !Int
--    deriving (Show, Read)

newtype IT = IT I

instance Eq IT where
    IT (I a1 b1) == IT (I a2 b2) = a1 == a2 && b1 == b2

instance Ord IT where
    IT (I a1 b1) `compare` IT (I a2 b2) = compare (a1,b1) (a2,b2)

instance Show I where
    show (I a b)
        | a == b = show a
        | otherwise = show a ++ "-" ++ show b

instance Binary I where
    put (I a b) = put a >> put b
    get = liftM2 I get get
instance Binary ReadSet where
    put (ReadSet a b) = put a >> put b
    get = liftM2 ReadSet get get

instance Eq ReadSet where
    ReadSet sz1 s1 == ReadSet sz2 s2 =
        sz1 == sz2 && itList s1 == itList s2
instance Ord ReadSet where
    compare (ReadSet sz1 s1) (ReadSet sz2 s2) =
        compare (cmpList s1) (cmpList s2)
        where cmpList s = [(b,b-a) | I a b <- reverse (Set.toList s)]
                          -- если длина интервала больше,
                          -- то и прочитанных больше

instance NFData ReadSet where
    rnf (ReadSet a b) = rnf a `seq` rnf b `seq` ()
instance NFData I where
    rnf i = i `seq` ()

itList = map IT . Set.toAscList

instance Eq I where
    a == b = compare a b == EQ

instance Ord I where
    compare (I a b) (I c d)
        | b < c = LT
        | d < a = GT
        | otherwise = EQ

empty = ReadSet 0 Set.empty

null (ReadSet 0 _) = True
null _ = False

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

lookupI :: Int -> Set.Set I -> Maybe I
lookupI x s =
    case Set.minView $ snd $ Set.spanAntitone (\ (I a b) -> b < x) s of
        Just (i@(I a _), _) | a <= x -> Just i
        _ -> Nothing

delete :: Int -> ReadSet -> ReadSet
delete i rs@(ReadSet t s) = case lookupI i s of
    Just (I a b)
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

fromDistinctAscList l = ReadSet (Set.foldl' (\ s (I a b) -> s + b-a+1) 0 s) s
    where s = Set.fromDistinctAscList l

test = -- readTill 5 $
       delete 10 $ insert 5 $ delete 1 $
       foldl (flip insert) empty [1,4,1,7,3,6, 1,2,6,0]

toList = concatMap iToList . Set.toAscList . set
    where iToList (I a b) = [a..b]

fromList = go empty
    where go rs [] = rs
          go !rs (x:xs) = go (insert x rs) xs

union a b
    | a == b = a
    | otherwise =
        fromDistinctAscList $ u (Set.toAscList $ set a) (Set.toAscList $ set b)
    where u [] bs = bs
          u as [] = as
          u aas@(a@(I a1 a2) : as) bbs@(b@(I b1 b2) : bs)
              | a2 + 1 < b1 = a : u as bbs
              | b2 + 1 < a1 = b : u aas bs
              | a2 > b2   = u (I (min a1 b1) a2 : as) bs
              | otherwise = u as (I (min a1 b1) b2 : bs)

intersection a b
    | size a == 0 || size b == 0 = empty
    | a == b = a
    | otherwise =
        fromDistinctAscList $ i (Set.toAscList $ set a) (Set.toAscList $ set b)
    where i [] bs = []
          i as [] = []
          i aas@(a@(I a1 a2) : as) bbs@(b@(I b1 b2) : bs)
              | a2 < b1 = i as bbs
              | b2 < a1 = i aas bs
              | a2 > b2   = I (max a1 b1) b2 : i aas bs
              | otherwise = I (max a1 b1) a2 : i as bbs

difference a b
    | size b == 0 = a
    | a == b = empty
    | otherwise =
        fromDistinctAscList $ d (Set.toAscList $ set a) (Set.toAscList $ set b)
    where d [] bs = []
          d as [] = as
          d aas@(a@(I a1 a2) : as) bbs@(b@(I b1 b2) : bs)
              | a2 < b1 = a : d as bbs
              | b2 < a1 = d aas bs
              | b1 > a1 = I a1 (b1-1) : rest
              | otherwise = rest
              where rest
                        | b2 < a2 = d (I (b2+1) a2 : as) bs
                        | otherwise = d as bbs

prop_union (fromList -> a) (fromList -> b) =
    union a b == fromIntSet (toIntSet a `IntSet.union` toIntSet b)
prop_intersection (fromList -> a) (fromList -> b) =
    intersection a b == fromIntSet (toIntSet a `IntSet.intersection` toIntSet b)
prop_difference (fromList -> a) (fromList -> b) =
    difference a b == fromIntSet (toIntSet a `IntSet.difference` toIntSet b)

maxIndex rs@(ReadSet t s)
    | Just (I _ m, _) <- Set.maxView s = Just m
    | otherwise = Nothing

toIntSet = IntSet.fromDistinctAscList . toList

fromIntSet s = ReadSet (IntSet.size s) set
    where !set = Set.fromDistinctAscList $ go $ IntSet.toList s
          go [] = []
          go (x:xs)
              | (x1,xs') <- findIncr x xs = I x x1 : go xs'
          findIncr x [] = (x,[])
          findIncr x l@(a:as)
              | a == x+1 = findIncr a as
              | otherwise = (x,l)
