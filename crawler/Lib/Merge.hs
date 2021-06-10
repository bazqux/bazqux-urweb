-- | Утилиты слияния ключей
module Lib.Merge
    ( unionByWith
    )
    where

import Data.List
import Data.Ord

-- | Объединяет два списка по ключам с объединением одинаковых значений.
unionByWith key merge a b = go (s a) (s b)
    where s = mergeSame . sortBy (comparing key)
          mergeSame [] = []
          mergeSame [x] = [x]
          mergeSame (a:b:xs)
              | key a == key b = mergeSame (merge a b : xs)
              | otherwise      = a : mergeSame (b:xs)
          go a [] = a
          go [] b = b
          go (a:as) (b:bs) = case compare (key a) (key b) of
              LT -> a : go as (b:bs)
              GT -> b : go (a:as) bs
              EQ -> go (merge a b : as) bs
