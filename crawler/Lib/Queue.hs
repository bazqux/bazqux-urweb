{-# LANGUAGE BangPatterns #-}
module Lib.Queue
    ( Queue, emptyQ, pushQ, tryPopQ, lengthQ, pushFrontQ
    )
    where

data Queue a = Queue !Int ![a] ![a]
    deriving Show

emptyQ = Queue 0 [] []

lengthQ (Queue n _ _) = n

pushQ !a (Queue n f b) = Queue (succ n) f (a:b)

pushFrontQ !a (Queue n f b) = Queue (succ n) (a:f) b

popQ (Queue _ [] []) = error "popQ: empty queue"
popQ (Queue n [] b) = popQ $ Queue n (reverse b) []
popQ (Queue 1 (x:f) b) = (x, emptyQ)
popQ (Queue n (x:f) b) = (x, Queue (pred n) f b)

tryPopQ (Queue _ [] []) = Nothing
tryPopQ q = Just $ popQ q

avgQ (Queue 0 _ _) = 0
avgQ (Queue n f b) = (sum f + sum b) / fromIntegral n

listQ l = Queue (length l) l []

tryRotQ q
    | Just (e, q') <- tryPopQ q = Just (e, pushQ e q')
    | otherwise = Nothing
