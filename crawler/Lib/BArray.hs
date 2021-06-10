{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | array, у которого пустые bounds участвуют в Eq
--
-- import qualified Lib.BArray as BA
--
-- listArray   (10,9) [] == listArray   (20,19) []
-- B.listArray (10,9) [] /= B.listArray (20,19) []
--
module Lib.BArray
    ( Array(..)
    , listArray, elems, (!), bounds, assocs, amap
    )
    where

import qualified Data.Array as A
import qualified Data.Array.IArray as A (amap)
import Data.Binary
import Control.DeepSeq

newtype Array i a = Array { unArray :: A.Array i a }
    deriving (Ord, Binary, NFData)

instance (A.Ix i, Show i, Show a) => Show (Array i a) where
    showsPrec i (Array a) = showsPrec i a
instance (A.Ix i, Eq e) => Eq (Array i e) where
    Array a == Array b = A.bounds a == A.bounds b && a == b

test =
    ( A.listArray (10,10) [1] == A.listArray (20,20) [1] -- False
    , A.listArray (10,9) [] == A.listArray (20,19) ([] :: [Int]) -- True
    , listArray (10,9) [] == listArray (20,19) ([] :: [Int])
    )

listArray b e = Array $ A.listArray b e
elems (Array a) = A.elems a
Array a ! i = a A.! i
bounds (Array a) = A.bounds a
assocs (Array a) = A.assocs a
amap f (Array a) = Array (A.amap f a)
