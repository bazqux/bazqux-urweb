{-# LANGUAGE ViewPatterns #-}
-- | Утилиты для чтения значений из Text, более быстрые, но
-- менее универсальные, чем read.
module Lib.ReadUtils
    ( tryReadUnsignedInt, readUnsignedInt
    , tryReadUnsignedInteger, readUnsignedInteger
    , tryReadInteger, readInteger
    , tryReadDouble
    ) where
-- чтение Integer на больших числах может быть медленнее Read
-- можно взять код из css-syntax

import qualified Data.Text as T
import Data.Char
import Data.Ratio

readSigned :: Num a => (T.Text -> a) -> T.Text -> a
readSigned f b
    | T.length b == 0    = 0
    | T.index b 0 == '-' = negate (f $ T.drop 1 b)
    | otherwise          = f b
{-# INLINE readSigned #-}

readInteger  = readSigned readUnsignedInteger
readInt      = readSigned readUnsignedInt
readRational = readSigned readUnsignedRational

tryReadSigned :: Num a => (T.Text -> Maybe a) -> T.Text -> Maybe a
tryReadSigned f (T.strip -> b)
    | T.index b 0 == '-' = negate <$> f (T.drop 1 b)
    | otherwise          = f b

tryReadInteger  = tryReadSigned tryReadUnsignedInteger
tryReadInt      = tryReadSigned tryReadUnsignedInt

readUnsignedInteger :: T.Text -> Integer
readUnsignedInteger =
    T.foldl' (\ acc digit -> acc * 10 + fromIntegral (ord digit - ord '0')) 0

readUnsignedInt :: T.Text -> Int
readUnsignedInt =
    T.foldl' (\ acc digit -> acc * 10 + ord digit - ord '0') 0

readUnsignedRational :: T.Text -> Rational
readUnsignedRational s
    | Just i <- T.findIndex (== '%') s =
        readUnsignedInteger (T.take i s) % readUnsignedInteger (T.drop (i+1) s)
    | otherwise = error "readRational: '%' not found"

tryReadUnsignedInt (T.strip -> t)
    | T.all (\ c -> c >= '0' && c <= '9') t && not (T.null t) =
        Just $ readUnsignedInt t
    | otherwise = Nothing

tryReadUnsignedInteger (T.strip -> t)
    | T.all (\ c -> c >= '0' && c <= '9') t && not (T.null t) =
        Just $ readUnsignedInteger t
    | otherwise = Nothing

tryReadDouble :: T.Text -> Maybe Double
tryReadDouble t = case reads (T.unpack t) of
    [(r, [])] -> Just r
    _ -> Nothing
