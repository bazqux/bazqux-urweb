{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Lib.StringConversion
    ( tbs, bst, tsb, sbt
    , showT, pattern (:.)
    , asciiToLowerT, asciiToLowerB, asciiWhitespace, stripB
    )
    where

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Short as SB
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B

tbs = T.encodeUtf8
{-# INLINE tbs #-}
bst = T.decodeUtf8With (\ _ -> fmap B.w2c)
{-# INLINE bst #-}

tsb = SB.toShort . tbs
{-# INLINE tsb #-}
sbt = bst . SB.fromShort
{-# INLINE sbt #-}

showT :: Show a => a -> T.Text
showT = T.pack . show

asciiIsUpper c = c >= 'A' && c <= 'Z'
asciiToLower c
    | asciiIsUpper c = chr (ord c + 32)
    | otherwise      = c

asciiToLowerB x
    | B.any asciiIsUpper x = B.map asciiToLower x
    | otherwise = x

asciiToLowerT x
    | T.any asciiIsUpper x = T.map asciiToLower x
    | otherwise = x

stripB = fst . B.spanEnd asciiWhitespace . B.dropWhile asciiWhitespace

asciiWhitespace = (`elem` ("\x09\x0A\x0C\x0D " :: String))

pattern (:.) :: Char -> T.Text -> T.Text
pattern x :. xs <- (T.uncons -> Just (x, xs))

infixr 5 :.
