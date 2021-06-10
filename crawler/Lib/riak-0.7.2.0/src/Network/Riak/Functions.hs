-- |
-- Module:      Network.Riak.Functions
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Mark Hibberd <mark@hibberd.id.au>, Nathan Hunter <nhunter@janrain.com>
-- Stability:   experimental
-- Portability: portable
--
-- Useful functions.

module Network.Riak.Functions
    (
      strict
    , lazy
    , mapEither
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L

strict :: L.ByteString -> B.ByteString
strict = B.concat . L.toChunks
{-# INLINE strict #-}

lazy :: B.ByteString -> L.ByteString
lazy s | B.null s  = L.Empty
       | otherwise = L.Chunk s L.Empty
{-# INLINE lazy #-}

mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapEither f _ (Left l)  = Left (f l)
mapEither _ g (Right r) = Right (g r)
{-# INLINE mapEither #-}
