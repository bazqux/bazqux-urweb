{-# LANGUAGE RecordWildCards #-}

-- |
-- Module:      Network.Riak.Request
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Smart deconstructors for Riak types.  These functions correctly
-- URL-unescape bucket, key, and link names.  You should thus use them
-- in preference to direct pattern matching against raw data
-- constructors.

module Network.Riak.Response
    (
    -- * Connection management
      getClientID
    -- * Data management
    , get
    , put
    -- * Metadata
    , listBuckets
    , getBucket
    , unescapeLinks
    ) where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Network.Riak.Escape (unescape)
import Network.Riak.Protocol.BucketProps
import Network.Riak.Protocol.Content
import Network.Riak.Protocol.GetBucketResponse
import Network.Riak.Protocol.GetClientIDResponse
import Network.Riak.Protocol.GetResponse
import Network.Riak.Protocol.Link
import Network.Riak.Protocol.ListBucketsResponse
import Network.Riak.Protocol.PutResponse
import Network.Riak.Types.Internal hiding (MessageTag(..))
import qualified Network.Riak.Protocol.Link as Link
import qualified Data.ByteString.Lazy as L
import qualified Data.Sequence as Seq

getClientID :: GetClientIDResponse -> ClientID
getClientID = client_id
{-# INLINE getClientID #-}

-- | Construct a get response.  Bucket and key names in links are
-- URL-unescaped.
get :: Maybe GetResponse -> Maybe (Seq.Seq Content, VClock)
get (Just (GetResponse content (Just vclock) _))
      = Just (unescapeLinks <$> content, VClock vclock)
get _ = Nothing
{-# INLINE get #-}

-- | Construct a put response.  Bucket and key names in links are
-- URL-unescaped.
put :: PutResponse -> (Seq.Seq Content, VClock)
put PutResponse{..} = (unescapeLinks <$> content,
                       VClock (fromMaybe L.empty vclock))
{-# INLINE put #-}

-- | Construct a list-buckets response.  Bucket names are unescaped.
listBuckets :: ListBucketsResponse -> Seq.Seq Bucket
listBuckets = fmap unescape . buckets
{-# INLINE listBuckets #-}

getBucket :: GetBucketResponse -> BucketProps
getBucket = props
{-# INLINE getBucket #-}

-- | URL-unescape the names of keys and buckets in the links of a
-- 'Content' value.
unescapeLinks :: Content -> Content
unescapeLinks c = c { links = go <$> links c }
  where go l = l { bucket = unescape <$> bucket l
                 , Link.key = unescape <$> Link.key l }
