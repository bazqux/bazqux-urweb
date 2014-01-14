{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Network.Riak.Request
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Smart constructors for Riak types.  These functions correctly
-- URL-escape bucket, key, and link names.  You should thus use them
-- in preference to the raw data constructors.

module Network.Riak.Request
    (
    -- * Connection management
      PingRequest
    , ping
    , GetClientIDRequest
    , getClientID
    , GetServerInfoRequest
    , getServerInfo
    -- * Data management
    , Get.GetRequest
    , get
    , Put.PutRequest
    , put
    , Del.DeleteRequest
    , delete
    -- * Metadata
    , Link.Link
    , link
    , ListBucketsRequest
    , listBuckets
    , Keys.ListKeysRequest
    , listKeys
    , GetBucket.GetBucketRequest
    , getBucket
    , SetBucket.SetBucketRequest
    , setBucket
    -- * Map/reduce
    , MapReduceRequest
    , mapReduce
    ) where

import Control.Applicative ((<$>))
import Network.Riak.Protocol.BucketProps
import Network.Riak.Protocol.Content
import Network.Riak.Protocol.GetClientIDRequest
import Network.Riak.Protocol.GetServerInfoRequest
import Network.Riak.Protocol.ListBucketsRequest
import Network.Riak.Protocol.MapReduceRequest
import Network.Riak.Protocol.PingRequest
import Network.Riak.Types.Internal hiding (MessageTag(..))
import Network.Riak.Escape (escape)
import qualified Network.Riak.Protocol.DeleteRequest as Del
import qualified Network.Riak.Protocol.Link as Link
import qualified Network.Riak.Protocol.GetBucketRequest as GetBucket
import qualified Network.Riak.Protocol.GetRequest as Get
import qualified Network.Riak.Protocol.ListKeysRequest as Keys
import qualified Network.Riak.Protocol.PutRequest as Put
import qualified Network.Riak.Protocol.SetBucketRequest as SetBucket

-- | Create a ping request.
ping :: PingRequest
ping = PingRequest
{-# INLINE ping #-}

-- | Create a client-ID request.
getClientID :: GetClientIDRequest
getClientID = GetClientIDRequest
{-# INLINE getClientID #-}

-- | Create a server-info request.
getServerInfo :: GetServerInfoRequest
getServerInfo = GetServerInfoRequest
{-# INLINE getServerInfo #-}

-- | Create a get request.  The bucket and key names are URL-escaped.
get :: Bucket -> Key -> R -> Get.GetRequest
get bucket key r' = Get.GetRequest { Get.bucket = escape bucket
                                  , Get.key = escape key
                                  , Get.r = fromQuorum r'
                                  , Get.pr = Nothing
                                  , Get.basic_quorum = Nothing
                                  , Get.notfound_ok = Nothing
                                  , Get.if_modified = Nothing
                                  , Get.head        = Nothing
                                  , Get.deletedvclock = Nothing
                                  , Get.timeout = Nothing
                                  }
{-# INLINE get #-}

-- | Create a put request.  The bucket and key names are URL-escaped.
-- Any 'Link' values inside the 'Content' are assumed to have been
-- constructed with the 'link' function, and hence /not/ escaped.
put :: Bucket -> Key -> Maybe VClock -> Content -> W -> DW -> Bool
    -> Put.PutRequest
put bucket key mvclock cont mw mdw returnBody =
    Put.PutRequest (escape bucket) (Just $ escape key) (fromVClock <$> mvclock)
                   cont (fromQuorum mw) (fromQuorum mdw) (Just returnBody)
                   Nothing Nothing Nothing Nothing Nothing
{-# INLINE put #-}

-- | Create a link.  The bucket and key names are URL-escaped.
link :: Bucket -> Key -> Tag -> Link.Link
link bucket key = Link.Link (Just (escape bucket)) (Just (escape key)) . Just
{-# INLINE link #-}

-- | Create a delete request.  The bucket and key names are URL-escaped.
delete :: Bucket -> Key -> RW -> Del.DeleteRequest
delete bucket key rw' = Del.DeleteRequest (escape bucket) (escape key)
                                          (fromQuorum rw') Nothing Nothing Nothing
                                          Nothing Nothing Nothing Nothing
{-# INLINE delete #-}

-- | Create a list-buckets request.
listBuckets :: ListBucketsRequest
listBuckets = ListBucketsRequest
{-# INLINE listBuckets #-}

-- | Create a list-keys request.  The bucket name is URL-escaped.
listKeys :: Bucket -> Keys.ListKeysRequest
listKeys = Keys.ListKeysRequest . escape
{-# INLINE listKeys #-}

-- | Create a get-bucket request.  The bucket name is URL-escaped.
getBucket :: Bucket -> GetBucket.GetBucketRequest
getBucket = GetBucket.GetBucketRequest . escape
{-# INLINE getBucket #-}

-- | Create a set-bucket request.  The bucket name is URL-escaped.
setBucket :: Bucket -> BucketProps -> SetBucket.SetBucketRequest
setBucket = SetBucket.SetBucketRequest . escape
{-# INLINE setBucket #-}

-- | Create a map-reduce request.
mapReduce :: Job -> MapReduceRequest
mapReduce (JSON bs)   = MapReduceRequest bs "application/json"
mapReduce (Erlang bs) = MapReduceRequest bs "application/x-erlang-binary"
