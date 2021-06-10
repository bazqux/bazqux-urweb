{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module:      Network.Riak.Content
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Mark Hibberd <mark@hibberd.id.au>, Nathan Hunter <nhunter@janrain.com>
-- Stability:   experimental
-- Portability: portable
--
-- im in ur msg system taggin ur msg types

module Network.Riak.Tag
    (
      putTag
    , getTag
    ) where

import Data.Binary.Put (Put, putWord8)
import Network.Riak.Protocol.DeleteRequest
import Network.Riak.Protocol.ErrorResponse
import Network.Riak.Protocol.GetBucketRequest
import Network.Riak.Protocol.GetBucketResponse
import Network.Riak.Protocol.GetClientIDRequest
import Network.Riak.Protocol.GetClientIDResponse
import Network.Riak.Protocol.GetRequest
import Network.Riak.Protocol.GetResponse
import Network.Riak.Protocol.GetServerInfoRequest
import Network.Riak.Protocol.ListBucketsRequest
import Network.Riak.Protocol.ListBucketsResponse
import Network.Riak.Protocol.ListKeysRequest
import Network.Riak.Protocol.ListKeysResponse
import Network.Riak.Protocol.MapReduce
import Network.Riak.Protocol.MapReduceRequest
import Network.Riak.Protocol.PingRequest
import Network.Riak.Protocol.PutRequest
import Network.Riak.Protocol.PutResponse
import Network.Riak.Protocol.ServerInfo
import Network.Riak.Protocol.SetBucketRequest
import Network.Riak.Protocol.SetClientIDRequest
import Network.Riak.Types.Internal as Types
import Text.ProtocolBuffers.Get (Get, getWord8)

instance Tagged ErrorResponse where
    messageTag _ = Types.ErrorResponse
    {-# INLINE messageTag #-}

instance Response ErrorResponse

instance Tagged PingRequest where
    messageTag _ = Types.PingRequest
    {-# INLINE messageTag #-}

instance Request PingRequest where
    expectedResponse _ = Types.PingResponse
    {-# INLINE expectedResponse #-}

instance Tagged GetClientIDRequest where
    messageTag _ = Types.GetClientIDRequest
    {-# INLINE messageTag #-}

instance Request GetClientIDRequest where
    expectedResponse _ = Types.GetClientIDResponse
    {-# INLINE expectedResponse #-}

instance Tagged GetClientIDResponse where
    messageTag _ = Types.GetClientIDResponse
    {-# INLINE messageTag #-}

instance Response GetClientIDResponse

instance Exchange GetClientIDRequest GetClientIDResponse

instance Tagged SetClientIDRequest where
    messageTag _ = Types.SetClientIDRequest
    {-# INLINE messageTag #-}

instance Request SetClientIDRequest where
    expectedResponse _ = Types.SetClientIDResponse
    {-# INLINE expectedResponse #-}

instance Tagged GetServerInfoRequest where
    messageTag _ = Types.GetServerInfoRequest
    {-# INLINE messageTag #-}

instance Request GetServerInfoRequest where
    expectedResponse _ = Types.GetServerInfoResponse
    {-# INLINE expectedResponse #-}

instance Tagged ServerInfo where
    messageTag _ = Types.GetServerInfoResponse
    {-# INLINE messageTag #-}

instance Response ServerInfo

instance Exchange GetServerInfoRequest ServerInfo

instance Tagged GetRequest where
    messageTag _ = Types.GetRequest
    {-# INLINE messageTag #-}

instance Request GetRequest where
    expectedResponse _ = Types.GetResponse
    {-# INLINE expectedResponse #-}

instance Tagged GetResponse where
    messageTag _ = Types.GetResponse
    {-# INLINE messageTag #-}

instance Response GetResponse

instance Exchange GetRequest GetResponse

instance Tagged PutRequest where
    messageTag _ = Types.PutRequest
    {-# INLINE messageTag #-}

instance Request PutRequest where
    expectedResponse _ = Types.PutResponse
    {-# INLINE expectedResponse #-}

instance Tagged PutResponse where
    messageTag _ = Types.PutResponse
    {-# INLINE messageTag #-}

instance Response PutResponse

instance Exchange PutRequest PutResponse

instance Tagged DeleteRequest where
    messageTag _ = Types.DeleteRequest
    {-# INLINE messageTag #-}

instance Request DeleteRequest where
    expectedResponse _ = Types.DeleteResponse
    {-# INLINE expectedResponse #-}

instance Tagged ListBucketsRequest where
    messageTag _ = Types.ListBucketsRequest
    {-# INLINE messageTag #-}

instance Request ListBucketsRequest where
    expectedResponse _ = Types.ListBucketsResponse
    {-# INLINE expectedResponse #-}

instance Tagged ListBucketsResponse where
    messageTag _ = Types.ListBucketsResponse
    {-# INLINE messageTag #-}

instance Response ListBucketsResponse

instance Exchange ListBucketsRequest ListBucketsResponse

instance Tagged ListKeysRequest where
    messageTag _ = Types.ListKeysRequest
    {-# INLINE messageTag #-}

instance Request ListKeysRequest where
    expectedResponse _ = Types.ListKeysResponse
    {-# INLINE expectedResponse #-}

instance Tagged ListKeysResponse where
    messageTag _ = Types.ListKeysResponse
    {-# INLINE messageTag #-}

instance Response ListKeysResponse

instance Tagged GetBucketRequest where
    messageTag _ = Types.GetBucketRequest
    {-# INLINE messageTag #-}

instance Request GetBucketRequest where
    expectedResponse _ = Types.GetBucketResponse
    {-# INLINE expectedResponse #-}

instance Tagged GetBucketResponse where
    messageTag _ = Types.GetBucketResponse
    {-# INLINE messageTag #-}

instance Response GetBucketResponse

instance Exchange GetBucketRequest GetBucketResponse

instance Tagged SetBucketRequest where
    messageTag _ = Types.SetBucketRequest
    {-# INLINE messageTag #-}

instance Request SetBucketRequest where
    expectedResponse _ = Types.SetBucketResponse
    {-# INLINE expectedResponse #-}

instance Tagged MapReduceRequest where
    messageTag _ = Types.MapReduceRequest
    {-# INLINE messageTag #-}

instance Request MapReduceRequest where
    expectedResponse _ = Types.MapReduceResponse
    {-# INLINE expectedResponse #-}

instance Tagged MapReduce where
    messageTag _ = Types.MapReduceResponse
    {-# INLINE messageTag #-}

instance Response MapReduce

instance Exchange MapReduceRequest MapReduce

putTag :: MessageTag -> Put
putTag = putWord8 . fromIntegral . fromEnum
{-# INLINE putTag #-}

getTag :: Get MessageTag
getTag = do
  n <- getWord8
  if n > 24
    then moduleError "getTag" $ "invalid riak message code: " ++ show n
    else return .  toEnum . fromIntegral $ n
{-# INLINE getTag #-}

moduleError :: String -> String -> a
moduleError = netError "Network.Riak.Tag"
