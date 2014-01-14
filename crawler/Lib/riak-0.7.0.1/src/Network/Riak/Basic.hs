{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards #-}

-- |
-- Module:      Network.Riak.Basic
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Basic support for the Riak decentralized data store.
--
-- When storing and retrieving data, the functions in this module do
-- not perform any encoding or decoding of data, nor do they resolve
-- conflicts.

module Network.Riak.Basic
    (
    -- * Client configuration and identification
      ClientID
    , Client(..)
    , defaultClient
    -- * Connection management
    , Connection(..)
    , connect
    , disconnect
    , ping
    , getClientID
    , setClientID
    , getServerInfo
    -- * Data management
    , Quorum(..)
    , get
    , put
    , put_
    , delete
    -- * Metadata
    , listBuckets
    , foldKeys
    , getBucket
    , setBucket
    -- * Map/reduce
    , mapReduce
    ) where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Network.Riak.Connection.Internal
import Network.Riak.Escape (unescape)
import Network.Riak.Protocol.BucketProps
import Network.Riak.Protocol.Content
import Network.Riak.Protocol.ListKeysResponse
import Network.Riak.Protocol.MapReduce as MapReduce
import Network.Riak.Protocol.ServerInfo
import Network.Riak.Types.Internal hiding (MessageTag(..))
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Network.Riak.Request as Req
import qualified Network.Riak.Response as Resp
import qualified Network.Riak.Types.Internal as T

-- | Check to see if the connection to the server is alive.
ping :: Connection -> IO ()
ping conn = exchange_ conn Req.ping

-- | Find out from the server what client ID this connection is using.
getClientID :: Connection -> IO ClientID
getClientID conn = Resp.getClientID <$> exchange conn Req.getClientID

-- | Retrieve information about the server.
getServerInfo :: Connection -> IO ServerInfo
getServerInfo conn = exchange conn Req.getServerInfo

-- | Retrieve a value.  This may return multiple conflicting siblings.
-- Choosing among them is your responsibility.
get :: Connection -> T.Bucket -> T.Key -> R
    -> IO (Maybe (Seq.Seq Content, VClock))
get conn bucket key r = Resp.get <$> exchangeMaybe conn (Req.get bucket key r)

-- | Store a single value.  This may return multiple conflicting
-- siblings.  Choosing among them, and storing a new value, is your
-- responsibility.
--
-- You should /only/ supply 'Nothing' as a 'T.VClock' if you are sure
-- that the given bucket+key combination does not already exist.  If
-- you omit a 'T.VClock' but the bucket+key /does/ exist, your value
-- will not be stored.
put :: Connection -> T.Bucket -> T.Key -> Maybe T.VClock
    -> Content -> W -> DW
    -> IO (Seq.Seq Content, VClock)
put conn bucket key mvclock cont w dw =
  Resp.put <$> exchange conn (Req.put bucket key mvclock cont w dw True)

-- | Store a single value, without the possibility of conflict
-- resolution.
--
-- You should /only/ supply 'Nothing' as a 'T.VClock' if you are sure
-- that the given bucket+key combination does not already exist.  If
-- you omit a 'T.VClock' but the bucket+key /does/ exist, your value
-- will not be stored, and you will not be notified.
put_ :: Connection -> T.Bucket -> T.Key -> Maybe T.VClock
     -> Content -> W -> DW
     -> IO ()
put_ conn bucket key mvclock cont w dw =
  exchange_ conn (Req.put bucket key mvclock cont w dw False)

-- | Delete a value.
delete :: Connection -> T.Bucket -> T.Key -> RW -> IO ()
delete conn bucket key rw = exchange_ conn $ Req.delete bucket key rw

-- List the buckets in the cluster.
--
-- /Note/: this operation is expensive.  Do not use it in production.
listBuckets :: Connection -> IO (Seq.Seq T.Bucket)
listBuckets conn = Resp.listBuckets <$> exchange conn Req.listBuckets

-- Fold over the keys in a bucket.
--
-- /Note/: this operation is expensive.  Do not use it in production.
foldKeys :: Connection -> T.Bucket -> (a -> Key -> IO a) -> a -> IO a
foldKeys conn bucket f z0 = do
  sendRequest conn $ Req.listKeys bucket
  let g z = f z . unescape
      loop z = do
        ListKeysResponse{..} <- recvResponse conn
        z1 <- F.foldlM g z keys
        if fromMaybe False done
          then return z1
          else loop z1
  loop z0

-- | Retrieve the properties of a bucket.
getBucket :: Connection -> T.Bucket -> IO BucketProps
getBucket conn bucket = Resp.getBucket <$> exchange conn (Req.getBucket bucket)

-- | Store new properties for a bucket.
setBucket :: Connection -> T.Bucket -> BucketProps -> IO ()
setBucket conn bucket props = exchange_ conn $ Req.setBucket bucket props

-- | Run a 'MapReduce' job.  Its result is consumed via a strict left
-- fold.
mapReduce :: Connection -> Job -> (a -> MapReduce -> a) -> a -> IO a
mapReduce conn job f z0 = loop z0 =<< (exchange conn . Req.mapReduce $ job)
  where
    loop z mr = do
      let !z' = f z mr
      if fromMaybe False . MapReduce.done $ mr
        then return z'
        else loop z' =<< recvResponse conn
        
