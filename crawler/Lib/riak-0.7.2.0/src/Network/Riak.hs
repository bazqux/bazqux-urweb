-- |
-- Module:      Network.Riak
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Mark Hibberd <mark@hibberd.id.au>, Nathan Hunter <nhunter@janrain.com>
-- Stability:   experimental
-- Portability: portable
--
-- A client for the Riak decentralized data store.
--
-- The functions in this module use JSON as the storage
-- representation, and automatically perform conflict resolution
-- during storage and retrieval.
--
-- This library is organized to allow a tradeoff between power
-- and ease of use.  If you would like a different degree of
-- automation with storage and conflict resolution, you may want to
-- use one of the following modules (ranked from easiest to most
-- tricky to use):
--
-- [Network.Riak.JSON.Resolvable] JSON for storage, automatic conflict
-- resolution.  (This module actually re-exports its definitions.)
-- This is the easiest module to work with.
--
-- [Network.Riak.JSON] JSON for storage, manual conflict resolution.
--
-- [Network.Riak.Value.Resolvable] More complex (but still automatic)
-- storage, automatic conflict resolution.
--
-- [Network.Riak.Value] More complex (but still automatic) storage,
-- manual conflict resolution.
--
-- [Network.Riak.Basic] manual storage, manual conflict resolution.
-- This is the most demanding module to work with, as you must encode
-- and decode data yourself, and handle all conflict resolution
-- yourself.

module Network.Riak
    (
    -- * Client configuration and identification
      ClientID
    , Client(..)
    , defaultClient
    , getClientID
    -- * Connection management
    , Connection(..)
    , connect
    , disconnect
    , ping
    , getServerInfo
    -- * Data management
    , Quorum(..)
    , Resolvable(..)
    , get
    , getMany
    , modify
    , modify_
    , delete
    -- ** Low-level modification functions
    , put
    , putMany
    -- * Metadata
    , listBuckets
    , foldKeys
    , getBucket
    , setBucket
    -- * Map/reduce
    , mapReduce
    ) where

import Network.Riak.Basic hiding (get, put, put_)
import Network.Riak.JSON.Resolvable (get, getMany, modify, modify_, put, putMany)
import Network.Riak.Resolvable (Resolvable(..))
