-- |
-- Module:      Network.Riak.Connection
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Mark Hibberd <mark@hibberd.id.au>, Nathan Hunter <nhunter@janrain.com>
-- Stability:   experimental
-- Portability: portable
--
-- Low-level network connection management.

module Network.Riak.Connection
    (
    -- * Connection management
      connect
    , disconnect
    -- * Client configuration
    , defaultClient
    , makeClientID
    -- * Requests and responses
    -- ** Sending and receiving
    , exchange
    , exchangeMaybe
    , exchange_
    -- ** Pipelining many requests
    , pipeline
    , pipelineMaybe
    , pipeline_
    , pipe, recvMaybeResponse
    ) where

import Network.Riak.Connection.Internal
