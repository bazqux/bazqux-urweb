-- |
-- Module:      Network.Riak.Types
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Mark Hibberd <mark@hibberd.id.au>, Nathan Hunter <nhunter@janrain.com>
-- Stability:   experimental
-- Portability: portable
--
-- Basic types.

module Network.Riak.Types
    (
    -- * Client management
      ClientID
    , Client(..)
    -- * Connection management
    , Connection(connClient)
    -- * Errors
    , RiakException(excModule, excFunction, excMessage)
    -- * Data types
    , Bucket
    , Key
    , Tag
    , VClock(..)
    , Job(..)
    -- * Quorum management
    , Quorum(..)
    , RW
    , R
    , W
    , DW
    -- * Message identification
    , Request
    , Response
    , Exchange
    , MessageTag(..)
    , Tagged(..)
    ) where

import Network.Riak.Types.Internal
