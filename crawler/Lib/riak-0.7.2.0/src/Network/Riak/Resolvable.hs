-- |
-- Module:      Network.Riak.Resolvable
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Mark Hibberd <mark@hibberd.id.au>, Nathan Hunter <nhunter@janrain.com>
-- Stability:   experimental
-- Portability: portable
--
-- Storage and retrieval of data with automatic conflict resolution.

module Network.Riak.Resolvable
    (
      Resolvable(..)
    , ResolvableMonoid(..)
    , ResolutionFailure(..)
    ) where

import Network.Riak.Resolvable.Internal
    (ResolutionFailure(..), Resolvable(..), ResolvableMonoid(..))
