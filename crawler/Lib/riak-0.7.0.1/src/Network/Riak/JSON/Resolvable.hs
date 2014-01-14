-- |
-- Module:      Network.Riak.JSON.Resolvable
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- This module allows storage and retrieval of JSON-encoded data.
--
-- Functions automatically resolve conflicts using 'Resolvable'
-- instances.  For instance, if a 'get' returns three siblings, a
-- winner will be chosen using 'resolve'.  If a 'put' results in a
-- conflict, a winner will be chosen using 'resolve', and the winner
-- will be 'put'; this will be repeated until either no conflict
-- occurs or the process has been repeated too many times.

module Network.Riak.JSON.Resolvable
    (
      Resolvable(..)
    , ResolutionFailure(..)
    , get
    , getMany
    , modify
    , modify_
    -- * Low-level modification functions
    , put
    , put_
    , putMany
    , putMany_
    ) where

import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Network.Riak.Resolvable.Internal (ResolutionFailure(..), Resolvable(..))
import Network.Riak.Types.Internal hiding (MessageTag(..))
import qualified Network.Riak.JSON as J
import qualified Network.Riak.Resolvable.Internal as R

-- | Retrieve a single value.  If conflicting values are returned,
-- 'resolve' is used to choose a winner.
get :: (FromJSON c, ToJSON c, Resolvable c) =>
       Connection -> Bucket -> Key -> R -> IO (Maybe (c, VClock))
get = R.get J.get
{-# INLINE get #-}

-- | Retrieve multiple values.  If conflicting values are returned for
-- a key, 'resolve' is used to choose a winner.
getMany :: (FromJSON c, ToJSON c, Resolvable c)
           => Connection -> Bucket -> [Key] -> R -> IO [Maybe (c, VClock)]
getMany = R.getMany J.getMany
{-# INLINE getMany #-}

-- | Modify a single value.  The value, if any, is retrieved using
-- 'get'; conflict resolution is performed if necessary.  The
-- modification function is called on the resulting value, and its
-- result is stored using 'put', which may again perform conflict
-- resolution.
--
-- The result of this function is whatever was returned by 'put',
-- along with the auxiliary value returned by the modification
-- function.
--
-- If the 'put' phase of this function gives up due to apparently
-- being stuck in a conflict resolution loop, it will throw a
-- 'ResolutionFailure' exception.
modify :: (FromJSON a, ToJSON a, Resolvable a) =>
          Connection -> Bucket -> Key -> R -> W -> DW
       -> (Maybe a -> IO (a,b))
       -- ^ Modification function.  Called with 'Just' the value if
       -- the key is present, 'Nothing' otherwise.
       -> IO (a,b)
modify = R.modify J.get J.put
{-# INLINE modify #-}

-- | Modify a single value.  The value, if any, is retrieved using
-- 'get'; conflict resolution is performed if necessary.  The
-- modification function is called on the resulting value, and its
-- result is stored using 'put', which may again perform conflict
-- resolution.
--
-- The result of this function is whatever was returned by 'put'.
--
-- If the 'put' phase of this function gives up due to apparently
-- being stuck in a conflict resolution loop, it will throw a
-- 'ResolutionFailure' exception.
modify_ :: (FromJSON a, ToJSON a, Resolvable a) =>
           Connection -> Bucket -> Key -> R -> W -> DW
        -> (Maybe a -> IO a) -> IO a
modify_ = R.modify_ J.get J.put
{-# INLINE modify_ #-}

-- | Store a single value, automatically resolving any vector clock
-- conflicts that arise.  A single invocation of this function may
-- involve several roundtrips to the server to resolve conflicts.
--
-- If a conflict arises, a winner will be chosen using 'resolve', and
-- the winner will be stored; this will be repeated until no conflict
-- occurs or a (fairly large) number of retries has been attempted
-- without success.
--
-- If this function gives up due to apparently being stuck in a
-- conflict resolution loop, it will throw a 'ResolutionFailure'
-- exception.
put :: (FromJSON c, ToJSON c, Resolvable c) =>
       Connection -> Bucket -> Key -> Maybe VClock -> c -> W -> DW
    -> IO (c, VClock)
put = R.put J.put
{-# INLINE put #-}

-- | Store a single value, automatically resolving any vector clock
-- conflicts that arise.  A single invocation of this function may
-- involve several roundtrips to the server to resolve conflicts.
--
-- If a conflict arises, a winner will be chosen using 'resolve', and
-- the winner will be stored; this will be repeated until no conflict
-- occurs or a (fairly large) number of retries has been attempted
-- without success.
--
-- If this function gives up due to apparently being stuck in a
-- conflict resolution loop, it will throw a 'ResolutionFailure'
-- exception.
put_ :: (FromJSON c, ToJSON c, Resolvable c) =>
        Connection -> Bucket -> Key -> Maybe VClock -> c -> W -> DW
     -> IO ()
put_ = R.put_ J.put
{-# INLINE put_ #-}

-- | Store multiple values, resolving any vector clock conflicts that
-- arise.  A single invocation of this function may involve several
-- roundtrips to the server to resolve conflicts.
--
-- If any conflicts arise, a winner will be chosen in each case using
-- 'resolve', and the winners will be stored; this will be repeated
-- until either no conflicts occur or a (fairly large) number of
-- retries has been attempted without success.
--
-- For each original value to be stored, the final value that was
-- stored at the end of any conflict resolution is returned.
--
-- If this function gives up due to apparently being stuck in a loop,
-- it will throw a 'ResolutionFailure' exception.
putMany :: (FromJSON c, ToJSON c, Resolvable c) =>
           Connection -> Bucket -> [(Key, Maybe VClock, c)] -> W -> DW
        -> IO [(c, VClock)]
putMany = R.putMany J.putMany
{-# INLINE putMany #-}

-- | Store multiple values, resolving any vector clock conflicts that
-- arise.  A single invocation of this function may involve several
-- roundtrips to the server to resolve conflicts.
--
-- If any conflicts arise, a winner will be chosen in each case using
-- 'resolve', and the winners will be stored; this will be repeated
-- until either no conflicts occur or a (fairly large) number of
-- retries has been attempted without success.
--
-- If this function gives up due to apparently being stuck in a loop,
-- it will throw a 'ResolutionFailure' exception.
putMany_ :: (FromJSON c, ToJSON c, Resolvable c) =>
            Connection -> Bucket -> [(Key, Maybe VClock, c)] -> W -> DW
         -> IO ()
putMany_ = R.putMany_ J.putMany
{-# INLINE putMany_ #-}
