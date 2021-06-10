-- |
-- Module:      Network.Riak.Value.Resolvable
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Mark Hibberd <mark@hibberd.id.au>, Nathan Hunter <nhunter@janrain.com>
-- Stability:   experimental
-- Portability: portable
--
-- This module allows storage and retrieval of data encoded using the
-- 'V.IsContent' typeclass.  This provides access to more of Riak's
-- storage features than JSON, e.g. links.
--
-- Functions automatically resolve conflicts using 'Resolvable'
-- instances.  For instance, if a 'get' returns three siblings, a
-- winner will be chosen using 'resolve'.  If a 'put' results in a
-- conflict, a winner will be chosen using 'resolve', and the winner
-- will be 'put'; this will be repeated until either no conflict
-- occurs or the process has been repeated too many times.

module Network.Riak.Value.Resolvable
    (
      V.IsContent(..)
    , Resolvable(..)
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

import Network.Riak.Resolvable.Internal (ResolutionFailure(..), Resolvable(..))
import Network.Riak.Types.Internal hiding (MessageTag(..))
import qualified Network.Riak.Resolvable.Internal as R
import qualified Network.Riak.Value as V

-- | Retrieve a single value.  If conflicting values are returned, the
-- 'Resolvable' is used to choose a winner.
get :: (Resolvable a, V.IsContent a) =>
       Connection -> Bucket -> Key -> R -> IO (Maybe (a, VClock))
get = R.get V.get
{-# INLINE get #-}

-- | Retrieve multiple values.  If conflicting values are returned for
-- a key, the 'Resolvable' is used to choose a winner.
getMany :: (Resolvable a, V.IsContent a) => Connection -> Bucket -> [Key] -> R
        -> IO [Maybe (a, VClock)]
getMany = R.getMany V.getMany
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
modify :: (Resolvable a, V.IsContent a) =>
          Connection -> Bucket -> Key -> R -> W -> DW
       -> (Maybe a -> IO (a,b))
       -- ^ Modification function.  Called with 'Just' the value if
       -- the key is present, 'Nothing' otherwise.
       -> IO (a,b)
modify = R.modify V.get V.put
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
modify_ :: (Resolvable a, V.IsContent a) =>
           Connection -> Bucket -> Key -> R -> W -> DW
        -> (Maybe a -> IO a) -> IO a
modify_ = R.modify_ V.get V.put
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
put :: (Resolvable a, V.IsContent a) =>
       Connection -> Bucket -> Key -> Maybe VClock -> a -> W -> DW
    -> IO (a, VClock)
put = R.put V.put
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
put_ :: (Resolvable a, V.IsContent a) =>
        Connection -> Bucket -> Key -> Maybe VClock -> a -> W -> DW
     -> IO ()
put_ = R.put_ V.put
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
putMany :: (Resolvable a, V.IsContent a) =>
           Connection -> Bucket -> [(Key, Maybe VClock, a)] -> W -> DW
        -> IO [(a, VClock)]
putMany = R.putMany V.putMany
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
putMany_ :: (Resolvable a, V.IsContent a) =>
            Connection -> Bucket -> [(Key, Maybe VClock, a)] -> W -> DW -> IO ()
putMany_ = R.putMany_ V.putMany
{-# INLINE putMany_ #-}
