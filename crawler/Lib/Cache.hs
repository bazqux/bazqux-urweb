{-# LANGUAGE BangPatterns, ViewPatterns #-}
module Lib.Cache
    ( Cache, new, insert, lookup, cached )
    where

import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as HM
import Lib.UrTime
import Lib.Queue
import Control.Concurrent

data Cache' k v
    = Cache
      { cValues :: !(HM.HashMap k v)
      , cQueue :: !(Queue (UrTime, k))
      , cExpires :: !Double
      }

type Cache k v = MVar (Cache' k v)

new :: Double -> IO (Cache k v)
new expires = newMVar $ Cache HM.empty emptyQ expires

insert k v c = do
    t <- getUrTime
    modifyMVar_ c $ \ (cleanCache t -> c) ->
        return $!
            Cache
            { cValues = HM.insert k v $ cValues c
            , cQueue = pushQ (t `plusUrTime` cExpires c, k) (cQueue c)
            , cExpires = cExpires c
            }

lookup k c = do
    t <- getUrTime
    modifyMVar c $ \ (cleanCache t -> c) ->
        return (c, HM.lookup k $ cValues c)

cleanCache t c
    | Just ((et,k), q') <- tryPopQ (cQueue c)
    , et < t =
        cleanCache t $
        Cache
        { cValues = HM.delete k $ cValues c
        , cQueue = q'
        , cExpires = cExpires c
        }
    | otherwise = c

cached c f k = do
    mbv <- lookup k c
    case mbv of
        Just v -> return v
        Nothing -> do
            v <- f k
            insert k v c
            return v

test = do
    c <- new 1 :: IO (Cache Int Int)
    insert 1 1 c
    insert 2 2 c
    print =<< lookup 1 c
    print =<< lookup 2 c
    threadDelay 1000000
    print =<< lookup 1 c
    print =<< lookup 2 c
