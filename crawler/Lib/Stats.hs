{-# LANGUAGE BangPatterns #-}
-- | Счетчики статистики [ключ-счетчик]
module Lib.Stats
    ( incrStat, getStatsMap, getStatsMapAndClean, addStatsMap
    , withEkgServer, runEkgServer )
    where

import Data.Maybe
import System.IO.Unsafe
import Control.Concurrent
import Control.Monad
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import System.Remote.Monitoring
import qualified System.Remote.Counter as Counter
import qualified System.Remote.Gauge as Gauge

{-# NOINLINE statsMVar #-}
statsMVar :: MVar (HashMap T.Text Int)
statsMVar = unsafePerformIO $ newMVar HM.empty

{-# NOINLINE ekgServerVar #-}
ekgServerVar :: MVar (Maybe Server)
ekgServerVar = unsafePerformIO $ newMVar Nothing

-- | Внимание -- при не запущенном ekg-сервере выполняет default
withEkgServer :: IO a -> (Server -> IO a) -> IO a
withEkgServer def f = do
    mbs <- readMVar ekgServerVar
    case mbs of
        Just s -> f s
        Nothing -> def

runEkgServer host port = do
    s <- forkServer host port
    modifyMVar_ ekgServerVar $ \ _ -> return $ Just s

incrStat _ 0 = return ()
incrStat !key !incr = modifyMVar_ statsMVar $ \ sm -> do
    withEkgServer (return ()) $ \ s -> do
        c <- getCounter key s
        Counter.inc c
    case HM.lookup key sm of
        Just !val -> return $! HM.insert key (val+incr) sm
        Nothing -> return $! HM.insert key incr sm

getStatsMap = readMVar statsMVar
getStatsMapAndClean = modifyMVar statsMVar $ \ sm ->
    return (HM.empty, sm)

addStatsMap smAdd = modifyMVar_ statsMVar $ \ sm -> do
    withEkgServer (return ()) $ \ s -> do
        forM_ (HM.toList smAdd) $ \ (key,incr) -> do
            c <- getCounter key s
            Counter.add c incr
    return $!
        foldl
        (\ sm (key,incr) ->
             case HM.lookup key sm of
                 Just !val -> HM.insert key (val+incr) sm
                 Nothing -> HM.insert key incr sm)
        sm (HM.toList smAdd)
