{-# LANGUAGE BangPatterns, ViewPatterns #-}
-- | Счетчики статистики [ключ-счетчик]
module Lib.Stats
    ( incrStat, getStatsMap, getStatsMapAndClean, addStatsMap, getGauge
    , withEkgServer, runEkgServer )
    where

import Data.Int
import System.IO.Unsafe
import Control.Concurrent
import Control.Monad
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import System.Remote.Monitoring hiding (getCounter, getGauge)
import qualified System.Remote.Monitoring as M
import qualified System.Remote.Counter as Counter
import qualified System.Remote.Gauge as Gauge

{-# NOINLINE statsMVar #-}
statsMVar :: MVar (HashMap T.Text Int64)
statsMVar = unsafePerformIO $ newMVar HM.empty

{-# NOINLINE ekgServerVar #-}
ekgServerVar :: MVar (Maybe (Server, HashMap T.Text Counter.Counter, HashMap T.Text Gauge.Gauge))
ekgServerVar = unsafePerformIO $ newMVar Nothing

getCounter name = modifyMVar ekgServerVar $ \ mbs ->
    case mbs of
        Nothing -> return (Nothing, error "no ekg server registered")
        Just (s,cs,gs)
            | Just c <- HM.lookup name cs ->
                return (mbs, c)
            | otherwise -> do
                c <- M.getCounter name s
                return (Just (s, HM.insert name c cs, gs), c)

getGauge name = modifyMVar ekgServerVar $ \ mbs ->
    case mbs of
        Nothing -> return (Nothing, error "no ekg server registered")
        Just (s,cs,gs)
            | Just g <- HM.lookup name gs ->
                return (mbs, g)
            | otherwise -> do
                g <- M.getGauge name s
                return (Just (s, cs, HM.insert name g gs), g)


-- | Внимание -- при не запущенном ekg-сервере выполняет default
withEkgServer :: IO a -> IO a -> IO a
withEkgServer def f = do
    mbs <- readMVar ekgServerVar
    case mbs of
        Just s -> f
        Nothing -> def

runEkgServer host port = do
    s <- forkServer host port
    modifyMVar_ ekgServerVar $ \ _ -> return $ Just (s, HM.empty, HM.empty)

incrStat _ 0 = return ()
incrStat !key (toEnum -> !incr) = modifyMVar_ statsMVar $ \ sm -> do
    withEkgServer (return ()) $ do
        c <- getCounter key
        Counter.inc c
    case HM.lookup key sm of
        Just !val -> return $! HM.insert key (val+incr) sm
        Nothing -> return $! HM.insert key incr sm

getStatsMap = readMVar statsMVar
getStatsMapAndClean = modifyMVar statsMVar $ \ sm ->
    return (HM.empty, sm)

addStatsMap smAdd = modifyMVar_ statsMVar $ \ sm -> do
    withEkgServer (return ()) $ do
        forM_ (HM.toList smAdd) $ \ (key,incr) -> do
            c <- getCounter key
            Counter.add c incr
    return $!
        foldl
        (\ sm (key,incr) ->
             case HM.lookup key sm of
                 Just !val -> HM.insert key (val+incr) sm
                 Nothing -> HM.insert key incr sm)
        sm (HM.toList smAdd)
