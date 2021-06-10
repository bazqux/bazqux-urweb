{-# LANGUAGE BangPatterns, ScopedTypeVariables, ViewPatterns,
             OverloadedStrings #-}
module Lib.Log
    ( nullLogger, directLogger, Logger, withLogger, logS, logBS, logLS
    , logTime, logTime'
    , getTime, logT, logTL, logLT, logLTL, showSecs, showMSecs, loggerFlush, time
    , newLogger, loggerGetAndClean
    ) where

import System.IO.Unsafe
import System.IO
-- import Criterion.Measurement
import Control.Concurrent
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import Control.Monad.Trans
import qualified Control.Exception as E
import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.Printf (printf)
import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

toText = T.decodeUtf8With (\ _ -> fmap B.w2c)

{-# NOINLINE logMVar #-}
logMVar :: MVar ()
logMVar = unsafePerformIO $ newMVar ()

-- | Строка в utf8
logS :: MonadIO m => String -> m ()
logS = logT . T.pack

logBS !s = liftIO $ withMVar logMVar $ \ _ -> do
    B.hPutStrLn stderr s
    hFlush stderr
logT :: MonadIO m => T.Text -> m ()
logT = logBS . T.encodeUtf8

logTL :: MonadIO m => [T.Text] -> m ()
logTL = logT . T.concat

data Logger
    = Logger (MVar [T.Text])
    | Null
    | Direct

nullLogger = Null
directLogger = Direct

logLS l s = logLT l (T.pack s)
logLBS l bs = logLT l (toText bs)
logLT (Logger m) !t = liftIO $ modifyMVar_ m $ \ l -> do
        let !r = t:l
        return r
logLT Direct t =
    logT t
logLT Null _ = return ()
logLTL l t = logLT l $ T.concat t

newLogger = liftIO $ fmap Logger $ newMVar []

loggerFlush (Logger m) = liftIO $ modifyMVar_ m $ \ s -> do
    when (not $ null s) $
        logT $ T.unlines $ reverse s
    return []
loggerFlush _ = return ()

loggerGetAndClean (Logger m) = liftIO $ modifyMVar m $ \ s -> do
    return ([], T.unlines $ reverse s)
loggerGetAndClean _ = return ""

withLogger = E.bracket newLogger loggerFlush

logTime Null _ a = a
logTime l what act = do
    start <- getTime
    logTime' l start what act
logTime' l start what act = do
    act `E.finally` do
        end <- getTime
        logLT l $ T.concat [what, ": ", T.pack $ showSecs $ end - start]

------------------------------------------------------------------------------
-- Скопировано из criterion (он тянет слишком много зависимостей и на
-- серваке какие-то беды с линковкой libstdc++.so)

time :: MonadIO m => m a -> m (Double, a)
time act = do
  start <- getTime
  result <- act
  end <- getTime
  let !delta = end - start
  return (delta, result)

getTime :: MonadIO m => m Double
getTime = realToFrac `liftM` liftIO getPOSIXTime

showSecs :: Double -> String
showSecs k
    | k < 0      = '-' : showSecs (-k)
    | k >= 2*24*3600 = (k/(24*3600)) `with` "days"
    | k >= 24*3600 = (k/(24*3600)) `with` "day"
    | k >= 3600  = (k/3600) `with` "hr"
    | k >= 60    = (k/60)   `with` "min"
    | k >= 1     = k        `with` "s"
    | k >= 1e-3  = (k*1e3)  `with` "ms"
    | k >= 1e-6  = (k*1e6)  `with` "us"
    | k >= 1e-9  = (k*1e9)  `with` "ns"
    | k >= 1e-12 = (k*1e12) `with` "ps"
    | otherwise  = printf "%g s" k
     where with (t :: Double) (u :: String)
               | t >= 1e9  = printf "%.4g %s" t u
               | t >= 1e6  = printf "%.0f %s" t u
               | t >= 1e5  = printf "%.1f %s" t u
               | t >= 1e4  = printf "%.2f %s" t u
               | t >= 1e3  = printf "%.3f %s" t u
               | t >= 1e2  = printf "%.4f %s" t u
               | t >= 1e1  = printf "%.5f %s" t u
               | otherwise = printf "%.6f %s" t u

showMSecs :: Double -> String
showMSecs t = show (round $ t*1000) ++ " ms"
