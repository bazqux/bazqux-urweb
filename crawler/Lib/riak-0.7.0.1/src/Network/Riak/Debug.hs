{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- |
-- Module:      Network.Riak.Debug
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Support for debug logging.  The code in this package only works if
-- the package was built with the @-fdebug@ flag.  Otherwise, they are
-- all no-ops.

module Network.Riak.Debug
    (
      level
    , debug
    , debugValues
    , setHandle
    , showM
    ) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, withMVar)
import Control.Exception hiding (handle)
import Control.Monad (forM_, when)
import Network.Riak.Types.Internal
import System.Environment (getEnv)
import System.IO (Handle, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

-- | The current debugging level.  This is established once by reading
-- the @RIAK_DEBUG@ environment variable.
level :: Int
#ifdef DEBUG
level = unsafePerformIO $ do
          es <- try $ getEnv "RIAK_DEBUG"
          case es of
            Left (_::SomeException)   -> return 0
            Right "on" -> return 1
            Right s    -> case reads s of
                            ((n,_):_) -> return n
                            _         -> return 1
{-# NOINLINE level #-}
#else
level = 0
{-# INLINE level #-}
#endif

#ifdef DEBUG
handle :: MVar Handle
handle = unsafePerformIO $ newMVar stderr
{-# NOINLINE handle #-}
#endif

-- | Set the 'Handle' to log to ('stderr' is the default).
setHandle :: Handle -> IO ()
#ifdef DEBUG
setHandle = modifyMVar_ handle . const . return
#else
setHandle _ = return ()
{-# INLINE setHandle #-}
#endif

-- | Print a debug message, if debugging is enabled.
debug :: String                 -- ^ Function name.
      -> String                 -- ^ Debug message.
      -> IO ()
#ifdef DEBUG
debug func str
    | level == 0 = return ()
    | otherwise  =
  withMVar handle $ \h -> hPutStrLn h $ str ++ " [" ++ func ++ "]"
#else
debug _ _ = return ()
{-# INLINE debug #-}
#endif

-- | Print a debug message, and information about some values.  If the
-- debug level is greater than 1, print the values themselves.
debugValues :: (Show a) =>
               String
            -> String
            -> [a]
            -> IO ()
debugValues func str values
#ifdef DEBUG
    | level == 0 = return ()
    | otherwise = 
  withMVar handle $ \h -> do
    hPutStrLn h $ str ++ ": " ++ show (length values) ++
                  " values [" ++ func ++ "]"
    when (level > 1) .
      forM_ (zip [(0::Int)..] values) $ \(i,v) ->
        hPutStrLn h $ "  [" ++ show i ++ "] " ++ show v
#else
debugValues _ _ _ = return ()
{-# INLINE debugValues #-}
#endif

-- | Show a 'Tagged' value.  Show the entire value if the debug level
-- is above 1, just the tag otherwise.
showM :: (Show a, Tagged a) => a -> String
showM m | level > 1 = show m
        | otherwise = show (messageTag m)
