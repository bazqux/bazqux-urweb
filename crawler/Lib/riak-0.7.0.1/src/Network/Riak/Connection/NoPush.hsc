{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module:      Network.Riak.Connection.NoPush
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- TCP madness.

module Network.Riak.Connection.NoPush (setNoPush) where

#include <sys/socket.h>
#include <netinet/tcp.h>
#include <netinet/in.h>

import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Foreign.Storable (sizeOf)
import Network.Socket (Socket(..))

noPush :: CInt
#if defined(TCP_NOPUSH)
-- TCP_NOPUSH is utterly fucked on OS X 10.6.  It introduces a delay
-- of about 4.5 seconds per outbound packet train. What. The. Fuck.
noPush = 0
--noPush = #const TCP_NOPUSH
#elif defined(TCP_CORK)
noPush = #const TCP_CORK
#else
noPush = 0
#endif

setNoPush :: Socket -> Bool -> IO ()
setNoPush _ _ | noPush == 0 = return ()
setNoPush (MkSocket fd _ _ _ _) onOff = do
  let v = if onOff then 1 else 0
  with v $ \ptr ->
    throwErrnoIfMinus1_ "setNoPush" $
      c_setsockopt fd (#const IPPROTO_TCP) noPush ptr (fromIntegral (sizeOf v))

foreign import ccall unsafe "setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
