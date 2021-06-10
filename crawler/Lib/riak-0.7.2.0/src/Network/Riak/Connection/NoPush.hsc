{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module:      Network.Riak.Connection.NoPush
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Mark Hibberd <mark@hibberd.id.au>, Nathan Hunter <nhunter@janrain.com>
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
import Network.Socket (Socket(..), fdSocket)

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
setNoPush s onOff = do
  let v = if onOff then 1 else 0
  fds <- fdSocket s
  with v $ \ptr ->
    throwErrnoIfMinus1_ "setNoPush" $
      c_setsockopt fds (#const IPPROTO_TCP) noPush ptr (fromIntegral (sizeOf v))

foreign import ccall unsafe "setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
