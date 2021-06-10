{-# LANGUAGE CPP, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module:      Network.Riak.Connection.Internal
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Mark Hibberd <mark@hibberd.id.au>, Nathan Hunter <nhunter@janrain.com>
-- Stability:   experimental
-- Portability: portable
--
-- Low-level network connection management.

module Network.Riak.Connection.Internal
    (
    -- * Connection management
      Network.Riak.Connection.Internal.connect
    , disconnect
    , setClientID
    -- * Client configuration
    , defaultClient
    , makeClientID
    -- * Requests and responses
    -- ** Sending and receiving requests and responses
    , exchange
    , exchangeMaybe
    , exchange_
    -- ** Pipelining many requests
    , pipe
    , pipeline
    , pipelineMaybe
    , pipeline_
    -- * Low-level protocol operations
    -- ** Sending and receiving
    , sendRequest
    , recvResponse
    , recvMaybeResponse
    , recvResponse_
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (Exception, IOException, throw)
import Control.Monad (forM_, replicateM, replicateM_)
import Data.Binary.Put (Put, putWord32be, runPut)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Network.Riak.Connection.NoPush (setNoPush)
import Network.Riak.Debug as Debug
import Network.Riak.Protocol.ErrorResponse
import Network.Riak.Protocol.SetClientIDRequest
import Network.Riak.Tag (getTag, putTag)
import Network.Riak.Types.Internal hiding (MessageTag(..))
import Network.Socket as Socket
import Numeric (showHex)
import System.Mem.Weak (addFinalizer)
import System.Random (randomIO)
import Text.ProtocolBuffers (messageGetM, messagePutM, messageSize)
import Text.ProtocolBuffers.Get (Get, Result(..), getWord32be, runGet)
import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Network.Riak.Types.Internal as T
import qualified Network.Socket.ByteString as B
import qualified Network.Socket.ByteString.Lazy as L

-- | Default client configuration.  Talks to localhost, port 8087,
-- with a randomly chosen client ID.
defaultClient :: Client
defaultClient = Client {
                  host = "127.0.0.1"
                , port = "8087"
                , clientID = L.empty
                }

-- | Tell the server our client ID.
setClientID :: Connection -> ClientID -> IO ()
setClientID conn i = do
  sendRequest conn $ SetClientIDRequest i
  recvResponse_ conn T.SetClientIDResponse

-- | Generate a random client ID.
makeClientID :: IO ClientID
makeClientID = do
  r <- randomIO :: IO Int
  return . L.append "hs_" . L.pack . showHex (abs r) $ ""

-- | Add a random 'ClientID' to a 'Client' if the 'Client' doesn't
-- already have one.
addClientID :: Client -> IO Client
addClientID client
  | L.null (clientID client) = do
    i <- makeClientID
    return client { clientID = i }
  | otherwise = return client

-- | Connect to a server.
connect :: Client -> IO Connection
connect cli0 = do
  client@Client{..} <- addClientID cli0
  let hints = defaultHints {
                addrFlags = [AI_ADDRCONFIG]
              , addrSocketType = Stream
              }
  debug "connect" $ "server " ++ host ++ ":" ++ port ++ ", client ID " ++
                    L.unpack clientID
  ais <- getAddrInfo (Just hints) (Just host) (Just port)
  let ai = case ais of
             (a:_) -> a
             _     -> moduleError "connect" $
                      "could not look up server " ++ host ++ ":" ++ port
  onIOException "connect" $ do
    sock <- socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)
    Socket.connect sock (addrAddress ai)
    buf <- newIORef L.empty
    let conn = Connection sock client buf
    addFinalizer conn $ close sock  -- Data.Pool doesn't guarantee our disconnect gets called...
    setClientID conn clientID
    return conn

-- | Disconnect from a server.
disconnect :: Connection -> IO ()
disconnect Connection{..} = onIOException "disconnect" $ do
  debug "disconnect" $ "server " ++ host connClient ++ ":" ++ port connClient ++
                       ", client ID " ++ L.unpack (clientID connClient)
  close connSock
  writeIORef connBuffer L.empty

-- | We use a larger receive buffer than we usually need, and
-- generally ask to receive more data than we know we'll need, in the
-- hope that we'll be able to buffer some of it and avoid future recv
-- system calls.
recvBufferSize :: Integral a => a
recvBufferSize = 16384
{-# INLINE recvBufferSize #-}

recvExactly :: Connection -> Int64 -> IO L.ByteString
recvExactly Connection{..} n0
    | n0 <= 0 = return L.empty
    | otherwise = do
  bs <- readIORef connBuffer
  let (h,t) = L.splitAt n0 bs
      len = L.length h
  if len == n0
    then writeIORef connBuffer t >> return h
    else go (reverse (L.toChunks h)) (n0-len)
  where
    maxInt = fromIntegral (maxBound :: Int)
    go (s:acc) n
      | n < 0 = do
        let (h,t) = B.splitAt (B.length s + fromIntegral n) s
        writeIORef connBuffer $! L.fromChunks [t]
        return $ L.fromChunks (reverse (h:acc))
    go acc n
      | n == 0 = do
        writeIORef connBuffer L.empty
        return $ L.fromChunks (reverse acc)
      | otherwise = do
        let n' = max recvBufferSize $ min n maxInt
        bs <- B.recv connSock (fromIntegral n')
        let len = B.length bs
        if len == 0
          then moduleError "recvExactly" "short read from network"
          else go (bs:acc) (n - fromIntegral len)

recvGet :: Connection -> Get a -> IO a
recvGet Connection{..} get = do
  let refill = do
        bs <- L.recv connSock recvBufferSize
        if L.null bs
          then shutdown connSock ShutdownReceive >> return Nothing
          else return (Just bs)
      step (Failed _ err)    = moduleError "recvGet" err
      step (Finished bs _ r) = writeIORef connBuffer bs >> return r
      step (Partial k)       = (step . k) =<< refill
  mbs <- do
    buf <- readIORef connBuffer
    if L.null buf
      then refill
      else return (Just buf)
  case mbs of
    Just bs -> step $ runGet get bs
    Nothing -> moduleError "recvGet" "socket closed"

recvGetN :: Connection -> Int64 -> Get a -> IO a
recvGetN conn n get = do
  bs <- recvExactly conn n
  case runGet get bs of
    Finished _ _ r -> return r
    Partial k    -> case k Nothing of
                      Finished _ _ r -> return r
                      Failed _ err -> moduleError "recvGetN" err
                      Partial _    -> moduleError "recvGetN"
                                      "parser wants more input!?"
    Failed _ err -> moduleError "recvGetN" err

putRequest :: (Request req) => req -> Put
putRequest req = do
  putWord32be (fromIntegral (1 + messageSize req))
  putTag (messageTag req)
  messagePutM req

instance Exception ErrorResponse

throwError :: ErrorResponse -> a
throwError = throw

getResponse :: (Response a) => T.MessageTag -> Get a
getResponse expected = do
  tag <- getTag
  case undefined of
   _| tag == expected        -> messageGetM
    | tag == T.ErrorResponse -> throwError `fmap` messageGetM
    | otherwise ->
        moduleError "getResponse" $ "received unexpected response: expected " ++
                                    show expected ++ ", received " ++ show tag

-- | Send a request to the server, and receive its response.
exchange :: Exchange req resp => Connection -> req -> IO resp
exchange conn@Connection{..} req = do
  debug "exchange" $ ">>> " ++ showM req
  onIOException ("exchange " ++ show (messageTag req)) $ do
    sendRequest conn req
    recvResponse conn

-- | Send a request to the server, and receive its response (which may
-- be empty).
exchangeMaybe :: Exchange req resp => Connection -> req -> IO (Maybe resp)
exchangeMaybe conn@Connection{..} req = do
  debug "exchangeMaybe" $ ">>> " ++ showM req
  onIOException ("exchangeMaybe " ++ show (messageTag req)) $ do
    sendRequest conn req
    recvMaybeResponse conn

-- | Send a request to the server, and receive its response, but do
-- not decode it.
exchange_ :: Request req => Connection -> req -> IO ()
exchange_ conn req = do
  debug "exchange_" $ ">>> " ++ showM req
  onIOException ("exchange_ " ++ show (messageTag req)) $ do
    sendRequest conn req
    recvResponse_ conn (expectedResponse req)

sendAll :: Socket -> L.ByteString -> IO ()
sendAll sock bs = do
  setNoPush sock True
  L.sendAll sock bs
  setNoPush sock False

sendRequest :: (Request req) => Connection -> req -> IO ()
sendRequest Connection{..} = sendAll connSock . runPut . putRequest

recvResponse :: (Response a) => Connection -> IO a
recvResponse conn = debugRecv showM $ go undefined where
  go :: Response b => b -> IO b
  go dummy = do
    len <- fromIntegral `fmap` recvGet conn getWord32be
    recvGetN conn len (getResponse (messageTag dummy))

recvResponse_ :: Connection -> T.MessageTag -> IO ()
recvResponse_ conn expected = debugRecv show $ do
  len <- fromIntegral `fmap` recvGet conn getWord32be
  recvCorrectTag "recvResponse_" conn expected (len-1) ()

recvMaybeResponse :: (Response a) => Connection -> IO (Maybe a)
recvMaybeResponse conn = debugRecv (maybe "Nothing" (("Just " ++) . showM)) $
                         go undefined where
  go :: Response b => b -> IO (Maybe b)
  go dummy = do
    len <- fromIntegral `fmap` recvGet conn getWord32be
    let tag = messageTag dummy
    if len == 1
      then recvCorrectTag "recvMaybeResponse" conn tag 1 Nothing
      else Just `fmap` recvGetN conn len (getResponse tag)

recvCorrectTag :: String -> Connection -> T.MessageTag -> Int64 -> a -> IO a
recvCorrectTag func conn expected len v = do
  tag <- recvGet conn getTag
  case undefined of
   _| tag == expected -> recvExactly conn (len-1) >> return v
    | tag == T.ErrorResponse -> throwError `fmap` recvGetN conn len messageGetM
    | otherwise -> moduleError func $
                   "received unexpected response: expected " ++
                   show expected ++ ", received " ++ show tag

debugRecv :: (a -> String) -> IO a -> IO a
#ifdef DEBUG
debugRecv f act = do
  r <- act
  debug "recv" $ "<<< " ++ f r
  return r
#else
debugRecv _ act = act
{-# INLINE debugRecv #-}
#endif

pipe :: (Request req, Show resp) =>
        (Connection -> IO resp) -> Connection -> [req] -> IO [resp]
pipe _ _ [] = return []
pipe receive conn@Connection{..} reqs = do
  ch <- newChan
  let numReqs = length reqs
  _ <- forkIO . replicateM_ numReqs $ writeChan ch =<< receive conn
  let tag = show (messageTag (head reqs))
  if Debug.level > 1
    then forM_ reqs $ \req -> debug "pipe" $ ">>> " ++ showM req
    else debug "pipe" $ ">>> " ++ show numReqs ++ "x " ++ tag
  onIOException ("pipe " ++ tag) .
    sendAll connSock . runPut . mapM_ putRequest $ reqs
  replicateM numReqs $ readChan ch

-- | Send a series of requests to the server, back to back, and
-- receive a response for each request sent.  The sending and
-- receiving will be overlapped if possible, to improve concurrency
-- and reduce latency.
pipeline :: (Exchange req resp) => Connection -> [req] -> IO [resp]
pipeline = pipe recvResponse

-- | Send a series of requests to the server, back to back, and
-- receive a response for each request sent (the responses may be
-- empty).  The sending and receiving will be overlapped if possible,
-- to improve concurrency and reduce latency.
pipelineMaybe :: (Exchange req resp) => Connection -> [req] -> IO [Maybe resp]
pipelineMaybe = pipe recvMaybeResponse

-- | Send a series of requests to the server, back to back, and
-- receive (but do not decode) a response for each request sent.  The
-- sending and receiving will be overlapped if possible, to improve
-- concurrency and reduce latency.
pipeline_ :: (Request req) => Connection -> [req] -> IO ()
pipeline_ _ [] = return ()
pipeline_ conn@Connection{..} reqs = do
  done <- newEmptyMVar
  _ <- forkIO $ do
         forM_ reqs (recvResponse_ conn . expectedResponse)
         putMVar done ()
  if Debug.level > 1
    then forM_ reqs $ \req -> debug "pipe" $ ">>> " ++ showM req
    else debug "pipe" $ ">>> " ++ show (length reqs) ++ "x " ++
                        show (messageTag (head reqs))
  sendAll connSock . runPut . mapM_ putRequest $ reqs
  takeMVar done

onIOException :: String -> IO a -> IO a
onIOException func act =
    act `E.catch` \(e::IOException) -> do
      let s = show e
      debug func $ "caught IO exception: " ++ s
      moduleError func s

moduleError :: String -> String -> a
moduleError = netError "Network.Riak.Connection.Internal"
