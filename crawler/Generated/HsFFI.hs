{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Generated.HsFFI where
import Generated.DataTypes
import Generated.RiakIO
import URL
import Lib.UrTime
import Lib.Log
import Lib.Stats

import UrCalls
import OPML
import Session
import Account
import Payments
import Preprocess
import Mailer
import Feedback
import Search
import UsageFlags
import Discovery (searchSubscriptions)
import APIServer
import Auth
import Data.Binary
import Data.List
import Text.HTML.TagSoup.Fast (escapeHtmlT)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Map as Map
import qualified Control.Exception as E
import Control.Concurrent
import System.IO.Unsafe
import System.IO
import System.Timeout
import Foreign hiding (unsafePerformIO)
import Foreign.C.Types

import Lib.BinaryInstances

type Ctx = Ptr ()

retMap :: MVar (Map.Map Ctx B.ByteString)
retMap = unsafePerformIO $ newMVar Map.empty
{-# NOINLINE retMap #-}
saveBSretPtr ctx bs = do
    modifyMVar_ retMap $ \ m -> do
        let r = Map.insert ctx bs m
            s = Map.size r
        --print s
        --hFlush stdout
        s `seq` return r
    -- сохраняем строку, дабы сборщик ее не убил 
    withForeignPtr fp $ \ p -> return (p `plusPtr` s)
    where (fp, s, l) = B.toForeignPtr bs
peekArg :: Binary a => Ptr () -> IO a
peekArg ptr = do
    len <- peek (ptr `plusPtr` (-8)) :: IO Int
    s <- B.create len (\ dst -> B.memcpy dst (castPtr ptr) (toEnum len))
    --print ("peekArg", s)
    return $ decode $ BL.fromChunks [s]
stripUserError :: String -> String
stripUserError s
    | isPrefixOf prefix s && isSuffixOf ")" s =
        drop (length prefix) $ init s
    | otherwise = s
    where prefix = "user error ("
retErr :: String -> Ctx -> String
       -> Ptr CLong -> IO (Ptr ())
retErr func ctx err0 pLen = do
    logS $ "Exception: " ++ err
    incrStat "exceptions" 1
    poke pLen (toEnum $ 0 - B.length bs)
    saveBSretPtr ctx bs
    where err = func ++ ": " ++ err0
          bs = T.encodeUtf8 $ T.pack err
ret :: String -> Ctx -> Either E.SomeException (Maybe B.ByteString) 
    -> Ptr CLong -> IO (Ptr ())
ret func ctx (Left ex) pLen = do
    retErr func ctx (stripUserError $ show ex) pLen
ret func ctx (Right Nothing) pLen = do
    incrStat "timeouts" 1
    retErr func ctx "Timeout" pLen
ret _ ctx (Right (Just bs)) pLen = do
    poke pLen (toEnum $ B.length bs)
    saveBSretPtr ctx bs
enc a = B.concat $ BL.toChunks $ encode a 

 
foreign export ccall uw_HsFFI_readUserSettings :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_readUserSettings ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- readUserSettings h1 :: IO (Maybe UserSettings)
        let bs = enc r
        return $! bs
    ret "readUserSettings" ctx r pLen

foreign export ccall uw_HsFFI_cachedReadUserSettings :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedReadUserSettings ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- cachedReadUserSettings h1 :: IO (Maybe UserSettings)
        let bs = enc r
        return $! bs
    ret "cachedReadUserSettings" ctx r pLen

foreign export ccall uw_HsFFI_cachedNothingReadUserSettings :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedNothingReadUserSettings ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- cachedNothingReadUserSettings h1 :: IO (Maybe UserSettings)
        let bs = enc r
        return $! bs
    ret "cachedNothingReadUserSettings" ctx r pLen

foreign export ccall uw_HsFFI_mergeWriteUserSettings :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_mergeWriteUserSettings ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (UserSettings)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- mergeWriteUserSettings h1 :: IO (())
        let bs = enc r
        return $! bs
    ret "mergeWriteUserSettings" ctx r pLen

foreign export ccall uw_HsFFI_deleteUserSettings :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_deleteUserSettings ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (UserSettings)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- deleteUserSettings h1 :: IO (())
        let bs = enc r
        return $! bs
    ret "deleteUserSettings" ctx r pLen

foreign export ccall uw_HsFFI_readManyUserSettingss :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_readManyUserSettingss ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([T.Text])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- readManyUserSettingss h1 :: IO ([Maybe UserSettings])
        let bs = enc r
        return $! bs
    ret "readManyUserSettingss" ctx r pLen

foreign export ccall uw_HsFFI_cachedReadManyUserSettingss :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedReadManyUserSettingss ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([T.Text])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- cachedReadManyUserSettingss h1 :: IO ([Maybe UserSettings])
        let bs = enc r
        return $! bs
    ret "cachedReadManyUserSettingss" ctx r pLen

foreign export ccall uw_HsFFI_cachedNothingReadManyUserSettingss :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedNothingReadManyUserSettingss ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([T.Text])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- cachedNothingReadManyUserSettingss h1 :: IO ([Maybe UserSettings])
        let bs = enc r
        return $! bs
    ret "cachedNothingReadManyUserSettingss" ctx r pLen

foreign export ccall uw_HsFFI_writeManyUserSettingss :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_writeManyUserSettingss ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([UserSettings])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- writeManyUserSettingss h1 :: IO (())
        let bs = enc r
        return $! bs
    ret "writeManyUserSettingss" ctx r pLen

foreign export ccall uw_HsFFI_readSession :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_readSession ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- readSession h1 :: IO (Maybe Session)
        let bs = enc r
        return $! bs
    ret "readSession" ctx r pLen

foreign export ccall uw_HsFFI_cachedReadSession :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedReadSession ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- cachedReadSession h1 :: IO (Maybe Session)
        let bs = enc r
        return $! bs
    ret "cachedReadSession" ctx r pLen

foreign export ccall uw_HsFFI_cachedNothingReadSession :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedNothingReadSession ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- cachedNothingReadSession h1 :: IO (Maybe Session)
        let bs = enc r
        return $! bs
    ret "cachedNothingReadSession" ctx r pLen

foreign export ccall uw_HsFFI_mergeWriteSession :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_mergeWriteSession ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (Session)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- mergeWriteSession h1 :: IO (())
        let bs = enc r
        return $! bs
    ret "mergeWriteSession" ctx r pLen

foreign export ccall uw_HsFFI_deleteSession :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_deleteSession ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (Session)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- deleteSession h1 :: IO (())
        let bs = enc r
        return $! bs
    ret "deleteSession" ctx r pLen

foreign export ccall uw_HsFFI_readManySessions :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_readManySessions ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([T.Text])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- readManySessions h1 :: IO ([Maybe Session])
        let bs = enc r
        return $! bs
    ret "readManySessions" ctx r pLen

foreign export ccall uw_HsFFI_cachedReadManySessions :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedReadManySessions ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([T.Text])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- cachedReadManySessions h1 :: IO ([Maybe Session])
        let bs = enc r
        return $! bs
    ret "cachedReadManySessions" ctx r pLen

foreign export ccall uw_HsFFI_cachedNothingReadManySessions :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedNothingReadManySessions ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([T.Text])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- cachedNothingReadManySessions h1 :: IO ([Maybe Session])
        let bs = enc r
        return $! bs
    ret "cachedNothingReadManySessions" ctx r pLen

foreign export ccall uw_HsFFI_writeManySessions :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_writeManySessions ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([Session])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- writeManySessions h1 :: IO (())
        let bs = enc r
        return $! bs
    ret "writeManySessions" ctx r pLen

foreign export ccall uw_HsFFI_readMsg :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_readMsg ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (MsgKey)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- readMsg h1 :: IO (Maybe Msg)
        let bs = enc r
        return $! bs
    ret "readMsg" ctx r pLen

foreign export ccall uw_HsFFI_cachedReadMsg :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedReadMsg ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (MsgKey)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- cachedReadMsg h1 :: IO (Maybe Msg)
        let bs = enc r
        return $! bs
    ret "cachedReadMsg" ctx r pLen

foreign export ccall uw_HsFFI_cachedNothingReadMsg :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedNothingReadMsg ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (MsgKey)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- cachedNothingReadMsg h1 :: IO (Maybe Msg)
        let bs = enc r
        return $! bs
    ret "cachedNothingReadMsg" ctx r pLen

foreign export ccall uw_HsFFI_mergeWriteMsg :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_mergeWriteMsg ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (Msg)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- mergeWriteMsg h1 :: IO (())
        let bs = enc r
        return $! bs
    ret "mergeWriteMsg" ctx r pLen

foreign export ccall uw_HsFFI_deleteMsg :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_deleteMsg ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (Msg)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- deleteMsg h1 :: IO (())
        let bs = enc r
        return $! bs
    ret "deleteMsg" ctx r pLen

foreign export ccall uw_HsFFI_readManyMsgs :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_readManyMsgs ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([MsgKey])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- readManyMsgs h1 :: IO ([Maybe Msg])
        let bs = enc r
        return $! bs
    ret "readManyMsgs" ctx r pLen

foreign export ccall uw_HsFFI_cachedReadManyMsgs :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedReadManyMsgs ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([MsgKey])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- cachedReadManyMsgs h1 :: IO ([Maybe Msg])
        let bs = enc r
        return $! bs
    ret "cachedReadManyMsgs" ctx r pLen

foreign export ccall uw_HsFFI_cachedNothingReadManyMsgs :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedNothingReadManyMsgs ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([MsgKey])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- cachedNothingReadManyMsgs h1 :: IO ([Maybe Msg])
        let bs = enc r
        return $! bs
    ret "cachedNothingReadManyMsgs" ctx r pLen

foreign export ccall uw_HsFFI_writeManyMsgs :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_writeManyMsgs ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([Msg])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- writeManyMsgs h1 :: IO (())
        let bs = enc r
        return $! bs
    ret "writeManyMsgs" ctx r pLen

foreign export ccall uw_HsFFI_loginGetForwardUrl :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_loginGetForwardUrl ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (ExternalLoginType)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (ExternalLoginAction)
    h4 <- peekArg x4 :: IO (TURL)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- loginGetForwardUrl h1 h2 h3 h4 :: IO (TURL)
        let bs = enc r
        return $! bs
    ret "loginGetForwardUrl" ctx r pLen

foreign export ccall uw_HsFFI_loginCallback :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_loginCallback ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (ExternalLoginType)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (TURL)
    h4 <- peekArg x4 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- loginCallback h1 h2 h3 h4 :: IO ((LoginType, LoginAccessToken, ExternalLoginAction, Maybe T.Text))
        let bs = enc r
        return $! bs
    ret "loginCallback" ctx r pLen

foreign export ccall uw_HsFFI_userSubscribe :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userSubscribe ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (Maybe T.Text)
    h4 <- peekArg x4 :: IO ([T.Text])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- userSubscribe h1 h2 h3 h4 :: IO (T.Text)
        let bs = enc r
        return $! bs
    ret "userSubscribe" ctx r pLen

foreign export ccall uw_HsFFI_userDiscoverySubscribe :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userDiscoverySubscribe ctx pLen  x1 x2 x3 x4 x5 x6 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    h4 <- peekArg x4 :: IO (T.Text)
    h5 <- peekArg x5 :: IO (Maybe T.Text)
    h6 <- peekArg x6 :: IO ([T.Text])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- userDiscoverySubscribe h1 h2 h3 h4 h5 h6 :: IO (T.Text)
        let bs = enc r
        return $! bs
    ret "userDiscoverySubscribe" ctx r pLen

foreign export ccall uw_HsFFI_userRenameSubscription :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userRenameSubscription ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- userRenameSubscription h1 h2 h3 :: IO (())
        let bs = enc r
        return $! bs
    ret "userRenameSubscription" ctx r pLen

foreign export ccall uw_HsFFI_userRenameFolder :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userRenameFolder ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- userRenameFolder h1 h2 h3 :: IO (T.Text)
        let bs = enc r
        return $! bs
    ret "userRenameFolder" ctx r pLen

foreign export ccall uw_HsFFI_userEditSubscriptionFolders :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userEditSubscriptionFolders ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    h4 <- peekArg x4 :: IO (Bool)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- userEditSubscriptionFolders h1 h2 h3 h4 :: IO (())
        let bs = enc r
        return $! bs
    ret "userEditSubscriptionFolders" ctx r pLen

foreign export ccall uw_HsFFI_userUnsubscribe :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userUnsubscribe ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO ([T.Text])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- userUnsubscribe h1 h2 :: IO (())
        let bs = enc r
        return $! bs
    ret "userUnsubscribe" ctx r pLen

foreign export ccall uw_HsFFI_userRetrySubscription :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userRetrySubscription ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- userRetrySubscription h1 h2 :: IO (())
        let bs = enc r
        return $! bs
    ret "userRetrySubscription" ctx r pLen

foreign export ccall uw_HsFFI_deleteFilter :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_deleteFilter ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (Int)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- deleteFilter h1 h2 :: IO (())
        let bs = enc r
        return $! bs
    ret "deleteFilter" ctx r pLen

foreign export ccall uw_HsFFI_deleteSmartStream :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_deleteSmartStream ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- deleteSmartStream h1 h2 :: IO (())
        let bs = enc r
        return $! bs
    ret "deleteSmartStream" ctx r pLen

foreign export ccall uw_HsFFI_checkQuerySyntax :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_checkQuerySyntax ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- checkQuerySyntax h1 :: IO (Maybe T.Text)
        let bs = enc r
        return $! bs
    ret "checkQuerySyntax" ctx r pLen

foreign export ccall uw_HsFFI_addFilter :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_addFilter ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (Bool)
    h4 <- peekArg x4 :: IO ([Int])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- addFilter h1 h2 h3 h4 :: IO (())
        let bs = enc r
        return $! bs
    ret "addFilter" ctx r pLen

foreign export ccall uw_HsFFI_editFilter :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_editFilter ctx pLen  x1 x2 x3 x4 x5 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (Int)
    h3 <- peekArg x3 :: IO (T.Text)
    h4 <- peekArg x4 :: IO (Bool)
    h5 <- peekArg x5 :: IO ([Int])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- editFilter h1 h2 h3 h4 h5 :: IO (())
        let bs = enc r
        return $! bs
    ret "editFilter" ctx r pLen

foreign export ccall uw_HsFFI_addSmartStream :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_addSmartStream ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    h4 <- peekArg x4 :: IO ([Int])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- addSmartStream h1 h2 h3 h4 :: IO (())
        let bs = enc r
        return $! bs
    ret "addSmartStream" ctx r pLen

foreign export ccall uw_HsFFI_editSmartStream :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_editSmartStream ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    h4 <- peekArg x4 :: IO ([Int])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- editSmartStream h1 h2 h3 h4 :: IO (())
        let bs = enc r
        return $! bs
    ret "editSmartStream" ctx r pLen

foreign export ccall uw_HsFFI_userOPML :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userOPML ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (Bool)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- userOPML h1 h2 :: IO (T.Text)
        let bs = enc r
        return $! bs
    ret "userOPML" ctx r pLen

foreign export ccall uw_HsFFI_opmlSubscriptions :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_opmlSubscriptions ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (B.ByteString)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- opmlSubscriptions h1 h2 :: IO (())
        let bs = enc r
        return $! bs
    ret "opmlSubscriptions" ctx r pLen

foreign export ccall uw_HsFFI_subscriptionsAndRenames :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_subscriptionsAndRenames ctx pLen  x1 x2 x3 x4 x5 x6 x7 x8 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (Bool)
    h3 <- peekArg x3 :: IO (UpdateFilters)
    h4 <- peekArg x4 :: IO (UrTime)
    h5 <- peekArg x5 :: IO (T.Text)
    h6 <- peekArg x6 :: IO (T.Text)
    h7 <- peekArg x7 :: IO (T.Text)
    h8 <- peekArg x8 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- subscriptionsAndRenames h1 h2 h3 h4 h5 h6 h7 h8 :: IO ((Maybe (T.Text, T.Text, [T.Text]), T.Text, [SubItemRpc], Maybe (([(Int, FilterQueryRpc)], [(T.Text, FilterQueryRpc)]), T.Text), [(UrTime, T.Text, T.Text)]))
        let bs = enc r
        return $! bs
    ret "subscriptionsAndRenames" ctx r pLen

foreign export ccall uw_HsFFI_subscriptionsAndSettings :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_subscriptionsAndSettings ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (Bool)
    h3 <- peekArg x3 :: IO (Bool)
    h4 <- peekArg x4 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- subscriptionsAndSettings h1 h2 h3 h4 :: IO (((Maybe (T.Text, T.Text, [T.Text]), T.Text, [SubItemRpc], Maybe (([(Int, FilterQueryRpc)], [(T.Text, FilterQueryRpc)]), T.Text), [(UrTime, T.Text, T.Text)]), ([T.Text], Bool, UserSettings), (WelcomeState, [(UrTime, T.Text, T.Text)], Int, Int)))
        let bs = enc r
        return $! bs
    ret "subscriptionsAndSettings" ctx r pLen

foreign export ccall uw_HsFFI_orderNotification :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_orderNotification ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- orderNotification h1 :: IO ((T.Text, Payment))
        let bs = enc r
        return $! bs
    ret "orderNotification" ctx r pLen

foreign export ccall uw_HsFFI_orderNotificationNew :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_orderNotificationNew ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- orderNotificationNew h1 h2 :: IO (())
        let bs = enc r
        return $! bs
    ret "orderNotificationNew" ctx r pLen

foreign export ccall uw_HsFFI_checkOrder :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_checkOrder ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- checkOrder h1 :: IO ((T.Text, Payment))
        let bs = enc r
        return $! bs
    ret "checkOrder" ctx r pLen

foreign export ccall uw_HsFFI_getPaidTill :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_getPaidTill ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- getPaidTill h1 :: IO (PaidTill)
        let bs = enc r
        return $! bs
    ret "getPaidTill" ctx r pLen

foreign export ccall uw_HsFFI_getUserAccountTypeAndRenewalUrl :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_getUserAccountTypeAndRenewalUrl ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- getUserAccountTypeAndRenewalUrl h1 :: IO ((T.Text, T.Text))
        let bs = enc r
        return $! bs
    ret "getUserAccountTypeAndRenewalUrl" ctx r pLen

foreign export ccall uw_HsFFI_getFeedDetails :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_getFeedDetails ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- getFeedDetails h1 h2 h3 :: IO ((T.Text, Maybe T.Text, Maybe T.Text, MsgTreeViewMode))
        let bs = enc r
        return $! bs
    ret "getFeedDetails" ctx r pLen

foreign export ccall uw_HsFFI_performBgActions :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_performBgActions ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO ([BgAction])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- performBgActions h1 h2 :: IO (T.Text)
        let bs = enc r
        return $! bs
    ret "performBgActions" ctx r pLen

foreign export ccall uw_HsFFI_tagsForest :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_tagsForest ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (ApiMode)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (Maybe [ItemTag])
    h4 <- peekArg x4 :: IO (MsgTreeViewMode)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- tagsForest h1 h2 h3 h4 :: IO ((MarkReq, [(Int, Int, Int, Int, Int)], MsgForest))
        let bs = enc r
        return $! bs
    ret "tagsForest" ctx r pLen

foreign export ccall uw_HsFFI_folderForest :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_folderForest ctx pLen  x1 x2 x3 x4 x5 x6 x7 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (Maybe T.Text)
    h3 <- peekArg x3 :: IO (FeedsOrDiscovery)
    h4 <- peekArg x4 :: IO ([PostsReq])
    h5 <- peekArg x5 :: IO (MsgTreeViewMode)
    h6 <- peekArg x6 :: IO (T.Text)
    h7 <- peekArg x7 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- folderForest h1 h2 h3 h4 h5 h6 h7 :: IO ((MarkReq, [(Int, Int, Int, Int, Int)], MsgForest))
        let bs = enc r
        return $! bs
    ret "folderForest" ctx r pLen

foreign export ccall uw_HsFFI_getTree :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_getTree ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (ApiMode)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (MsgTreeViewMode)
    h4 <- peekArg x4 :: IO ([TreeReq])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- getTree h1 h2 h3 h4 :: IO ([Maybe MsgForest])
        let bs = enc r
        return $! bs
    ret "getTree" ctx r pLen

foreign export ccall uw_HsFFI_filterForest :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_filterForest ctx pLen  x1 x2 x3 x4 x5 x6 x7 x8 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (Maybe T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    h4 <- peekArg x4 :: IO (Maybe T.Text)
    h5 <- peekArg x5 :: IO (FeedsOrDiscovery)
    h6 <- peekArg x6 :: IO (MsgTreeViewMode)
    h7 <- peekArg x7 :: IO (T.Text)
    h8 <- peekArg x8 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- filterForest h1 h2 h3 h4 h5 h6 h7 h8 :: IO (Either T.Text (MarkReq, [(Int, Int, Int, Int, Int)], FilterResults))
        let bs = enc r
        return $! bs
    ret "filterForest" ctx r pLen

foreign export ccall uw_HsFFI_filterTagsForest :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_filterTagsForest ctx pLen  x1 x2 x3 x4 x5 x6 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (Maybe [ItemTag])
    h4 <- peekArg x4 :: IO (MsgTreeViewMode)
    h5 <- peekArg x5 :: IO (T.Text)
    h6 <- peekArg x6 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- filterTagsForest h1 h2 h3 h4 h5 h6 :: IO (Either T.Text (MarkReq, [(Int, Int, Int, Int, Int)], FilterResults))
        let bs = enc r
        return $! bs
    ret "filterTagsForest" ctx r pLen

foreign export ccall uw_HsFFI_smartStreamForest :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_smartStreamForest ctx pLen  x1 x2 x3 x4 x5 = do
    h1 <- peekArg x1 :: IO (ApiMode)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    h4 <- peekArg x4 :: IO ([(Int, Int, Int, Int, Int)])
    h5 <- peekArg x5 :: IO (MsgTreeViewMode)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- smartStreamForest h1 h2 h3 h4 h5 :: IO ((MarkReq, [(Int, Int, Int, Int, Int)], MsgForest))
        let bs = enc r
        return $! bs
    ret "smartStreamForest" ctx r pLen

foreign export ccall uw_HsFFI_markReqReadCounters :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_markReqReadCounters ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (MsgTreeViewMode)
    h3 <- peekArg x3 :: IO (MarkReq)
    h4 <- peekArg x4 :: IO ([MsgId])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- markReqReadCounters h1 h2 h3 h4 :: IO ([(Int, Int, Int, Int, Int)])
        let bs = enc r
        return $! bs
    ret "markReqReadCounters" ctx r pLen

foreign export ccall uw_HsFFI_pageFromFile :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_pageFromFile ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- pageFromFile h1 :: IO (T.Text)
        let bs = enc r
        return $! bs
    ret "pageFromFile" ctx r pLen

foreign export ccall uw_HsFFI_addWebpackScripts :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_addWebpackScripts ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- addWebpackScripts h1 :: IO (T.Text)
        let bs = enc r
        return $! bs
    ret "addWebpackScripts" ctx r pLen

foreign export ccall uw_HsFFI_webpackStyles :: Ctx -> Ptr CLong -> IO (Ptr ())
uw_HsFFI_webpackStyles ctx pLen  = do
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- webpackStyles :: IO (T.Text)
        let bs = enc r
        return $! bs
    ret "webpackStyles" ctx r pLen

foreign export ccall uw_HsFFI_blessId :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_blessId ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        let r = id h1 :: (T.Text)
        let bs = enc r
        return $! bs
    ret "blessId" ctx r pLen

foreign export ccall uw_HsFFI_parseQueryStringUtf8Only :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_parseQueryStringUtf8Only ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        let r = parseQueryStringUtf8Only h1 :: ([(T.Text, T.Text)])
        let bs = enc r
        return $! bs
    ret "parseQueryStringUtf8Only" ctx r pLen

foreign export ccall uw_HsFFI_userEmail :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_userEmail ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- userEmail h1 :: IO (Maybe EmailAddress)
        let bs = enc r
        return $! bs
    ret "userEmail" ctx r pLen

foreign export ccall uw_HsFFI_buyPage :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_buyPage ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (Maybe EmailAddress)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- buyPage h1 h2 h3 :: IO (T.Text)
        let bs = enc r
        return $! bs
    ret "buyPage" ctx r pLen

foreign export ccall uw_HsFFI_invoiceLink :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_invoiceLink ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        let r = invoiceLink h1 :: (T.Text)
        let bs = enc r
        return $! bs
    ret "invoiceLink" ctx r pLen

foreign export ccall uw_HsFFI_prettyUID :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_prettyUID ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- prettyUID h1 :: IO (T.Text)
        let bs = enc r
        return $! bs
    ret "prettyUID" ctx r pLen

foreign export ccall uw_HsFFI_xbodyStringToString :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_xbodyStringToString ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        let r = id h1 :: (T.Text)
        let bs = enc r
        return $! bs
    ret "xbodyStringToString" ctx r pLen

foreign export ccall uw_HsFFI_xbodyStringToXbody :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_xbodyStringToXbody ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        let r = id h1 :: (T.Text)
        let bs = enc r
        return $! bs
    ret "xbodyStringToXbody" ctx r pLen

foreign export ccall uw_HsFFI_escapeXbody :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_escapeXbody ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (B.ByteString)
    r <- E.try $ timeout (120*1000*1000) $ do
        let r = escapeXbody h1 :: (T.Text)
        let bs = enc r
        return $! bs
    ret "escapeXbody" ctx r pLen

foreign export ccall uw_HsFFI_hyphenatePage :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_hyphenatePage ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        let r = hyphenateHtml h1 :: (T.Text)
        let bs = enc r
        return $! bs
    ret "hyphenatePage" ctx r pLen

foreign export ccall uw_HsFFI_hyphenateXbody :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_hyphenateXbody ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        let r = hyphenateHtml h1 :: (T.Text)
        let bs = enc r
        return $! bs
    ret "hyphenateXbody" ctx r pLen

foreign export ccall uw_HsFFI_toLowerCase :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_toLowerCase ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        let r = T.toLower h1 :: (T.Text)
        let bs = enc r
        return $! bs
    ret "toLowerCase" ctx r pLen

foreign export ccall uw_HsFFI_addTwitterScreenName :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_addTwitterScreenName ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- addTwitterScreenName h1 h2 h3 :: IO (())
        let bs = enc r
        return $! bs
    ret "addTwitterScreenName" ctx r pLen

foreign export ccall uw_HsFFI_newSessionJunk :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_newSessionJunk ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (LoginType)
    h2 <- peekArg x2 :: IO (LoginAccessToken)
    h3 <- peekArg x3 :: IO ([(JunkText, JunkText)])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- newSessionJunk h1 h2 h3 :: IO (Session)
        let bs = enc r
        return $! bs
    ret "newSessionJunk" ctx r pLen

foreign export ccall uw_HsFFI_getUserByLogin :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_getUserByLogin ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (LoginType)
    h2 <- peekArg x2 :: IO (Maybe T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- getUserByLogin h1 h2 :: IO (Maybe T.Text)
        let bs = enc r
        return $! bs
    ret "getUserByLogin" ctx r pLen

foreign export ccall uw_HsFFI_clearSession :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_clearSession ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- clearSession h1 h2 :: IO (())
        let bs = enc r
        return $! bs
    ret "clearSession" ctx r pLen

foreign export ccall uw_HsFFI_userEvent :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userEvent ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- userEvent h1 h2 h3 :: IO (())
        let bs = enc r
        return $! bs
    ret "userEvent" ctx r pLen

foreign export ccall uw_HsFFI_runTasks :: Ctx -> Ptr CLong -> IO (Ptr ())
uw_HsFFI_runTasks ctx pLen  = do
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- runTasks :: IO (())
        let bs = enc r
        return $! bs
    ret "runTasks" ctx r pLen

foreign export ccall uw_HsFFI_runApiServer :: Ctx -> Ptr CLong -> IO (Ptr ())
uw_HsFFI_runApiServer ctx pLen  = do
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- runApiServer :: IO (())
        let bs = enc r
        return $! bs
    ret "runApiServer" ctx r pLen

foreign export ccall uw_HsFFI_reloadBrowserPage :: Ctx -> Ptr CLong -> IO (Ptr ())
uw_HsFFI_reloadBrowserPage ctx pLen  = do
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- reloadBrowserPage :: IO (())
        let bs = enc r
        return $! bs
    ret "reloadBrowserPage" ctx r pLen

foreign export ccall uw_HsFFI_logOutAllSessions :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_logOutAllSessions ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO ([T.Text])
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- logOutAllSessions h1 h2 :: IO (())
        let bs = enc r
        return $! bs
    ret "logOutAllSessions" ctx r pLen

foreign export ccall uw_HsFFI_getFullText :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_getFullText ctx pLen  x1 x2 x3 x4 x5 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (Bool)
    h3 <- peekArg x3 :: IO (T.Text)
    h4 <- peekArg x4 :: IO (T.Text)
    h5 <- peekArg x5 :: IO (MsgKey)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- getFullText h1 h2 h3 h4 h5 :: IO (Either T.Text T.Text)
        let bs = enc r
        return $! bs
    ret "getFullText" ctx r pLen

foreign export ccall uw_HsFFI_getUrTime :: Ctx -> Ptr CLong -> IO (Ptr ())
uw_HsFFI_getUrTime ctx pLen  = do
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- getUrTime :: IO (UrTime)
        let bs = enc r
        return $! bs
    ret "getUrTime" ctx r pLen

foreign export ccall uw_HsFFI_setUsername :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_setUsername ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (Maybe T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- setUsername h1 h2 h3 :: IO (Bool)
        let bs = enc r
        return $! bs
    ret "setUsername" ctx r pLen

foreign export ccall uw_HsFFI_setPassword :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_setPassword ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (Maybe T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- setPassword h1 h2 h3 :: IO (())
        let bs = enc r
        return $! bs
    ret "setPassword" ctx r pLen

foreign export ccall uw_HsFFI_tryRemoveAssociatedAccount :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_tryRemoveAssociatedAccount ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (Maybe T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (LoginType)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- tryRemoveAssociatedAccount h1 h2 h3 :: IO (Bool)
        let bs = enc r
        return $! bs
    ret "tryRemoveAssociatedAccount" ctx r pLen

foreign export ccall uw_HsFFI_tryAddAssociatedAccount :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_tryAddAssociatedAccount ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (Maybe T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (LoginType)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- tryAddAssociatedAccount h1 h2 h3 :: IO (Bool)
        let bs = enc r
        return $! bs
    ret "tryAddAssociatedAccount" ctx r pLen

foreign export ccall uw_HsFFI_tryGetFeverUser :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_tryGetFeverUser ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- tryGetFeverUser h1 :: IO (Maybe T.Text)
        let bs = enc r
        return $! bs
    ret "tryGetFeverUser" ctx r pLen

foreign export ccall uw_HsFFI_enablePublicFeed :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_enablePublicFeed ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (PublicFeedType)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- enablePublicFeed h1 h2 :: IO ([(T.Text, Bool, Maybe T.Text)])
        let bs = enc r
        return $! bs
    ret "enablePublicFeed" ctx r pLen

foreign export ccall uw_HsFFI_disablePublicFeed :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_disablePublicFeed ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (PublicFeedType)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- disablePublicFeed h1 h2 :: IO ([(T.Text, Bool, Maybe T.Text)])
        let bs = enc r
        return $! bs
    ret "disablePublicFeed" ctx r pLen

foreign export ccall uw_HsFFI_generateNewPublicFeed :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_generateNewPublicFeed ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (PublicFeedType)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- generateNewPublicFeed h1 h2 :: IO ([(T.Text, Bool, Maybe T.Text)])
        let bs = enc r
        return $! bs
    ret "generateNewPublicFeed" ctx r pLen

foreign export ccall uw_HsFFI_discover :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_discover ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    h4 <- peekArg x4 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- discover h1 h2 h3 h4 :: IO (Maybe (T.Text, [(T.Text, MsgTreeViewMode)]))
        let bs = enc r
        return $! bs
    ret "discover" ctx r pLen

foreign export ccall uw_HsFFI_restoreSubscriptionsFromBackup :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_restoreSubscriptionsFromBackup ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- restoreSubscriptionsFromBackup h1 :: IO (())
        let bs = enc r
        return $! bs
    ret "restoreSubscriptionsFromBackup" ctx r pLen

foreign export ccall uw_HsFFI_isUserExists :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_isUserExists ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- isUserExists h1 :: IO (Bool)
        let bs = enc r
        return $! bs
    ret "isUserExists" ctx r pLen

foreign export ccall uw_HsFFI_deleteAccount :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_deleteAccount ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (Bool)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- deleteAccount h1 h2 :: IO (())
        let bs = enc r
        return $! bs
    ret "deleteAccount" ctx r pLen

foreign export ccall uw_HsFFI_recordWebUsage :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_recordWebUsage ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (Maybe JunkText)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- recordWebUsage h1 h2 :: IO (())
        let bs = enc r
        return $! bs
    ret "recordWebUsage" ctx r pLen

foreign export ccall uw_HsFFI_readMsgAndApplyFixes :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_readMsgAndApplyFixes ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (MsgKey)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- readMsgAndApplyFixes h1 h2 h3 :: IO (Maybe Msg)
        let bs = enc r
        return $! bs
    ret "readMsgAndApplyFixes" ctx r pLen

foreign export ccall uw_HsFFI_parseRenewalUserId :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_parseRenewalUserId ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- parseRenewalUserId h1 :: IO (T.Text)
        let bs = enc r
        return $! bs
    ret "parseRenewalUserId" ctx r pLen

foreign export ccall uw_HsFFI_passwordResetEmail :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_passwordResetEmail ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- passwordResetEmail h1 :: IO (Either T.Text (T.Text, T.Text))
        let bs = enc r
        return $! bs
    ret "passwordResetEmail" ctx r pLen

foreign export ccall uw_HsFFI_sendSignUpEmail :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_sendSignUpEmail ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- sendSignUpEmail h1 h2 h3 :: IO (Bool)
        let bs = enc r
        return $! bs
    ret "sendSignUpEmail" ctx r pLen

foreign export ccall uw_HsFFI_sendPasswordResetEmail :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_sendPasswordResetEmail ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- sendPasswordResetEmail h1 h2 h3 :: IO (Bool)
        let bs = enc r
        return $! bs
    ret "sendPasswordResetEmail" ctx r pLen

foreign export ccall uw_HsFFI_sendChangeEmailEmail :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_sendChangeEmailEmail ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- sendChangeEmailEmail h1 h2 h3 :: IO (Bool)
        let bs = enc r
        return $! bs
    ret "sendChangeEmailEmail" ctx r pLen

foreign export ccall uw_HsFFI_verifySignUpToken :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_verifySignUpToken ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- verifySignUpToken h1 :: IO (Maybe T.Text)
        let bs = enc r
        return $! bs
    ret "verifySignUpToken" ctx r pLen

foreign export ccall uw_HsFFI_verifyPasswordResetToken :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_verifyPasswordResetToken ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- verifyPasswordResetToken h1 h2 h3 :: IO (Maybe T.Text)
        let bs = enc r
        return $! bs
    ret "verifyPasswordResetToken" ctx r pLen

foreign export ccall uw_HsFFI_verifyChangeEmailToken :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_verifyChangeEmailToken ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- verifyChangeEmailToken h1 h2 :: IO (Maybe T.Text)
        let bs = enc r
        return $! bs
    ret "verifyChangeEmailToken" ctx r pLen

foreign export ccall uw_HsFFI_verifyRestoreAccessToken :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_verifyRestoreAccessToken ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- verifyRestoreAccessToken h1 h2 h3 :: IO (Maybe T.Text)
        let bs = enc r
        return $! bs
    ret "verifyRestoreAccessToken" ctx r pLen

foreign export ccall uw_HsFFI_validateEmail :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_validateEmail ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        let r = validateEmail h1 :: (Maybe T.Text)
        let bs = enc r
        return $! bs
    ret "validateEmail" ctx r pLen

foreign export ccall uw_HsFFI_maskEmail :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_maskEmail ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        let r = maskEmail h1 :: (T.Text)
        let bs = enc r
        return $! bs
    ret "maskEmail" ctx r pLen

foreign export ccall uw_HsFFI_userAddToPocket :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userAddToPocket ctx pLen  x1 x2 x3 x4 x5 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (TURL)
    h4 <- peekArg x4 :: IO (T.Text)
    h5 <- peekArg x5 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- userAddToPocket h1 h2 h3 h4 h5 :: IO (OkErrorRedirect)
        let bs = enc r
        return $! bs
    ret "userAddToPocket" ctx r pLen

foreign export ccall uw_HsFFI_userAuthorizeAndAddToPocket :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_userAuthorizeAndAddToPocket ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- userAuthorizeAndAddToPocket h1 :: IO (())
        let bs = enc r
        return $! bs
    ret "userAuthorizeAndAddToPocket" ctx r pLen

foreign export ccall uw_HsFFI_logT :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_logT ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- logT h1 :: IO (())
        let bs = enc r
        return $! bs
    ret "logT" ctx r pLen

foreign export ccall uw_HsFFI_findUsersLeft :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_findUsersLeft ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (Int)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- findUsersLeft h1 :: IO (([FeedbackUserInfo], [FeedbackUserInfo]))
        let bs = enc r
        return $! bs
    ret "findUsersLeft" ctx r pLen

foreign export ccall uw_HsFFI_updateFeedbackUserInfo :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_updateFeedbackUserInfo ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (FeedbackUserInfo)
    r <- E.try $ timeout (120*1000*1000) $ do
        r <- updateFeedbackUserInfo h1 :: IO (())
        let bs = enc r
        return $! bs
    ret "updateFeedbackUserInfo" ctx r pLen

