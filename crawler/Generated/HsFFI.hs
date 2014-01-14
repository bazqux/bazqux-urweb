{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Generated.HsFFI where
import Generated.DataTypes
import Generated.RiakIO
import URL (TURL)
import Lib.UrTime
import UrCalls
import Discovery (searchSubscriptions)
import API
import Auth
import Data.Binary
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Map as Map
import qualified Control.Exception as E
import Control.Concurrent
import System.IO.Unsafe
import System.IO
import Foreign hiding (unsafePerformIO)
import Foreign.C.Types

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
ret :: Ctx -> Either E.SomeException B.ByteString 
    -> Ptr CLong -> IO (Ptr ())
ret ctx (Left ex) pLen = do
    poke pLen (toEnum $ 0 - B.length bs)
    saveBSretPtr ctx bs
    where bs' = B.pack (show ex)
          bs = if bs' /= "" then bs' else "emtpy error?"
ret ctx (Right bs) pLen = do
    poke pLen (toEnum $ B.length bs)
    saveBSretPtr ctx bs
enc a = B.concat $ BL.toChunks $ encode a 

 
foreign export ccall uw_HsFFI_readUserSettings :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_readUserSettings ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ do
        r <- readUserSettings h1 :: IO (Maybe UserSettings)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_cachedReadUserSettings :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedReadUserSettings ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ do
        r <- cachedReadUserSettings h1 :: IO (Maybe UserSettings)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_cachedNothingReadUserSettings :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedNothingReadUserSettings ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ do
        r <- cachedNothingReadUserSettings h1 :: IO (Maybe UserSettings)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_mergeWriteUserSettings :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_mergeWriteUserSettings ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (UserSettings)
    r <- E.try $ do
        r <- mergeWriteUserSettings h1 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_deleteUserSettings :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_deleteUserSettings ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (UserSettings)
    r <- E.try $ do
        r <- deleteUserSettings h1 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_readManyUserSettingss :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_readManyUserSettingss ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([T.Text])
    r <- E.try $ do
        r <- readManyUserSettingss h1 :: IO ([Maybe UserSettings])
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_cachedReadManyUserSettingss :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedReadManyUserSettingss ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([T.Text])
    r <- E.try $ do
        r <- cachedReadManyUserSettingss h1 :: IO ([Maybe UserSettings])
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_cachedNothingReadManyUserSettingss :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedNothingReadManyUserSettingss ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([T.Text])
    r <- E.try $ do
        r <- cachedNothingReadManyUserSettingss h1 :: IO ([Maybe UserSettings])
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_writeManyUserSettingss :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_writeManyUserSettingss ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([UserSettings])
    r <- E.try $ do
        r <- writeManyUserSettingss h1 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_readSession :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_readSession ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ do
        r <- readSession h1 :: IO (Maybe Session)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_cachedReadSession :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedReadSession ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ do
        r <- cachedReadSession h1 :: IO (Maybe Session)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_cachedNothingReadSession :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedNothingReadSession ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ do
        r <- cachedNothingReadSession h1 :: IO (Maybe Session)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_mergeWriteSession :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_mergeWriteSession ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (Session)
    r <- E.try $ do
        r <- mergeWriteSession h1 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_deleteSession :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_deleteSession ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (Session)
    r <- E.try $ do
        r <- deleteSession h1 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_readManySessions :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_readManySessions ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([T.Text])
    r <- E.try $ do
        r <- readManySessions h1 :: IO ([Maybe Session])
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_cachedReadManySessions :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedReadManySessions ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([T.Text])
    r <- E.try $ do
        r <- cachedReadManySessions h1 :: IO ([Maybe Session])
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_cachedNothingReadManySessions :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedNothingReadManySessions ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([T.Text])
    r <- E.try $ do
        r <- cachedNothingReadManySessions h1 :: IO ([Maybe Session])
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_writeManySessions :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_writeManySessions ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([Session])
    r <- E.try $ do
        r <- writeManySessions h1 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_readMsg :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_readMsg ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (MsgKey)
    r <- E.try $ do
        r <- readMsg h1 :: IO (Maybe Msg)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_cachedReadMsg :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedReadMsg ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (MsgKey)
    r <- E.try $ do
        r <- cachedReadMsg h1 :: IO (Maybe Msg)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_cachedNothingReadMsg :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedNothingReadMsg ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (MsgKey)
    r <- E.try $ do
        r <- cachedNothingReadMsg h1 :: IO (Maybe Msg)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_mergeWriteMsg :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_mergeWriteMsg ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (Msg)
    r <- E.try $ do
        r <- mergeWriteMsg h1 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_deleteMsg :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_deleteMsg ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (Msg)
    r <- E.try $ do
        r <- deleteMsg h1 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_readManyMsgs :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_readManyMsgs ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([MsgKey])
    r <- E.try $ do
        r <- readManyMsgs h1 :: IO ([Maybe Msg])
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_cachedReadManyMsgs :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedReadManyMsgs ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([MsgKey])
    r <- E.try $ do
        r <- cachedReadManyMsgs h1 :: IO ([Maybe Msg])
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_cachedNothingReadManyMsgs :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedNothingReadManyMsgs ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([MsgKey])
    r <- E.try $ do
        r <- cachedNothingReadManyMsgs h1 :: IO ([Maybe Msg])
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_writeManyMsgs :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_writeManyMsgs ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([Msg])
    r <- E.try $ do
        r <- writeManyMsgs h1 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_loginGetForwardUrl :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_loginGetForwardUrl ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (LoginType)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    h4 <- peekArg x4 :: IO (TURL)
    r <- E.try $ do
        r <- loginGetForwardUrl h1 h2 h3 h4 :: IO (TURL)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_loginCallback :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_loginCallback ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (LoginType)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (TURL)
    h4 <- peekArg x4 :: IO (T.Text)
    r <- E.try $ do
        r <- loginCallback h1 h2 h3 h4 :: IO ((UID, Maybe T.Text))
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_fbTokenGetForwardUrl :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_fbTokenGetForwardUrl ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (TURL)
    r <- E.try $ do
        r <- fbTokenGetForwardUrl h1 h2 :: IO (TURL)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_fbTokenCallback :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_fbTokenCallback ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (TURL)
    h3 <- peekArg x3 :: IO (T.Text)
    r <- E.try $ do
        r <- fbTokenCallback h1 h2 h3 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_importFromGoogleReaderGetForwardUrl :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_importFromGoogleReaderGetForwardUrl ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (TURL)
    r <- E.try $ do
        r <- importFromGoogleReaderGetForwardUrl h1 h2 :: IO (TURL)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_importFromGoogleReaderCallback :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_importFromGoogleReaderCallback ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    h4 <- peekArg x4 :: IO (T.Text)
    r <- E.try $ do
        r <- importFromGoogleReaderCallback h1 h2 h3 h4 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_importStarredAndTaggedItemsFromGoogleReaderCallback :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_importStarredAndTaggedItemsFromGoogleReaderCallback ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    h4 <- peekArg x4 :: IO (T.Text)
    r <- E.try $ do
        r <- importStarredAndTaggedItemsFromGoogleReaderCallback h1 h2 h3 h4 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_userSubscribe :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userSubscribe ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (Maybe T.Text)
    h4 <- peekArg x4 :: IO ([T.Text])
    r <- E.try $ do
        r <- userSubscribe h1 h2 h3 h4 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_userDiscoverySubscribe :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userDiscoverySubscribe ctx pLen  x1 x2 x3 x4 x5 x6 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    h4 <- peekArg x4 :: IO (T.Text)
    h5 <- peekArg x5 :: IO (Maybe T.Text)
    h6 <- peekArg x6 :: IO ([T.Text])
    r <- E.try $ do
        r <- userDiscoverySubscribe h1 h2 h3 h4 h5 h6 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_userRenameSubscription :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userRenameSubscription ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    r <- E.try $ do
        r <- userRenameSubscription h1 h2 h3 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_userRenameFolder :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userRenameFolder ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    r <- E.try $ do
        r <- userRenameFolder h1 h2 h3 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_userEditSubscriptionFolders :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userEditSubscriptionFolders ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    h4 <- peekArg x4 :: IO (Bool)
    r <- E.try $ do
        r <- userEditSubscriptionFolders h1 h2 h3 h4 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_userUnsubscribe :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userUnsubscribe ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO ([T.Text])
    r <- E.try $ do
        r <- userUnsubscribe h1 h2 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_userRetrySubscription :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userRetrySubscription ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ do
        r <- userRetrySubscription h1 h2 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_userOPML :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_userOPML ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ do
        r <- userOPML h1 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_opmlSubscriptions :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_opmlSubscriptions ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (B.ByteString)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ do
        r <- opmlSubscriptions h1 h2 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_userSubscriptionsAndRenames :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userSubscriptionsAndRenames ctx pLen  x1 x2 x3 x4 x5 x6 = do
    h1 <- peekArg x1 :: IO (Bool)
    h2 <- peekArg x2 :: IO (UrTime)
    h3 <- peekArg x3 :: IO (T.Text)
    h4 <- peekArg x4 :: IO (T.Text)
    h5 <- peekArg x5 :: IO ([T.Text])
    h6 <- peekArg x6 :: IO (T.Text)
    r <- E.try $ do
        r <- userSubscriptionsAndRenames h1 h2 h3 h4 h5 h6 :: IO ((Maybe (T.Text, T.Text, [T.Text]), T.Text, [SubItemRpc], Bool, [(UrTime, T.Text, T.Text)]))
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_userSubscriptionsAndSettings :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userSubscriptionsAndSettings ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ do
        r <- userSubscriptionsAndSettings h1 h2 :: IO ((Maybe (T.Text, T.Text, [T.Text]), T.Text, [SubItemRpc], (Bool, Bool, [(UrTime, T.Text, T.Text)], [T.Text], UserSettings)))
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_orderNotification :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_orderNotification ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ do
        r <- orderNotification h1 :: IO (Payment)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_checkOrder :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_checkOrder ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ do
        r <- checkOrder h1 :: IO (Payment)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_getPaidTill :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_getPaidTill ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ do
        r <- getPaidTill h1 :: IO (PaidTill)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_activeGRImportsCount :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_activeGRImportsCount ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (())
    r <- E.try $ do
        r <- activeGRImportsCount h1 :: IO (Int)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_activeGRImportNames :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_activeGRImportNames ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (())
    r <- E.try $ do
        r <- activeGRImportNames h1 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_getFeedDetails :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_getFeedDetails ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ do
        r <- getFeedDetails h1 h2 :: IO ((T.Text, Maybe T.Text, Maybe T.Text, MsgTreeViewMode))
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_tagsMsgForest :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_tagsMsgForest ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (ApiMode)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (Maybe [ItemTag])
    h4 <- peekArg x4 :: IO (MsgTreeViewMode)
    r <- E.try $ do
        r <- tagsMsgForest h1 h2 h3 h4 :: IO (MsgForest)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_folderMsgForest :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_folderMsgForest ctx pLen  x1 x2 x3 x4 x5 = do
    h1 <- peekArg x1 :: IO (ApiMode)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO ([(T.Text, Int, Int, Int, Int)])
    h4 <- peekArg x4 :: IO ([PostsReq])
    h5 <- peekArg x5 :: IO (MsgTreeViewMode)
    r <- E.try $ do
        r <- folderMsgForest h1 h2 h3 h4 h5 :: IO (([(T.Text, Int, Int, Int, Int)], MsgForest))
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_userGetTree :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userGetTree ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (ApiMode)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (MsgTreeViewMode)
    h4 <- peekArg x4 :: IO ([TreeReq])
    r <- E.try $ do
        r <- userGetTree h1 h2 h3 h4 :: IO ([Maybe MsgForest])
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_performBgActions :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_performBgActions ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO ([BgAction])
    r <- E.try $ do
        r <- performBgActions h1 h2 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_searchMsgForest :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_searchMsgForest ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO ([(T.Text, Int, Int, Int, Int)])
    h4 <- peekArg x4 :: IO (MsgTreeViewMode)
    r <- E.try $ do
        r <- searchMsgForest h1 h2 h3 h4 :: IO (SearchResults)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_searchTagsMsgForest :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_searchTagsMsgForest ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (Maybe [ItemTag])
    h4 <- peekArg x4 :: IO (MsgTreeViewMode)
    r <- E.try $ do
        r <- searchTagsMsgForest h1 h2 h3 h4 :: IO (SearchResults)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_htmlHead :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_htmlHead ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (())
    r <- E.try $ do
        r <- htmlHead h1 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_htmlHeadMain :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_htmlHeadMain ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (())
    r <- E.try $ do
        r <- htmlHeadMain h1 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_htmlHeadMainNoTranslate :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_htmlHeadMainNoTranslate ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (())
    r <- E.try $ do
        r <- htmlHeadMainNoTranslate h1 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_htmlLikeButtons :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_htmlLikeButtons ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (())
    r <- E.try $ do
        r <- htmlLikeButtons h1 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_htmlLandingScripts :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_htmlLandingScripts ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (())
    r <- E.try $ do
        r <- htmlLandingScripts h1 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_htmlOpenIdSignInButton :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_htmlOpenIdSignInButton ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (())
    r <- E.try $ do
        r <- htmlOpenIdSignInButton h1 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_htmlConversionLogin :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_htmlConversionLogin ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (())
    r <- E.try $ do
        r <- htmlConversionLogin h1 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_version :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_version ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (())
    r <- E.try $ do
        r <- version h1 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_blessId :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_blessId ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ do
        r <- blessId h1 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_parseQueryStringUtf8Only :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_parseQueryStringUtf8Only ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ do
        r <- parseQueryStringUtf8Only h1 :: IO ([(T.Text, T.Text)])
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_buyLink :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_buyLink ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ do
        r <- buyLink h1 h2 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_encodeURIComponent :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_encodeURIComponent ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ do
        r <- encodeURIComponent h1 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_prettyUID :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_prettyUID ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ do
        r <- prettyUID h1 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_textToXbody :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_textToXbody ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ do
        r <- textToXbody h1 :: IO (T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_newSession :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_newSession ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (UID)
    h2 <- peekArg x2 :: IO ([(T.Text, T.Text)])
    r <- E.try $ do
        r <- newSession h1 h2 :: IO (Session)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_clearSession :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_clearSession ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ do
        r <- clearSession h1 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_userEvent :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userEvent ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    r <- E.try $ do
        r <- userEvent h1 h2 h3 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_initMailer :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_initMailer ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (())
    r <- E.try $ do
        r <- initMailer h1 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_initApiServer :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_initApiServer ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (())
    r <- E.try $ do
        r <- initApiServer h1 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_readFullTextCache :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_readFullTextCache ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (TURL)
    r <- E.try $ do
        r <- readFullTextCache h1 :: IO (Maybe FullTextCache)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_cachedReadFullTextCache :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedReadFullTextCache ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (TURL)
    r <- E.try $ do
        r <- cachedReadFullTextCache h1 :: IO (Maybe FullTextCache)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_cachedNothingReadFullTextCache :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedNothingReadFullTextCache ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (TURL)
    r <- E.try $ do
        r <- cachedNothingReadFullTextCache h1 :: IO (Maybe FullTextCache)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_mergeWriteFullTextCache :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_mergeWriteFullTextCache ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (FullTextCache)
    r <- E.try $ do
        r <- mergeWriteFullTextCache h1 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_deleteFullTextCache :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_deleteFullTextCache ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (FullTextCache)
    r <- E.try $ do
        r <- deleteFullTextCache h1 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_readManyFullTextCaches :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_readManyFullTextCaches ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([TURL])
    r <- E.try $ do
        r <- readManyFullTextCaches h1 :: IO ([Maybe FullTextCache])
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_cachedReadManyFullTextCaches :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedReadManyFullTextCaches ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([TURL])
    r <- E.try $ do
        r <- cachedReadManyFullTextCaches h1 :: IO ([Maybe FullTextCache])
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_cachedNothingReadManyFullTextCaches :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_cachedNothingReadManyFullTextCaches ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([TURL])
    r <- E.try $ do
        r <- cachedNothingReadManyFullTextCaches h1 :: IO ([Maybe FullTextCache])
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_writeManyFullTextCaches :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_writeManyFullTextCaches ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO ([FullTextCache])
    r <- E.try $ do
        r <- writeManyFullTextCaches h1 :: IO (())
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_getFullText :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_getFullText ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (MsgKey)
    r <- E.try $ do
        r <- getFullText h1 :: IO (Either T.Text T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_getUrTime_ :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_getUrTime_ ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (())
    r <- E.try $ do
        r <- getUrTime_ h1 :: IO (UrTime)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_setMobileLogin :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_setMobileLogin ctx pLen  x1 x2 x3 x4 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    h4 <- peekArg x4 :: IO (T.Text)
    r <- E.try $ do
        r <- setMobileLogin h1 h2 h3 h4 :: IO (Bool)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_tryGetFeverUser :: Ctx -> Ptr CLong -> Ptr () -> IO (Ptr ())
uw_HsFFI_tryGetFeverUser ctx pLen  x1 = do
    h1 <- peekArg x1 :: IO (T.Text)
    r <- E.try $ do
        r <- tryGetFeverUser h1 :: IO (Maybe T.Text)
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_userEnablePublicFeed :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userEnablePublicFeed ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (PublicFeedType)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ do
        r <- userEnablePublicFeed h1 h2 :: IO ([(T.Text, Bool, Maybe T.Text)])
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_userDisablePublicFeed :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userDisablePublicFeed ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (PublicFeedType)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ do
        r <- userDisablePublicFeed h1 h2 :: IO ([(T.Text, Bool, Maybe T.Text)])
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_userGenerateNewPublicFeed :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_userGenerateNewPublicFeed ctx pLen  x1 x2 = do
    h1 <- peekArg x1 :: IO (PublicFeedType)
    h2 <- peekArg x2 :: IO (T.Text)
    r <- E.try $ do
        r <- userGenerateNewPublicFeed h1 h2 :: IO ([(T.Text, Bool, Maybe T.Text)])
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

foreign export ccall uw_HsFFI_searchSubscriptions :: Ctx -> Ptr CLong -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
uw_HsFFI_searchSubscriptions ctx pLen  x1 x2 x3 = do
    h1 <- peekArg x1 :: IO (T.Text)
    h2 <- peekArg x2 :: IO (T.Text)
    h3 <- peekArg x3 :: IO (T.Text)
    r <- E.try $ do
        r <- searchSubscriptions h1 h2 h3 :: IO (Maybe (T.Text, [(T.Text, MsgTreeViewMode)]))
        let bs = enc r
        B.length bs `seq` return bs
    ret ctx r pLen

