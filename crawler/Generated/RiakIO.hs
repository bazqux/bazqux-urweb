{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module Generated.RiakIO where
import Generated.DataTypes
import Data.Maybe
import Control.Monad
import qualified Data.Text as T
import Resolvables
import Riak
import Lib.UrTime
import URL
import System.IO.Unsafe
 
instance KV Stats where
    type Key Stats = T.Text
    kvBucket _ = "Stats"
    kvKey = statsKey
    kvCache _ = _statsCache
    kvPool _ = riakPool
_statsCache = unsafePerformIO $ newCache 0 0 (0*1024*1024)
{-# NOINLINE _statsCache #-}
readStats :: T.Text -> IO (Maybe Stats)
readStats = readKV

cachedReadStats :: T.Text -> IO (Maybe Stats)
cachedReadStats = cachedReadKV

cachedNothingReadStats :: T.Text -> IO (Maybe Stats)
cachedNothingReadStats = cachedNothingReadKV

mergeWriteStats :: Stats -> IO (())
mergeWriteStats = mergeWriteKV

deleteStats :: Stats -> IO (())
deleteStats = deleteKV

readManyStatss :: [T.Text] -> IO ([Maybe Stats])
readManyStatss = readManyKVs

cachedReadManyStatss :: [T.Text] -> IO ([Maybe Stats])
cachedReadManyStatss = cachedReadManyKVs

cachedNothingReadManyStatss :: [T.Text] -> IO ([Maybe Stats])
cachedNothingReadManyStatss = cachedNothingReadManyKVs

writeManyStatss :: [Stats] -> IO (())
writeManyStatss = writeManyKVs

modifyStats :: T.Text -> (Maybe Stats -> IO (Stats, b)) -> IO b
modifyStats = modifyKV

modifyStats_ :: T.Text -> (Maybe Stats -> IO Stats) -> IO ()
modifyStats_ = modifyKV_

modifyStats' :: T.Text -> (Stats -> IO (Stats, b)) -> IO b
modifyStats' key f = modifyKV key (f . fromMaybe (defaultStats key))

modifyStats'_ :: T.Text -> (Stats -> IO Stats) -> IO ()
modifyStats'_ key f = modifyKV_ key (f . fromMaybe (defaultStats key))

readStats' :: Key Stats -> IO Stats
readStats' key = liftM (fromMaybe (defaultStats key)) (readKV key)

cachedReadStats' :: Key Stats -> IO Stats
cachedReadStats' key = liftM (fromMaybe (defaultStats key)) (cachedReadKV key)

cachedNothingReadStats' :: Key Stats -> IO Stats
cachedNothingReadStats' key = liftM (fromMaybe (defaultStats key)) (cachedNothingReadKV key)

instance KV User where
    type Key User = T.Text
    kvBucket _ = "User"
    kvKey = uId
    kvCache _ = _uCache
    kvPool _ = riakPool
_uCache = unsafePerformIO $ newCache 240 240 (200*1024*1024)
{-# NOINLINE _uCache #-}
readUser :: T.Text -> IO (Maybe User)
readUser = readKV

cachedReadUser :: T.Text -> IO (Maybe User)
cachedReadUser = cachedReadKV

cachedNothingReadUser :: T.Text -> IO (Maybe User)
cachedNothingReadUser = cachedNothingReadKV

mergeWriteUser :: User -> IO (())
mergeWriteUser = mergeWriteKV

deleteUser :: User -> IO (())
deleteUser = deleteKV

readManyUsers :: [T.Text] -> IO ([Maybe User])
readManyUsers = readManyKVs

cachedReadManyUsers :: [T.Text] -> IO ([Maybe User])
cachedReadManyUsers = cachedReadManyKVs

cachedNothingReadManyUsers :: [T.Text] -> IO ([Maybe User])
cachedNothingReadManyUsers = cachedNothingReadManyKVs

writeManyUsers :: [User] -> IO (())
writeManyUsers = writeManyKVs

modifyUser :: T.Text -> (Maybe User -> IO (User, b)) -> IO b
modifyUser = modifyKV

modifyUser_ :: T.Text -> (Maybe User -> IO User) -> IO ()
modifyUser_ = modifyKV_

modifyUser' :: T.Text -> (User -> IO (User, b)) -> IO b
modifyUser' key f = modifyKV key (f . fromMaybe (defaultUser key))

modifyUser'_ :: T.Text -> (User -> IO User) -> IO ()
modifyUser'_ key f = modifyKV_ key (f . fromMaybe (defaultUser key))

readUser' :: Key User -> IO User
readUser' key = liftM (fromMaybe (defaultUser key)) (readKV key)

cachedReadUser' :: Key User -> IO User
cachedReadUser' key = liftM (fromMaybe (defaultUser key)) (cachedReadKV key)

cachedNothingReadUser' :: Key User -> IO User
cachedNothingReadUser' key = liftM (fromMaybe (defaultUser key)) (cachedNothingReadKV key)

instance KV UserFilters where
    type Key UserFilters = T.Text
    kvBucket _ = "UserFilters"
    kvKey = ufUser
    kvCache _ = _ufCache
    kvPool _ = riakPool
_ufCache = unsafePerformIO $ newCache 240 240 (10*1024*1024)
{-# NOINLINE _ufCache #-}
readUserFilters :: T.Text -> IO (Maybe UserFilters)
readUserFilters = readKV

cachedReadUserFilters :: T.Text -> IO (Maybe UserFilters)
cachedReadUserFilters = cachedReadKV

cachedNothingReadUserFilters :: T.Text -> IO (Maybe UserFilters)
cachedNothingReadUserFilters = cachedNothingReadKV

mergeWriteUserFilters :: UserFilters -> IO (())
mergeWriteUserFilters = mergeWriteKV

deleteUserFilters :: UserFilters -> IO (())
deleteUserFilters = deleteKV

readManyUserFilterss :: [T.Text] -> IO ([Maybe UserFilters])
readManyUserFilterss = readManyKVs

cachedReadManyUserFilterss :: [T.Text] -> IO ([Maybe UserFilters])
cachedReadManyUserFilterss = cachedReadManyKVs

cachedNothingReadManyUserFilterss :: [T.Text] -> IO ([Maybe UserFilters])
cachedNothingReadManyUserFilterss = cachedNothingReadManyKVs

writeManyUserFilterss :: [UserFilters] -> IO (())
writeManyUserFilterss = writeManyKVs

modifyUserFilters :: T.Text -> (Maybe UserFilters -> IO (UserFilters, b)) -> IO b
modifyUserFilters = modifyKV

modifyUserFilters_ :: T.Text -> (Maybe UserFilters -> IO UserFilters) -> IO ()
modifyUserFilters_ = modifyKV_

modifyUserFilters' :: T.Text -> (UserFilters -> IO (UserFilters, b)) -> IO b
modifyUserFilters' key f = modifyKV key (f . fromMaybe (defaultUserFilters key))

modifyUserFilters'_ :: T.Text -> (UserFilters -> IO UserFilters) -> IO ()
modifyUserFilters'_ key f = modifyKV_ key (f . fromMaybe (defaultUserFilters key))

readUserFilters' :: Key UserFilters -> IO UserFilters
readUserFilters' key = liftM (fromMaybe (defaultUserFilters key)) (readKV key)

cachedReadUserFilters' :: Key UserFilters -> IO UserFilters
cachedReadUserFilters' key = liftM (fromMaybe (defaultUserFilters key)) (cachedReadKV key)

cachedNothingReadUserFilters' :: Key UserFilters -> IO UserFilters
cachedNothingReadUserFilters' key = liftM (fromMaybe (defaultUserFilters key)) (cachedNothingReadKV key)

instance KV UserSettings where
    type Key UserSettings = T.Text
    kvBucket _ = "UserSettings"
    kvKey = ustUser
    kvCache _ = _ustCache
    kvPool _ = riakPool
_ustCache = unsafePerformIO $ newCache 240 240 (10*1024*1024)
{-# NOINLINE _ustCache #-}
readUserSettings :: T.Text -> IO (Maybe UserSettings)
readUserSettings = readKV

cachedReadUserSettings :: T.Text -> IO (Maybe UserSettings)
cachedReadUserSettings = cachedReadKV

cachedNothingReadUserSettings :: T.Text -> IO (Maybe UserSettings)
cachedNothingReadUserSettings = cachedNothingReadKV

mergeWriteUserSettings :: UserSettings -> IO (())
mergeWriteUserSettings = mergeWriteKV

deleteUserSettings :: UserSettings -> IO (())
deleteUserSettings = deleteKV

readManyUserSettingss :: [T.Text] -> IO ([Maybe UserSettings])
readManyUserSettingss = readManyKVs

cachedReadManyUserSettingss :: [T.Text] -> IO ([Maybe UserSettings])
cachedReadManyUserSettingss = cachedReadManyKVs

cachedNothingReadManyUserSettingss :: [T.Text] -> IO ([Maybe UserSettings])
cachedNothingReadManyUserSettingss = cachedNothingReadManyKVs

writeManyUserSettingss :: [UserSettings] -> IO (())
writeManyUserSettingss = writeManyKVs

modifyUserSettings :: T.Text -> (Maybe UserSettings -> IO (UserSettings, b)) -> IO b
modifyUserSettings = modifyKV

modifyUserSettings_ :: T.Text -> (Maybe UserSettings -> IO UserSettings) -> IO ()
modifyUserSettings_ = modifyKV_

modifyUserSettings' :: T.Text -> (UserSettings -> IO (UserSettings, b)) -> IO b
modifyUserSettings' key f = modifyKV key (f . fromMaybe (defaultUserSettings key))

modifyUserSettings'_ :: T.Text -> (UserSettings -> IO UserSettings) -> IO ()
modifyUserSettings'_ key f = modifyKV_ key (f . fromMaybe (defaultUserSettings key))

readUserSettings' :: Key UserSettings -> IO UserSettings
readUserSettings' key = liftM (fromMaybe (defaultUserSettings key)) (readKV key)

cachedReadUserSettings' :: Key UserSettings -> IO UserSettings
cachedReadUserSettings' key = liftM (fromMaybe (defaultUserSettings key)) (cachedReadKV key)

cachedNothingReadUserSettings' :: Key UserSettings -> IO UserSettings
cachedNothingReadUserSettings' key = liftM (fromMaybe (defaultUserSettings key)) (cachedNothingReadKV key)

instance KV PublicFeed where
    type Key PublicFeed = T.Text
    kvBucket _ = "PublicFeed"
    kvKey = pfId
    kvCache _ = _pfCache
    kvPool _ = riakPool
_pfCache = unsafePerformIO $ newCache 240 240 (10*1024*1024)
{-# NOINLINE _pfCache #-}
readPublicFeed :: T.Text -> IO (Maybe PublicFeed)
readPublicFeed = readKV

cachedReadPublicFeed :: T.Text -> IO (Maybe PublicFeed)
cachedReadPublicFeed = cachedReadKV

cachedNothingReadPublicFeed :: T.Text -> IO (Maybe PublicFeed)
cachedNothingReadPublicFeed = cachedNothingReadKV

mergeWritePublicFeed :: PublicFeed -> IO (())
mergeWritePublicFeed = mergeWriteKV

deletePublicFeed :: PublicFeed -> IO (())
deletePublicFeed = deleteKV

readManyPublicFeeds :: [T.Text] -> IO ([Maybe PublicFeed])
readManyPublicFeeds = readManyKVs

cachedReadManyPublicFeeds :: [T.Text] -> IO ([Maybe PublicFeed])
cachedReadManyPublicFeeds = cachedReadManyKVs

cachedNothingReadManyPublicFeeds :: [T.Text] -> IO ([Maybe PublicFeed])
cachedNothingReadManyPublicFeeds = cachedNothingReadManyKVs

writeManyPublicFeeds :: [PublicFeed] -> IO (())
writeManyPublicFeeds = writeManyKVs

modifyPublicFeed :: T.Text -> (Maybe PublicFeed -> IO (PublicFeed, b)) -> IO b
modifyPublicFeed = modifyKV

modifyPublicFeed_ :: T.Text -> (Maybe PublicFeed -> IO PublicFeed) -> IO ()
modifyPublicFeed_ = modifyKV_

modifyPublicFeed' :: T.Text -> (PublicFeed -> IO (PublicFeed, b)) -> IO b
modifyPublicFeed' key f = modifyKV key (f . fromMaybe (defaultPublicFeed key))

modifyPublicFeed'_ :: T.Text -> (PublicFeed -> IO PublicFeed) -> IO ()
modifyPublicFeed'_ key f = modifyKV_ key (f . fromMaybe (defaultPublicFeed key))

readPublicFeed' :: Key PublicFeed -> IO PublicFeed
readPublicFeed' key = liftM (fromMaybe (defaultPublicFeed key)) (readKV key)

cachedReadPublicFeed' :: Key PublicFeed -> IO PublicFeed
cachedReadPublicFeed' key = liftM (fromMaybe (defaultPublicFeed key)) (cachedReadKV key)

cachedNothingReadPublicFeed' :: Key PublicFeed -> IO PublicFeed
cachedNothingReadPublicFeed' key = liftM (fromMaybe (defaultPublicFeed key)) (cachedNothingReadKV key)

instance KV MobileLogin where
    type Key MobileLogin = T.Text
    kvBucket _ = "MobileLogin"
    kvKey = mlLogin
    kvCache _ = _mlCache
    kvPool _ = riakPool
_mlCache = unsafePerformIO $ newCache 300 300 (10*1024*1024)
{-# NOINLINE _mlCache #-}
readMobileLogin :: T.Text -> IO (Maybe MobileLogin)
readMobileLogin = readKV

cachedReadMobileLogin :: T.Text -> IO (Maybe MobileLogin)
cachedReadMobileLogin = cachedReadKV

cachedNothingReadMobileLogin :: T.Text -> IO (Maybe MobileLogin)
cachedNothingReadMobileLogin = cachedNothingReadKV

mergeWriteMobileLogin :: MobileLogin -> IO (())
mergeWriteMobileLogin = mergeWriteKV

deleteMobileLogin :: MobileLogin -> IO (())
deleteMobileLogin = deleteKV

readManyMobileLogins :: [T.Text] -> IO ([Maybe MobileLogin])
readManyMobileLogins = readManyKVs

cachedReadManyMobileLogins :: [T.Text] -> IO ([Maybe MobileLogin])
cachedReadManyMobileLogins = cachedReadManyKVs

cachedNothingReadManyMobileLogins :: [T.Text] -> IO ([Maybe MobileLogin])
cachedNothingReadManyMobileLogins = cachedNothingReadManyKVs

writeManyMobileLogins :: [MobileLogin] -> IO (())
writeManyMobileLogins = writeManyKVs

modifyMobileLogin :: T.Text -> (Maybe MobileLogin -> IO (MobileLogin, b)) -> IO b
modifyMobileLogin = modifyKV

modifyMobileLogin_ :: T.Text -> (Maybe MobileLogin -> IO MobileLogin) -> IO ()
modifyMobileLogin_ = modifyKV_

modifyMobileLogin' :: T.Text -> (MobileLogin -> IO (MobileLogin, b)) -> IO b
modifyMobileLogin' key f = modifyKV key (f . fromMaybe (defaultMobileLogin key))

modifyMobileLogin'_ :: T.Text -> (MobileLogin -> IO MobileLogin) -> IO ()
modifyMobileLogin'_ key f = modifyKV_ key (f . fromMaybe (defaultMobileLogin key))

readMobileLogin' :: Key MobileLogin -> IO MobileLogin
readMobileLogin' key = liftM (fromMaybe (defaultMobileLogin key)) (readKV key)

cachedReadMobileLogin' :: Key MobileLogin -> IO MobileLogin
cachedReadMobileLogin' key = liftM (fromMaybe (defaultMobileLogin key)) (cachedReadKV key)

cachedNothingReadMobileLogin' :: Key MobileLogin -> IO MobileLogin
cachedNothingReadMobileLogin' key = liftM (fromMaybe (defaultMobileLogin key)) (cachedNothingReadKV key)

instance KV FeverApiKey where
    type Key FeverApiKey = T.Text
    kvBucket _ = "FeverApiKey"
    kvKey = fakKey
    kvCache _ = _fakCache
    kvPool _ = riakPool
_fakCache = unsafePerformIO $ newCache 300 300 (10*1024*1024)
{-# NOINLINE _fakCache #-}
readFeverApiKey :: T.Text -> IO (Maybe FeverApiKey)
readFeverApiKey = readKV

cachedReadFeverApiKey :: T.Text -> IO (Maybe FeverApiKey)
cachedReadFeverApiKey = cachedReadKV

cachedNothingReadFeverApiKey :: T.Text -> IO (Maybe FeverApiKey)
cachedNothingReadFeverApiKey = cachedNothingReadKV

mergeWriteFeverApiKey :: FeverApiKey -> IO (())
mergeWriteFeverApiKey = mergeWriteKV

deleteFeverApiKey :: FeverApiKey -> IO (())
deleteFeverApiKey = deleteKV

readManyFeverApiKeys :: [T.Text] -> IO ([Maybe FeverApiKey])
readManyFeverApiKeys = readManyKVs

cachedReadManyFeverApiKeys :: [T.Text] -> IO ([Maybe FeverApiKey])
cachedReadManyFeverApiKeys = cachedReadManyKVs

cachedNothingReadManyFeverApiKeys :: [T.Text] -> IO ([Maybe FeverApiKey])
cachedNothingReadManyFeverApiKeys = cachedNothingReadManyKVs

writeManyFeverApiKeys :: [FeverApiKey] -> IO (())
writeManyFeverApiKeys = writeManyKVs

modifyFeverApiKey :: T.Text -> (Maybe FeverApiKey -> IO (FeverApiKey, b)) -> IO b
modifyFeverApiKey = modifyKV

modifyFeverApiKey_ :: T.Text -> (Maybe FeverApiKey -> IO FeverApiKey) -> IO ()
modifyFeverApiKey_ = modifyKV_

modifyFeverApiKey' :: T.Text -> (FeverApiKey -> IO (FeverApiKey, b)) -> IO b
modifyFeverApiKey' key f = modifyKV key (f . fromMaybe (defaultFeverApiKey key))

modifyFeverApiKey'_ :: T.Text -> (FeverApiKey -> IO FeverApiKey) -> IO ()
modifyFeverApiKey'_ key f = modifyKV_ key (f . fromMaybe (defaultFeverApiKey key))

readFeverApiKey' :: Key FeverApiKey -> IO FeverApiKey
readFeverApiKey' key = liftM (fromMaybe (defaultFeverApiKey key)) (readKV key)

cachedReadFeverApiKey' :: Key FeverApiKey -> IO FeverApiKey
cachedReadFeverApiKey' key = liftM (fromMaybe (defaultFeverApiKey key)) (cachedReadKV key)

cachedNothingReadFeverApiKey' :: Key FeverApiKey -> IO FeverApiKey
cachedNothingReadFeverApiKey' key = liftM (fromMaybe (defaultFeverApiKey key)) (cachedNothingReadKV key)

instance KV FeverIds where
    type Key FeverIds = T.Text
    kvBucket _ = "FeverIds"
    kvKey = fiUser
    kvCache _ = _fiCache
    kvPool _ = riakPool
_fiCache = unsafePerformIO $ newCache 240 240 (100*1024*1024)
{-# NOINLINE _fiCache #-}
readFeverIds :: T.Text -> IO (Maybe FeverIds)
readFeverIds = readKV

cachedReadFeverIds :: T.Text -> IO (Maybe FeverIds)
cachedReadFeverIds = cachedReadKV

cachedNothingReadFeverIds :: T.Text -> IO (Maybe FeverIds)
cachedNothingReadFeverIds = cachedNothingReadKV

mergeWriteFeverIds :: FeverIds -> IO (())
mergeWriteFeverIds = mergeWriteKV

deleteFeverIds :: FeverIds -> IO (())
deleteFeverIds = deleteKV

readManyFeverIdss :: [T.Text] -> IO ([Maybe FeverIds])
readManyFeverIdss = readManyKVs

cachedReadManyFeverIdss :: [T.Text] -> IO ([Maybe FeverIds])
cachedReadManyFeverIdss = cachedReadManyKVs

cachedNothingReadManyFeverIdss :: [T.Text] -> IO ([Maybe FeverIds])
cachedNothingReadManyFeverIdss = cachedNothingReadManyKVs

writeManyFeverIdss :: [FeverIds] -> IO (())
writeManyFeverIdss = writeManyKVs

modifyFeverIds :: T.Text -> (Maybe FeverIds -> IO (FeverIds, b)) -> IO b
modifyFeverIds = modifyKV

modifyFeverIds_ :: T.Text -> (Maybe FeverIds -> IO FeverIds) -> IO ()
modifyFeverIds_ = modifyKV_

modifyFeverIds' :: T.Text -> (FeverIds -> IO (FeverIds, b)) -> IO b
modifyFeverIds' key f = modifyKV key (f . fromMaybe (defaultFeverIds key))

modifyFeverIds'_ :: T.Text -> (FeverIds -> IO FeverIds) -> IO ()
modifyFeverIds'_ key f = modifyKV_ key (f . fromMaybe (defaultFeverIds key))

readFeverIds' :: Key FeverIds -> IO FeverIds
readFeverIds' key = liftM (fromMaybe (defaultFeverIds key)) (readKV key)

cachedReadFeverIds' :: Key FeverIds -> IO FeverIds
cachedReadFeverIds' key = liftM (fromMaybe (defaultFeverIds key)) (cachedReadKV key)

cachedNothingReadFeverIds' :: Key FeverIds -> IO FeverIds
cachedNothingReadFeverIds' key = liftM (fromMaybe (defaultFeverIds key)) (cachedNothingReadKV key)

instance KV UserStats where
    type Key UserStats = T.Text
    kvBucket _ = "UserStats"
    kvKey = usId
    kvCache _ = _usCache
    kvPool _ = riakPool
_usCache = unsafePerformIO $ newCache 240 240 (200*1024*1024)
{-# NOINLINE _usCache #-}
readUserStats :: T.Text -> IO (Maybe UserStats)
readUserStats = readKV

cachedReadUserStats :: T.Text -> IO (Maybe UserStats)
cachedReadUserStats = cachedReadKV

cachedNothingReadUserStats :: T.Text -> IO (Maybe UserStats)
cachedNothingReadUserStats = cachedNothingReadKV

mergeWriteUserStats :: UserStats -> IO (())
mergeWriteUserStats = mergeWriteKV

deleteUserStats :: UserStats -> IO (())
deleteUserStats = deleteKV

readManyUserStatss :: [T.Text] -> IO ([Maybe UserStats])
readManyUserStatss = readManyKVs

cachedReadManyUserStatss :: [T.Text] -> IO ([Maybe UserStats])
cachedReadManyUserStatss = cachedReadManyKVs

cachedNothingReadManyUserStatss :: [T.Text] -> IO ([Maybe UserStats])
cachedNothingReadManyUserStatss = cachedNothingReadManyKVs

writeManyUserStatss :: [UserStats] -> IO (())
writeManyUserStatss = writeManyKVs

modifyUserStats :: T.Text -> (Maybe UserStats -> IO (UserStats, b)) -> IO b
modifyUserStats = modifyKV

modifyUserStats_ :: T.Text -> (Maybe UserStats -> IO UserStats) -> IO ()
modifyUserStats_ = modifyKV_

modifyUserStats' :: T.Text -> (UserStats -> IO (UserStats, b)) -> IO b
modifyUserStats' key f = modifyKV key (f . fromMaybe (defaultUserStats key))

modifyUserStats'_ :: T.Text -> (UserStats -> IO UserStats) -> IO ()
modifyUserStats'_ key f = modifyKV_ key (f . fromMaybe (defaultUserStats key))

readUserStats' :: Key UserStats -> IO UserStats
readUserStats' key = liftM (fromMaybe (defaultUserStats key)) (readKV key)

cachedReadUserStats' :: Key UserStats -> IO UserStats
cachedReadUserStats' key = liftM (fromMaybe (defaultUserStats key)) (cachedReadKV key)

cachedNothingReadUserStats' :: Key UserStats -> IO UserStats
cachedNothingReadUserStats' key = liftM (fromMaybe (defaultUserStats key)) (cachedNothingReadKV key)

instance KV MailQueue where
    type Key MailQueue = T.Text
    kvBucket _ = "MailQueue"
    kvKey = mqId
    kvCache _ = _mqCache
    kvPool _ = riakPool
_mqCache = unsafePerformIO $ newCache 300 300 (200*1024*1024)
{-# NOINLINE _mqCache #-}
readMailQueue :: T.Text -> IO (Maybe MailQueue)
readMailQueue = readKV

cachedReadMailQueue :: T.Text -> IO (Maybe MailQueue)
cachedReadMailQueue = cachedReadKV

cachedNothingReadMailQueue :: T.Text -> IO (Maybe MailQueue)
cachedNothingReadMailQueue = cachedNothingReadKV

mergeWriteMailQueue :: MailQueue -> IO (())
mergeWriteMailQueue = mergeWriteKV

deleteMailQueue :: MailQueue -> IO (())
deleteMailQueue = deleteKV

readManyMailQueues :: [T.Text] -> IO ([Maybe MailQueue])
readManyMailQueues = readManyKVs

cachedReadManyMailQueues :: [T.Text] -> IO ([Maybe MailQueue])
cachedReadManyMailQueues = cachedReadManyKVs

cachedNothingReadManyMailQueues :: [T.Text] -> IO ([Maybe MailQueue])
cachedNothingReadManyMailQueues = cachedNothingReadManyKVs

writeManyMailQueues :: [MailQueue] -> IO (())
writeManyMailQueues = writeManyKVs

modifyMailQueue :: T.Text -> (Maybe MailQueue -> IO (MailQueue, b)) -> IO b
modifyMailQueue = modifyKV

modifyMailQueue_ :: T.Text -> (Maybe MailQueue -> IO MailQueue) -> IO ()
modifyMailQueue_ = modifyKV_

modifyMailQueue' :: T.Text -> (MailQueue -> IO (MailQueue, b)) -> IO b
modifyMailQueue' key f = modifyKV key (f . fromMaybe (defaultMailQueue key))

modifyMailQueue'_ :: T.Text -> (MailQueue -> IO MailQueue) -> IO ()
modifyMailQueue'_ key f = modifyKV_ key (f . fromMaybe (defaultMailQueue key))

readMailQueue' :: Key MailQueue -> IO MailQueue
readMailQueue' key = liftM (fromMaybe (defaultMailQueue key)) (readKV key)

cachedReadMailQueue' :: Key MailQueue -> IO MailQueue
cachedReadMailQueue' key = liftM (fromMaybe (defaultMailQueue key)) (cachedReadKV key)

cachedNothingReadMailQueue' :: Key MailQueue -> IO MailQueue
cachedNothingReadMailQueue' key = liftM (fromMaybe (defaultMailQueue key)) (cachedNothingReadKV key)

instance KV Session where
    type Key Session = T.Text
    kvBucket _ = "Session"
    kvKey = sessionKey
    kvCache _ = _sessionCache
    kvPool _ = riakPool
_sessionCache = unsafePerformIO $ newCache 3600 3600 (100*1024*1024)
{-# NOINLINE _sessionCache #-}
readSession :: T.Text -> IO (Maybe Session)
readSession = readKV

cachedReadSession :: T.Text -> IO (Maybe Session)
cachedReadSession = cachedReadKV

cachedNothingReadSession :: T.Text -> IO (Maybe Session)
cachedNothingReadSession = cachedNothingReadKV

mergeWriteSession :: Session -> IO (())
mergeWriteSession = mergeWriteKV

deleteSession :: Session -> IO (())
deleteSession = deleteKV

readManySessions :: [T.Text] -> IO ([Maybe Session])
readManySessions = readManyKVs

cachedReadManySessions :: [T.Text] -> IO ([Maybe Session])
cachedReadManySessions = cachedReadManyKVs

cachedNothingReadManySessions :: [T.Text] -> IO ([Maybe Session])
cachedNothingReadManySessions = cachedNothingReadManyKVs

writeManySessions :: [Session] -> IO (())
writeManySessions = writeManyKVs

modifySession :: T.Text -> (Maybe Session -> IO (Session, b)) -> IO b
modifySession = modifyKV

modifySession_ :: T.Text -> (Maybe Session -> IO Session) -> IO ()
modifySession_ = modifyKV_

modifySession' :: T.Text -> (Session -> IO (Session, b)) -> IO b
modifySession' key f = modifyKV key (f . fromMaybe (defaultSession key))

modifySession'_ :: T.Text -> (Session -> IO Session) -> IO ()
modifySession'_ key f = modifyKV_ key (f . fromMaybe (defaultSession key))

readSession' :: Key Session -> IO Session
readSession' key = liftM (fromMaybe (defaultSession key)) (readKV key)

cachedReadSession' :: Key Session -> IO Session
cachedReadSession' key = liftM (fromMaybe (defaultSession key)) (cachedReadKV key)

cachedNothingReadSession' :: Key Session -> IO Session
cachedNothingReadSession' key = liftM (fromMaybe (defaultSession key)) (cachedNothingReadKV key)

instance KV SubscriptionUrlInfo where
    type Key SubscriptionUrlInfo = TURL
    kvBucket _ = "SubscriptionUrlInfo"
    kvKey = suiUrl
    kvCache _ = _suiCache
    kvPool _ = riakPool
_suiCache = unsafePerformIO $ newCache 0 0 (0*1024*1024)
{-# NOINLINE _suiCache #-}
readSubscriptionUrlInfo :: TURL -> IO (Maybe SubscriptionUrlInfo)
readSubscriptionUrlInfo = readKV

cachedReadSubscriptionUrlInfo :: TURL -> IO (Maybe SubscriptionUrlInfo)
cachedReadSubscriptionUrlInfo = cachedReadKV

cachedNothingReadSubscriptionUrlInfo :: TURL -> IO (Maybe SubscriptionUrlInfo)
cachedNothingReadSubscriptionUrlInfo = cachedNothingReadKV

mergeWriteSubscriptionUrlInfo :: SubscriptionUrlInfo -> IO (())
mergeWriteSubscriptionUrlInfo = mergeWriteKV

deleteSubscriptionUrlInfo :: SubscriptionUrlInfo -> IO (())
deleteSubscriptionUrlInfo = deleteKV

readManySubscriptionUrlInfos :: [TURL] -> IO ([Maybe SubscriptionUrlInfo])
readManySubscriptionUrlInfos = readManyKVs

cachedReadManySubscriptionUrlInfos :: [TURL] -> IO ([Maybe SubscriptionUrlInfo])
cachedReadManySubscriptionUrlInfos = cachedReadManyKVs

cachedNothingReadManySubscriptionUrlInfos :: [TURL] -> IO ([Maybe SubscriptionUrlInfo])
cachedNothingReadManySubscriptionUrlInfos = cachedNothingReadManyKVs

writeManySubscriptionUrlInfos :: [SubscriptionUrlInfo] -> IO (())
writeManySubscriptionUrlInfos = writeManyKVs

modifySubscriptionUrlInfo :: TURL -> (Maybe SubscriptionUrlInfo -> IO (SubscriptionUrlInfo, b)) -> IO b
modifySubscriptionUrlInfo = modifyKV

modifySubscriptionUrlInfo_ :: TURL -> (Maybe SubscriptionUrlInfo -> IO SubscriptionUrlInfo) -> IO ()
modifySubscriptionUrlInfo_ = modifyKV_

modifySubscriptionUrlInfo' :: TURL -> (SubscriptionUrlInfo -> IO (SubscriptionUrlInfo, b)) -> IO b
modifySubscriptionUrlInfo' key f = modifyKV key (f . fromMaybe (defaultSubscriptionUrlInfo key))

modifySubscriptionUrlInfo'_ :: TURL -> (SubscriptionUrlInfo -> IO SubscriptionUrlInfo) -> IO ()
modifySubscriptionUrlInfo'_ key f = modifyKV_ key (f . fromMaybe (defaultSubscriptionUrlInfo key))

readSubscriptionUrlInfo' :: Key SubscriptionUrlInfo -> IO SubscriptionUrlInfo
readSubscriptionUrlInfo' key = liftM (fromMaybe (defaultSubscriptionUrlInfo key)) (readKV key)

cachedReadSubscriptionUrlInfo' :: Key SubscriptionUrlInfo -> IO SubscriptionUrlInfo
cachedReadSubscriptionUrlInfo' key = liftM (fromMaybe (defaultSubscriptionUrlInfo key)) (cachedReadKV key)

cachedNothingReadSubscriptionUrlInfo' :: Key SubscriptionUrlInfo -> IO SubscriptionUrlInfo
cachedNothingReadSubscriptionUrlInfo' key = liftM (fromMaybe (defaultSubscriptionUrlInfo key)) (cachedNothingReadKV key)

instance KV Msg where
    type Key Msg = MsgKey
    kvBucket _ = "Msg"
    kvKey = msgKey
    kvCache _ = _msgCache
    kvPool _ = riakPool
_msgCache = unsafePerformIO $ newCache 60 60 (10*1024*1024)
{-# NOINLINE _msgCache #-}
readMsg :: MsgKey -> IO (Maybe Msg)
readMsg = readKV

cachedReadMsg :: MsgKey -> IO (Maybe Msg)
cachedReadMsg = cachedReadKV

cachedNothingReadMsg :: MsgKey -> IO (Maybe Msg)
cachedNothingReadMsg = cachedNothingReadKV

mergeWriteMsg :: Msg -> IO (())
mergeWriteMsg = mergeWriteKV

deleteMsg :: Msg -> IO (())
deleteMsg = deleteKV

readManyMsgs :: [MsgKey] -> IO ([Maybe Msg])
readManyMsgs = readManyKVs

cachedReadManyMsgs :: [MsgKey] -> IO ([Maybe Msg])
cachedReadManyMsgs = cachedReadManyKVs

cachedNothingReadManyMsgs :: [MsgKey] -> IO ([Maybe Msg])
cachedNothingReadManyMsgs = cachedNothingReadManyKVs

writeManyMsgs :: [Msg] -> IO (())
writeManyMsgs = writeManyKVs

modifyMsg :: MsgKey -> (Maybe Msg -> IO (Msg, b)) -> IO b
modifyMsg = modifyKV

modifyMsg_ :: MsgKey -> (Maybe Msg -> IO Msg) -> IO ()
modifyMsg_ = modifyKV_

modifyMsg' :: MsgKey -> (Msg -> IO (Msg, b)) -> IO b
modifyMsg' key f = modifyKV key (f . fromMaybe (defaultMsg key))

modifyMsg'_ :: MsgKey -> (Msg -> IO Msg) -> IO ()
modifyMsg'_ key f = modifyKV_ key (f . fromMaybe (defaultMsg key))

readMsg' :: Key Msg -> IO Msg
readMsg' key = liftM (fromMaybe (defaultMsg key)) (readKV key)

cachedReadMsg' :: Key Msg -> IO Msg
cachedReadMsg' key = liftM (fromMaybe (defaultMsg key)) (cachedReadKV key)

cachedNothingReadMsg' :: Key Msg -> IO Msg
cachedNothingReadMsg' key = liftM (fromMaybe (defaultMsg key)) (cachedNothingReadKV key)

instance KV BlogPostsScanned where
    type Key BlogPostsScanned = TURL
    kvBucket _ = "BlogPostsScanned"
    kvKey = bpsBlogFeedUrl
    kvCache _ = _bpsCache
    kvPool _ = riakPool
_bpsCache = unsafePerformIO $ newCache 3 240 (128*1024*1024)
{-# NOINLINE _bpsCache #-}
readBlogPostsScanned :: TURL -> IO (Maybe BlogPostsScanned)
readBlogPostsScanned = readKV

cachedReadBlogPostsScanned :: TURL -> IO (Maybe BlogPostsScanned)
cachedReadBlogPostsScanned = cachedReadKV

cachedNothingReadBlogPostsScanned :: TURL -> IO (Maybe BlogPostsScanned)
cachedNothingReadBlogPostsScanned = cachedNothingReadKV

mergeWriteBlogPostsScanned :: BlogPostsScanned -> IO (())
mergeWriteBlogPostsScanned = mergeWriteKV

deleteBlogPostsScanned :: BlogPostsScanned -> IO (())
deleteBlogPostsScanned = deleteKV

readManyBlogPostsScanneds :: [TURL] -> IO ([Maybe BlogPostsScanned])
readManyBlogPostsScanneds = readManyKVs

cachedReadManyBlogPostsScanneds :: [TURL] -> IO ([Maybe BlogPostsScanned])
cachedReadManyBlogPostsScanneds = cachedReadManyKVs

cachedNothingReadManyBlogPostsScanneds :: [TURL] -> IO ([Maybe BlogPostsScanned])
cachedNothingReadManyBlogPostsScanneds = cachedNothingReadManyKVs

writeManyBlogPostsScanneds :: [BlogPostsScanned] -> IO (())
writeManyBlogPostsScanneds = writeManyKVs

modifyBlogPostsScanned :: TURL -> (Maybe BlogPostsScanned -> IO (BlogPostsScanned, b)) -> IO b
modifyBlogPostsScanned = modifyKV

modifyBlogPostsScanned_ :: TURL -> (Maybe BlogPostsScanned -> IO BlogPostsScanned) -> IO ()
modifyBlogPostsScanned_ = modifyKV_

modifyBlogPostsScanned' :: TURL -> (BlogPostsScanned -> IO (BlogPostsScanned, b)) -> IO b
modifyBlogPostsScanned' key f = modifyKV key (f . fromMaybe (defaultBlogPostsScanned key))

modifyBlogPostsScanned'_ :: TURL -> (BlogPostsScanned -> IO BlogPostsScanned) -> IO ()
modifyBlogPostsScanned'_ key f = modifyKV_ key (f . fromMaybe (defaultBlogPostsScanned key))

readBlogPostsScanned' :: Key BlogPostsScanned -> IO BlogPostsScanned
readBlogPostsScanned' key = liftM (fromMaybe (defaultBlogPostsScanned key)) (readKV key)

cachedReadBlogPostsScanned' :: Key BlogPostsScanned -> IO BlogPostsScanned
cachedReadBlogPostsScanned' key = liftM (fromMaybe (defaultBlogPostsScanned key)) (cachedReadKV key)

cachedNothingReadBlogPostsScanned' :: Key BlogPostsScanned -> IO BlogPostsScanned
cachedNothingReadBlogPostsScanned' key = liftM (fromMaybe (defaultBlogPostsScanned key)) (cachedNothingReadKV key)

instance KV Posts where
    type Key Posts = TURL
    kvBucket _ = "Posts"
    kvKey = pBlogFeedUrl
    kvCache _ = _pCache
    kvPool _ = riakPool
_pCache = unsafePerformIO $ newCache 3 140 (700*1024*1024)
{-# NOINLINE _pCache #-}
readPosts :: TURL -> IO (Maybe Posts)
readPosts = readKV

cachedReadPosts :: TURL -> IO (Maybe Posts)
cachedReadPosts = cachedReadKV

cachedNothingReadPosts :: TURL -> IO (Maybe Posts)
cachedNothingReadPosts = cachedNothingReadKV

mergeWritePosts :: Posts -> IO (())
mergeWritePosts = mergeWriteKV

deletePosts :: Posts -> IO (())
deletePosts = deleteKV

readManyPostss :: [TURL] -> IO ([Maybe Posts])
readManyPostss = readManyKVs

cachedReadManyPostss :: [TURL] -> IO ([Maybe Posts])
cachedReadManyPostss = cachedReadManyKVs

cachedNothingReadManyPostss :: [TURL] -> IO ([Maybe Posts])
cachedNothingReadManyPostss = cachedNothingReadManyKVs

writeManyPostss :: [Posts] -> IO (())
writeManyPostss = writeManyKVs

modifyPosts :: TURL -> (Maybe Posts -> IO (Posts, b)) -> IO b
modifyPosts = modifyKV

modifyPosts_ :: TURL -> (Maybe Posts -> IO Posts) -> IO ()
modifyPosts_ = modifyKV_

modifyPosts' :: TURL -> (Posts -> IO (Posts, b)) -> IO b
modifyPosts' key f = modifyKV key (f . fromMaybe (defaultPosts key))

modifyPosts'_ :: TURL -> (Posts -> IO Posts) -> IO ()
modifyPosts'_ key f = modifyKV_ key (f . fromMaybe (defaultPosts key))

readPosts' :: Key Posts -> IO Posts
readPosts' key = liftM (fromMaybe (defaultPosts key)) (readKV key)

cachedReadPosts' :: Key Posts -> IO Posts
cachedReadPosts' key = liftM (fromMaybe (defaultPosts key)) (cachedReadKV key)

cachedNothingReadPosts' :: Key Posts -> IO Posts
cachedNothingReadPosts' key = liftM (fromMaybe (defaultPosts key)) (cachedNothingReadKV key)

instance KV DiscoveryFeed where
    type Key DiscoveryFeed = TURL
    kvBucket _ = "DiscoveryFeed"
    kvKey = dfUrl
    kvCache _ = _dfCache
    kvPool _ = riakPool
_dfCache = unsafePerformIO $ newCache 60 60 (100*1024*1024)
{-# NOINLINE _dfCache #-}
readDiscoveryFeed :: TURL -> IO (Maybe DiscoveryFeed)
readDiscoveryFeed = readKV

cachedReadDiscoveryFeed :: TURL -> IO (Maybe DiscoveryFeed)
cachedReadDiscoveryFeed = cachedReadKV

cachedNothingReadDiscoveryFeed :: TURL -> IO (Maybe DiscoveryFeed)
cachedNothingReadDiscoveryFeed = cachedNothingReadKV

mergeWriteDiscoveryFeed :: DiscoveryFeed -> IO (())
mergeWriteDiscoveryFeed = mergeWriteKV

deleteDiscoveryFeed :: DiscoveryFeed -> IO (())
deleteDiscoveryFeed = deleteKV

readManyDiscoveryFeeds :: [TURL] -> IO ([Maybe DiscoveryFeed])
readManyDiscoveryFeeds = readManyKVs

cachedReadManyDiscoveryFeeds :: [TURL] -> IO ([Maybe DiscoveryFeed])
cachedReadManyDiscoveryFeeds = cachedReadManyKVs

cachedNothingReadManyDiscoveryFeeds :: [TURL] -> IO ([Maybe DiscoveryFeed])
cachedNothingReadManyDiscoveryFeeds = cachedNothingReadManyKVs

writeManyDiscoveryFeeds :: [DiscoveryFeed] -> IO (())
writeManyDiscoveryFeeds = writeManyKVs

modifyDiscoveryFeed :: TURL -> (Maybe DiscoveryFeed -> IO (DiscoveryFeed, b)) -> IO b
modifyDiscoveryFeed = modifyKV

modifyDiscoveryFeed_ :: TURL -> (Maybe DiscoveryFeed -> IO DiscoveryFeed) -> IO ()
modifyDiscoveryFeed_ = modifyKV_

modifyDiscoveryFeed' :: TURL -> (DiscoveryFeed -> IO (DiscoveryFeed, b)) -> IO b
modifyDiscoveryFeed' key f = modifyKV key (f . fromMaybe (defaultDiscoveryFeed key))

modifyDiscoveryFeed'_ :: TURL -> (DiscoveryFeed -> IO DiscoveryFeed) -> IO ()
modifyDiscoveryFeed'_ key f = modifyKV_ key (f . fromMaybe (defaultDiscoveryFeed key))

readDiscoveryFeed' :: Key DiscoveryFeed -> IO DiscoveryFeed
readDiscoveryFeed' key = liftM (fromMaybe (defaultDiscoveryFeed key)) (readKV key)

cachedReadDiscoveryFeed' :: Key DiscoveryFeed -> IO DiscoveryFeed
cachedReadDiscoveryFeed' key = liftM (fromMaybe (defaultDiscoveryFeed key)) (cachedReadKV key)

cachedNothingReadDiscoveryFeed' :: Key DiscoveryFeed -> IO DiscoveryFeed
cachedNothingReadDiscoveryFeed' key = liftM (fromMaybe (defaultDiscoveryFeed key)) (cachedNothingReadKV key)

instance KV PostsClearTime where
    type Key PostsClearTime = TURL
    kvBucket _ = "PostsClearTime"
    kvKey = pctBlogFeedUrl
    kvCache _ = _pctCache
    kvPool _ = riakPool
_pctCache = unsafePerformIO $ newCache 300 300 (100*1024*1024)
{-# NOINLINE _pctCache #-}
readPostsClearTime :: TURL -> IO (Maybe PostsClearTime)
readPostsClearTime = readKV

cachedReadPostsClearTime :: TURL -> IO (Maybe PostsClearTime)
cachedReadPostsClearTime = cachedReadKV

cachedNothingReadPostsClearTime :: TURL -> IO (Maybe PostsClearTime)
cachedNothingReadPostsClearTime = cachedNothingReadKV

mergeWritePostsClearTime :: PostsClearTime -> IO (())
mergeWritePostsClearTime = mergeWriteKV

deletePostsClearTime :: PostsClearTime -> IO (())
deletePostsClearTime = deleteKV

readManyPostsClearTimes :: [TURL] -> IO ([Maybe PostsClearTime])
readManyPostsClearTimes = readManyKVs

cachedReadManyPostsClearTimes :: [TURL] -> IO ([Maybe PostsClearTime])
cachedReadManyPostsClearTimes = cachedReadManyKVs

cachedNothingReadManyPostsClearTimes :: [TURL] -> IO ([Maybe PostsClearTime])
cachedNothingReadManyPostsClearTimes = cachedNothingReadManyKVs

writeManyPostsClearTimes :: [PostsClearTime] -> IO (())
writeManyPostsClearTimes = writeManyKVs

modifyPostsClearTime :: TURL -> (Maybe PostsClearTime -> IO (PostsClearTime, b)) -> IO b
modifyPostsClearTime = modifyKV

modifyPostsClearTime_ :: TURL -> (Maybe PostsClearTime -> IO PostsClearTime) -> IO ()
modifyPostsClearTime_ = modifyKV_

modifyPostsClearTime' :: TURL -> (PostsClearTime -> IO (PostsClearTime, b)) -> IO b
modifyPostsClearTime' key f = modifyKV key (f . fromMaybe (defaultPostsClearTime key))

modifyPostsClearTime'_ :: TURL -> (PostsClearTime -> IO PostsClearTime) -> IO ()
modifyPostsClearTime'_ key f = modifyKV_ key (f . fromMaybe (defaultPostsClearTime key))

readPostsClearTime' :: Key PostsClearTime -> IO PostsClearTime
readPostsClearTime' key = liftM (fromMaybe (defaultPostsClearTime key)) (readKV key)

cachedReadPostsClearTime' :: Key PostsClearTime -> IO PostsClearTime
cachedReadPostsClearTime' key = liftM (fromMaybe (defaultPostsClearTime key)) (cachedReadKV key)

cachedNothingReadPostsClearTime' :: Key PostsClearTime -> IO PostsClearTime
cachedNothingReadPostsClearTime' key = liftM (fromMaybe (defaultPostsClearTime key)) (cachedNothingReadKV key)

instance KV PostsSubscribers where
    type Key PostsSubscribers = TURL
    kvBucket _ = "PostsSubscribers"
    kvKey = psBlogFeedUrl
    kvCache _ = _psCache
    kvPool _ = riakPool
_psCache = unsafePerformIO $ newCache 300 300 (100*1024*1024)
{-# NOINLINE _psCache #-}
readPostsSubscribers :: TURL -> IO (Maybe PostsSubscribers)
readPostsSubscribers = readKV

cachedReadPostsSubscribers :: TURL -> IO (Maybe PostsSubscribers)
cachedReadPostsSubscribers = cachedReadKV

cachedNothingReadPostsSubscribers :: TURL -> IO (Maybe PostsSubscribers)
cachedNothingReadPostsSubscribers = cachedNothingReadKV

mergeWritePostsSubscribers :: PostsSubscribers -> IO (())
mergeWritePostsSubscribers = mergeWriteKV

deletePostsSubscribers :: PostsSubscribers -> IO (())
deletePostsSubscribers = deleteKV

readManyPostsSubscriberss :: [TURL] -> IO ([Maybe PostsSubscribers])
readManyPostsSubscriberss = readManyKVs

cachedReadManyPostsSubscriberss :: [TURL] -> IO ([Maybe PostsSubscribers])
cachedReadManyPostsSubscriberss = cachedReadManyKVs

cachedNothingReadManyPostsSubscriberss :: [TURL] -> IO ([Maybe PostsSubscribers])
cachedNothingReadManyPostsSubscriberss = cachedNothingReadManyKVs

writeManyPostsSubscriberss :: [PostsSubscribers] -> IO (())
writeManyPostsSubscriberss = writeManyKVs

modifyPostsSubscribers :: TURL -> (Maybe PostsSubscribers -> IO (PostsSubscribers, b)) -> IO b
modifyPostsSubscribers = modifyKV

modifyPostsSubscribers_ :: TURL -> (Maybe PostsSubscribers -> IO PostsSubscribers) -> IO ()
modifyPostsSubscribers_ = modifyKV_

modifyPostsSubscribers' :: TURL -> (PostsSubscribers -> IO (PostsSubscribers, b)) -> IO b
modifyPostsSubscribers' key f = modifyKV key (f . fromMaybe (defaultPostsSubscribers key))

modifyPostsSubscribers'_ :: TURL -> (PostsSubscribers -> IO PostsSubscribers) -> IO ()
modifyPostsSubscribers'_ key f = modifyKV_ key (f . fromMaybe (defaultPostsSubscribers key))

readPostsSubscribers' :: Key PostsSubscribers -> IO PostsSubscribers
readPostsSubscribers' key = liftM (fromMaybe (defaultPostsSubscribers key)) (readKV key)

cachedReadPostsSubscribers' :: Key PostsSubscribers -> IO PostsSubscribers
cachedReadPostsSubscribers' key = liftM (fromMaybe (defaultPostsSubscribers key)) (cachedReadKV key)

cachedNothingReadPostsSubscribers' :: Key PostsSubscribers -> IO PostsSubscribers
cachedNothingReadPostsSubscribers' key = liftM (fromMaybe (defaultPostsSubscribers key)) (cachedNothingReadKV key)

instance KV ActiveCheckSubscriptions where
    type Key ActiveCheckSubscriptions = ()
    kvBucket _ = "ActiveCheckSubscriptions"
    kvKey = acsKey
    kvCache _ = _acsCache
    kvPool _ = riakPool
_acsCache = unsafePerformIO $ newCache 0 0 (0*1024*1024)
{-# NOINLINE _acsCache #-}
readActiveCheckSubscriptions :: () -> IO (Maybe ActiveCheckSubscriptions)
readActiveCheckSubscriptions = readKV

cachedReadActiveCheckSubscriptions :: () -> IO (Maybe ActiveCheckSubscriptions)
cachedReadActiveCheckSubscriptions = cachedReadKV

cachedNothingReadActiveCheckSubscriptions :: () -> IO (Maybe ActiveCheckSubscriptions)
cachedNothingReadActiveCheckSubscriptions = cachedNothingReadKV

mergeWriteActiveCheckSubscriptions :: ActiveCheckSubscriptions -> IO (())
mergeWriteActiveCheckSubscriptions = mergeWriteKV

deleteActiveCheckSubscriptions :: ActiveCheckSubscriptions -> IO (())
deleteActiveCheckSubscriptions = deleteKV

readManyActiveCheckSubscriptionss :: [()] -> IO ([Maybe ActiveCheckSubscriptions])
readManyActiveCheckSubscriptionss = readManyKVs

cachedReadManyActiveCheckSubscriptionss :: [()] -> IO ([Maybe ActiveCheckSubscriptions])
cachedReadManyActiveCheckSubscriptionss = cachedReadManyKVs

cachedNothingReadManyActiveCheckSubscriptionss :: [()] -> IO ([Maybe ActiveCheckSubscriptions])
cachedNothingReadManyActiveCheckSubscriptionss = cachedNothingReadManyKVs

writeManyActiveCheckSubscriptionss :: [ActiveCheckSubscriptions] -> IO (())
writeManyActiveCheckSubscriptionss = writeManyKVs

modifyActiveCheckSubscriptions :: () -> (Maybe ActiveCheckSubscriptions -> IO (ActiveCheckSubscriptions, b)) -> IO b
modifyActiveCheckSubscriptions = modifyKV

modifyActiveCheckSubscriptions_ :: () -> (Maybe ActiveCheckSubscriptions -> IO ActiveCheckSubscriptions) -> IO ()
modifyActiveCheckSubscriptions_ = modifyKV_

modifyActiveCheckSubscriptions' :: () -> (ActiveCheckSubscriptions -> IO (ActiveCheckSubscriptions, b)) -> IO b
modifyActiveCheckSubscriptions' key f = modifyKV key (f . fromMaybe (defaultActiveCheckSubscriptions key))

modifyActiveCheckSubscriptions'_ :: () -> (ActiveCheckSubscriptions -> IO ActiveCheckSubscriptions) -> IO ()
modifyActiveCheckSubscriptions'_ key f = modifyKV_ key (f . fromMaybe (defaultActiveCheckSubscriptions key))

readActiveCheckSubscriptions' :: Key ActiveCheckSubscriptions -> IO ActiveCheckSubscriptions
readActiveCheckSubscriptions' key = liftM (fromMaybe (defaultActiveCheckSubscriptions key)) (readKV key)

cachedReadActiveCheckSubscriptions' :: Key ActiveCheckSubscriptions -> IO ActiveCheckSubscriptions
cachedReadActiveCheckSubscriptions' key = liftM (fromMaybe (defaultActiveCheckSubscriptions key)) (cachedReadKV key)

cachedNothingReadActiveCheckSubscriptions' :: Key ActiveCheckSubscriptions -> IO ActiveCheckSubscriptions
cachedNothingReadActiveCheckSubscriptions' key = liftM (fromMaybe (defaultActiveCheckSubscriptions key)) (cachedNothingReadKV key)

instance KV Comments where
    type Key Comments = CommentsKey
    kvBucket _ = "Comments"
    kvKey = cKey
    kvCache _ = _cCache
    kvPool _ = riakPool
_cCache = unsafePerformIO $ newCache 3 60 (20*1024*1024)
{-# NOINLINE _cCache #-}
readComments :: CommentsKey -> IO (Maybe Comments)
readComments = readKV

cachedReadComments :: CommentsKey -> IO (Maybe Comments)
cachedReadComments = cachedReadKV

cachedNothingReadComments :: CommentsKey -> IO (Maybe Comments)
cachedNothingReadComments = cachedNothingReadKV

mergeWriteComments :: Comments -> IO (())
mergeWriteComments = mergeWriteKV

deleteComments :: Comments -> IO (())
deleteComments = deleteKV

readManyCommentss :: [CommentsKey] -> IO ([Maybe Comments])
readManyCommentss = readManyKVs

cachedReadManyCommentss :: [CommentsKey] -> IO ([Maybe Comments])
cachedReadManyCommentss = cachedReadManyKVs

cachedNothingReadManyCommentss :: [CommentsKey] -> IO ([Maybe Comments])
cachedNothingReadManyCommentss = cachedNothingReadManyKVs

writeManyCommentss :: [Comments] -> IO (())
writeManyCommentss = writeManyKVs

modifyComments :: CommentsKey -> (Maybe Comments -> IO (Comments, b)) -> IO b
modifyComments = modifyKV

modifyComments_ :: CommentsKey -> (Maybe Comments -> IO Comments) -> IO ()
modifyComments_ = modifyKV_

modifyComments' :: CommentsKey -> (Comments -> IO (Comments, b)) -> IO b
modifyComments' key f = modifyKV key (f . fromMaybe (defaultComments key))

modifyComments'_ :: CommentsKey -> (Comments -> IO Comments) -> IO ()
modifyComments'_ key f = modifyKV_ key (f . fromMaybe (defaultComments key))

readComments' :: Key Comments -> IO Comments
readComments' key = liftM (fromMaybe (defaultComments key)) (readKV key)

cachedReadComments' :: Key Comments -> IO Comments
cachedReadComments' key = liftM (fromMaybe (defaultComments key)) (cachedReadKV key)

cachedNothingReadComments' :: Key Comments -> IO Comments
cachedNothingReadComments' key = liftM (fromMaybe (defaultComments key)) (cachedNothingReadKV key)

instance KV UrlToScan where
    type Key UrlToScan = TURL
    kvBucket _ = "UrlToScan"
    kvKey = utsUrl
    kvCache _ = _utsCache
    kvPool _ = riakPool
_utsCache = unsafePerformIO $ newCache 0 0 (0*1024*1024)
{-# NOINLINE _utsCache #-}
readUrlToScan :: TURL -> IO (Maybe UrlToScan)
readUrlToScan = readKV

cachedReadUrlToScan :: TURL -> IO (Maybe UrlToScan)
cachedReadUrlToScan = cachedReadKV

cachedNothingReadUrlToScan :: TURL -> IO (Maybe UrlToScan)
cachedNothingReadUrlToScan = cachedNothingReadKV

mergeWriteUrlToScan :: UrlToScan -> IO (())
mergeWriteUrlToScan = mergeWriteKV

deleteUrlToScan :: UrlToScan -> IO (())
deleteUrlToScan = deleteKV

readManyUrlToScans :: [TURL] -> IO ([Maybe UrlToScan])
readManyUrlToScans = readManyKVs

cachedReadManyUrlToScans :: [TURL] -> IO ([Maybe UrlToScan])
cachedReadManyUrlToScans = cachedReadManyKVs

cachedNothingReadManyUrlToScans :: [TURL] -> IO ([Maybe UrlToScan])
cachedNothingReadManyUrlToScans = cachedNothingReadManyKVs

writeManyUrlToScans :: [UrlToScan] -> IO (())
writeManyUrlToScans = writeManyKVs

modifyUrlToScan :: TURL -> (Maybe UrlToScan -> IO (UrlToScan, b)) -> IO b
modifyUrlToScan = modifyKV

modifyUrlToScan_ :: TURL -> (Maybe UrlToScan -> IO UrlToScan) -> IO ()
modifyUrlToScan_ = modifyKV_

modifyUrlToScan' :: TURL -> (UrlToScan -> IO (UrlToScan, b)) -> IO b
modifyUrlToScan' key f = modifyKV key (f . fromMaybe (defaultUrlToScan key))

modifyUrlToScan'_ :: TURL -> (UrlToScan -> IO UrlToScan) -> IO ()
modifyUrlToScan'_ key f = modifyKV_ key (f . fromMaybe (defaultUrlToScan key))

readUrlToScan' :: Key UrlToScan -> IO UrlToScan
readUrlToScan' key = liftM (fromMaybe (defaultUrlToScan key)) (readKV key)

cachedReadUrlToScan' :: Key UrlToScan -> IO UrlToScan
cachedReadUrlToScan' key = liftM (fromMaybe (defaultUrlToScan key)) (cachedReadKV key)

cachedNothingReadUrlToScan' :: Key UrlToScan -> IO UrlToScan
cachedNothingReadUrlToScan' key = liftM (fromMaybe (defaultUrlToScan key)) (cachedNothingReadKV key)

instance KV ScanList where
    type Key ScanList = UrTime
    kvBucket _ = "ScanList"
    kvKey = slTime
    kvCache _ = _slCache
    kvPool _ = riakPool
_slCache = unsafePerformIO $ newCache 0 0 (0*1024*1024)
{-# NOINLINE _slCache #-}
readScanList :: UrTime -> IO (Maybe ScanList)
readScanList = readKV

cachedReadScanList :: UrTime -> IO (Maybe ScanList)
cachedReadScanList = cachedReadKV

cachedNothingReadScanList :: UrTime -> IO (Maybe ScanList)
cachedNothingReadScanList = cachedNothingReadKV

mergeWriteScanList :: ScanList -> IO (())
mergeWriteScanList = mergeWriteKV

deleteScanList :: ScanList -> IO (())
deleteScanList = deleteKV

readManyScanLists :: [UrTime] -> IO ([Maybe ScanList])
readManyScanLists = readManyKVs

cachedReadManyScanLists :: [UrTime] -> IO ([Maybe ScanList])
cachedReadManyScanLists = cachedReadManyKVs

cachedNothingReadManyScanLists :: [UrTime] -> IO ([Maybe ScanList])
cachedNothingReadManyScanLists = cachedNothingReadManyKVs

writeManyScanLists :: [ScanList] -> IO (())
writeManyScanLists = writeManyKVs

modifyScanList :: UrTime -> (Maybe ScanList -> IO (ScanList, b)) -> IO b
modifyScanList = modifyKV

modifyScanList_ :: UrTime -> (Maybe ScanList -> IO ScanList) -> IO ()
modifyScanList_ = modifyKV_

modifyScanList' :: UrTime -> (ScanList -> IO (ScanList, b)) -> IO b
modifyScanList' key f = modifyKV key (f . fromMaybe (defaultScanList key))

modifyScanList'_ :: UrTime -> (ScanList -> IO ScanList) -> IO ()
modifyScanList'_ key f = modifyKV_ key (f . fromMaybe (defaultScanList key))

readScanList' :: Key ScanList -> IO ScanList
readScanList' key = liftM (fromMaybe (defaultScanList key)) (readKV key)

cachedReadScanList' :: Key ScanList -> IO ScanList
cachedReadScanList' key = liftM (fromMaybe (defaultScanList key)) (cachedReadKV key)

cachedNothingReadScanList' :: Key ScanList -> IO ScanList
cachedNothingReadScanList' key = liftM (fromMaybe (defaultScanList key)) (cachedNothingReadKV key)

instance KV PostsRead where
    type Key PostsRead = (T.Text, TURL)
    kvBucket _ = "PostsRead"
    kvKey = prKey
    kvCache _ = _prCache
    kvPool _ = riakPool
_prCache = unsafePerformIO $ newCache 600 600 (200*1024*1024)
{-# NOINLINE _prCache #-}
readPostsRead :: (T.Text, TURL) -> IO (Maybe PostsRead)
readPostsRead = readKV

cachedReadPostsRead :: (T.Text, TURL) -> IO (Maybe PostsRead)
cachedReadPostsRead = cachedReadKV

cachedNothingReadPostsRead :: (T.Text, TURL) -> IO (Maybe PostsRead)
cachedNothingReadPostsRead = cachedNothingReadKV

mergeWritePostsRead :: PostsRead -> IO (())
mergeWritePostsRead = mergeWriteKV

deletePostsRead :: PostsRead -> IO (())
deletePostsRead = deleteKV

readManyPostsReads :: [(T.Text, TURL)] -> IO ([Maybe PostsRead])
readManyPostsReads = readManyKVs

cachedReadManyPostsReads :: [(T.Text, TURL)] -> IO ([Maybe PostsRead])
cachedReadManyPostsReads = cachedReadManyKVs

cachedNothingReadManyPostsReads :: [(T.Text, TURL)] -> IO ([Maybe PostsRead])
cachedNothingReadManyPostsReads = cachedNothingReadManyKVs

writeManyPostsReads :: [PostsRead] -> IO (())
writeManyPostsReads = writeManyKVs

modifyPostsRead :: (T.Text, TURL) -> (Maybe PostsRead -> IO (PostsRead, b)) -> IO b
modifyPostsRead = modifyKV

modifyPostsRead_ :: (T.Text, TURL) -> (Maybe PostsRead -> IO PostsRead) -> IO ()
modifyPostsRead_ = modifyKV_

modifyPostsRead' :: (T.Text, TURL) -> (PostsRead -> IO (PostsRead, b)) -> IO b
modifyPostsRead' key f = modifyKV key (f . fromMaybe (defaultPostsRead key))

modifyPostsRead'_ :: (T.Text, TURL) -> (PostsRead -> IO PostsRead) -> IO ()
modifyPostsRead'_ key f = modifyKV_ key (f . fromMaybe (defaultPostsRead key))

readPostsRead' :: Key PostsRead -> IO PostsRead
readPostsRead' key = liftM (fromMaybe (defaultPostsRead key)) (readKV key)

cachedReadPostsRead' :: Key PostsRead -> IO PostsRead
cachedReadPostsRead' key = liftM (fromMaybe (defaultPostsRead key)) (cachedReadKV key)

cachedNothingReadPostsRead' :: Key PostsRead -> IO PostsRead
cachedNothingReadPostsRead' key = liftM (fromMaybe (defaultPostsRead key)) (cachedNothingReadKV key)

instance KV PostsTagged where
    type Key PostsTagged = TURL
    kvBucket _ = "PostsTagged"
    kvKey = ptBlogFeedUrl
    kvCache _ = _ptCache
    kvPool _ = riakPool
_ptCache = unsafePerformIO $ newCache 120 120 (200*1024*1024)
{-# NOINLINE _ptCache #-}
readPostsTagged :: TURL -> IO (Maybe PostsTagged)
readPostsTagged = readKV

cachedReadPostsTagged :: TURL -> IO (Maybe PostsTagged)
cachedReadPostsTagged = cachedReadKV

cachedNothingReadPostsTagged :: TURL -> IO (Maybe PostsTagged)
cachedNothingReadPostsTagged = cachedNothingReadKV

mergeWritePostsTagged :: PostsTagged -> IO (())
mergeWritePostsTagged = mergeWriteKV

deletePostsTagged :: PostsTagged -> IO (())
deletePostsTagged = deleteKV

readManyPostsTaggeds :: [TURL] -> IO ([Maybe PostsTagged])
readManyPostsTaggeds = readManyKVs

cachedReadManyPostsTaggeds :: [TURL] -> IO ([Maybe PostsTagged])
cachedReadManyPostsTaggeds = cachedReadManyKVs

cachedNothingReadManyPostsTaggeds :: [TURL] -> IO ([Maybe PostsTagged])
cachedNothingReadManyPostsTaggeds = cachedNothingReadManyKVs

writeManyPostsTaggeds :: [PostsTagged] -> IO (())
writeManyPostsTaggeds = writeManyKVs

modifyPostsTagged :: TURL -> (Maybe PostsTagged -> IO (PostsTagged, b)) -> IO b
modifyPostsTagged = modifyKV

modifyPostsTagged_ :: TURL -> (Maybe PostsTagged -> IO PostsTagged) -> IO ()
modifyPostsTagged_ = modifyKV_

modifyPostsTagged' :: TURL -> (PostsTagged -> IO (PostsTagged, b)) -> IO b
modifyPostsTagged' key f = modifyKV key (f . fromMaybe (defaultPostsTagged key))

modifyPostsTagged'_ :: TURL -> (PostsTagged -> IO PostsTagged) -> IO ()
modifyPostsTagged'_ key f = modifyKV_ key (f . fromMaybe (defaultPostsTagged key))

readPostsTagged' :: Key PostsTagged -> IO PostsTagged
readPostsTagged' key = liftM (fromMaybe (defaultPostsTagged key)) (readKV key)

cachedReadPostsTagged' :: Key PostsTagged -> IO PostsTagged
cachedReadPostsTagged' key = liftM (fromMaybe (defaultPostsTagged key)) (cachedReadKV key)

cachedNothingReadPostsTagged' :: Key PostsTagged -> IO PostsTagged
cachedNothingReadPostsTagged' key = liftM (fromMaybe (defaultPostsTagged key)) (cachedNothingReadKV key)

instance KV PostsTaggedGuids where
    type Key PostsTaggedGuids = TURL
    kvBucket _ = "PostsTaggedGuids"
    kvKey = ptgBlogFeedUrl
    kvCache _ = _ptgCache
    kvPool _ = riakPool
_ptgCache = unsafePerformIO $ newCache 120 120 (200*1024*1024)
{-# NOINLINE _ptgCache #-}
readPostsTaggedGuids :: TURL -> IO (Maybe PostsTaggedGuids)
readPostsTaggedGuids = readKV

cachedReadPostsTaggedGuids :: TURL -> IO (Maybe PostsTaggedGuids)
cachedReadPostsTaggedGuids = cachedReadKV

cachedNothingReadPostsTaggedGuids :: TURL -> IO (Maybe PostsTaggedGuids)
cachedNothingReadPostsTaggedGuids = cachedNothingReadKV

mergeWritePostsTaggedGuids :: PostsTaggedGuids -> IO (())
mergeWritePostsTaggedGuids = mergeWriteKV

deletePostsTaggedGuids :: PostsTaggedGuids -> IO (())
deletePostsTaggedGuids = deleteKV

readManyPostsTaggedGuidss :: [TURL] -> IO ([Maybe PostsTaggedGuids])
readManyPostsTaggedGuidss = readManyKVs

cachedReadManyPostsTaggedGuidss :: [TURL] -> IO ([Maybe PostsTaggedGuids])
cachedReadManyPostsTaggedGuidss = cachedReadManyKVs

cachedNothingReadManyPostsTaggedGuidss :: [TURL] -> IO ([Maybe PostsTaggedGuids])
cachedNothingReadManyPostsTaggedGuidss = cachedNothingReadManyKVs

writeManyPostsTaggedGuidss :: [PostsTaggedGuids] -> IO (())
writeManyPostsTaggedGuidss = writeManyKVs

modifyPostsTaggedGuids :: TURL -> (Maybe PostsTaggedGuids -> IO (PostsTaggedGuids, b)) -> IO b
modifyPostsTaggedGuids = modifyKV

modifyPostsTaggedGuids_ :: TURL -> (Maybe PostsTaggedGuids -> IO PostsTaggedGuids) -> IO ()
modifyPostsTaggedGuids_ = modifyKV_

modifyPostsTaggedGuids' :: TURL -> (PostsTaggedGuids -> IO (PostsTaggedGuids, b)) -> IO b
modifyPostsTaggedGuids' key f = modifyKV key (f . fromMaybe (defaultPostsTaggedGuids key))

modifyPostsTaggedGuids'_ :: TURL -> (PostsTaggedGuids -> IO PostsTaggedGuids) -> IO ()
modifyPostsTaggedGuids'_ key f = modifyKV_ key (f . fromMaybe (defaultPostsTaggedGuids key))

readPostsTaggedGuids' :: Key PostsTaggedGuids -> IO PostsTaggedGuids
readPostsTaggedGuids' key = liftM (fromMaybe (defaultPostsTaggedGuids key)) (readKV key)

cachedReadPostsTaggedGuids' :: Key PostsTaggedGuids -> IO PostsTaggedGuids
cachedReadPostsTaggedGuids' key = liftM (fromMaybe (defaultPostsTaggedGuids key)) (cachedReadKV key)

cachedNothingReadPostsTaggedGuids' :: Key PostsTaggedGuids -> IO PostsTaggedGuids
cachedNothingReadPostsTaggedGuids' key = liftM (fromMaybe (defaultPostsTaggedGuids key)) (cachedNothingReadKV key)

instance KV GRIds where
    type Key GRIds = T.Text
    kvBucket _ = "GRIds"
    kvKey = griUser
    kvCache _ = _griCache
    kvPool _ = riakPool
_griCache = unsafePerformIO $ newCache 200 200 (200*1024*1024)
{-# NOINLINE _griCache #-}
readGRIds :: T.Text -> IO (Maybe GRIds)
readGRIds = readKV

cachedReadGRIds :: T.Text -> IO (Maybe GRIds)
cachedReadGRIds = cachedReadKV

cachedNothingReadGRIds :: T.Text -> IO (Maybe GRIds)
cachedNothingReadGRIds = cachedNothingReadKV

mergeWriteGRIds :: GRIds -> IO (())
mergeWriteGRIds = mergeWriteKV

deleteGRIds :: GRIds -> IO (())
deleteGRIds = deleteKV

readManyGRIdss :: [T.Text] -> IO ([Maybe GRIds])
readManyGRIdss = readManyKVs

cachedReadManyGRIdss :: [T.Text] -> IO ([Maybe GRIds])
cachedReadManyGRIdss = cachedReadManyKVs

cachedNothingReadManyGRIdss :: [T.Text] -> IO ([Maybe GRIds])
cachedNothingReadManyGRIdss = cachedNothingReadManyKVs

writeManyGRIdss :: [GRIds] -> IO (())
writeManyGRIdss = writeManyKVs

modifyGRIds :: T.Text -> (Maybe GRIds -> IO (GRIds, b)) -> IO b
modifyGRIds = modifyKV

modifyGRIds_ :: T.Text -> (Maybe GRIds -> IO GRIds) -> IO ()
modifyGRIds_ = modifyKV_

modifyGRIds' :: T.Text -> (GRIds -> IO (GRIds, b)) -> IO b
modifyGRIds' key f = modifyKV key (f . fromMaybe (defaultGRIds key))

modifyGRIds'_ :: T.Text -> (GRIds -> IO GRIds) -> IO ()
modifyGRIds'_ key f = modifyKV_ key (f . fromMaybe (defaultGRIds key))

readGRIds' :: Key GRIds -> IO GRIds
readGRIds' key = liftM (fromMaybe (defaultGRIds key)) (readKV key)

cachedReadGRIds' :: Key GRIds -> IO GRIds
cachedReadGRIds' key = liftM (fromMaybe (defaultGRIds key)) (cachedReadKV key)

cachedNothingReadGRIds' :: Key GRIds -> IO GRIds
cachedNothingReadGRIds' key = liftM (fromMaybe (defaultGRIds key)) (cachedNothingReadKV key)

instance KV UserBackup where
    type Key UserBackup = (T.Text, UrTime)
    kvBucket _ = "UserBackup"
    kvKey = ubKey
    kvCache _ = _ubCache
    kvPool _ = riakPool
_ubCache = unsafePerformIO $ newCache 200 200 (200*1024*1024)
{-# NOINLINE _ubCache #-}
readUserBackup :: (T.Text, UrTime) -> IO (Maybe UserBackup)
readUserBackup = readKV

cachedReadUserBackup :: (T.Text, UrTime) -> IO (Maybe UserBackup)
cachedReadUserBackup = cachedReadKV

cachedNothingReadUserBackup :: (T.Text, UrTime) -> IO (Maybe UserBackup)
cachedNothingReadUserBackup = cachedNothingReadKV

mergeWriteUserBackup :: UserBackup -> IO (())
mergeWriteUserBackup = mergeWriteKV

deleteUserBackup :: UserBackup -> IO (())
deleteUserBackup = deleteKV

readManyUserBackups :: [(T.Text, UrTime)] -> IO ([Maybe UserBackup])
readManyUserBackups = readManyKVs

cachedReadManyUserBackups :: [(T.Text, UrTime)] -> IO ([Maybe UserBackup])
cachedReadManyUserBackups = cachedReadManyKVs

cachedNothingReadManyUserBackups :: [(T.Text, UrTime)] -> IO ([Maybe UserBackup])
cachedNothingReadManyUserBackups = cachedNothingReadManyKVs

writeManyUserBackups :: [UserBackup] -> IO (())
writeManyUserBackups = writeManyKVs

modifyUserBackup :: (T.Text, UrTime) -> (Maybe UserBackup -> IO (UserBackup, b)) -> IO b
modifyUserBackup = modifyKV

modifyUserBackup_ :: (T.Text, UrTime) -> (Maybe UserBackup -> IO UserBackup) -> IO ()
modifyUserBackup_ = modifyKV_

modifyUserBackup' :: (T.Text, UrTime) -> (UserBackup -> IO (UserBackup, b)) -> IO b
modifyUserBackup' key f = modifyKV key (f . fromMaybe (defaultUserBackup key))

modifyUserBackup'_ :: (T.Text, UrTime) -> (UserBackup -> IO UserBackup) -> IO ()
modifyUserBackup'_ key f = modifyKV_ key (f . fromMaybe (defaultUserBackup key))

readUserBackup' :: Key UserBackup -> IO UserBackup
readUserBackup' key = liftM (fromMaybe (defaultUserBackup key)) (readKV key)

cachedReadUserBackup' :: Key UserBackup -> IO UserBackup
cachedReadUserBackup' key = liftM (fromMaybe (defaultUserBackup key)) (cachedReadKV key)

cachedNothingReadUserBackup' :: Key UserBackup -> IO UserBackup
cachedNothingReadUserBackup' key = liftM (fromMaybe (defaultUserBackup key)) (cachedNothingReadKV key)

instance KV DeletedUser where
    type Key DeletedUser = T.Text
    kvBucket _ = "DeletedUser"
    kvKey = duUser
    kvCache _ = _duCache
    kvPool _ = riakPool
_duCache = unsafePerformIO $ newCache 0 0 (0*1024*1024)
{-# NOINLINE _duCache #-}
readDeletedUser :: T.Text -> IO (Maybe DeletedUser)
readDeletedUser = readKV

cachedReadDeletedUser :: T.Text -> IO (Maybe DeletedUser)
cachedReadDeletedUser = cachedReadKV

cachedNothingReadDeletedUser :: T.Text -> IO (Maybe DeletedUser)
cachedNothingReadDeletedUser = cachedNothingReadKV

mergeWriteDeletedUser :: DeletedUser -> IO (())
mergeWriteDeletedUser = mergeWriteKV

deleteDeletedUser :: DeletedUser -> IO (())
deleteDeletedUser = deleteKV

readManyDeletedUsers :: [T.Text] -> IO ([Maybe DeletedUser])
readManyDeletedUsers = readManyKVs

cachedReadManyDeletedUsers :: [T.Text] -> IO ([Maybe DeletedUser])
cachedReadManyDeletedUsers = cachedReadManyKVs

cachedNothingReadManyDeletedUsers :: [T.Text] -> IO ([Maybe DeletedUser])
cachedNothingReadManyDeletedUsers = cachedNothingReadManyKVs

writeManyDeletedUsers :: [DeletedUser] -> IO (())
writeManyDeletedUsers = writeManyKVs

modifyDeletedUser :: T.Text -> (Maybe DeletedUser -> IO (DeletedUser, b)) -> IO b
modifyDeletedUser = modifyKV

modifyDeletedUser_ :: T.Text -> (Maybe DeletedUser -> IO DeletedUser) -> IO ()
modifyDeletedUser_ = modifyKV_

modifyDeletedUser' :: T.Text -> (DeletedUser -> IO (DeletedUser, b)) -> IO b
modifyDeletedUser' key f = modifyKV key (f . fromMaybe (defaultDeletedUser key))

modifyDeletedUser'_ :: T.Text -> (DeletedUser -> IO DeletedUser) -> IO ()
modifyDeletedUser'_ key f = modifyKV_ key (f . fromMaybe (defaultDeletedUser key))

readDeletedUser' :: Key DeletedUser -> IO DeletedUser
readDeletedUser' key = liftM (fromMaybe (defaultDeletedUser key)) (readKV key)

cachedReadDeletedUser' :: Key DeletedUser -> IO DeletedUser
cachedReadDeletedUser' key = liftM (fromMaybe (defaultDeletedUser key)) (cachedReadKV key)

cachedNothingReadDeletedUser' :: Key DeletedUser -> IO DeletedUser
cachedNothingReadDeletedUser' key = liftM (fromMaybe (defaultDeletedUser key)) (cachedNothingReadKV key)

instance KV UsageFlags where
    type Key UsageFlags = UrTime
    kvBucket _ = "UsageFlags"
    kvKey = uflTime
    kvCache _ = _uflCache
    kvPool _ = riakPool
_uflCache = unsafePerformIO $ newCache 200 200 (200*1024*1024)
{-# NOINLINE _uflCache #-}
readUsageFlags :: UrTime -> IO (Maybe UsageFlags)
readUsageFlags = readKV

cachedReadUsageFlags :: UrTime -> IO (Maybe UsageFlags)
cachedReadUsageFlags = cachedReadKV

cachedNothingReadUsageFlags :: UrTime -> IO (Maybe UsageFlags)
cachedNothingReadUsageFlags = cachedNothingReadKV

mergeWriteUsageFlags :: UsageFlags -> IO (())
mergeWriteUsageFlags = mergeWriteKV

deleteUsageFlags :: UsageFlags -> IO (())
deleteUsageFlags = deleteKV

readManyUsageFlagss :: [UrTime] -> IO ([Maybe UsageFlags])
readManyUsageFlagss = readManyKVs

cachedReadManyUsageFlagss :: [UrTime] -> IO ([Maybe UsageFlags])
cachedReadManyUsageFlagss = cachedReadManyKVs

cachedNothingReadManyUsageFlagss :: [UrTime] -> IO ([Maybe UsageFlags])
cachedNothingReadManyUsageFlagss = cachedNothingReadManyKVs

writeManyUsageFlagss :: [UsageFlags] -> IO (())
writeManyUsageFlagss = writeManyKVs

modifyUsageFlags :: UrTime -> (Maybe UsageFlags -> IO (UsageFlags, b)) -> IO b
modifyUsageFlags = modifyKV

modifyUsageFlags_ :: UrTime -> (Maybe UsageFlags -> IO UsageFlags) -> IO ()
modifyUsageFlags_ = modifyKV_

modifyUsageFlags' :: UrTime -> (UsageFlags -> IO (UsageFlags, b)) -> IO b
modifyUsageFlags' key f = modifyKV key (f . fromMaybe (defaultUsageFlags key))

modifyUsageFlags'_ :: UrTime -> (UsageFlags -> IO UsageFlags) -> IO ()
modifyUsageFlags'_ key f = modifyKV_ key (f . fromMaybe (defaultUsageFlags key))

readUsageFlags' :: Key UsageFlags -> IO UsageFlags
readUsageFlags' key = liftM (fromMaybe (defaultUsageFlags key)) (readKV key)

cachedReadUsageFlags' :: Key UsageFlags -> IO UsageFlags
cachedReadUsageFlags' key = liftM (fromMaybe (defaultUsageFlags key)) (cachedReadKV key)

cachedNothingReadUsageFlags' :: Key UsageFlags -> IO UsageFlags
cachedNothingReadUsageFlags' key = liftM (fromMaybe (defaultUsageFlags key)) (cachedNothingReadKV key)

instance KV FullTextCache where
    type Key FullTextCache = TURL
    kvBucket _ = "FullTextCache"
    kvKey = ftcUrl
    kvCache _ = _ftcCache
    kvPool _ = riakPool
_ftcCache = unsafePerformIO $ newCache 60 60 (200*1024*1024)
{-# NOINLINE _ftcCache #-}
readFullTextCache :: TURL -> IO (Maybe FullTextCache)
readFullTextCache = readKV

cachedReadFullTextCache :: TURL -> IO (Maybe FullTextCache)
cachedReadFullTextCache = cachedReadKV

cachedNothingReadFullTextCache :: TURL -> IO (Maybe FullTextCache)
cachedNothingReadFullTextCache = cachedNothingReadKV

mergeWriteFullTextCache :: FullTextCache -> IO (())
mergeWriteFullTextCache = mergeWriteKV

deleteFullTextCache :: FullTextCache -> IO (())
deleteFullTextCache = deleteKV

readManyFullTextCaches :: [TURL] -> IO ([Maybe FullTextCache])
readManyFullTextCaches = readManyKVs

cachedReadManyFullTextCaches :: [TURL] -> IO ([Maybe FullTextCache])
cachedReadManyFullTextCaches = cachedReadManyKVs

cachedNothingReadManyFullTextCaches :: [TURL] -> IO ([Maybe FullTextCache])
cachedNothingReadManyFullTextCaches = cachedNothingReadManyKVs

writeManyFullTextCaches :: [FullTextCache] -> IO (())
writeManyFullTextCaches = writeManyKVs

modifyFullTextCache :: TURL -> (Maybe FullTextCache -> IO (FullTextCache, b)) -> IO b
modifyFullTextCache = modifyKV

modifyFullTextCache_ :: TURL -> (Maybe FullTextCache -> IO FullTextCache) -> IO ()
modifyFullTextCache_ = modifyKV_

modifyFullTextCache' :: TURL -> (FullTextCache -> IO (FullTextCache, b)) -> IO b
modifyFullTextCache' key f = modifyKV key (f . fromMaybe (defaultFullTextCache key))

modifyFullTextCache'_ :: TURL -> (FullTextCache -> IO FullTextCache) -> IO ()
modifyFullTextCache'_ key f = modifyKV_ key (f . fromMaybe (defaultFullTextCache key))

readFullTextCache' :: Key FullTextCache -> IO FullTextCache
readFullTextCache' key = liftM (fromMaybe (defaultFullTextCache key)) (readKV key)

cachedReadFullTextCache' :: Key FullTextCache -> IO FullTextCache
cachedReadFullTextCache' key = liftM (fromMaybe (defaultFullTextCache key)) (cachedReadKV key)

cachedNothingReadFullTextCache' :: Key FullTextCache -> IO FullTextCache
cachedNothingReadFullTextCache' key = liftM (fromMaybe (defaultFullTextCache key)) (cachedNothingReadKV key)

