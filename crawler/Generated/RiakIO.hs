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

alterStats :: T.Text -> (Maybe Stats -> IO (Maybe Stats, b)) -> IO b
alterStats = alterKV

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

alterUser :: T.Text -> (Maybe User -> IO (Maybe User, b)) -> IO b
alterUser = alterKV

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

alterUserFilters :: T.Text -> (Maybe UserFilters -> IO (Maybe UserFilters, b)) -> IO b
alterUserFilters = alterKV

readUserFilters' :: Key UserFilters -> IO UserFilters
readUserFilters' key = liftM (fromMaybe (defaultUserFilters key)) (readKV key)

cachedReadUserFilters' :: Key UserFilters -> IO UserFilters
cachedReadUserFilters' key = liftM (fromMaybe (defaultUserFilters key)) (cachedReadKV key)

cachedNothingReadUserFilters' :: Key UserFilters -> IO UserFilters
cachedNothingReadUserFilters' key = liftM (fromMaybe (defaultUserFilters key)) (cachedNothingReadKV key)

instance KV Login where
    type Key Login = LoginType
    kvBucket _ = "Login"
    kvKey = lLoginType
    kvCache _ = _lCache
_lCache = unsafePerformIO $ newCache 300 300 (10*1024*1024)
{-# NOINLINE _lCache #-}
readLogin :: LoginType -> IO (Maybe Login)
readLogin = readKV

cachedReadLogin :: LoginType -> IO (Maybe Login)
cachedReadLogin = cachedReadKV

cachedNothingReadLogin :: LoginType -> IO (Maybe Login)
cachedNothingReadLogin = cachedNothingReadKV

mergeWriteLogin :: Login -> IO (())
mergeWriteLogin = mergeWriteKV

deleteLogin :: Login -> IO (())
deleteLogin = deleteKV

readManyLogins :: [LoginType] -> IO ([Maybe Login])
readManyLogins = readManyKVs

cachedReadManyLogins :: [LoginType] -> IO ([Maybe Login])
cachedReadManyLogins = cachedReadManyKVs

cachedNothingReadManyLogins :: [LoginType] -> IO ([Maybe Login])
cachedNothingReadManyLogins = cachedNothingReadManyKVs

writeManyLogins :: [Login] -> IO (())
writeManyLogins = writeManyKVs

modifyLogin :: LoginType -> (Maybe Login -> IO (Login, b)) -> IO b
modifyLogin = modifyKV

modifyLogin_ :: LoginType -> (Maybe Login -> IO Login) -> IO ()
modifyLogin_ = modifyKV_

modifyLogin' :: LoginType -> (Login -> IO (Login, b)) -> IO b
modifyLogin' key f = modifyKV key (f . fromMaybe (defaultLogin key))

modifyLogin'_ :: LoginType -> (Login -> IO Login) -> IO ()
modifyLogin'_ key f = modifyKV_ key (f . fromMaybe (defaultLogin key))

alterLogin :: LoginType -> (Maybe Login -> IO (Maybe Login, b)) -> IO b
alterLogin = alterKV

readLogin' :: Key Login -> IO Login
readLogin' key = liftM (fromMaybe (defaultLogin key)) (readKV key)

cachedReadLogin' :: Key Login -> IO Login
cachedReadLogin' key = liftM (fromMaybe (defaultLogin key)) (cachedReadKV key)

cachedNothingReadLogin' :: Key Login -> IO Login
cachedNothingReadLogin' key = liftM (fromMaybe (defaultLogin key)) (cachedNothingReadKV key)

instance KV UserSettings where
    type Key UserSettings = T.Text
    kvBucket _ = "UserSettings"
    kvKey = ustUser
    kvCache _ = _ustCache
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

alterUserSettings :: T.Text -> (Maybe UserSettings -> IO (Maybe UserSettings, b)) -> IO b
alterUserSettings = alterKV

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

alterPublicFeed :: T.Text -> (Maybe PublicFeed -> IO (Maybe PublicFeed, b)) -> IO b
alterPublicFeed = alterKV

readPublicFeed' :: Key PublicFeed -> IO PublicFeed
readPublicFeed' key = liftM (fromMaybe (defaultPublicFeed key)) (readKV key)

cachedReadPublicFeed' :: Key PublicFeed -> IO PublicFeed
cachedReadPublicFeed' key = liftM (fromMaybe (defaultPublicFeed key)) (cachedReadKV key)

cachedNothingReadPublicFeed' :: Key PublicFeed -> IO PublicFeed
cachedNothingReadPublicFeed' key = liftM (fromMaybe (defaultPublicFeed key)) (cachedNothingReadKV key)

instance KV FeverIds where
    type Key FeverIds = T.Text
    kvBucket _ = "FeverIds"
    kvKey = fiUser
    kvCache _ = _fiCache
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

alterFeverIds :: T.Text -> (Maybe FeverIds -> IO (Maybe FeverIds, b)) -> IO b
alterFeverIds = alterKV

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

alterUserStats :: T.Text -> (Maybe UserStats -> IO (Maybe UserStats, b)) -> IO b
alterUserStats = alterKV

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

alterMailQueue :: T.Text -> (Maybe MailQueue -> IO (Maybe MailQueue, b)) -> IO b
alterMailQueue = alterKV

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

alterSession :: T.Text -> (Maybe Session -> IO (Maybe Session, b)) -> IO b
alterSession = alterKV

readSession' :: Key Session -> IO Session
readSession' key = liftM (fromMaybe (defaultSession key)) (readKV key)

cachedReadSession' :: Key Session -> IO Session
cachedReadSession' key = liftM (fromMaybe (defaultSession key)) (cachedReadKV key)

cachedNothingReadSession' :: Key Session -> IO Session
cachedNothingReadSession' key = liftM (fromMaybe (defaultSession key)) (cachedNothingReadKV key)

instance KV EmailVerificationToken where
    type Key EmailVerificationToken = T.Text
    kvBucket _ = "EmailVerificationToken"
    kvKey = evtkToken
    kvCache _ = _evtkCache
_evtkCache = unsafePerformIO $ newCache 60 60 (10*1024*1024)
{-# NOINLINE _evtkCache #-}
readEmailVerificationToken :: T.Text -> IO (Maybe EmailVerificationToken)
readEmailVerificationToken = readKV

cachedReadEmailVerificationToken :: T.Text -> IO (Maybe EmailVerificationToken)
cachedReadEmailVerificationToken = cachedReadKV

cachedNothingReadEmailVerificationToken :: T.Text -> IO (Maybe EmailVerificationToken)
cachedNothingReadEmailVerificationToken = cachedNothingReadKV

mergeWriteEmailVerificationToken :: EmailVerificationToken -> IO (())
mergeWriteEmailVerificationToken = mergeWriteKV

deleteEmailVerificationToken :: EmailVerificationToken -> IO (())
deleteEmailVerificationToken = deleteKV

readManyEmailVerificationTokens :: [T.Text] -> IO ([Maybe EmailVerificationToken])
readManyEmailVerificationTokens = readManyKVs

cachedReadManyEmailVerificationTokens :: [T.Text] -> IO ([Maybe EmailVerificationToken])
cachedReadManyEmailVerificationTokens = cachedReadManyKVs

cachedNothingReadManyEmailVerificationTokens :: [T.Text] -> IO ([Maybe EmailVerificationToken])
cachedNothingReadManyEmailVerificationTokens = cachedNothingReadManyKVs

writeManyEmailVerificationTokens :: [EmailVerificationToken] -> IO (())
writeManyEmailVerificationTokens = writeManyKVs

modifyEmailVerificationToken :: T.Text -> (Maybe EmailVerificationToken -> IO (EmailVerificationToken, b)) -> IO b
modifyEmailVerificationToken = modifyKV

modifyEmailVerificationToken_ :: T.Text -> (Maybe EmailVerificationToken -> IO EmailVerificationToken) -> IO ()
modifyEmailVerificationToken_ = modifyKV_

modifyEmailVerificationToken' :: T.Text -> (EmailVerificationToken -> IO (EmailVerificationToken, b)) -> IO b
modifyEmailVerificationToken' key f = modifyKV key (f . fromMaybe (defaultEmailVerificationToken key))

modifyEmailVerificationToken'_ :: T.Text -> (EmailVerificationToken -> IO EmailVerificationToken) -> IO ()
modifyEmailVerificationToken'_ key f = modifyKV_ key (f . fromMaybe (defaultEmailVerificationToken key))

alterEmailVerificationToken :: T.Text -> (Maybe EmailVerificationToken -> IO (Maybe EmailVerificationToken, b)) -> IO b
alterEmailVerificationToken = alterKV

readEmailVerificationToken' :: Key EmailVerificationToken -> IO EmailVerificationToken
readEmailVerificationToken' key = liftM (fromMaybe (defaultEmailVerificationToken key)) (readKV key)

cachedReadEmailVerificationToken' :: Key EmailVerificationToken -> IO EmailVerificationToken
cachedReadEmailVerificationToken' key = liftM (fromMaybe (defaultEmailVerificationToken key)) (cachedReadKV key)

cachedNothingReadEmailVerificationToken' :: Key EmailVerificationToken -> IO EmailVerificationToken
cachedNothingReadEmailVerificationToken' key = liftM (fromMaybe (defaultEmailVerificationToken key)) (cachedNothingReadKV key)

instance KV EmailVerification where
    type Key EmailVerification = T.Text
    kvBucket _ = "EmailVerification"
    kvKey = evEmail
    kvCache _ = _evCache
_evCache = unsafePerformIO $ newCache 60 60 (10*1024*1024)
{-# NOINLINE _evCache #-}
readEmailVerification :: T.Text -> IO (Maybe EmailVerification)
readEmailVerification = readKV

cachedReadEmailVerification :: T.Text -> IO (Maybe EmailVerification)
cachedReadEmailVerification = cachedReadKV

cachedNothingReadEmailVerification :: T.Text -> IO (Maybe EmailVerification)
cachedNothingReadEmailVerification = cachedNothingReadKV

mergeWriteEmailVerification :: EmailVerification -> IO (())
mergeWriteEmailVerification = mergeWriteKV

deleteEmailVerification :: EmailVerification -> IO (())
deleteEmailVerification = deleteKV

readManyEmailVerifications :: [T.Text] -> IO ([Maybe EmailVerification])
readManyEmailVerifications = readManyKVs

cachedReadManyEmailVerifications :: [T.Text] -> IO ([Maybe EmailVerification])
cachedReadManyEmailVerifications = cachedReadManyKVs

cachedNothingReadManyEmailVerifications :: [T.Text] -> IO ([Maybe EmailVerification])
cachedNothingReadManyEmailVerifications = cachedNothingReadManyKVs

writeManyEmailVerifications :: [EmailVerification] -> IO (())
writeManyEmailVerifications = writeManyKVs

modifyEmailVerification :: T.Text -> (Maybe EmailVerification -> IO (EmailVerification, b)) -> IO b
modifyEmailVerification = modifyKV

modifyEmailVerification_ :: T.Text -> (Maybe EmailVerification -> IO EmailVerification) -> IO ()
modifyEmailVerification_ = modifyKV_

modifyEmailVerification' :: T.Text -> (EmailVerification -> IO (EmailVerification, b)) -> IO b
modifyEmailVerification' key f = modifyKV key (f . fromMaybe (defaultEmailVerification key))

modifyEmailVerification'_ :: T.Text -> (EmailVerification -> IO EmailVerification) -> IO ()
modifyEmailVerification'_ key f = modifyKV_ key (f . fromMaybe (defaultEmailVerification key))

alterEmailVerification :: T.Text -> (Maybe EmailVerification -> IO (Maybe EmailVerification, b)) -> IO b
alterEmailVerification = alterKV

readEmailVerification' :: Key EmailVerification -> IO EmailVerification
readEmailVerification' key = liftM (fromMaybe (defaultEmailVerification key)) (readKV key)

cachedReadEmailVerification' :: Key EmailVerification -> IO EmailVerification
cachedReadEmailVerification' key = liftM (fromMaybe (defaultEmailVerification key)) (cachedReadKV key)

cachedNothingReadEmailVerification' :: Key EmailVerification -> IO EmailVerification
cachedNothingReadEmailVerification' key = liftM (fromMaybe (defaultEmailVerification key)) (cachedNothingReadKV key)

instance KV UserEmailVerificationTokens where
    type Key UserEmailVerificationTokens = T.Text
    kvBucket _ = "UserEmailVerificationTokens"
    kvKey = uevtUser
    kvCache _ = _uevtCache
_uevtCache = unsafePerformIO $ newCache 60 60 (10*1024*1024)
{-# NOINLINE _uevtCache #-}
readUserEmailVerificationTokens :: T.Text -> IO (Maybe UserEmailVerificationTokens)
readUserEmailVerificationTokens = readKV

cachedReadUserEmailVerificationTokens :: T.Text -> IO (Maybe UserEmailVerificationTokens)
cachedReadUserEmailVerificationTokens = cachedReadKV

cachedNothingReadUserEmailVerificationTokens :: T.Text -> IO (Maybe UserEmailVerificationTokens)
cachedNothingReadUserEmailVerificationTokens = cachedNothingReadKV

mergeWriteUserEmailVerificationTokens :: UserEmailVerificationTokens -> IO (())
mergeWriteUserEmailVerificationTokens = mergeWriteKV

deleteUserEmailVerificationTokens :: UserEmailVerificationTokens -> IO (())
deleteUserEmailVerificationTokens = deleteKV

readManyUserEmailVerificationTokenss :: [T.Text] -> IO ([Maybe UserEmailVerificationTokens])
readManyUserEmailVerificationTokenss = readManyKVs

cachedReadManyUserEmailVerificationTokenss :: [T.Text] -> IO ([Maybe UserEmailVerificationTokens])
cachedReadManyUserEmailVerificationTokenss = cachedReadManyKVs

cachedNothingReadManyUserEmailVerificationTokenss :: [T.Text] -> IO ([Maybe UserEmailVerificationTokens])
cachedNothingReadManyUserEmailVerificationTokenss = cachedNothingReadManyKVs

writeManyUserEmailVerificationTokenss :: [UserEmailVerificationTokens] -> IO (())
writeManyUserEmailVerificationTokenss = writeManyKVs

modifyUserEmailVerificationTokens :: T.Text -> (Maybe UserEmailVerificationTokens -> IO (UserEmailVerificationTokens, b)) -> IO b
modifyUserEmailVerificationTokens = modifyKV

modifyUserEmailVerificationTokens_ :: T.Text -> (Maybe UserEmailVerificationTokens -> IO UserEmailVerificationTokens) -> IO ()
modifyUserEmailVerificationTokens_ = modifyKV_

modifyUserEmailVerificationTokens' :: T.Text -> (UserEmailVerificationTokens -> IO (UserEmailVerificationTokens, b)) -> IO b
modifyUserEmailVerificationTokens' key f = modifyKV key (f . fromMaybe (defaultUserEmailVerificationTokens key))

modifyUserEmailVerificationTokens'_ :: T.Text -> (UserEmailVerificationTokens -> IO UserEmailVerificationTokens) -> IO ()
modifyUserEmailVerificationTokens'_ key f = modifyKV_ key (f . fromMaybe (defaultUserEmailVerificationTokens key))

alterUserEmailVerificationTokens :: T.Text -> (Maybe UserEmailVerificationTokens -> IO (Maybe UserEmailVerificationTokens, b)) -> IO b
alterUserEmailVerificationTokens = alterKV

readUserEmailVerificationTokens' :: Key UserEmailVerificationTokens -> IO UserEmailVerificationTokens
readUserEmailVerificationTokens' key = liftM (fromMaybe (defaultUserEmailVerificationTokens key)) (readKV key)

cachedReadUserEmailVerificationTokens' :: Key UserEmailVerificationTokens -> IO UserEmailVerificationTokens
cachedReadUserEmailVerificationTokens' key = liftM (fromMaybe (defaultUserEmailVerificationTokens key)) (cachedReadKV key)

cachedNothingReadUserEmailVerificationTokens' :: Key UserEmailVerificationTokens -> IO UserEmailVerificationTokens
cachedNothingReadUserEmailVerificationTokens' key = liftM (fromMaybe (defaultUserEmailVerificationTokens key)) (cachedNothingReadKV key)

instance KV SubscriptionUrlInfo where
    type Key SubscriptionUrlInfo = TURL
    kvBucket _ = "SubscriptionUrlInfo"
    kvKey = suiUrl
    kvCache _ = _suiCache
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

alterSubscriptionUrlInfo :: TURL -> (Maybe SubscriptionUrlInfo -> IO (Maybe SubscriptionUrlInfo, b)) -> IO b
alterSubscriptionUrlInfo = alterKV

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

alterMsg :: MsgKey -> (Maybe Msg -> IO (Maybe Msg, b)) -> IO b
alterMsg = alterKV

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

alterBlogPostsScanned :: TURL -> (Maybe BlogPostsScanned -> IO (Maybe BlogPostsScanned, b)) -> IO b
alterBlogPostsScanned = alterKV

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
_pCache = unsafePerformIO $ newCache 3 30 (1000*1024*1024)
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

alterPosts :: TURL -> (Maybe Posts -> IO (Maybe Posts, b)) -> IO b
alterPosts = alterKV

readPosts' :: Key Posts -> IO Posts
readPosts' key = liftM (fromMaybe (defaultPosts key)) (readKV key)

cachedReadPosts' :: Key Posts -> IO Posts
cachedReadPosts' key = liftM (fromMaybe (defaultPosts key)) (cachedReadKV key)

cachedNothingReadPosts' :: Key Posts -> IO Posts
cachedNothingReadPosts' key = liftM (fromMaybe (defaultPosts key)) (cachedNothingReadKV key)

instance KV PostsClearTime where
    type Key PostsClearTime = TURL
    kvBucket _ = "PostsClearTime"
    kvKey = pctBlogFeedUrl
    kvCache _ = _pctCache
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

alterPostsClearTime :: TURL -> (Maybe PostsClearTime -> IO (Maybe PostsClearTime, b)) -> IO b
alterPostsClearTime = alterKV

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

alterPostsSubscribers :: TURL -> (Maybe PostsSubscribers -> IO (Maybe PostsSubscribers, b)) -> IO b
alterPostsSubscribers = alterKV

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

alterActiveCheckSubscriptions :: () -> (Maybe ActiveCheckSubscriptions -> IO (Maybe ActiveCheckSubscriptions, b)) -> IO b
alterActiveCheckSubscriptions = alterKV

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

alterComments :: CommentsKey -> (Maybe Comments -> IO (Maybe Comments, b)) -> IO b
alterComments = alterKV

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

alterUrlToScan :: TURL -> (Maybe UrlToScan -> IO (Maybe UrlToScan, b)) -> IO b
alterUrlToScan = alterKV

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

alterScanList :: UrTime -> (Maybe ScanList -> IO (Maybe ScanList, b)) -> IO b
alterScanList = alterKV

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
_prCache = unsafePerformIO $ newCache 600 600 (400*1024*1024)
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

alterPostsRead :: (T.Text, TURL) -> (Maybe PostsRead -> IO (Maybe PostsRead, b)) -> IO b
alterPostsRead = alterKV

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

alterPostsTagged :: TURL -> (Maybe PostsTagged -> IO (Maybe PostsTagged, b)) -> IO b
alterPostsTagged = alterKV

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

alterPostsTaggedGuids :: TURL -> (Maybe PostsTaggedGuids -> IO (Maybe PostsTaggedGuids, b)) -> IO b
alterPostsTaggedGuids = alterKV

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
_griCache = unsafePerformIO $ newCache 200 200 (400*1024*1024)
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

alterGRIds :: T.Text -> (Maybe GRIds -> IO (Maybe GRIds, b)) -> IO b
alterGRIds = alterKV

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

alterUserBackup :: (T.Text, UrTime) -> (Maybe UserBackup -> IO (Maybe UserBackup, b)) -> IO b
alterUserBackup = alterKV

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
_duCache = unsafePerformIO $ newCache 200 200 (200*1024*1024)
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

alterDeletedUser :: T.Text -> (Maybe DeletedUser -> IO (Maybe DeletedUser, b)) -> IO b
alterDeletedUser = alterKV

readDeletedUser' :: Key DeletedUser -> IO DeletedUser
readDeletedUser' key = liftM (fromMaybe (defaultDeletedUser key)) (readKV key)

cachedReadDeletedUser' :: Key DeletedUser -> IO DeletedUser
cachedReadDeletedUser' key = liftM (fromMaybe (defaultDeletedUser key)) (cachedReadKV key)

cachedNothingReadDeletedUser' :: Key DeletedUser -> IO DeletedUser
cachedNothingReadDeletedUser' key = liftM (fromMaybe (defaultDeletedUser key)) (cachedNothingReadKV key)

instance KV MailsSent where
    type Key MailsSent = T.Text
    kvBucket _ = "MailsSent"
    kvKey = msUser
    kvCache _ = _msCache
_msCache = unsafePerformIO $ newCache 200 200 (200*1024*1024)
{-# NOINLINE _msCache #-}
readMailsSent :: T.Text -> IO (Maybe MailsSent)
readMailsSent = readKV

cachedReadMailsSent :: T.Text -> IO (Maybe MailsSent)
cachedReadMailsSent = cachedReadKV

cachedNothingReadMailsSent :: T.Text -> IO (Maybe MailsSent)
cachedNothingReadMailsSent = cachedNothingReadKV

mergeWriteMailsSent :: MailsSent -> IO (())
mergeWriteMailsSent = mergeWriteKV

deleteMailsSent :: MailsSent -> IO (())
deleteMailsSent = deleteKV

readManyMailsSents :: [T.Text] -> IO ([Maybe MailsSent])
readManyMailsSents = readManyKVs

cachedReadManyMailsSents :: [T.Text] -> IO ([Maybe MailsSent])
cachedReadManyMailsSents = cachedReadManyKVs

cachedNothingReadManyMailsSents :: [T.Text] -> IO ([Maybe MailsSent])
cachedNothingReadManyMailsSents = cachedNothingReadManyKVs

writeManyMailsSents :: [MailsSent] -> IO (())
writeManyMailsSents = writeManyKVs

modifyMailsSent :: T.Text -> (Maybe MailsSent -> IO (MailsSent, b)) -> IO b
modifyMailsSent = modifyKV

modifyMailsSent_ :: T.Text -> (Maybe MailsSent -> IO MailsSent) -> IO ()
modifyMailsSent_ = modifyKV_

modifyMailsSent' :: T.Text -> (MailsSent -> IO (MailsSent, b)) -> IO b
modifyMailsSent' key f = modifyKV key (f . fromMaybe (defaultMailsSent key))

modifyMailsSent'_ :: T.Text -> (MailsSent -> IO MailsSent) -> IO ()
modifyMailsSent'_ key f = modifyKV_ key (f . fromMaybe (defaultMailsSent key))

alterMailsSent :: T.Text -> (Maybe MailsSent -> IO (Maybe MailsSent, b)) -> IO b
alterMailsSent = alterKV

readMailsSent' :: Key MailsSent -> IO MailsSent
readMailsSent' key = liftM (fromMaybe (defaultMailsSent key)) (readKV key)

cachedReadMailsSent' :: Key MailsSent -> IO MailsSent
cachedReadMailsSent' key = liftM (fromMaybe (defaultMailsSent key)) (cachedReadKV key)

cachedNothingReadMailsSent' :: Key MailsSent -> IO MailsSent
cachedNothingReadMailsSent' key = liftM (fromMaybe (defaultMailsSent key)) (cachedNothingReadKV key)

instance KV Filters where
    type Key Filters = T.Text
    kvBucket _ = "Filters"
    kvKey = fUser
    kvCache _ = _fCache
_fCache = unsafePerformIO $ newCache 600 600 (400*1024*1024)
{-# NOINLINE _fCache #-}
readFilters :: T.Text -> IO (Maybe Filters)
readFilters = readKV

cachedReadFilters :: T.Text -> IO (Maybe Filters)
cachedReadFilters = cachedReadKV

cachedNothingReadFilters :: T.Text -> IO (Maybe Filters)
cachedNothingReadFilters = cachedNothingReadKV

mergeWriteFilters :: Filters -> IO (())
mergeWriteFilters = mergeWriteKV

deleteFilters :: Filters -> IO (())
deleteFilters = deleteKV

readManyFilterss :: [T.Text] -> IO ([Maybe Filters])
readManyFilterss = readManyKVs

cachedReadManyFilterss :: [T.Text] -> IO ([Maybe Filters])
cachedReadManyFilterss = cachedReadManyKVs

cachedNothingReadManyFilterss :: [T.Text] -> IO ([Maybe Filters])
cachedNothingReadManyFilterss = cachedNothingReadManyKVs

writeManyFilterss :: [Filters] -> IO (())
writeManyFilterss = writeManyKVs

modifyFilters :: T.Text -> (Maybe Filters -> IO (Filters, b)) -> IO b
modifyFilters = modifyKV

modifyFilters_ :: T.Text -> (Maybe Filters -> IO Filters) -> IO ()
modifyFilters_ = modifyKV_

modifyFilters' :: T.Text -> (Filters -> IO (Filters, b)) -> IO b
modifyFilters' key f = modifyKV key (f . fromMaybe (defaultFilters key))

modifyFilters'_ :: T.Text -> (Filters -> IO Filters) -> IO ()
modifyFilters'_ key f = modifyKV_ key (f . fromMaybe (defaultFilters key))

alterFilters :: T.Text -> (Maybe Filters -> IO (Maybe Filters, b)) -> IO b
alterFilters = alterKV

readFilters' :: Key Filters -> IO Filters
readFilters' key = liftM (fromMaybe (defaultFilters key)) (readKV key)

cachedReadFilters' :: Key Filters -> IO Filters
cachedReadFilters' key = liftM (fromMaybe (defaultFilters key)) (cachedReadKV key)

cachedNothingReadFilters' :: Key Filters -> IO Filters
cachedNothingReadFilters' key = liftM (fromMaybe (defaultFilters key)) (cachedNothingReadKV key)

instance KV UsageFlags where
    type Key UsageFlags = UrTime
    kvBucket _ = "UsageFlags"
    kvKey = uflTime
    kvCache _ = _uflCache
_uflCache = unsafePerformIO $ newCache 3600 3600 (200*1024*1024)
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

alterUsageFlags :: UrTime -> (Maybe UsageFlags -> IO (Maybe UsageFlags, b)) -> IO b
alterUsageFlags = alterKV

readUsageFlags' :: Key UsageFlags -> IO UsageFlags
readUsageFlags' key = liftM (fromMaybe (defaultUsageFlags key)) (readKV key)

cachedReadUsageFlags' :: Key UsageFlags -> IO UsageFlags
cachedReadUsageFlags' key = liftM (fromMaybe (defaultUsageFlags key)) (cachedReadKV key)

cachedNothingReadUsageFlags' :: Key UsageFlags -> IO UsageFlags
cachedNothingReadUsageFlags' key = liftM (fromMaybe (defaultUsageFlags key)) (cachedNothingReadKV key)

instance KV UserSessions where
    type Key UserSessions = T.Text
    kvBucket _ = "UserSessions"
    kvKey = uSessionsUser
    kvCache _ = _uSessionsCache
_uSessionsCache = unsafePerformIO $ newCache 3600 3600 (100*1024*1024)
{-# NOINLINE _uSessionsCache #-}
readUserSessions :: T.Text -> IO (Maybe UserSessions)
readUserSessions = readKV

cachedReadUserSessions :: T.Text -> IO (Maybe UserSessions)
cachedReadUserSessions = cachedReadKV

cachedNothingReadUserSessions :: T.Text -> IO (Maybe UserSessions)
cachedNothingReadUserSessions = cachedNothingReadKV

mergeWriteUserSessions :: UserSessions -> IO (())
mergeWriteUserSessions = mergeWriteKV

deleteUserSessions :: UserSessions -> IO (())
deleteUserSessions = deleteKV

readManyUserSessionss :: [T.Text] -> IO ([Maybe UserSessions])
readManyUserSessionss = readManyKVs

cachedReadManyUserSessionss :: [T.Text] -> IO ([Maybe UserSessions])
cachedReadManyUserSessionss = cachedReadManyKVs

cachedNothingReadManyUserSessionss :: [T.Text] -> IO ([Maybe UserSessions])
cachedNothingReadManyUserSessionss = cachedNothingReadManyKVs

writeManyUserSessionss :: [UserSessions] -> IO (())
writeManyUserSessionss = writeManyKVs

modifyUserSessions :: T.Text -> (Maybe UserSessions -> IO (UserSessions, b)) -> IO b
modifyUserSessions = modifyKV

modifyUserSessions_ :: T.Text -> (Maybe UserSessions -> IO UserSessions) -> IO ()
modifyUserSessions_ = modifyKV_

modifyUserSessions' :: T.Text -> (UserSessions -> IO (UserSessions, b)) -> IO b
modifyUserSessions' key f = modifyKV key (f . fromMaybe (defaultUserSessions key))

modifyUserSessions'_ :: T.Text -> (UserSessions -> IO UserSessions) -> IO ()
modifyUserSessions'_ key f = modifyKV_ key (f . fromMaybe (defaultUserSessions key))

alterUserSessions :: T.Text -> (Maybe UserSessions -> IO (Maybe UserSessions, b)) -> IO b
alterUserSessions = alterKV

readUserSessions' :: Key UserSessions -> IO UserSessions
readUserSessions' key = liftM (fromMaybe (defaultUserSessions key)) (readKV key)

cachedReadUserSessions' :: Key UserSessions -> IO UserSessions
cachedReadUserSessions' key = liftM (fromMaybe (defaultUserSessions key)) (cachedReadKV key)

cachedNothingReadUserSessions' :: Key UserSessions -> IO UserSessions
cachedNothingReadUserSessions' key = liftM (fromMaybe (defaultUserSessions key)) (cachedNothingReadKV key)

instance KV FullTextCache where
    type Key FullTextCache = TURL
    kvBucket _ = "FullTextCache"
    kvKey = ftcUrl
    kvCache _ = _ftcCache
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

alterFullTextCache :: TURL -> (Maybe FullTextCache -> IO (Maybe FullTextCache, b)) -> IO b
alterFullTextCache = alterKV

readFullTextCache' :: Key FullTextCache -> IO FullTextCache
readFullTextCache' key = liftM (fromMaybe (defaultFullTextCache key)) (readKV key)

cachedReadFullTextCache' :: Key FullTextCache -> IO FullTextCache
cachedReadFullTextCache' key = liftM (fromMaybe (defaultFullTextCache key)) (cachedReadKV key)

cachedNothingReadFullTextCache' :: Key FullTextCache -> IO FullTextCache
cachedNothingReadFullTextCache' key = liftM (fromMaybe (defaultFullTextCache key)) (cachedNothingReadKV key)

instance KV PageInfo where
    type Key PageInfo = TURL
    kvBucket _ = "PageInfo"
    kvKey = piUrl
    kvCache _ = _piCache
_piCache = unsafePerformIO $ newCache 600 600 (200*1024*1024)
{-# NOINLINE _piCache #-}
readPageInfo :: TURL -> IO (Maybe PageInfo)
readPageInfo = readKV

cachedReadPageInfo :: TURL -> IO (Maybe PageInfo)
cachedReadPageInfo = cachedReadKV

cachedNothingReadPageInfo :: TURL -> IO (Maybe PageInfo)
cachedNothingReadPageInfo = cachedNothingReadKV

mergeWritePageInfo :: PageInfo -> IO (())
mergeWritePageInfo = mergeWriteKV

deletePageInfo :: PageInfo -> IO (())
deletePageInfo = deleteKV

readManyPageInfos :: [TURL] -> IO ([Maybe PageInfo])
readManyPageInfos = readManyKVs

cachedReadManyPageInfos :: [TURL] -> IO ([Maybe PageInfo])
cachedReadManyPageInfos = cachedReadManyKVs

cachedNothingReadManyPageInfos :: [TURL] -> IO ([Maybe PageInfo])
cachedNothingReadManyPageInfos = cachedNothingReadManyKVs

writeManyPageInfos :: [PageInfo] -> IO (())
writeManyPageInfos = writeManyKVs

modifyPageInfo :: TURL -> (Maybe PageInfo -> IO (PageInfo, b)) -> IO b
modifyPageInfo = modifyKV

modifyPageInfo_ :: TURL -> (Maybe PageInfo -> IO PageInfo) -> IO ()
modifyPageInfo_ = modifyKV_

modifyPageInfo' :: TURL -> (PageInfo -> IO (PageInfo, b)) -> IO b
modifyPageInfo' key f = modifyKV key (f . fromMaybe (defaultPageInfo key))

modifyPageInfo'_ :: TURL -> (PageInfo -> IO PageInfo) -> IO ()
modifyPageInfo'_ key f = modifyKV_ key (f . fromMaybe (defaultPageInfo key))

alterPageInfo :: TURL -> (Maybe PageInfo -> IO (Maybe PageInfo, b)) -> IO b
alterPageInfo = alterKV

readPageInfo' :: Key PageInfo -> IO PageInfo
readPageInfo' key = liftM (fromMaybe (defaultPageInfo key)) (readKV key)

cachedReadPageInfo' :: Key PageInfo -> IO PageInfo
cachedReadPageInfo' key = liftM (fromMaybe (defaultPageInfo key)) (cachedReadKV key)

cachedNothingReadPageInfo' :: Key PageInfo -> IO PageInfo
cachedNothingReadPageInfo' key = liftM (fromMaybe (defaultPageInfo key)) (cachedNothingReadKV key)

instance KV Favicon where
    type Key Favicon = TURL
    kvBucket _ = "Favicon"
    kvKey = faviconSourceUrl
    kvCache _ = _faviconCache
_faviconCache = unsafePerformIO $ newCache 600 600 (200*1024*1024)
{-# NOINLINE _faviconCache #-}
readFavicon :: TURL -> IO (Maybe Favicon)
readFavicon = readKV

cachedReadFavicon :: TURL -> IO (Maybe Favicon)
cachedReadFavicon = cachedReadKV

cachedNothingReadFavicon :: TURL -> IO (Maybe Favicon)
cachedNothingReadFavicon = cachedNothingReadKV

mergeWriteFavicon :: Favicon -> IO (())
mergeWriteFavicon = mergeWriteKV

deleteFavicon :: Favicon -> IO (())
deleteFavicon = deleteKV

readManyFavicons :: [TURL] -> IO ([Maybe Favicon])
readManyFavicons = readManyKVs

cachedReadManyFavicons :: [TURL] -> IO ([Maybe Favicon])
cachedReadManyFavicons = cachedReadManyKVs

cachedNothingReadManyFavicons :: [TURL] -> IO ([Maybe Favicon])
cachedNothingReadManyFavicons = cachedNothingReadManyKVs

writeManyFavicons :: [Favicon] -> IO (())
writeManyFavicons = writeManyKVs

modifyFavicon :: TURL -> (Maybe Favicon -> IO (Favicon, b)) -> IO b
modifyFavicon = modifyKV

modifyFavicon_ :: TURL -> (Maybe Favicon -> IO Favicon) -> IO ()
modifyFavicon_ = modifyKV_

modifyFavicon' :: TURL -> (Favicon -> IO (Favicon, b)) -> IO b
modifyFavicon' key f = modifyKV key (f . fromMaybe (defaultFavicon key))

modifyFavicon'_ :: TURL -> (Favicon -> IO Favicon) -> IO ()
modifyFavicon'_ key f = modifyKV_ key (f . fromMaybe (defaultFavicon key))

alterFavicon :: TURL -> (Maybe Favicon -> IO (Maybe Favicon, b)) -> IO b
alterFavicon = alterKV

readFavicon' :: Key Favicon -> IO Favicon
readFavicon' key = liftM (fromMaybe (defaultFavicon key)) (readKV key)

cachedReadFavicon' :: Key Favicon -> IO Favicon
cachedReadFavicon' key = liftM (fromMaybe (defaultFavicon key)) (cachedReadKV key)

cachedNothingReadFavicon' :: Key Favicon -> IO Favicon
cachedNothingReadFavicon' key = liftM (fromMaybe (defaultFavicon key)) (cachedNothingReadKV key)

instance KV HotLinks where
    type Key HotLinks = T.Text
    kvBucket _ = "HotLinks"
    kvKey = hlsUser
    kvCache _ = _hlsCache
_hlsCache = unsafePerformIO $ newCache 600 600 (200*1024*1024)
{-# NOINLINE _hlsCache #-}
readHotLinks :: T.Text -> IO (Maybe HotLinks)
readHotLinks = readKV

cachedReadHotLinks :: T.Text -> IO (Maybe HotLinks)
cachedReadHotLinks = cachedReadKV

cachedNothingReadHotLinks :: T.Text -> IO (Maybe HotLinks)
cachedNothingReadHotLinks = cachedNothingReadKV

mergeWriteHotLinks :: HotLinks -> IO (())
mergeWriteHotLinks = mergeWriteKV

deleteHotLinks :: HotLinks -> IO (())
deleteHotLinks = deleteKV

readManyHotLinkss :: [T.Text] -> IO ([Maybe HotLinks])
readManyHotLinkss = readManyKVs

cachedReadManyHotLinkss :: [T.Text] -> IO ([Maybe HotLinks])
cachedReadManyHotLinkss = cachedReadManyKVs

cachedNothingReadManyHotLinkss :: [T.Text] -> IO ([Maybe HotLinks])
cachedNothingReadManyHotLinkss = cachedNothingReadManyKVs

writeManyHotLinkss :: [HotLinks] -> IO (())
writeManyHotLinkss = writeManyKVs

modifyHotLinks :: T.Text -> (Maybe HotLinks -> IO (HotLinks, b)) -> IO b
modifyHotLinks = modifyKV

modifyHotLinks_ :: T.Text -> (Maybe HotLinks -> IO HotLinks) -> IO ()
modifyHotLinks_ = modifyKV_

modifyHotLinks' :: T.Text -> (HotLinks -> IO (HotLinks, b)) -> IO b
modifyHotLinks' key f = modifyKV key (f . fromMaybe (defaultHotLinks key))

modifyHotLinks'_ :: T.Text -> (HotLinks -> IO HotLinks) -> IO ()
modifyHotLinks'_ key f = modifyKV_ key (f . fromMaybe (defaultHotLinks key))

alterHotLinks :: T.Text -> (Maybe HotLinks -> IO (Maybe HotLinks, b)) -> IO b
alterHotLinks = alterKV

readHotLinks' :: Key HotLinks -> IO HotLinks
readHotLinks' key = liftM (fromMaybe (defaultHotLinks key)) (readKV key)

cachedReadHotLinks' :: Key HotLinks -> IO HotLinks
cachedReadHotLinks' key = liftM (fromMaybe (defaultHotLinks key)) (cachedReadKV key)

cachedNothingReadHotLinks' :: Key HotLinks -> IO HotLinks
cachedNothingReadHotLinks' key = liftM (fromMaybe (defaultHotLinks key)) (cachedNothingReadKV key)

instance KV FeedbackUserInfosList where
    type Key FeedbackUserInfosList = T.Text
    kvBucket _ = "FeedbackUserInfosList"
    kvKey = fuilId
    kvCache _ = _fuilCache
_fuilCache = unsafePerformIO $ newCache 60 60 (10*1024*1024)
{-# NOINLINE _fuilCache #-}
readFeedbackUserInfosList :: T.Text -> IO (Maybe FeedbackUserInfosList)
readFeedbackUserInfosList = readKV

cachedReadFeedbackUserInfosList :: T.Text -> IO (Maybe FeedbackUserInfosList)
cachedReadFeedbackUserInfosList = cachedReadKV

cachedNothingReadFeedbackUserInfosList :: T.Text -> IO (Maybe FeedbackUserInfosList)
cachedNothingReadFeedbackUserInfosList = cachedNothingReadKV

mergeWriteFeedbackUserInfosList :: FeedbackUserInfosList -> IO (())
mergeWriteFeedbackUserInfosList = mergeWriteKV

deleteFeedbackUserInfosList :: FeedbackUserInfosList -> IO (())
deleteFeedbackUserInfosList = deleteKV

readManyFeedbackUserInfosLists :: [T.Text] -> IO ([Maybe FeedbackUserInfosList])
readManyFeedbackUserInfosLists = readManyKVs

cachedReadManyFeedbackUserInfosLists :: [T.Text] -> IO ([Maybe FeedbackUserInfosList])
cachedReadManyFeedbackUserInfosLists = cachedReadManyKVs

cachedNothingReadManyFeedbackUserInfosLists :: [T.Text] -> IO ([Maybe FeedbackUserInfosList])
cachedNothingReadManyFeedbackUserInfosLists = cachedNothingReadManyKVs

writeManyFeedbackUserInfosLists :: [FeedbackUserInfosList] -> IO (())
writeManyFeedbackUserInfosLists = writeManyKVs

modifyFeedbackUserInfosList :: T.Text -> (Maybe FeedbackUserInfosList -> IO (FeedbackUserInfosList, b)) -> IO b
modifyFeedbackUserInfosList = modifyKV

modifyFeedbackUserInfosList_ :: T.Text -> (Maybe FeedbackUserInfosList -> IO FeedbackUserInfosList) -> IO ()
modifyFeedbackUserInfosList_ = modifyKV_

modifyFeedbackUserInfosList' :: T.Text -> (FeedbackUserInfosList -> IO (FeedbackUserInfosList, b)) -> IO b
modifyFeedbackUserInfosList' key f = modifyKV key (f . fromMaybe (defaultFeedbackUserInfosList key))

modifyFeedbackUserInfosList'_ :: T.Text -> (FeedbackUserInfosList -> IO FeedbackUserInfosList) -> IO ()
modifyFeedbackUserInfosList'_ key f = modifyKV_ key (f . fromMaybe (defaultFeedbackUserInfosList key))

alterFeedbackUserInfosList :: T.Text -> (Maybe FeedbackUserInfosList -> IO (Maybe FeedbackUserInfosList, b)) -> IO b
alterFeedbackUserInfosList = alterKV

readFeedbackUserInfosList' :: Key FeedbackUserInfosList -> IO FeedbackUserInfosList
readFeedbackUserInfosList' key = liftM (fromMaybe (defaultFeedbackUserInfosList key)) (readKV key)

cachedReadFeedbackUserInfosList' :: Key FeedbackUserInfosList -> IO FeedbackUserInfosList
cachedReadFeedbackUserInfosList' key = liftM (fromMaybe (defaultFeedbackUserInfosList key)) (cachedReadKV key)

cachedNothingReadFeedbackUserInfosList' :: Key FeedbackUserInfosList -> IO FeedbackUserInfosList
cachedNothingReadFeedbackUserInfosList' key = liftM (fromMaybe (defaultFeedbackUserInfosList key)) (cachedNothingReadKV key)

instance KV OfdReceipt where
    type Key OfdReceipt = (T.Text, Bool)
    kvBucket _ = "OfdReceipt"
    kvKey = orOrderIdRefund
    kvCache _ = _orCache
_orCache = unsafePerformIO $ newCache 60 60 (10*1024*1024)
{-# NOINLINE _orCache #-}
readOfdReceipt :: (T.Text, Bool) -> IO (Maybe OfdReceipt)
readOfdReceipt = readKV

cachedReadOfdReceipt :: (T.Text, Bool) -> IO (Maybe OfdReceipt)
cachedReadOfdReceipt = cachedReadKV

cachedNothingReadOfdReceipt :: (T.Text, Bool) -> IO (Maybe OfdReceipt)
cachedNothingReadOfdReceipt = cachedNothingReadKV

mergeWriteOfdReceipt :: OfdReceipt -> IO (())
mergeWriteOfdReceipt = mergeWriteKV

deleteOfdReceipt :: OfdReceipt -> IO (())
deleteOfdReceipt = deleteKV

readManyOfdReceipts :: [(T.Text, Bool)] -> IO ([Maybe OfdReceipt])
readManyOfdReceipts = readManyKVs

cachedReadManyOfdReceipts :: [(T.Text, Bool)] -> IO ([Maybe OfdReceipt])
cachedReadManyOfdReceipts = cachedReadManyKVs

cachedNothingReadManyOfdReceipts :: [(T.Text, Bool)] -> IO ([Maybe OfdReceipt])
cachedNothingReadManyOfdReceipts = cachedNothingReadManyKVs

writeManyOfdReceipts :: [OfdReceipt] -> IO (())
writeManyOfdReceipts = writeManyKVs

modifyOfdReceipt :: (T.Text, Bool) -> (Maybe OfdReceipt -> IO (OfdReceipt, b)) -> IO b
modifyOfdReceipt = modifyKV

modifyOfdReceipt_ :: (T.Text, Bool) -> (Maybe OfdReceipt -> IO OfdReceipt) -> IO ()
modifyOfdReceipt_ = modifyKV_

modifyOfdReceipt' :: (T.Text, Bool) -> (OfdReceipt -> IO (OfdReceipt, b)) -> IO b
modifyOfdReceipt' key f = modifyKV key (f . fromMaybe (defaultOfdReceipt key))

modifyOfdReceipt'_ :: (T.Text, Bool) -> (OfdReceipt -> IO OfdReceipt) -> IO ()
modifyOfdReceipt'_ key f = modifyKV_ key (f . fromMaybe (defaultOfdReceipt key))

alterOfdReceipt :: (T.Text, Bool) -> (Maybe OfdReceipt -> IO (Maybe OfdReceipt, b)) -> IO b
alterOfdReceipt = alterKV

readOfdReceipt' :: Key OfdReceipt -> IO OfdReceipt
readOfdReceipt' key = liftM (fromMaybe (defaultOfdReceipt key)) (readKV key)

cachedReadOfdReceipt' :: Key OfdReceipt -> IO OfdReceipt
cachedReadOfdReceipt' key = liftM (fromMaybe (defaultOfdReceipt key)) (cachedReadKV key)

cachedNothingReadOfdReceipt' :: Key OfdReceipt -> IO OfdReceipt
cachedNothingReadOfdReceipt' key = liftM (fromMaybe (defaultOfdReceipt key)) (cachedNothingReadKV key)

instance KV ParserEnvironment where
    type Key ParserEnvironment = T.Text
    kvBucket _ = "ParserEnvironment"
    kvKey = peKey
    kvCache _ = _peCache
_peCache = unsafePerformIO $ newCache 3600 3600 (100*1024*1024)
{-# NOINLINE _peCache #-}
readParserEnvironment :: T.Text -> IO (Maybe ParserEnvironment)
readParserEnvironment = readKV

cachedReadParserEnvironment :: T.Text -> IO (Maybe ParserEnvironment)
cachedReadParserEnvironment = cachedReadKV

cachedNothingReadParserEnvironment :: T.Text -> IO (Maybe ParserEnvironment)
cachedNothingReadParserEnvironment = cachedNothingReadKV

mergeWriteParserEnvironment :: ParserEnvironment -> IO (())
mergeWriteParserEnvironment = mergeWriteKV

deleteParserEnvironment :: ParserEnvironment -> IO (())
deleteParserEnvironment = deleteKV

readManyParserEnvironments :: [T.Text] -> IO ([Maybe ParserEnvironment])
readManyParserEnvironments = readManyKVs

cachedReadManyParserEnvironments :: [T.Text] -> IO ([Maybe ParserEnvironment])
cachedReadManyParserEnvironments = cachedReadManyKVs

cachedNothingReadManyParserEnvironments :: [T.Text] -> IO ([Maybe ParserEnvironment])
cachedNothingReadManyParserEnvironments = cachedNothingReadManyKVs

writeManyParserEnvironments :: [ParserEnvironment] -> IO (())
writeManyParserEnvironments = writeManyKVs

modifyParserEnvironment :: T.Text -> (Maybe ParserEnvironment -> IO (ParserEnvironment, b)) -> IO b
modifyParserEnvironment = modifyKV

modifyParserEnvironment_ :: T.Text -> (Maybe ParserEnvironment -> IO ParserEnvironment) -> IO ()
modifyParserEnvironment_ = modifyKV_

modifyParserEnvironment' :: T.Text -> (ParserEnvironment -> IO (ParserEnvironment, b)) -> IO b
modifyParserEnvironment' key f = modifyKV key (f . fromMaybe (defaultParserEnvironment key))

modifyParserEnvironment'_ :: T.Text -> (ParserEnvironment -> IO ParserEnvironment) -> IO ()
modifyParserEnvironment'_ key f = modifyKV_ key (f . fromMaybe (defaultParserEnvironment key))

alterParserEnvironment :: T.Text -> (Maybe ParserEnvironment -> IO (Maybe ParserEnvironment, b)) -> IO b
alterParserEnvironment = alterKV

readParserEnvironment' :: Key ParserEnvironment -> IO ParserEnvironment
readParserEnvironment' key = liftM (fromMaybe (defaultParserEnvironment key)) (readKV key)

cachedReadParserEnvironment' :: Key ParserEnvironment -> IO ParserEnvironment
cachedReadParserEnvironment' key = liftM (fromMaybe (defaultParserEnvironment key)) (cachedReadKV key)

cachedNothingReadParserEnvironment' :: Key ParserEnvironment -> IO ParserEnvironment
cachedNothingReadParserEnvironment' key = liftM (fromMaybe (defaultParserEnvironment key)) (cachedNothingReadKV key)

