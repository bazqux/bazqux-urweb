{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards, TupleSections,
             BangPatterns, TransformListComp, ScopedTypeVariables, LambdaCase,
             PatternSynonyms
#-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Session
    ( newSessionJunk, newSession, tryGetFeverUser
    , clearSession, clearSession', logOutAllSessions
    , looksLikePublicFeed
    , addTwitterScreenName
    ) where

import Control.Monad
import Data.List
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Generated.DataTypes
import Data.Binary
import Lib.UrTime
import Lib.BinaryInstances
import Lib.Log
import Data.Maybe
import URL
import Riak
import Generated.RiakIO
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import System.Random
import qualified Data.ByteString.Char8 as B
import Lib.Hash
import Lib.StringConversion
import Mailer
import Account
import Resolvables

newSessionJunk :: LoginType -> LoginAccessToken -> [(JunkText, JunkText)]
    -> IO Session
newSessionJunk lt l p = newSession False lt l (unjunk p)
    where unjunk = map $ \ (a,b) -> (unJunkText a, unJunkText b)

newSession :: Bool -> LoginType -> LoginAccessToken -> [(T.Text, T.Text)]
    -> IO Session
newSession apiUser lt lat p = withLogger $ \ l -> do
    t <- getUrTime

    u <- getUserByLoginOrCreateAccount lt

    rnd <- replicateM 200 randomIO :: IO [Word8]

    let expire
            | Just "Feeddler" <- lookup "source" p = 3
            | Just ua <- lookup "User-Agent" p
            , "fiery" `T.isInfixOf` T.toLower ua = 3
              -- Feeddler и Fiery постоянно логинятся при каждой синхронизации
            | Just ua <- lookup "User-Agent" p
            , "axios" `T.isInfixOf` T.toLower ua = 1
              -- чей-то скрипт стучит каждые 20 сек
            | Just "Reeder" <- lookup "client" p = 45
              -- Reeder, требует перелогиниваться при ответе Unauthorized,
              -- раньше с таймаутом 45 дней вроде нормально работал
            | Just who <- lookup "Who" p
            , "facebook.com" `T.isInfixOf` who = 45
              -- к сожалению, поскольку у Facebook rate limit-ы зависят
              -- от числа активных пользователей,
              -- пользователям FB придется заходить почаще
              -- (в дальшейшем это надо решать используя токены
              -- пользователей при обновлении фидов, чтобы они были
              -- действительно активными)
            | apiUser = 15
            | otherwise = 180
              -- для веба можно и подольше, у Google expire 1-2 года.
              -- У нас через месяц-два после окончания free trial удаляются
              -- пользователи, но это не страшно, т.к. при проверке сессии
              -- проверяется существование пользователя, а если он
              -- снова создаст аккаунт, то чтобы ему и не зайти по старой сессии
        s = Session
            { sessionKey =
                bst $ B.map fix $ B.filter (/= '=') $
                base64_sha3_512 $ BL.toStrict $ encode (t, u, p, rnd)
            , sessionExpire =
                t `plusUrTime` (expire*24*3600)
            , sessionCleared = False
            , sessionUser = u
            }
    updUserSessions HS.insert s
    mergeWriteSession s

    logLT l $ T.concat
        ["User authenticated ", u, " ", T.pack $ show p
        ," session expires in ", T.pack $ show expire, " days"
        ," (session key ", sessionKey s, ")"]

    let updateKeysAndNames = do
            case lat of
                LATNone -> return ()
                LATFacebook a -> updApiKeys u $ \ k ->
                    k { akFacebookAccessToken = Just (t, a) }
                LATTwitter a -> updApiKeys u $ \ k ->
                    k { akTwitterAccessToken = Just (t, a) }
            updateTwitterScreenNames u

    modifyUserStats_ u $ \ mbus ->
        case mbus of
            Just us -> do
                updateKeysAndNames
                -- mailEvent us "Sign In" ""
                return $ incrUserStatsCounters "SignIn" us
            Nothing -> do -- new User!!!
                updUserSettings u $ \ ust ->
                    ust { ustExperiments = Just [UENo9] }
                    -- убираем $9 у всех новых пользователей
                addToMailQueue mNoSubscriptions u
                (du,ub) <- tryRestoreUser u
                updateKeysAndNames
                --  ^ после восстановления пользователя, но перед mail,
                --  чтобы twitter account name был уже задан
                let us =
                        UserStats
                        { usId = u
                        , usUID = mkUID u
                        , usFirstSignInTime = t { urtMicroseconds = 0 }
                        , usFirstSignInDetails = p <> firstTime
                        , usCounters  = Map.singleton "SignIn" 1
                        , usSubscriptionCounters = Map.empty
                        , usReserved1 = Map.empty
                        , usReserved2 = Map.empty
                        , usReserved3 = Map.empty
                        , usReserved4 = Map.empty
                        }
                    mtime (UrTime a _) = T.pack $ showUrTime (UrTime a 0)
                    maxTime t = T.pack $ formatUrTime "%Y-%m-%d" $
                                maximum $ UrTime 0 0 : t
                    ft = "FirstTime"
                    firstTime = case ub of
                        Just (ubUserStats -> pus)
                            | Just t <- lookup ft $ usFirstSignInDetails pus
                            -> [(ft,t)]
                            | otherwise -> [(ft,mtime $ usFirstSignInTime pus)]
                        _ -> []

                mailEvent2 us
                    (T.concat [ "First Sign In"
                              , case du of
                                    Just d -> T.concat
                                        [ " (deleted at ", maxTime (duBackups d)
                                        , case duMailsSent d of
                                              Just m ->
                                                  T.concat [ ", mailed at "
                                                           , maxTime m ]
                                              Nothing -> ""
                                        , ")"
                                        ]
                                    Nothing -> ""
                              , "\n\n"
                              , u, " ", loginUserStats us
                              ])
                    ""
                    (maybe []
                     (\ ub ->
                      let pus = ubUserStats ub in
                      (["\n\nPrevious stats:"
                       , T.concat [u, " ", loginUserStats pus]
                       , T.pack $ show (length (uSubscriptions $ ubUser ub))
                                ++ " subscriptions"
                       , ""
                       , mtime (usFirstSignInTime pus)
                       , "-----"
                       ]
                       ++ formatUserStatsForEmail pus)) ub)
                return us

    return s
    where w32 :: Int -> Word32
          w32 = toEnum
          fix '+' = '-'
          fix '/' = '.'
          fix '=' = '_'
          fix c   = c
          -- чтобы не escape-ить в riak и cookie

updateTwitterScreenNames u = updUserSettings u $ \ s ->
    case akTwitterAccessToken =<< ustApiKeys s of
        Just (_, a)
            | Just i <- lookup "user_id" a
            , Just n <- lookup "screen_name" a
            -> addTwitterScreenName' i n s
        _ -> s

addTwitterScreenName u i n = updUserSettings u $ addTwitterScreenName' i n

addTwitterScreenName' userId screenName = modifyUSTEx  $ \e ->
    e { usteAssociatedAccountNames =
          Map.insert (LTTwitter userId) ("@" <> screenName)
              $ usteAssociatedAccountNames e }

loginUserStats us =
    T.concat ["(", d "Country", ", ", browser
             , ", "
             , maybe "-" (topLevelHostName . hostNameFromUrlT)
                         (lookup "Referrer" $ usFirstSignInDetails us)
             , ")"]
    where d n = fromMaybe "-" $ lookup n $ usFirstSignInDetails us
          ua = d "User-Agent"
          browser = fromMaybe "-" $
                    find (`T.isInfixOf` T.toLower ua) $ map T.toLower
                    [ "Android", "webOS", "iPad", "iPod", "iPhone"
                    , "BlackBerry", "IEMobile", "Opera Mini"
                    , "Chrome", "MSIE", "Safari", "Opera", "Firefox"
                    , "Reeder", "Mr.Reader" ]

looksLikePublicFeed k =
    T.length k == 20 &&
    T.all (\ c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))
         (T.toLower k)

tryGetFeverUser k
    | T.length k == 32 =
        getUserByLogin (LTFeverApiKey k) Nothing
    | looksLikePublicFeed k = do
        pf <- readPublicFeed (T.toLower k)
        return $ pf >>= pfUser
    | otherwise = return Nothing

clearSession :: T.Text -> Key Session -> IO ()
clearSession reason k = do
    s <- cachedNothingReadSession k
    maybe (return ()) (clearSession' reason) s
--          (mergeWriteSession . \ s -> s { sessionCleared = True }) s

clearSession' reason s = do
    logS $ "Deleting session " ++ T.unpack (sessionKey s)
         ++ " (sessionExpire = " ++ show (sessionExpire s) ++ ", reason = "
         ++ T.unpack reason ++ ")"
    deleteSession s
    updUserSessions HS.delete s

initUserSessions = do
    ss <- riakBucketKeys "Session"
    forM_ ss $
        mapM_ (updUserSessions HS.insert) <=< readSession

logOutAllSessions u except = do
    s <- readUserSessions' u
    mapM_ (clearSession "logOutAllSessions") $
        filter (`notElem` except) $ HS.toList $ uSessionsSessions s

removeNonexistentSessions u =
    go . HS.toList . uSessionsSessions =<< readUserSessions' u
    where go [] = return ()
          go (splitAt 1000 -> (slice, rest)) = do
              ss <- readManySessions slice
              let notExists = HS.fromList [s | (Nothing, s) <- zip ss slice]
              when (not $ HS.null notExists) $ do
                  logTL ["Removing ", showT (HS.size notExists)
                        ," non-existent sessions for ", u]
                  updUserSessions' (`HS.difference` notExists) u
              go rest

updUserSessions f s =
    updUserSessions' (f $ sessionKey s) (sessionUser s)
updUserSessions' f u =
    modifyUserSessions'_ u $ \ us -> return $
        us { uSessionsSessions = f (uSessionsSessions us) }

clearFacebookSessions = do
    -- время начала сессии у нас не хранится, так что можем удалять только
    -- сессии, которые гарантированно не 45-дневные (т.е. все 180-дневные
    -- сессии у которых осталось больше 45 дней)
    -- и мы точно не знаем, зашел ли пользователь с facebook,
    -- так что чистим сессии всех пользователей (можно определять
    -- твиттер/openid, но это одноразовая ф-я)
    ss <- riakBucketKeys "Session"
    t <- getUrTime
    let go n [] =
            logS $ "Cleared " ++ show n ++ "/" ++ show (length ss)
                ++ " sessions"
        go !n (key:ss) = do
            mbs <- readSession key
            case mbs of
                Just s
                    | d <- diffUrTime (sessionExpire s) t / day
                    , d > 45 && d < 180 - 45 -> do
--                        print s
                        clearSession' "expired" s
                        go (n+1) ss
                _ ->
                    go n ss
    go 0 ss
