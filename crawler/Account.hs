{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards, TupleSections,
             BangPatterns, ScopedTypeVariables, LambdaCase
#-}
-- | Утилиты работы с аккаунтами
module Account
    ( updUserSettings, updUserSettingsEx, updApiKeys, isLTFeverApiKey

    , backupAndRemoveAccount, backupAccount, deleteAccount
    , tryReadLastBackup, tryRestoreUser, griUpdateRemovedFeeds
    , restoreSubscriptionsFromBackup
    , archiveExpiredUsers

    , setUsername, setPassword
    , tryAddAssociatedAccount, tryRemoveAssociatedAccount
    , getUserByLogin
    , getUserByLoginOrCreateAccount

    , sendSignUpEmail
    , sendPasswordResetEmail
    , sendChangeEmailEmail
    , verifySignUpToken
    , verifyPasswordResetToken
    , verifyChangeEmailToken
    , verifyRestoreAccessToken
    , passwordResetEmail, maskEmail
    , clearEmailVerificationToken'
    )
    where

import Control.Monad
import qualified Control.Exception as E
import Control.Applicative
import Data.List
import Data.Binary (encode)
import Data.Char
import Data.Word (Word8)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Generated.DataTypes
import Lib.UrTime
import Lib.Log
import Lib.Hash
import Data.Maybe
import Resolvables
import Riak
import Generated.RiakIO
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import System.Random.Shuffle
import System.Random
import Control.Concurrent
import Discovery (postsAddSubscriber)
import Search
import Lib.StringConversion
import UsageFlags
import Payments (clearPaidTill, looksLikeOrderId, tryDownloadFastSpringOrder, fsoReferrer)
import URL (validateEmail)
import Mailer
import OPML
import Subscriptions

ustAssociatedAccounts = usteAssociatedAccounts . ustEx'

updUserSettings :: Key User -> (UserSettings -> UserSettings) -> IO ()
updUserSettings user f = do
    modifyUserSettings'_ user $ \ s ->
        return $ (f s) { ustEditsCount = ustEditsCount s + 1 }

updUserSettingsEx :: Key User -> (UserSettingsEx -> UserSettingsEx) -> IO ()
updUserSettingsEx user f = updUserSettings user (modifyUSTEx f)

updApiKeys :: Key User -> (ApiKeys -> ApiKeys) -> IO ()
updApiKeys user f = updUserSettings user $ \ us ->
    us { ustApiKeys = Just $ f $ fromMaybe defaultApiKeys $ ustApiKeys us }

sameShuffle [] = []
sameShuffle l  = shuffle' l (length l) (mkStdGen 12345)

-- curl 'localhost:8098/buckets/DeletedUser/index/$bucket/_' >all_users.js
sendWelcomeBacks = do
    us <- fmap sameShuffle (riakBucketKeys "DeletedUser")
    t <- getUrTime
    let Just maxTime =
            tryParseTime ["%Y-%m-%d %H:%M:%S"] "2013-09-07 00:00:00"
        go 0 _ = return ()
        go _ [] = return ()
        go n (u:us) = do
             logT u
             (du,ub) <- tryReadLastBackup' False u
             user <- readUser u
             case (user, du,ub) of
                 (Just _, _, _) -> do
                     logS "Already returned"
                     go n us
                 (Nothing, Just du, Just ub)
                     | "://" `T.isInfixOf` u -> do
                         logS "No mail"
                         go n us
                     | null $ uSubscriptions (ubUser ub) -> do
                         logS "No subscriptions"
                         go n us
                     | all (isNothing . ssFeed) (uSubscriptions (ubUser ub)) -> do
                         logS "Error subscriptions"
                         go n us
--                      | usFirstSignInTime (ubUserStats ub) > maxTime -> do
--                          logS "Later"
--                          go n us
                     | duMailsSent du `notElem` [Nothing, Just []] -> do
                         logS $ "Already sent " ++ show (duMailsSent du)
                         go n us
                     | Just email <- userEmail' (ubUserSettings ub) -> do
                         modifyDeletedUser'_ u $ \ du ->
                             return $ du { duMailsSent = Just [t] }
                         opml <- subscriptionsOPML t $ filter ((/= "") . sUrl) $ uSubscriptions $ ubUser ub
                         mailOpml
                             (case uvmPaidTill $ uViewMode $ ubUser ub of
                                  PTPaid _ -> True
                                  PTPaidFinished _ -> True
                                  _ -> False)
                             (fromMaybe "" $ lookup "Country" $
                              usFirstSignInDetails (ubUserStats ub))
                             email
                             (BL.fromStrict $ tbs opml)
                         logS "SENT"
                         threadDelay 1000000
                         go (n-1) us
                     | otherwise -> do
                         logS "No email"
                         go n us
                 _ -> go n us
    go 500 us

-- Добавить сохранение Filters в UserBackup?
-- Хотя подписки-то удаляются?
-- Надо еще подумать, пока не удаляем их вообще
archiveExpiredUsers = do
    us <- getAllUsers
    t <- getUrTime
    let expired u = case uvmPaidTill (uViewMode u) of
            PTUnknown -> True
            --  ^ совсем старые аккаунты, в которые так никто больше не зашел
            PTFreeTrial pt -> diffUrTime t pt > 30*day
            PTFreeTrialFinished pt -> diffUrTime t pt > 30*day
            PTPaidFinished pt -> diffUrTime t pt > 60*day
            PTPaid pt -> diffUrTime t pt > 60*day
    forM_ (zip [1..] us) $ \ (n,u) -> do
        mbuser <- readUser u
        case mbuser of
            Just user | expired user && u `notElem` ["fte", "se", "demo"] -> do
                let nsubs = length $ uSubscriptions user
                f <- readFilters u
                let hasF =
                        case f of
                            Nothing -> False
                            Just f -> fFilters f /= [] || fSmartStreams f /= []
                when (not hasF) $ do
                    logS $ "Archiving " ++ T.unpack u
                        ++ " (" ++ show nsubs ++ " subs, "
                        ++ show (uvmPaidTill (uViewMode user)) ++ ")"
                    backupAndRemoveAccount u
            _ ->
                return ()

backupAndRemoveAccount u = do
    t <- backupAccount u
    mergeWriteDeletedUser $
        (defaultDeletedUser u) { duBackups = [t] }
    deleteAccount False u

backupAccount u = do
    t <- getUrTime
    let ubKey = (u,t)
        ubReserved1 = False
        ubReserved2 = False
        ubReserved3 = False
        ubReserved4 = False
    ubUser         <- readUser' u
    ubUserStats    <- readUserStats' u
    ubUserFilters  <- readUserFilters' u
    ubUserSettings <- readUserSettings' u
    ubGRIds        <- readGRIds' u
    ubFeverIds     <- readFeverIds' u
    mergeWriteUserBackup $ UserBackup {..}
    return t

deleteAccount completeDelete acc = do
    e <- isJust <$> readUser acc
    if not e && not completeDelete then
        logT "Already deleted"
    else if not e then do
        logT "Restoring account before complete delete"
        (du, bu) <- tryRestoreUser acc
        if isNothing bu then
            fail $ "No such account " <> show acc <> " (already deleted?)"
        else
            del
    else
        del
    where del = deleteAccount' completeDelete acc

deleteAccount' completeDelete acc = do
    userDeleteTaggedItems acc
    s <- readUserSettings' acc
    when completeDelete $ do
        usageFlag acc UFDeleteAccount
        userEvent acc "Delete account" ""
        du <- readDeletedUser' acc
        forM_ (duBackups du) $ \ t ->
            deleteUserBackup $ defaultUserBackup (acc, t)
        deleteDeletedUser du
        ust <- readUserSettings' acc
        forM_ (concat $ maybe [] Map.elems $ ustPublicFeeds ust) $ \ (f,_,_) ->
            deletePublicFeed $ defaultPublicFeed f
        mapM_ (deleteLogin . defaultLogin) (ustAssociatedAccounts s)
    mapM_ (flip removeFromMailQueue acc)
        [mNoSubscriptions, mSubscriptionsReady, mFeedback]
    clearUserSessions acc
    clearUserEmailVerificationTokens HM.keys acc
    deleteUserEmailVerificationTokens $ defaultUserEmailVerificationTokens acc
    u <- readUser' acc
    forM_ (filter ((/= "") . sUrl) $ uSubscriptions u) $ \ s -> do
        deletePostsRead $ defaultPostsRead (acc, sUrl s)
        postsAddSubscriber (sUrl s) acc False
    deleteUserStats $ defaultUserStats acc
    deleteUserFilters $ defaultUserFilters acc
    deleteUserSettings s
    deleteGRIds $ defaultGRIds acc
    deleteFeverIds $ defaultFeverIds acc
    deleteFilters $ defaultFilters acc
    deleteUser u

clearUserSessions acc = do
    ss <- readUserSessions' acc
    mapM_ (deleteSession . defaultSession) $ HS.toList $ uSessionsSessions ss
    deleteUserSessions ss

tryReadLastBackup = tryReadLastBackup' True
tryReadLastBackup' cached user = do
    du <- readDeletedUser user
    ub <- case du of
        Just d | (t:_) <- duBackups d ->
            (if cached then cachedReadUserBackup else readUserBackup) (user, t)
        _ -> return Nothing
    return (du,ub)

tryRestoreUser user = do
    (du, ub) <- tryReadLastBackup user
    case ub of
        Just (UserBackup {..}) -> do
            modifyUserSettings'_ user $ \ _ ->
                return ubUserSettings
            modifyUserFilters'_ user $ \ _ ->
                return ubUserFilters
            modifyFeverIds'_ user $ \ _ ->
                return $ (defaultFeverIds user)
                           { fiMaxId = fiMaxId ubFeverIds + 1 }
                           -- чтобы нормально синхронизировались существующие
                           -- клиенты (хотя у нас mobile login чистится)
            modifyUser'_ user $ \ u ->
                return $ u { uPayments = uPayments ubUser }
                -- восстанавливаем только платежи, show updated subscriptions
                -- не трогаем, чтобы нормально видеть восстанавливаемые
                -- подписки. Режимы просмотра тоже не надо восстанавливать
                -- (чтобы у старых пользователей комментарии оказались
                -- свернутыми и в starred/tagged не было смешанного режима
                -- просмотра, который нельзя изменить)
            let urls = filter (/= "") $ map sUrl $ uSubscriptions ubUser
            ps <- cachedReadManyPostss urls
            t <- getUrTime
            modifyGRIds'_ user $ \ _ ->
                griUpdateRemovedFeeds t ubUser urls ps ubGRIds
                -- восстанавливаем removedFeeds, чтобы звездочки правильно
                -- показывались
            void $ forkIO $ reindexUserTags user
        _ -> return ()
    return (du,ub)

griUpdateRemovedFeeds t u urls ps gri =
    let titles = HM.fromList [ (sUrl, fromMaybe sUrl sTitle)
                             | Subscription {..} <- uSubscriptions u]
        urlsSet = HS.fromList urls
    in
        return $
        gri
        { griLastId = griLastId gri + 1
        , griActiveFeeds = Just $
            (fromMaybe HS.empty $ griActiveFeeds gri)
            `HS.difference` urlsSet
        , griRemovedFeeds = Just $
            HM.fromList
            [( u, RemovedFeedInfo (HM.lookupDefault u u titles)
                    (fromMaybe u $ msgLink (pRootMessage p)) t 0)
            | Just p <- ps
            , let u = pBlogFeedUrl p]
            `HM.union` (fromMaybe HM.empty $ griRemovedFeeds gri)
        }

restoreSubscriptionsFromBackup user = do
    (du,ub) <- tryReadLastBackup user
    case ub of
        Just (UserBackup {..}) -> do
            let ss = [ s { sState = SSAdded } | s <- uSubscriptions ubUser
                     , sUrl s /= "" -- а то было такое у кого-то
                     ]
            userAddSubscriptions user ss
            userEvent user "Restore subscriptions" $ T.pack $
                show (length ss) ++ " subscriptions"
        _ ->
            return ()

copyViewMode from to = do
    fromU <- readUser' from
--    modifyUser'_ to $ \ u -> return $ u { uViewMode = (uViewMode u) { uvmSubViewModes = HM.union (uvmSubViewModes $ uViewMode u) (uvmSubViewModes $ uViewMode fromU) } }
    modifyUser'_ to $ \ u -> return $ u { uViewMode = (uViewMode u) { uvmSubViewModes = HM.map (\(i,vm) -> (i,vm { mtvmExpandedComments = False })) (uvmSubViewModes $ uViewMode u) } }

copyUserFeeds from to = do
    fromU <- readUser' from
    toU <- readUser' to
--     forM_ (filter (isJust . ssFeed) $ uSubscriptions toU) $ \ s ->
--         postsAddSubscriber (sUrl s) to True
    let existingFeeds = HS.fromList $ map sUrl $ uSubscriptions toU
        u = [ s { sFolders = [] }
            | s <- uSubscriptions fromU
            , isJust (ssFeed s)
            , not $ HS.member (sUrl s) existingFeeds]
    print ( length u
          , length (uSubscriptions fromU), length (uSubscriptions toU))
    prs <- readManyPostsReads $ map (\ s -> (from, sUrl s)) u
    writeManyPostsReads $
        map (\ pr -> pr { prKey = (to, snd $ prKey pr) }) $ catMaybes prs
    userAddSubscriptions to u
    forM_ u $ \ s ->
        postsAddSubscriber (sUrl s) to True

transferAccount old new = do
    modifyUser'_ new $ \ _ -> do
        u <- fmap (\ u -> u { uId = new }) $ readUser' old
        prs <- readManyPostsReads $
            map (\ s -> (old, sUrl s)) $ uSubscriptions u
        writeManyPostsReads $
            map (\ pr -> pr { prKey = (new, snd $ prKey pr) }) $ catMaybes prs
        forM_ (filter (isJust . ssFeed) $ uSubscriptions u) $ \ s ->
            postsAddSubscriber (sUrl s) new True
        return u
    t <- getUrTime
    clearPaidTill old
    modifyUserStats'_ new $ \ _ ->
        fmap (\ u -> u { usId = new }) $ readUserStats' old
    modifyUserFilters'_ new $ \ _ ->
        fmap (\ u -> u { ufUser = new }) $ readUserFilters' old
    modifyUserSettings'_ new $ \ _ -> do
        u <- fmap (\ u -> u { ustUser = new }) $ readUserSettings' old
        -- перемещать associated accounts
--         modifyUserSettings'_ old $ \ u ->
--             return $ u { associated accounts = [] }
        return u
    modifyGRIds'_ new $ \ _ ->
        fmap (\ u -> u { griUser = new }) $ readGRIds' old
    modifyFeverIds'_ new $ \ _ ->
        fmap (\ u -> u { fiUser = new }) $ readFeverIds' old

data CantRemoveAssociatedAccount = CantRemoveAssociatedAccount
    deriving Show

instance E.Exception CantRemoveAssociatedAccount

tryEditAA act =
    (act >> return True) `E.catch` \ CantRemoveAssociatedAccount -> return False

tryAddAssociatedAccount h u lt    = tryEditAA $ addAssociatedAccount h u lt
tryRemoveAssociatedAccount h u lt = tryEditAA $ removeAssociatedAccount h u lt

addAssociatedAccount host u (normalizeLoginType -> lt) = withLogger $ \ l -> do
    logLTL l ["addAssociatedAccount ", u, " ", showT lt]
    us <- readUserSettings u
    (_, lb) <- tryReadLastBackup u
    case us <|> (ubUserSettings <$> lb) of
        Nothing ->
            logLTL l ["User not found: ", u]
        Just s -> do
            lg <- readLogin' lt
            when (lUserID lg /= "" && lUserID lg /= u) $
                removeAssociatedAccount host (lUserID lg) lt
            em <- case associatedLTEmail lt s of
                Just lte -> do
                    modifyLogin' lte $ \ lg ->
                        if lUserID lg /= "" && lUserID lg /= u then do
                            logLTL l [showT lte, " is already busy with ", lUserID lg]
                            return (lg, [])
                        else do
                            logLTL l ["adding ", showT lte]
                            return (lg { lUserID = u }, [lte])
                Nothing ->
                    return []
            let modify =
                    modifyUSTEx (\ e -> e { usteAssociatedAccounts =
                        nub $ usteAssociatedAccounts e <> [lt] <> em })
            when (isJust us) $
                updUserSettings u modify
            modifyBackups
                (\ ub -> ub { ubUserSettings = modify $ ubUserSettings ub }) u
            modifyLogin'_ lt $ \ l -> return $ l { lUserID = u }
            accountEvent s host True u lt

accountEvent s0 h added uid lt = forM_ h $ \ host -> do
    s <- readUserSettings' uid
    let oem = userLTEmail' s0
        oun = listToMaybe [u | LTUsername u <- ustAssociatedAccounts s0]
    forM_ (associatedAccountDescription s lt) $ \ descr -> do
        userEvent uid
            ((if added then "Add" else "Remove") <> " associated account") descr
        forM_ oem $ \ oldEmail ->
            sendAssociatedAccountChange added descr host uid oldEmail
    case (added, lt) of
        (True, LTEmail e) -> do
            userEvent uid "Change email"
                (fromMaybe "none" oem <> " => " <> e)
            forM_ oem $ \ oldEmail ->
                sendChangeEmailBackEmail host uid oldEmail e
        (True, LTUsername un) -> do
            userEvent uid "Change username"
                (fromMaybe "none" oun <> " => " <> un)
            forM_ oem $ \ oldEmail ->
                sendUsernameChangeEmail host uid oldEmail un
        (False, LTEmail e) -> do
            userEvent uid "Email removed?" e
            forM_ oem $ \ oldEmail ->
                sendEmailRemovedEmail host uid oldEmail e
        (False, LTUsername un) -> do
            userEvent uid "Username removed" un
            forM_ oem $ \ oldEmail ->
                sendUsernameRemovedEmail host uid oldEmail un
        _ ->
            return ()

associatedLTEmail lt s
--     | null [() | LTEmail _ <- ustAssociatedAccounts s]
    = case lt of
        LTGoogle e -> lte e
        LTFacebook e -> lte e
        _ -> Nothing
--     | otherwise = Nothing
    where lte e
              | null [() | LTEmail e' <- ustAssociatedAccounts s
                     , e' /= e] = Just $ LTEmail e
              | otherwise = Nothing

removeAssociatedAccount host u (normalizeLoginType -> lt) = withLogger $ \ l -> do
    logLTL l ["removeAssociatedAccount ", u, " => ", showT lt]
    us <- readUserSettings u
    (_, lb) <- tryReadLastBackup u
    case us <|> (ubUserSettings <$> lb) of
        Nothing ->
            logLTL l ["User not found: ", u]
        Just s -> do
            let aa = filter (/= lt) $ ustAssociatedAccounts s
                canLogin lt = case lt of
                    LTEmail _ -> True
                    LTGoogle _ -> True
                    LTFacebook _ -> True
                    LTTwitter _ -> True
                    LTOpenId _ -> True
                    -- LTUsername нельзя, т.к. пароль не восстановишь
                    -- LTFeverApiKey тоже не считается
                    _ -> False
            when (not $ any canLogin aa) $
                E.throwIO CantRemoveAssociatedAccount
            when (isJust us) $
                updUserSettings u modify
            modifyBackups
                (\ ub -> ub { ubUserSettings = modify $ ubUserSettings ub })
                u
            l <- readLogin' lt
            when (lUserID l == u || lUserID l == "") $
                deleteLogin l
            accountEvent s host False u lt
    where modify =
              modifyUSTEx (\ e -> e { usteAssociatedAccounts =
                  rmFak $ filter (/= lt) $ usteAssociatedAccounts e })
          rmFak = case lt of
              LTUsername _ -> filter (not . isLTFeverApiKey)
              _ -> id

setPasswordHash u ph = do
    us <- readUserSettings u
    when (isJust us) $
        updUserSettings u modify
    modifyBackups
        (\ ub -> ub { ubUserSettings = modify $ ubUserSettings ub })
        u
    where modify = modifyUSTEx $ \ e -> e { ustePasswordHash = Just ph }

setPassword host u pwd = do
    em <- userEmail u
    case (em, host) of
        (Just e, Just h) ->
            void $ sendPasswordResetBackEmail h u (eaEmail e)
        _ ->
            return ()
    usageFlag u UFSetPassword
    us <- readUserSettings u
    (_, lb) <- tryReadLastBackup u
    case us <|> (ubUserSettings <$> lb) of
        Nothing ->
            logTL ["setPassword: User not found: ", u]
        Just s -> do
            let aa = ustAssociatedAccounts s
                oldKeys = filter isLTFeverApiKey aa
                newKeys = mapMaybe fak aa
            when (not $ checkPasswordHash pwd s) $ do
                ph <- newPasswordHash pwd
                setPasswordHash u ph
            mapM_ (addAssociatedAccount Nothing u) (newKeys \\ oldKeys)
            mapM_ (removeAssociatedAccount Nothing u) (oldKeys \\ newKeys)
    where fak (LTUsername un) = fak' un
          fak (LTEmail e)     = fak' e
          fak _ = Nothing
          fak' e = Just $ LTFeverApiKey $ feverApiKey e pwd

isLTFeverApiKey = \ case { LTFeverApiKey _ -> True; _ -> False }
isLTUsername    = \ case { LTUsername _ -> True; _ -> False }
isLTEmail       = \ case { LTEmail _ -> True; _ -> False }

validUsername (T.toLower . T.strip -> n) =
    T.length n >= 4 && T.all (\c -> isDigit c || c >= 'a' && c <= 'z') n

setUsername host u n
    | validUsername n =
        setEmailOrUsername host isLTUsername UFSetUsername u (LTUsername n)
    | otherwise =
        fail "setUsername: invalid username"
setEmail host u n =
    setEmailOrUsername host isLTEmail UFSetEmail u (LTEmail n)

setEmailOrUsername _ _ _ "demo" _ =
    fail "Email/username change is disabled for demo account"
setEmailOrUsername host is uf u (normalizeLoginType -> lt) = do
    usageFlag u uf
    mbu <- readLogin lt
    case mbu of
        Just lu
            | lUserID lu /= u -> return False
        _ -> do
            s <- readUserSettings' u
            let aa = ustAssociatedAccounts s
                unsOld = filter is aa
                unsNew = [lt]
            mapM_ (addAssociatedAccount host u) (unsNew \\ unsOld)
            mapM_ (removeAssociatedAccount Nothing u) (unsOld \\ unsNew)
            -- в remove без host, чтобы не отправлял сообщение
            -- об удалении email/username
            return True

normalizeUsername = stripGmail . T.toLower . T.strip
    where stripGmail x =
              fromMaybe x $
              T.stripSuffix "@gmail.com" x <|>
              -- FeedDemon добавляет @gmail.com
              T.stripSuffix "@bazqux.com" x
              -- JustReader, кажется, может такое добавить

feverApiKey login password =
    md5 $ T.concat [login, ":", password]

checkPasswordHash p s =
    (checkPasswordHash' p <$> ustePasswordHash (ustEx' s)) == Just True

checkPasswordHash' p ph
    | T.length ph == 60
    , salt <- T.take 32 ph
        = hashData (tbs $ salt <> p) == T.drop 32 ph
    | otherwise = False

newPasswordHash p = do
    salt <- bst . Base64.encode . B.pack
        <$> (replicateM 24 randomIO :: IO [Word8])
    return $ salt <> hashData (tbs $ salt <> p)

normalizeLoginType = \ case
    LTEmail e -> LTEmail $ normE e
    LTGoogle e -> LTGoogle $ normE e
    LTFacebook e -> LTFacebook $ normE e
    -- Google/FB тоже нормализуем на тот редкий (0.02%) случай,
    -- когда домен email-а в upper case
    LTUsername un -> LTUsername $ normalizeUsername un
    LTFeverApiKey k -> LTFeverApiKey $ T.toLower $ T.strip k
    l -> l
    where normE (T.strip -> e) = fromMaybe e (validateEmail e)

getUserByLogin :: LoginType -> Maybe T.Text -> IO (Maybe (Key User))
getUserByLogin lg p
    | n <- normalizeLoginType lg
    , n /= lg
    = getUserByLogin n p
getUserByLogin lg password = do
    mbl <- readLogin lg
    case mbl of
        Just (lUserID -> uid) -> checkPassword uid
        _ ->
            case lg of
                LTEmail e -> getUserByLogin (LTUsername e) password
                LTFacebook e -> tryEmail e
                LTGoogle e -> tryEmail e
                _ -> return Nothing
    where tryEmail e = do
              mbu <- getUserByLogin (LTEmail e) password
              forM_ mbu $ \ u ->
                  tryAddAssociatedAccount Nothing u lg
              return mbu
          checkPassword uid = case password of
              Just p -> do
                  s0 <- readUserSettings uid
                  s <- case s0 of
                      Just s -> return s
                      Nothing -> do
                          (_, ub) <- tryReadLastBackup uid
                          return $ maybe (defaultUserSettings uid)
                              ubUserSettings ub
                  if checkPasswordHash p s then do
                      -- обновляем FeverApiKeys
                      -- setPassword' s uid p
                      return $ Just uid
                  else
                      return Nothing
              Nothing ->
                  return $ Just uid

getUserByLoginOrCreateAccount lt = do
    u <- getUserByLogin lt Nothing
    maybe loop return u
    where loop = do
              id <- T.take 8 . bst . base16_sha3_512 . B.pack
                  <$> (replicateM 1024 randomIO :: IO [Word8])
              u <- readUser id
              du <- readDeletedUser id
              if isJust u || isJust du then
                  loop
              else do
                  logTL ["Creating account ", id, " for ", showT lt]
                  modifyUser'_ id return
                  modifyUserSettings'_ id return
                  --  ^ чтобы addAssociatedAccount сработал
                  addAssociatedAccount Nothing id lt
                  return id

modifyBackups f u = do
    du <- readDeletedUser' u
    forM_ (duBackups du) $ \ t ->
        modifyUserBackup'_ (u, t) $ return . f

checkNoEmail = do
    us <- getAllUsers
    forM_ us $ \ u -> do
        aa <- ustAssociatedAccounts <$> readUserSettings' u
        when (not (any isLTEmail aa) &&
              any (\case { LTFacebook _ -> True; LTGoogle _ -> True; _ -> False }) aa) $
            print (u, aa)

------------------------------------------------------------------------------
-- Верификация email

updUserEmailVerificationTokens f u =
    modifyUserEmailVerificationTokens'_ u $ \ ts -> return $
        ts { uevtTokens = f (uevtTokens ts) }

newEmailVerificationToken ttl newEV email typ = modifyEmailVerification' email $ \ e0 -> do
    t <- getUrTime
    let rmExpired = filter $ \ (expire, _) -> expire > t
        e = e0 { evSignUpTokens = rmExpired $ evSignUpTokens e0
               , evResetTokens = rmExpired $ evResetTokens e0
               , evChangeEmailTokens = HM.map rmExpired $ evChangeEmailTokens e0
               }

    case newEV e of
        Just e' -> do
            rnd <- replicateM 1024 randomIO :: IO [Word8]

            let key = bst $ base16_md5 $ BL.toStrict $ encode (t, typ, rnd)
                expire = t `plusUrTime` ttl

            mergeWriteEmailVerificationToken $
                EmailVerificationToken
                { evtkToken            = key
                , evtkExpire           = expire
                , evtkVerified         = False
                , evtkEmail            = email
                , evtkVerificationType = typ
                , evtkReserved1        = 0
                , evtkReserved2        = 0
                , evtkReserved3        = 0
                , evtkReserved4        = 0
                }

            forM_ (tokenUser typ) $
                updUserEmailVerificationTokens (HM.insert key (t, email, typ))

            return (e' (expire, key), Just key)
        Nothing ->
            return (e, Nothing)

tokenUser = \ case
    EVTSignUp {} -> Nothing
    EVTResetPassword u -> Just u
    EVTChangeEmail u -> Just u
    EVTRestoreAccess u -> Just u

mailToken ttl newEV email evt subj text = do
    t <- newEmailVerificationToken ttl newEV email evt
    case t of
        Just token -> do
            signUpMail email subj (T.unlines $ text token)
            return True
        Nothing ->
            return False

ifNotClickable =
    [""
    ,"If the above link is not clickable, try copying and pasting it into the address bar of your web browser."]

sendSignUpEmail host email password = do
    ph <- newPasswordHash password
    let evt = EVTSignUp ph (feverApiKey email password)
        newEV e
            | length (evSignUpTokens e) < 5
            = Just $ \ tok -> e { evSignUpTokens = tok : evSignUpTokens e }
            | otherwise
            = Nothing
    mailToken (2*day) newEV email evt "Confirm your new account" $ \ token ->
        ["Welcome to BazQux Reader!"
        ,""
        ,"Click the following link to confirm and activate your new account:"
        ,"https://" <> host <> "/activate_account/" <> token
        ]
        <> ifNotClickable

sendPasswordResetEmail host user email = do
    mailToken (3*3600) newEV email (EVTResetPassword user)
        "Password reset" $ \ token ->
        ["Somebody asked to reset your password in BazQux Reader."
        ,""
        ,"If it was not you, you can safely ignore this email."
        ,""
        ,"Click the following link to choose a new password:"
        ,"https://" <> host <> "/password_reset/" <> token
        ]
        <> ifNotClickable
    where newEV e
              | length (evResetTokens e) < 5
              = Just $ \ tok -> e { evResetTokens = tok : evResetTokens e }
              | otherwise
              = Nothing

sendChangeEmailEmail host user email = do
    mailToken (3*3600) newEV email (EVTChangeEmail user)
        "Confirm your new email" $ \ token ->
        ["Somebody asked to change their BazQux Reader account email \
         \to your email."
        ,""
        ,"If it was not you, you can safely ignore this email."
        ,""
        ,"Click the following link to confirm your email and set it \
         \to your account:"
        ,"https://" <> host <> "/change_email/" <> token
        ]
        <> ifNotClickable
    where newEV e
              | ts <- HM.lookupDefault [] user $ evChangeEmailTokens e
              , length ts < 5
              = Just $ \ tok -> e { evChangeEmailTokens =
                  HM.insert user (tok:ts) $ evChangeEmailTokens e }
              | otherwise
              = Nothing

sendPasswordResetBackEmail host user email = do
    sendRestoreAccessEmail host user email
        "Your account password was changed"
        ["Somebody changed your password in BazQux Reader."]
        []

sendAssociatedAccountChange added descr host user email =
    sendRestoreAccessEmail host user email
        (if added then "New associated account was added"
         else "Associated account was removed")
        ["Somebody has "
         <> (if added then "added" else "removed")
         <> " following associated account "
         <> (if added then "to" else "from")
         <> " your BazQux Reader account:"
        ,""
        ,"  " <> descr
        ]
        []

sendUsernameChangeEmail host user email newUsername =
    sendRestoreAccessEmail host user email
        "Your username was changed"
        ["Somebody changed your username in BazQux Reader to “"
            <> newUsername <> "”."]
        []

sendUsernameRemovedEmail host user email un =
    sendRestoreAccessEmail host user email
        "Your username was removed"
        ["Somebody removed your username “" <> un
         <> "” from your BazQux Reader account."]
        []

sendEmailRemovedEmail host user email e =
    sendRestoreAccessEmail host user email
        "Your email was removed from your account"
        ["Somebody removed your email " <> e
         <> " from your BazQux Reader account."]
        []

sendChangeEmailBackEmail host user email newEmail =
    sendRestoreAccessEmail host user email
        "Your account email was changed"
        ["Somebody changed your account email in BazQux Reader to "
            <> newEmail <> "."]
        [""
        ,"If it was you and you don’t want to use this email address anymore, \
         \please, delete this email \
         \as it contains link to restore account email."]

sendRestoreAccessEmail host user email subj text text2 =
    mailToken week (Just . const) email (EVTRestoreAccess user)
        subj $ \ token ->
        text
        <>
        [""
        ,"If it was you, you can safely ignore this email."
        ]
        <>
        text2
        <>
        [""
        ,"If it was not you, change your email password first \
         \(it could be leaked). \
         \Then follow this link to restore access \
         \to your BazQux Reader account (it will reset password, \
         \restore account email back to " <> email <> " and remove username \
         \and associated accounts if any):"
        ,"https://" <> host <> "/restore_access/" <> token
        ]
        <>
        ifNotClickable
        <>
        [""
        ,"Reply to this email if you still can’t restore access \
         \to your account."]

verifyToken vtCheck tokenId = withLogger $ \ l -> do
    logLTL l ["Verifying token ", tokenId]
    mbt <- if tokenId == "" then return Nothing else
        readEmailVerificationToken tokenId
    t <- getUrTime
    case mbt of
        Just (EmailVerificationToken {..})
            | evtkVerified -> do
                logLT l "Token already verified"
                return Nothing
            | t > evtkExpire -> do
                logLT l "Token expired"
                return Nothing
            | otherwise -> do
                r <- vtCheck evtkEmail evtkVerificationType
                case r of
                    Nothing -> do
                        logLT l "Invalid token verification type"
                        return Nothing
                    Just u -> do
                        logLT l "Token verified"
                        modifyEmailVerification'_ evtkEmail $ \ ev -> return $
                            ev { evVerified = evVerified ev <> [u] }
                        clearEmailVerificationToken tokenId
                        return $ Just evtkEmail
        Nothing -> do
            logLT l "Token doesn’t exist"
            return Nothing

verifySignUpToken t = flip verifyToken t $ \ email -> \ case
    EVTSignUp {..} -> do
        let lt = LTEmail email
        uid <- getUserByLogin lt Nothing
        if isJust uid then do
            logTL ["verifySignUpToken: Account “", email, "” already exists."]
            return Nothing
        else do
            uid <- getUserByLoginOrCreateAccount lt
            setPasswordHash uid evtPasswordHash
            addAssociatedAccount Nothing uid (LTFeverApiKey evtFeverApiKey)
            ts <- evSignUpTokens <$> readEmailVerification' email
            mapM_ clearEmailVerificationToken (filter (/= t) $ map snd ts)
            return $ Just uid
    _ ->
        return Nothing

verifyPasswordResetToken host t newPassword = flip verifyToken t $ \ email -> \ case
    EVTResetPassword rUid -> do
        let lt = LTEmail email
        u <- getUserByLogin lt Nothing
        case u of
            Just uid
                | rUid /= uid ->
                    fail $ "This account is no longer linked with “"
                        <> T.unpack email
                        <> "”. Perhaps account email was changed \
                           \ or account was removed."
                | otherwise -> do
                    clearUserSessions uid
                    setPassword (Just host) uid newPassword
                    clearUserChangeEmailAndPasswordResetTokens t uid
                    userEvent uid "Password reset" ""
                    return $ Just uid
            Nothing ->
                fail $ "Can’t find account “" <> T.unpack email <> "”. Perhaps \
                    \account email was changed or account was removed."
    _ ->
        return Nothing

verifyChangeEmailToken host t = flip verifyToken t $ \ email -> \ case
    EVTChangeEmail {..} -> do
        clearUserSessions evtUser
        oldEmail <- userLTEmail evtUser
        ok <- setEmail (Just host) evtUser email
        when (not ok) $
            fail $ "verifyChangeEmailToken: email " <> T.unpack email
                <> " is already linked with another user. \
                   \Please, login to this account and chage its email \
                   \or delete it."
        clearUserChangeEmailAndPasswordResetTokens t evtUser
        return $ Just evtUser
    _ ->
        return Nothing

verifyRestoreAccessToken host t newPassword = flip verifyToken t $ \ email -> \ case
    EVTRestoreAccess {..} -> do
        clearUserSessions evtUser
        oldEmail <- userLTEmail evtUser
        ok <- setEmail Nothing evtUser email
        when (not ok) $
            fail $ "verifyRestoreAccessToken: email " <> T.unpack email
                <> " is already linked with another user. \
                   \Please, login to this account and chage its email \
                   \or delete it."
        setPassword Nothing evtUser newPassword
        s <- readUserSettings' evtUser
        let aa = ustAssociatedAccounts s
        mapM_ (removeAssociatedAccount Nothing evtUser)
            $ filter (not . isLTEmail) aa
        let aads = mapMaybe (associatedAccountDescription s) aa
            mailText = T.unlines $
                ["Access to your account was restored. \
                 \Following actions were done:"
                ,""
                ,"- Your password was reset."]
                <>
                (maybe [] (\ old ->
                    if old == email then [] else
                    ["", "- Your BazQux Reader account email was restored from "
                     <> old <> " to " <> email <> "."]) oldEmail)
                <>
                (if any isLTUsername aa
                 then ["", "- Your username was removed."]
                 else [])
                <>
                (if not (null aads) then
                    ["", "- Following associated accounts were removed:", ""]
                    <> map ("   - " <>) aads
                 else [])
                <>
                [" "]
            clear m = case HM.lookup t m of
                Just (time0, _, _) ->
                    [tok | (tok, (time, e, EVTRestoreAccess _)) <- HM.toList m
                    ,time > time0, e /= email]
                Nothing -> []
        clearUserEmailVerificationTokens clear evtUser
        clearUserChangeEmailAndPasswordResetTokens t evtUser
        signUpMail email "Access to your account was restored" mailText
        userEvent evtUser "Access restored"
            (fromMaybe "none" oldEmail <> " => " <> email <> "\n\n" <> mailText)
        return $ Just evtUser
    _ ->
        return Nothing

clearUserChangeEmailAndPasswordResetTokens t u =
    clearUserEmailVerificationTokens clear u
    where clear m =
              [tok | (tok, (_, _, typ)) <- HM.toList m
              ,tok /= t
              ,case typ of
                  EVTResetPassword _ -> True
                  EVTChangeEmail _ -> True
                  _ -> False]

clearUserEmailVerificationTokens f u = do
    ts <- readUserEmailVerificationTokens' u
    mapM_ clearEmailVerificationToken (f $ uevtTokens ts)

clearEmailVerificationToken tokenId = do
    modifyEmailVerificationToken'_ tokenId $ \ t -> do
        forM_ (tokenUser (evtkVerificationType t)) $
            updUserEmailVerificationTokens (HM.delete tokenId)
        return $ t { evtkVerified = True }
    deleteEmailVerificationToken $ defaultEmailVerificationToken tokenId

clearEmailVerificationToken' = clearEmailVerificationToken . evtkToken

userLTEmail u =
    userLTEmail' <$> readUserSettings' u
userLTEmail' s = listToMaybe [e | LTEmail e <- ustAssociatedAccounts s]

passwordResetEmail (T.strip -> t)
    | Just e <- validateEmail t = do
        u <- getUserByLogin (LTEmail t) Nothing
        return $ maybe (Left $ "Can’t find user with email “" <> e <> "”.")
            (\ uid -> Right (uid, e)) u
    | looksLikeOrderId t = do
        o <- tryDownloadFastSpringOrder t
        maybe
            (return $ Left $ "Can’t find order " <> t <> ".")
            (userPasswordResetEmail "order" . fsoReferrer)
            o
    | validUsername t = do
        u <- getUserByLogin (LTUsername t) Nothing
        maybe (return $ Left $ "Can’t find user with username “" <> t <> "”.")
            (userPasswordResetEmail "username") u
    | otherwise =
        return $ Left "Please, enter valid email, username or order ID."

userPasswordResetEmail what u = do
    us <- readUserSettings u
    (_, lb) <- tryReadLastBackup u
    case us <|> (ubUserSettings <$> lb) of
        Nothing ->
            return $ Left "Account linked with this order was deleted. Please, sign up for new account."
        Just s -> do
            let aa = ustAssociatedAccounts s
                please x = ". Please, sign in with " <> x <> "."
            return $ case [e | LTEmail e <- aa] of
                (e:_) -> Right (u, e)
                [] -> Left $ "Account linked with this " <> what
                    <> " has no email"
                    <> case mapMaybe (associatedAccountDescription s) aa of
                        [x] -> please x
                        (x:xs) -> please
                            $ x <> " (" <> T.intercalate ", " xs <> ")"
                        [] ->
                            "or other methods of login. "
                            <> "Please, mail support@bazqux.com "
                            <> "to restore access to your account."

associatedAccountDescription s = \ case
    LTGoogle e -> Just $ "Google account " <> e
    LTFacebook e ->
        Just $ "Facebook account linked with " <> e
    LTTwitter i -> Just $ "Twitter account "
        <> twitterAccountName i s
    LTOpenId u -> Just $ "OpenID account " <> u
    _ -> Nothing

maskEmail e
    | fc :. (T.dropWhile (/= '@')
            -> '@' :. fd :. (T.dropWhile (/= '.') -> '.' :. d)) <- e
    = T.pack [fc, '*', '@', fd, '*', '.'] <> d
    | otherwise = e

keyToLogin k
    | Just e <- T.stripPrefix "g+" k = LTGoogle e
    | Just e <- T.stripPrefix "f+" k = LTFacebook e
    | Just e <- T.stripPrefix "t+" k = LTTwitter e
    | Just e <- T.stripPrefix "o+" k = LTOpenId e
    | Just e <- T.stripPrefix "e+" k = LTEmail e
    | Just e <- T.stripPrefix "u+" k = LTUsername e
    | Just e <- T.stripPrefix "fak+" k = LTFeverApiKey e
    | otherwise = error $ "keyToLogin: bad key " <> show k

checkOrphanLogins = do
    ls <- riakBucketKeys "Login"
    forM_ ls $ \ (keyToLogin -> lt) -> do
        l <- readLogin lt
        case l of
            Nothing ->
                logTL ["Login not found", showT lt]
            Just l -> do
                let u = lUserID l
                us <- readUserSettings u
                (_, lb) <- tryReadLastBackup u
                case us <|> (ubUserSettings <$> lb) of
                    Nothing -> do
                        logTL ["User ", u, " not found for ", showT lt]
                        deleteLogin l
                    Just s | lt `notElem` ustAssociatedAccounts s -> do
                        logTL ["User ", u, " has no ", showT lt]
                        addAssociatedAccount Nothing u lt
                    _ ->
                        return ()
--                        logTL ["ok ", showT lt, " => ", u]
