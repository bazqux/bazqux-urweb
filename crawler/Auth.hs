{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards, TupleSections,
             StandaloneDeriving #-}
module Auth
    ( loginGetForwardUrl, loginCallback
    , parseQueryStringUtf8Only
    , signTwitter, signTwitterWithAccessToken
    , readerDownloaderSettings, withReaderDL
    , postUrlEncoded, postUrlEncodedRaw
    ) where

import Lib.Log
import Control.Monad
import Control.Applicative
import qualified Control.Exception as E
import Data.List
import qualified Data.Aeson as JSON
import Lib.Json
import qualified Data.HashMap.Lazy as HM
import qualified Data.ByteString.Char8 as B
import Generated.DataTypes
import Web.Authenticate.OAuth
import Web.Authenticate.OpenId (getForwardUrl, authenticateClaimed, OpenIdResponse(..), identifier)
import Data.String
import qualified Network.HTTP.Client as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import URL
import System.IO.Unsafe
import qualified Network.HTTP.Types as N
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Conduit.Downloader
import qualified Data.Default as C
import Lib.StringConversion
import Config
import Parser.Facebook (parseFacebookError)

withReaderDL f = f readerDL

readerDL = unsafePerformIO $
    withDownloaderSettings readerDownloaderSettings return
{-# NOINLINE readerDL #-}

-- appAccessToken = withReaderDL $ \ d -> do
--     DROK
--        concat [ fbApiPath, "/oauth/access_token?client_id="
--               , fbAppId, "&client_secret=", fbSecret
--               , "&grant_type=client_credentials" ]
--     rsp <- C.httpLbs req m
--     return $ BL.unpack $ BL.drop (BL.length "access_token=") $ C.responseBody rsp

twitterOAuth cb =
    def
    { oauthServerName      = error "oauthServerName"
    , oauthRequestUri      = "https://api.twitter.com/oauth/request_token"
    , oauthAccessTokenUri  = "https://api.twitter.com/oauth/access_token"
    , oauthAuthorizeUri    = "https://api.twitter.com/oauth/authorize"
    , oauthSignatureMethod = HMACSHA1
    , oauthConsumerKey     = twitterOAuthConsumerKey
    , oauthConsumerSecret  = twitterOAuthConsumerSecret
    , oauthCallback        = Just cb
    , oauthRealm           = Nothing
    }

prefixUrl host u = T.concat ["https://", host, u]

fbTokenCallback host cb q = do
    case lookup "code" q of
        Just code -> withReaderDL $ \ d -> do
            at <- download d
               (T.unpack $ T.concat
                [ fbApiPath, "/oauth/access_token?"
                , "client_id=", fbAppId
                , "&redirect_uri="
                , encodeURIComponentT (prefixUrl host cb)
                , "&client_secret=", fbSecret
                , "&code=", code ]) Nothing []
            let fbAt r = do
                  JSON.Object root <- decodeJson r
                  JSON.String i <- HM.lookup "access_token" root
                  return i
            q <- case at of
                 DROK r _ -> return r
                 DRError e ->
                     fail $ "Can’t get access_token: " ++ e
                 _ -> fail "Can’t get access_token"
            shortLivedToken <-
                maybe (fail "Can’t find access_token") return $ fbAt q
            lat <- download d
               (T.unpack $ T.concat
                [ fbApiPath, "/oauth/access_token?"
                , "grant_type=fb_exchange_token"
                , "&client_id=", fbAppId
                , "&client_secret=", fbSecret
                , "&fb_exchange_token=", shortLivedToken]) Nothing []
            lq <- case lat of
                 DROK r _ -> return r
                 DRError e ->
                     fail $ "Can’t get long-lived access_token: " ++ e
                 _ -> fail "Can’t get long-lived access_token"
            longLivedToken <-
                maybe (fail "Can’t find long-lived access_token") return $ fbAt lq
--             logS $ "Short-lived token: " ++ T.unpack shortLivedToken
--             logS $ "Long-lived token: " ++ T.unpack longLivedToken
            (me, meRdr) <- rawDownload return d (T.unpack fbApiPath ++ "/me?fields=email,id&access_token="
                   ++ T.unpack longLivedToken) Nothing []
--                  протестить fb-посты/группы/комменты/techcrunch
--             logT $ rspT rsp
            let fbProfile r = do
                  JSON.Object root <- decodeJson r
                  JSON.String i <- HM.lookup "id" root
--                   JSON.String n <- HM.lookup "name" root
--                   JSON.String u <- HM.lookup "username" root
--                  JSON.String l <- HM.lookup "link" root
                  JSON.String e <- HM.lookup "email" root
                      <|>
                      fmap JSON.String (lookup i facebookIdsToEmail)
                      <|>
                      return (JSON.String "")
                  return (LTFacebook e, LATFacebook longLivedToken,
                          Just $ "http://www.facebook.com/profile.php?id=" <> i)
--                          , Just (T.concat [u, " (", n, ") ", e]))
            case me of
                 DROK r _
                     | Just (LTFacebook "", _, _) <- fbProfile r -> do
                         logT $ bst r
                         fail "Can’t get email. Have you removed the email permission? It’s required for identification inside the reader."
                     | Just p <- fbProfile r ->
                         return p
                     | otherwise -> do
                         logT $ bst r
                         fail "Error getting facebook profile id"
                 DRError e -> do
                     maybe (return ()) (logT . bst . showRdr) meRdr
                     fail $ "Can’t get profile id: " ++
                         case fmap (parseFacebookError . rdrBody) meRdr of
                             Just (Right fbe) -> T.unpack fbe
                             _ -> e
                 _ ->
                     fail "Can’t get profile id"
        _ | Just "access_denied" <- lookup "error" q
            -> fail "Can’t login: access denied"
          | otherwise
            -> fail "Can’t login"

showRdr (RawDownloadResult {..}) =
    B.unlines $
    ["===== Response headers ====="] ++
    [B.concat [CI.original h, ": ", v] | (h,v) <- rdrHeaders] ++
    ["===== Response body ========", rdrBody]

loginGetForwardUrl :: ExternalLoginType -> T.Text -> ExternalLoginAction -> TURL -> IO TURL
loginGetForwardUrl elt host ela u = case elt of
    Google -> return $ T.concat
        [ "https://accounts.google.com/o/oauth2/auth"
        , "?scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email"
        , "&client_id=", googleOAuthClientId
        , "&response_type=code"
        , "&redirect_uri=", encodeURIComponentT (prefixUrl host u)
        , "&state=", s
        ]
    OpenId ou -> openIdForwardUrlG (T.unpack ou) host [] uWithState
    Facebook -> return $ T.concat
        [ "https://www.facebook.com/", fbApiVersion, "/dialog/oauth"
        , "?scope=email&auth_type=rerequest&display=page"
        , "&client_id=", fbAppId
        , "&response_type=code"
        , "&redirect_uri=", encodeURIComponentT (prefixUrl host u)
        , "&state=", s
        ]
    Twitter -> E.handle handleTwitter $ withManager $ \ m -> do
        let o = twitterOAuth $ T.encodeUtf8 $ prefixUrl host uWithState
        rt <- getTemporaryCredential o m
--         logS $ show rt
        return $ T.pack $ authorizeUrl o rt
    where uWithState = u <> "?state=" <> s
          s = encodeURIComponentT $ showT ela
          handleTwitter :: E.SomeException -> IO a
          handleTwitter = fail . show

loginCallback :: ExternalLoginType -> T.Text -> TURL -> T.Text -> IO (LoginType, LoginAccessToken, ExternalLoginAction, Maybe T.Text)
loginCallback elt host cb qss = addLA <$> case elt of
    Google -> googleCallback host (T.unpack cb) qs
    OpenId _ -> do
        oir <- withManager $ authenticateClaimed qs
--         print (i, ps)
        return (LTOpenId $ identifier $ oirOpLocal oir, LATNone, Nothing)
    Facebook -> fbTokenCallback host cb qs
    Twitter -> withManager $ \ m -> do
        let o = twitterOAuth $ T.encodeUtf8 $ prefixUrl host cb
            c = Credential [(tbs p, tbs v) | (p,v) <- qs]
        at <- getAccessToken o c m
--         logS $ show at
--         ^ содержит oauth_token, oauth_token_secret, user_id, screen_name
        maybe (fail "Can’t get profile id") return
            (do uid <- lookup "user_id" $ unCredential at
                sn <- lookup "screen_name" $ unCredential at
                return ( LTTwitter (bst uid)
                       , LATTwitter [(bst n, bst v) | (n, v) <- unCredential at]
                       , Just $ T.decodeUtf8 sn ))
    where qs0 = parseQueryStringUtf8Only qss
          qs = filter ((/= "state") . fst) qs0
          addLA (lt, lat, who) = (lt, lat, externalLoginAction qs0, who)

-- "https://twitter.com/intent/user?user_id="

loginManager = unsafePerformIO $ C.newManager ((dsManagerSettings C.def))-- { C.managerResponseTimeout = Just 19000000 })
-- чтобы уложиться в наш таймаут
-- вместо timeout вылезает C.FailedConnectionException, что менее понятно,
-- чем timeout
{-# NOINLINE loginManager #-}
withManager :: (C.Manager -> IO a) -> IO a
withManager act = withReaderDL $ const $ act loginManager
                  --  ^ для инициализации OpenSSL

openIdForwardUrlG :: String -> T.Text -> [(T.Text, T.Text)] -> TURL -> IO TURL
openIdForwardUrlG endpoint host a u =
      withManager $ getForwardUrl (fromString endpoint)
               (prefixUrl host u)
               --  ^ для OAuth должно соответствовать Consumer
               (Just (T.append "https://" host)) -- необязательно, можно и Nothing
         $ [ ( "openid.ns.ui"
             , "http://specs.openid.net/extensions/ui/1.0" ) ] ++ a

externalLoginAction :: [(T.Text, T.Text)] -> ExternalLoginAction
externalLoginAction qs = case lookup "state" qs of
    Nothing -> error "Can’t get “state” parameter to extract login action"
    Just a
        | [(r, "")] <- reads $ T.unpack a -> r
        | otherwise -> error "Can’t parse login action"

deriving instance Read ExternalLoginAction

googleCallback :: T.Text -> URL -> [(T.Text, T.Text)] -> IO (LoginType, LoginAccessToken, Maybe T.Text)
googleCallback host cb =
    googleCallback' True "user info" host cb
    "https://www.googleapis.com/oauth2/v1/userinfo?" $ \ _ ui ->
        case decodeJson $ BL.toStrict ui of
            Just (JSON.Object o)
                | Just (JSON.String e) <- HM.lookup "email" o ->
                    (LTGoogle e, LATNone, Nothing)
            _ -> error $ "Can’t get email:\n" ++ BL.unpack ui

googleCallback' :: Bool -> String -> T.Text -> URL -> URL -> (T.Text -> BL.ByteString -> a) -> [(T.Text, T.Text)] -> IO a
googleCallback' firstTime what host cb url f q = withReaderDL $ \ d -> do
    case lookup "error" q of
        Just "access_denied" ->
            fail "Access denied"
        Just e ->
            fail $ T.unpack e
        _ -> return ()
    logS $ show q
    let c = T.encodeUtf8 $ fromMaybe "no code?" $ lookup "code" q
        body = B.concat  ["code=", c
                         , "&client_id=", googleOAuthClientId
                         , "&client_secret=", googleOAuthClientSecret
                         , "&redirect_uri=https%3A%2F%2F"
                         , tbs host
                         , B.pack $ encodeURIComponent cb
                         , "&grant_type=authorization_code" ]
    dr <- postUrlEncoded d "https://accounts.google.com/o/oauth2/token" Nothing
        body
    case dr of
        DROK r _ -> do
            at <- case decodeJson r of
                Just (JSON.Object o)
                    | Just (JSON.String at) <- HM.lookup "access_token" o ->
                        return at
                _ -> error $ "Can’t get access_token:\n" ++ T.unpack (bst r)
            when ("import" `isInfixOf` cb) $
                logS $ "Import access token: " ++ T.unpack at
            dr <- download d (url ++ "access_token=" ++ T.unpack at) Nothing []
            case dr of
                DROK r _ ->
                    return $ f at $ BL.fromStrict r
                DRError e
                    | firstTime -> googleCallback' False what host cb url f q
                    | otherwise -> fail $ "Can’t get " ++ what ++ ":\n" ++ e
                _ -> fail $ "Can’t get " ++ what
        DRError e
            | firstTime -> googleCallback' False what host cb url f q
            | otherwise -> fail $ "Can’t get access_token:\n" ++ e
        _ -> fail $ "Can’t get access_token"

signTwitterWithAccessToken c r = do
    r' <- signOAuth (twitterOAuth "")
        (Credential
            [(tbs n, tbs v) | (n, v) <- c
            ,n == "oauth_token" || n == "oauth_token_secret"])
        (r { C.host = "api.twitter.com" }) -- учитываем, что у нас могут быть ipUrl
    return $ r' { C.host = C.host r
                , C.requestHeaders =
                    ("Accept-Encoding", "") : C.requestHeaders r'
                    -- https://dev.twitter.com/issues/927
                }

signTwitter r =
    r { C.requestHeaders =
          ("Accept-Encoding", "")
          -- https://dev.twitter.com/issues/927
          : ("Authorization", "Bearer " <> tbs twitterBearer)
          : C.requestHeaders r
      }

testTwitterUrl u = withManager $ \ m -> do
    req <- C.parseRequest u
    let sreq0 = signTwitter req
        sreq = sreq0 { C.requestHeaders =
                           [("Accept", "*/*")
                           ,("User-Agent", "curl/7.25.0 (x86_64-apple-darwin11.3.0) libcurl/7.25.0 OpenSSL/1.0.1e zlib/1.2.7 libidn/1.22")]
                           ++
                           map (\(h,v) -> (h, T.encodeUtf8 $ T.replace "," ", " $ T.pack $ B.unpack v))
                           (C.requestHeaders sreq0)
                     }
               -- не помогает
    forM_ (C.requestHeaders sreq) $ \ (h,v) ->
        putStrLn $ B.unpack (CI.original h) ++ ": " ++ B.unpack v
    rsp <- C.httpLbs sreq m
    mapM_ print $ C.responseHeaders rsp
    BL.putStrLn $ C.responseBody rsp
-- testTwitterUrl "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=BazQuxReader"

postUrlEncoded d u ha body = fmap fst $ postUrlEncodedRaw return d u ha body

postUrlEncodedRaw f d u ha body =
    rawDownload
        (\ req -> f $
            req { C.requestHeaders =
                  [( "Content-Type"
                   , "application/x-www-form-urlencoded" )]
                , C.method = N.methodPost
                , C.requestBody = C.RequestBodyBS body
                })
        d u ha []
