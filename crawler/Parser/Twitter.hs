{-# LANGUAGE MultiWayIf, ViewPatterns, TupleSections, OverloadedStrings #-}
-- | Twitter
module Parser.Twitter
    ( customParsers
    , calculateTwitterRateLimitDelay, twitterErrors
    ) where

import Control.Monad
import Data.Maybe
import Data.Either
import Data.List
import Data.Ord
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson as JSON
import Data.Aeson
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Parser.Types
import Parser.Custom
import URL
import Lib.UrTime
import Lib.Regex
import Lib.ReadUtils
import Lib.Log
import Lib.StringConversion
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import Control.Applicative

customParsers :: CustomParsers
customParsers =
    mkCustomParsers ["twitter.com"]
    [ ( rt "mobile\\.twitter\\.com/(.+)", noScan, CPTags redirectTwitterMobile )
    , ( wt "twitter\\.com/([^/]*)/with_replies", noScan, CPTags redirectTwitterUser )
    , ( wt "twitter\\.com/([^/]*)/media", noScan, CPTags redirectTwitterUser )
    , ( wt "twitter\\.com/.+/status/[0-9]+", noScan, CPTags redirectTwitterStatus )
    , ( wt "twitter\\.com/search/realtime\\?q=", changeUrl $ transformTwitterComSearchUrl "recent", CPJSON handleTwitterSearch )
--     , ( rt "twitter\\.com/search\\?f=realtime&q=", changeUrl $ transformTwitterComSearchUrl "recent", CPJSON handleTwitterSearch )
    , ( wt "twitter\\.com/search\\?.*q=", changeUrl $ transformTwitterComSearchUrl "mixed", CPJSON handleTwitterSearch )
    , ( wt "twitter\\.com/hashtag/", changeUrl $ transformTwitterComHashtagUrl, CPJSON handleTwitterSearch )
    , ( wt "twitter\\.com/i/lists/([0-9]+)", changeUrl $ transformTwitterListIdUrl, CPJSON handleTwitter )
    , ( wt "twitter\\.com/([^/]*)/lists/", changeUrl $ transformTwitterListsUrl, CPJSON handleTwitter )
    , ( wt "twitter\\.com/([^/]+)/?$", changeUrl $ transformTwitterUrl, CPJSON handleTwitter )
    , ( wt "twitter\\.com/([^/]*)/likes", changeUrl $ transformTwitterLikesUrl, CPJSON $ handleTwitter' (<> " likes"))
    , ( wt "twitter\\.com/statuses/user_timeline/[0-9]+.rss$", changeUrl $ transformOldTwitterUrl, CPJSON handleTwitter )
    , ( rt "api\\.twitter\\.com/1/statuses/user_timeline\\.rss", changeUrl $ transformOldApiTwitterUrl, CPJSON handleTwitter )
    , ( rt "search\\.twitter\\.com/search\\.atom\\?", changeUrl $ transformSearchTwitterUrl, CPJSON handleTwitterSearch )
    ]

tweetMode = "?tweet_mode=extended&count=200"

twitterApiUserTimeline =
    "https://api.twitter.com/1.1/statuses/user_timeline.json" <> tweetMode
twitterApiFavoritesList =
    "https://api.twitter.com/1.1/favorites/list.json" <> tweetMode
twitterApiSearchTweets =
    "https://api.twitter.com/1.1/search/tweets.json" <> tweetMode
twitterApiListsStatuses =
    "https://api.twitter.com/1.1/lists/statuses.json" <> tweetMode

transformOldApiTwitterUrl =
    T.replace "https://api.twitter.com/1/statuses/user_timeline.rss?"
        (T.append twitterApiUserTimeline "&") .
    httpToHttps
transformTwitterUrl u
    | [[_, user]] <- regexGet "twitter.com/([^/?]+)" u =
        addExtraTwitterParameters u $ T.concat
        [twitterApiUserTimeline, "&screen_name=", encodeURIComponentT user]
    | otherwise = u
transformTwitterLikesUrl u
    | [[_, user]] <- regexGet "twitter.com/([^/?]+)" u =
        addExtraTwitterParameters u $ T.concat
        [twitterApiFavoritesList, "&screen_name=", encodeURIComponentT user]
    | otherwise = u
addExtraTwitterParameters u r
    | Right qs <- urlQueryStringUtf8Only u =
        T.concat $ r : concat
        [["&", n, "=", encodeURIComponentT v] | (n,v) <- concatMap mapP qs]
    | otherwise = r
    where mapP ((`elem` ["exclude_retweets", "no_retweets"]) -> True, "") =
              [("include_rts", "false")]
          mapP ("include_rts", p) =
              [("include_rts", if p == "" then "true" else p)]
          mapP _ = []
hasExcludeReplies u
    | Right qs <- urlQueryStringUtf8Only u =
       go False qs
    | otherwise = False
    where go e (((`elem` ["exclude_replies", "no_replies"]) -> True, p):ps) =
              go (p `elem` ["", "true", "t", "1"]) ps
          go e (_:ps) = go e ps
          go e [] = e

transformOldTwitterUrl u
    | [[_, user]] <- regexGet "twitter\\.com/statuses/user_timeline/([0-9]+).rss$" u =
        T.concat [twitterApiUserTimeline, "&user_id=", user]
-- на самом деле надо еще
-- "&exclude_replies=true&count=1000"
-- но они доступны только в xml/json
-- у какой-нить cocacola куча ответов (что с ними делать?),
-- но мало самих твитов
    | otherwise = u

transformSearchTwitterUrl =
--     T.replace "count=" "rpp=" .
--     T.replace "https://api.twitter.com/1.1/search/tweets.json" "http://search.twitter.com/search.json" .
--     T.replace "http://api.twitter.com/1.1/search/tweets.json" "http://search.twitter.com/search.json" .
    T.replace "https://search.twitter.com/search.atom" twitterApiSearchTweets .
    T.replace "?" "&" .
    T.replace "rpp=" "count=" .
    T.replace "http%3A" "https:".
    T.replace "https%3A" "https:".
    T.replace ":" "%3A" . -- почему-то с ним не хочет делать авторизацию
    T.replace "+" "%20" .
    T.replace "@" "%40" .
    httpToHttps
transformTwitterComSearchUrl rt u = fromMaybe u $ do
    [[_, decodeURIComponentT -> q]] <- return $ regexGet "q=([^&]+)" u
    return $ T.concat [ -- "http://search.twitter.com/search.json?q="
                        -- у старого API другой JSON
                        twitterApiSearchTweets, "&q="
                      , encodeURIComponentT (q <> " exclude:retweets")
                      , "&result_type="
                      , if any (`T.isInfixOf` u) ["f=videos"] then
                            "videos"
                        else if any (`T.isInfixOf` u) ["f=images"] then
                            "images"
                        else if any (`T.isInfixOf` u)
                               ["mode=realtime", "f=realtime", "f=tweets"] then
                            "recent"
                        else rt
                      ]
transformTwitterComHashtagUrl u = fromMaybe u $ do
    [[_, decodeURIComponentT -> t]] <- return $ regexGet "hashtag/([^\\?]+)" u
    return $ T.concat [ -- "http://search.twitter.com/search.json?q="
                        -- у старого API другой JSON
                        twitterApiSearchTweets, "&q="
                      , encodeURIComponentT ("#" <> t <> " exclude:retweets")
                      , "&result_type="
                      , if "f=realtime" `T.isInfixOf` u
                        then "recent" else "mixed"
                      ]
    -- В поиске нет опций убирания ответов/ретвитов
transformTwitterListIdUrl u = fromMaybe u $ do
    [[_, listId]] <- return $ regexGet "twitter.com/i/lists/([0-9]+)" u
    return $ addExtraTwitterParameters u $ T.concat
        [ twitterApiListsStatuses, "&list_id=", listId ]
transformTwitterListsUrl u = fromMaybe u $ do
    [[_, decodeURIComponentT -> owner, decodeURIComponentT -> list]] <-
        return $ regexGet "twitter.com/([^/]+)/lists/([^/&\\?]+)" u
    return $ addExtraTwitterParameters u $ T.concat
        [ twitterApiListsStatuses, "&slug="
        , encodeURIComponentT list
        , "&owner_screen_name=", encodeURIComponentT owner
        ]
    -- в lists/statuses API есть только include_rts, возможности убрать
    -- ответы нет (хотя, похоже, их в принципе в списках нет)

redirectTwitterStatus _ u _
    | [[_, r]] <- regexGet "(^https?://twitter\\.com/.+)/status/[0-9]+" u =
        PRRedirect r
    | otherwise = PRError "regexGet error???"

redirectTwitterMobile _ u _
    | [[_, _, r]] <- rg "mobile\\.twitter\\.com/(.+)" u =
        PRRedirect $ T.append "https://twitter.com/" r
    | otherwise = PRError "regexGet error???"

redirectTwitterUser _ u _
    | [[_, _, r]] <- rg "twitter\\.com/([^/]*)/" u =
        PRRedirect $ T.append "https://twitter.com/" r
    | otherwise = PRError "regexGet error???"

twitterErrors :: B.ByteString -> Either String [(Int, T.Text)]
twitterErrors = jsonParser t
    where t = withObject "root object" $
              (.: "errors") >=> withArray "errors list" (mapM err . V.toList)
          err = withObject "error" $ \ e -> do
              m <- (e .: "message") >>= withText "error message" return
              c <- (e .: "code") >>= withScientific "error code" return
              return (truncate c, m)

handleTwitter = handleTwitter' id

handleTwitter' mkName _ u json = fromMaybe (PRError "Can’t parse Twitter JSON") $ do
    l <- jsonArray json
    isThread <- mkIsTwitterThread l
    tweets <- mapM (tweet u isThread id) l
    return $ PRFeed u
                    (defaultFeedMsg
                     { fmSubject = mkName name
                     , fmLinks = link })
                    (catMaybes tweets)
    where name = maybe "twitter" (T.takeWhile (/= '?')) $
              do [[_, name]] <- return $ regexGet "screen_name=([^&]+)" u
                 return name
              <|>
              do [[_, name]] <- return $ regexGet "user_id=([^&]+)" u
                 return name
              <|>
              do [[_, name]] <- return $ regexGet "user_timeline/([0-9]+).rss" u
                 return name
              <|>
              do [[_, _]] <- return $ regexGet "twitter.com/i/lists/([0-9]+)" u
                 return "list"
              <|>
              do [[_, owner, list]] <- return $ regexGet "twitter.com/([^/]+)/lists/([^/\\?&]+)" u
                 return (list <> " @" <> owner)
              <|>
              do [[_, name]] <- return $ regexGet "twitter.com/([^/]+)" u
                 return name
          link = fromMaybe [] $
              do [[_, name]] <- return $ regexGet "screen_name=([^&]+)" u
                 return [(LKLink, "https://twitter.com/" <> name)]
              <|>
              do [[_, name]] <- return $ regexGet "user_timeline/([0-9]+).rss" u
                 return [(LKLink, "https://twitter.com/account/redirect_by_id?id=" <> name)]
              <|>
              do [[_, name]] <- return $ regexGet "user_id=([^&]+)" u
                 return [(LKLink, "https://twitter.com/account/redirect_by_id?id=" <> name)]
              <|>
              Just [(LKLink, u)]

handleTwitterSearch _ u json = fromMaybe (PRError "Can’t parse Twitter search JSON") $ do
    JSON.Object o <- return json
    l <- jsonArray =<< HM.lookup "statuses" o
    isThread <- mkIsTwitterThread l
    tweets <- mapM (tweet u isThread id) l
    return $ PRFeed u
                    (defaultFeedMsg
                     { fmSubject = name
                     , fmLinks = link })
                    (catMaybes tweets)
    where name = fromMaybe "Twitter Search" $
              do [[_, q]] <- return $ regexGet "q=([^&]+)" u
                 q <- lookup "q" $ parseQueryStringUtf8Only ("q=" <> q)
                 return $ q <> " - Twitter Search"
              <|>
              do [[_, t]] <- return $ regexGet "/hashtag/([^/\\?&]+)" u
                 return $ "#" <> decodeURIComponentT t
          link = fromMaybe [] $
              do [[_, q]] <- return $ regexGet "q=([^&]+)" u
                 return [(LKLink, "https://twitter.com/search?q=" <> q)]
              <|>
              do [[_, t]] <- return $ regexGet "/hashtag/([^/\\?&]+)" u
                 return [(LKLink, u)]

mkIsTwitterThread tweets = do
    replies <- fmap HM.fromList $ forM tweets $ \ t -> do
        JSON.Object d <- return t
        JSON.Object u <- HM.lookup "user" d
        status <- str "id_str" d
        user <- str "id_str" u
        return (status, (str "in_reply_to_status_id_str" d, user))
    -- с таким подходом мы можем потерять некоторые нити,
    -- если исходный твит был более 200 твитов назад,
    -- но другого способа отделить нити от ответов я не вижу
    let checkParent Nothing u = Just u
        checkParent p@(Just pu) u
            | pu /= u = Nothing
            | otherwise = p
        isThread pu i = case HM.lookup i replies of
            Nothing -> False
            Just (to, u)
                | pu'@(Just _) <- checkParent pu u ->
                    case to of
                        Nothing -> True
                        Just t -> isThread pu' t
                | otherwise -> False
    return $ isThread Nothing

tweet :: TURL -> (T.Text -> Bool) -> (T.Text -> T.Text) -> Value -> Maybe (Maybe FeedMsg)
tweet url isThread fixBody x = do
  JSON.Object d <- return x
  JSON.Object u <- HM.lookup "user" d
  guid <- str "id_str" d
  let retweeted_status = HM.lookup "retweeted_status" d
      exclude = hasExcludeReplies url && not (isJust retweeted_status) &&
          (isJust (str "in_reply_to_status_id_str" d) && not (isThread guid))
  protected <- bool "protected" u
  if protected || exclude then return Nothing else Just <$> do
    name <- str "name" u
    t <- str "full_text" d
    created_at <- str "created_at" d
    screen_name <- str "screen_name" u
    let link = T.concat ["https://twitter.com/", screen_name, "/status/", guid]
        addRetweeted b =
            T.concat
            [ "<p class=bqrWithRetweet>", b, "</p><p class=bqrRetweet>"
            , renderTagsT [TagOpen "a" [("href", T.append "https://twitter.com/" screen_name),("class", "bqrRetweetLink")]]
            , "<span class=bqrRetweetIcon></span>"
            , "<span class=bqrRetweetText>"
            , "Retweeted by <span class=bqrRetweetedByProfile>"
            , renderTagsT [TagText (if name /= "" then name else screen_name)]
            , "</span></span></a></p>"
            ]
    case retweeted_status of
        Just (tweet "" (const False) addRetweeted -> Just (Just fm)) ->
            return $ fm
                { fmGuid = link
                , fmPublishedTime = readRfc822 $ T.unpack created_at
                }
        _ -> do
            img <- str "profile_image_url_https" u
            JSON.Object e <- HM.lookup "entities" d
            JSON.Object ee <-
                HM.lookup "extended_entities" d <|>
                return (JSON.Object HM.empty)
            let mapEntities' :: HM.HashMap T.Text JSON.Value
                            -> T.Text -> (JSON.Object -> Maybe a) -> [a]
                mapEntities' e typ f =
                    case HM.lookup typ e of
                        Just (JSON.Array (V.toList -> ms)) -> catMaybes
                            [f m | JSON.Object m <- ms]
                        _ -> []
                mapEntities t f =
                    hashSetNub $ mapEntities' ee t f ++ mapEntities' e t f
                    -- первая картинка в extended_entities дублирует картинку
                    -- из entities, поэтому nub и extended_entities сначала
                (photos, enclosures) =
                    partitionEithers $ map snd $ hashSetNub' fst $
                    mapEntities "media" photoOrVideo
                photoOrVideo m = do
                    id_str <- str "id_str" m
                    u <- str "media_url_https" m
                    eu <- str "expanded_url" m
                    JSON.Object sizes <- HM.lookup "sizes" m
                    JSON.Object s <- HM.lookup "medium" sizes
                    JSON.Number (truncate -> w) <- HM.lookup "w" s
                    JSON.Number (truncate -> h) <- HM.lookup "h" s
                    let video = do
                          -- animated_gif тоже содержит видео
                          -- vine ничего, кроме ссылки, не содержит
                          JSON.Object vi <- HM.lookup "video_info" m
                          variants <- arr "variants" vi
                          vs <-
                              fmap (map snd . sortBy (comparing $ Down . fst)) $
                              forM variants $ \ v -> do
                                  JSON.Object v <- return v
                                  ct <- str "content_type" v
                                  url <- str "url" v
                                  br <- int "bitrate" v <|> Just 0
                                  -- у m3u8 нет bitrate
                                  return (br, (ct, url))
                          (ct,url) <-
                              find ((==) "video/mp4" . fst) vs <|>
                              find (T.isPrefixOf "video/" . fst) vs <|>
                              -- бывает m3u8 (содержит ссылки на видео в разных
                              -- разрешениях), webm и другие,
                              -- по-этому сначала пытаемся универсальный
                              -- mp4 найти или хотя бы video,
                              -- а не application/x-mpegURL
                              listToMaybe vs
                          let i = T.pack . show
                              duration = fromMaybe [] $ do
                                  JSON.Number (truncate -> d) <-
                                      HM.lookup "duration_millis" vi
                                  return [("duration", i $ d `div` 1000)]
                              loop = fromMaybe [] $ do
                                  "animated_gif" <- str "type" m
                                  return [("loop", "")]
                          return $ Right (url, [("url", url), ("type", ct)
                                               ,("poster", u)
                                               ,("width", i w)
                                               ,("height", i h)
                                               ] ++ duration ++ loop)
                        image =
                            return $ Left $ renderTagsT $
                            [TagOpen "p" []]
                            ++ aimgTags' [ ("width", T.pack $ show w)
                                         , ("height", T.pack $ show h)]
                                (if isJust $ HM.lookup "large" sizes then
                                     T.append u ":large"
                                 else
                                     u)
                                u ""
                            ++ [TagClose "p"]
                    fmap (id_str,) $ video <|> image
                -- ((start, end), new text)
                indices o = do
                    JSON.Array (V.toList ->
                                     [JSON.Number (truncate -> a),
                                      JSON.Number (truncate -> b)]) <- HM.lookup "indices" o
                    return (a :: Int, b :: Int)
                replace t o [] = [t]
                replace t o (((fromEnum -> a, fromEnum -> b), r) : rs) =
                    T.take (a-o) t
                    : (if ">pic.twitter.com" `T.isInfixOf` r then ""
                       else if ">twitter.com" `T.isInfixOf` r && quoted /= ""
                       then "" else r)
                      -- убираем ссылки на pic.twitter.com, т.к. сам Twitter
                      -- их тоже не показывает
                      -- а также ссылки на исходный твит
                    : replace (T.drop (b-o) t) b rs
                entities = sort $
                    (mapEntities "media" $ \ m -> do
                        u <- str "expanded_url" m <|> str "url" m
                        du <- str "display_url" m
                        i <- indices m
                        return (i, renderTagsT [TagOpen "a" [("href", u)],
                                                TagText du, TagClose "a"])) ++
                    (mapEntities "urls" $ \ m -> do
                        u <- str "expanded_url" m <|> str "url" m
                        du <- str "display_url" m <|> str "url" m <|> Just u
                        i <- indices m
                        return (i, renderTagsT
                                   [TagOpen "a" [("href", u)]
                                   ,TagText du, TagClose "a"])) ++
                    (mapEntities "user_mentions" $ \ m -> do
                        n <- str "screen_name" m
                        i <- indices m
                        let l = "https://twitter.com/" <> n
                        return (i, renderTagsT
                                     [TagOpen "a" [("href", l)],
                                      TagOpen "span" [("class", "bqrTwitterChar")],
                                      TagText "@", TagClose "span",
                                      TagText n, TagClose "a"])) ++
                    (mapEntities "hashtags" $ hashTag "#") ++
                    (mapEntities "symbols" $ hashTag "$")
                hashTag char m = do
                    t <- str "text" m
                    i <- indices m
                    let l = "https://twitter.com/search?q="
                            <> encodeURIComponentT char <> t
                    return (i, renderTagsT
                                 [TagOpen "a" [("href", l)],
                                  TagOpen "span" [("class", "bqrTwitterChar")],
                                  TagText char, TagClose "span",
                                  TagText t, TagClose "a"])
                embeds =
                    filter (/= "") $
                    mapEntities "urls" $ \ m -> do
                        u <- str "expanded_url" m <|> str "url" m
                        return $
                            if | Just i <- instagramPostId u ->
                                   embedInstagram u $
                                   renderTagsT $ aimgTags u
                                   (T.concat ["https://instagram.com/p/", i
                                             ,"/media/?size=l"]) ""
                               | rt "vine\\.co/v/[a-zA-Z0-9]+$" u ->
                                   renderTagsT
                                   [ TagClose "p"
                                   , TagOpen "iframe"
                                     [ ("src", T.append u "/embed/simple")
                                     , ("width", "600")
                                     , ("height", "600")
                                     , ("frameborder", "0")
                                     ]
                                   , TagClose "iframe"
                                   , TagOpen "p" [] ]
                               | Just videoId <- youtubeVideoId u ->
                                   embedYoutube videoId
                               | Just videoId <- vimeoVideoId u ->
                                   embedVimeo videoId
                               | otherwise ->
                                   ""
                text =
                    editTagSoup newlinesToBr $
                    fixBody (T.concat $ replace t 0 entities ++ embeds)
                (quoted, qEnclosures) =
                    fromMaybe ("", []) $
                    insertMsg =<< join . tweet "" (const False) id
                    =<< HM.lookup "quoted_status" d
            return $
                defaultFeedMsg
                { fmGuid = link
                , fmBody =
                    T.replace "<p></p>" "" $
                    T.concat ["<p>", text, "</p>",
                              if null embeds then
                                  T.concat photos
                                  -- иногда картинка из instagram/youtube
                                  -- дублируется в твите
                              else
                                  ""
                             , quoted]
                , fmAuthor = if name == "" then screen_name else
                                 T.concat [name, " (", screen_name, ")"]
                , fmPublishedTime = readRfc822 $ T.unpack created_at
                , fmEnclosures = qEnclosures ++ enclosures
                , fmLinks =
                    [ (LKAuthor, T.concat ["https://twitter.com/", screen_name])
                    , (LKAuthorPic, img)
                    , (LKLink, link)]
                }

calculateTwitterRateLimitDelay headers
    | Just date <- readRfc822 . B.unpack =<< lookup "date" headers
    , Just l <- int "x-rate-limit-limit"
    , Just r <- int "x-rate-limit-remaining"
    , Just resetSec <- int "x-rate-limit-reset"
    , reset <- UrTime resetSec 0
    , dt <- reset `diffUrTime` date
    , rd <- fromIntegral r
    , ld <- fromIntegral l
    = Just ( T.concat [showT r, "/", showT l, " in ", T.pack (showSecs dt)]
           , if rd > 0.8 * ld then
                 0.0
                 -- в начале качаем на максимальной скорости,
                 -- это нужно для поиска и пользовательских токенов,
                 -- которые не используются полностью, чтобы они не вносили
                 -- ненужную задержку
             else
                 min 10.0 $ max 0.0 $ dt / (max 1.0 (rd - ld * 0.1)))
                 -- оставляем 10% от общего лимита как запас
    | otherwise = Nothing
    where int n = tryReadUnsignedInt . bst =<< lookup n headers
-- https://developer.twitter.com/en/docs/basics/rate-limits
