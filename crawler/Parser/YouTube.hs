{-# LANGUAGE MultiWayIf, ViewPatterns, OverloadedStrings #-}
-- | YouTube
module Parser.YouTube
    ( customParsers
    ) where

import Control.Monad
import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson as JSON
import Lib.Json
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Parser.Types
import Parser.Custom
import Lib.UrTime
import Lib.StringConversion
import Lib.Regex
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import Control.Applicative
import URL
import Config (googleApiKey)

customParsers =
    mkCustomParsers ["youtube.com"]
    [ ( rt "youtube\\.com/channel/",
        changeUrl transformYoutubeChannel, CPJSON handleYoutubeChannel )
    , ( rt "youtube\\.com/c/",
        noChange, CPTags handleYoutubeCustomUrlChannel )
    , ( rt "youtube\\.com/user/",
        changeUrl transformYoutubeUser, CPJSON handleYoutubeChannel )
    , ( isJust . youtubeUserAndPlaylistV2,
        changeUrl transformYoutubeUser, CPJSON handleYoutubeChannel )
    , ( isJust . youtubeVideosXmlToChannel,
        changeUrl (fromJust . youtubeVideosXmlToChannel), CPJSON handleYoutubeChannel )
    , ( isJust . youtubeMostPopularUrl,
        changeUrl (fromJust . youtubeMostPopularUrl), CPJSON handleYoutubeMostPopular )
    , ( isJust . youtubeSearchUrlAndSubject,
        changeUrl (fst . fromJust . youtubeSearchUrlAndSubject), CPJSON handleYoutubeSearch )
    , ( isJust . youtubePlaylistId,
        youtubePlaylistParser . fromJust . youtubePlaylistId, CPJSON handleYoutubePlaylist )
    ]

ytBaseUrl = "https://www.googleapis.com/youtube/v3"

-- +id%2C+snippet%2C+status%2C+
-- https://developers.google.com/youtube/v3/docs/playlistItems
-- contentDetails у playlistItems содержит только videoId и videoPublishedAt.
-- Для получения duration нужен дополнительный запрос video details
youtubePlaylistParser p _ =
    return ( playlistItemsUrl 20 Nothing p
           , Just
             (T.concat [ytBaseUrl, "/playlists?part=snippet&id="
                       , p, "&", googleApiKey]
             ,\ a b -> B.concat ["[", a, ",", b, "]"])
           , Nothing)
-- неприятный момент -- некоторые плейлисты отсортированы от самых ранних
-- и выдаются именно в таком порядке, т.е. надо paging использовать
-- Но uploads отсортированы в обратном порядке и нормально работают

playlistItemsUrl maxResults nextPageToken playlistId =
    T.concat [ytBaseUrl, "/playlistItems?part=contentDetails&maxResults="
             , showT maxResults
             , maybe "" ("&pageToken=" <>) nextPageToken
             , "&playlistId=", playlistId, "&", googleApiKey]

youtubePlaylistId u
    | [[_,id]] <- regexGet "youtube\\.com/playlist\\?list=([^&=?]+)" u =
        Just id
    | regexTest "^https?://(www.|gdata.)?youtube.com/(xml/)?feeds/videos.xml" u
    , Right qs <- urlQueryStringUtf8Only u
    , Just id <- lookup "playlist_id" qs =
        Just id
    | [[_, _, _, pl]] <-
        regexGet "^https?://(www.|gdata.)?youtube.com/feeds/(api/|base/)?playlists/([^/?&#]+)" u
    = Just pl
    | otherwise = Nothing

-- testYoutube = withLogger $ \ l -> logTime l "testYoutube" $ do
--     -- parseUrlT "http://www.youtube.com/user/billhiltonbiz"
--     -- parseUrlT "http://www.youtube.com/results?search_query=jazz+piano"
--     -- parseUrlT "http://gdata.youtube.com/feeds/api/standardfeeds/US/most_viewed"
-- --    parseUrlT "http://gdata.youtube.com/feeds/api/users/tailcalled/newsubscriptionvideos"
-- --    parseUrlT "http://gdata.youtube.com/feeds/api/users/TheSuicideBanana/newsubscriptionvideos"
--     parseUrlT "http://gdata.youtube.com/feeds/api/users/vshabanov/newsubscriptionvideos?start-index=1&max-results=25&client=ytapi-youtube-user&v=2"
--     -- почему-то subscriptions list выдает HTTP 403 subscriptionForbidden
--     -- https://www.youtube.com/account_privacy
-- --    parseUrlT "http://gdata.youtube.com/feeds/api/users/UCXlFBEpZS7wvjwN-p_K4bHQ/newsubscriptionvideos"
-- --    parseUrlT "http://gdata.youtube.com/feeds/base/users/zI51nrQntG9h00_TKk_1vA/newsubscriptionvideos" -- так не находит

handleYoutubeItems guidFormat xs result = do
    JSON.Array (V.toList -> is) <- HM.lookup "items" xs
    let videoId = withObj $ \ i ->
           do cd <-
                  obj "contentDetails" i  -- playlist
                  <|>
                  obj "id" i  -- search results
              str "videoId" cd
           <|>
           str "id" i -- most popular
        vids = mapMaybe videoId is
    return $ youtubeVideosBatch guidFormat result vids

youtubeVideosBatch guidFormat result [] = result []
youtubeVideosBatch guidFormat result xs =
    PRAdditionalDownload "YouTube video details"
    [T.concat
     [ ytBaseUrl, "/videos?part=id%2CcontentDetails%2Csnippet&id="
     , encodeURIComponentT $ T.intercalate "," ids
     , "&maxResults=50&", googleApiKey ]
    | ids <- groupByN 50 xs ]
    (combineDownloadsBS $ fromMaybe (PRError "Invalid JSON") .
     handleYoutubeVideos guidFormat result)

handleYoutubeVideos guidFormat result dat =
    fmap (result . take 100 . sortBy (comparing $ Down . fmPublishedTime) . concat) $
    forM dat $ \ d -> do
    JSON.Object xs <- decodeJson d
    is <- arr "items" xs
    let mkGuid
            | Just f <- guidFormat = \ g -> T.replace "VIDEO_ID" g f
            | otherwise = id
        feedMsg js = do
--           error $ show js
           JSON.Object i <- return js
--            JSON.Object status <- HM.lookup "status" i
--            JSON.String privacyStatus <- HM.lookup "privacyStatus" status
--            guard (privacyStatus /= "private")
           snippet <- obj "snippet" i
           videoId <- str "id" i
           title <- str "title" snippet
           description <- str "description" snippet
           publishedAt <- str "publishedAt" snippet
           cd <- obj "contentDetails" i
           d <- str "duration" cd
           duration <- readIso8601Duration $ T.unpack d
--            statistics <- obj "statistics" i
--            views <- str "viewCount" statistics
--            likes <- str "likeCount" statistics <|> Just "0"
--            dislikes <- str "dislikeCount" statistics <|> Just "0"
           return $
               defaultFeedMsg
               { fmGuid = mkGuid videoId
               , fmAuthor = fromMaybe "" $ str "channelTitle" snippet
               , fmPublishedTime = readRfc3339 $ T.unpack publishedAt
               , fmBody =
                   T.concat
                   [embedYoutube videoId
                   ,videoDuration duration
                   ,htmlfyYoutubeDescription
                    (addHashTags 1
                     $ T.append "https://www.youtube.com/hashtag/" . T.toLower)
                    description]
               , fmSubject = title
               , fmLinks =
                   [(LKLink, T.concat ["https://www.youtube.com/watch?v="
                                      , videoId])]
                   ++ fromMaybe [] (do
                        c <- str "channelId" snippet
                        return [( LKAuthor
                                , "http://www.youtube.com/channel/" <> c)
--                                ,( LKAuthorPic
--                                 , "https://i.ytimg.com/i/" <> T.drop 2 c <> "/1.jpg")
-- Не везде такая картинка работает
-- http://www.daniel-mitchell.com/blog/display-youtube-user-avatar-profile-picture-by-username/
                               ])
               }
    return $ mapMaybe feedMsg is

handleYoutubeMostPopular _ u j = fromMaybe (PRError "Invalid JSON") $ do
    JSON.Object xs <- return j
    handleYoutubeItems (Just $ youtubeGuidFormat u) xs $
        PRFeed u
               (defaultFeedMsg
                { fmSubject = "Most Popular"
                , fmLinks = [(LKLink, "https://www.youtube.com")] })
handleYoutubeSearch _ u j = fromMaybe (PRError "Invalid JSON") $ do
    JSON.Object xs <- return j
    (_,(subject,url)) <- youtubeSearchUrlAndSubject u
    handleYoutubeItems (Just $ youtubeGuidFormat u) xs $
        PRFeed u
               (defaultFeedMsg
                { fmSubject = subject
                , fmLinks = [(LKLink, url)] })
handleYoutubePlaylist _ u j = fromMaybe (PRError "Invalid JSON") $ do
    JSON.Array (V.toList -> [JSON.Object xs, JSON.Object playlist]) <- return j
    JSON.Array (V.toList -> (JSON.Object i:_)) <- HM.lookup "items" playlist
    JSON.String playlistId <- HM.lookup "id" i
    JSON.Object snippet <- HM.lookup "snippet" i
    JSON.String title <- HM.lookup "title" snippet
    let guidFormat = either (const Nothing) (lookup "bq_guid_format")
            $ urlQueryStringUtf8Only u
        result =
            PRFeed u
            (defaultFeedMsg
             { fmSubject = fromMaybe title $ T.stripPrefix "Uploads from " title
             , fmLinks =
                 [(LKLink, "https://www.youtube.com/playlist?list=" <> playlistId) ] })
        batch = youtubeVideosBatch guidFormat result . map snd
        sortIs = sortBy (comparing $ Down . fst)
        loop n xs acc
            | n < 2
            , Just npt <- str "nextPageToken" xs =
                PRAdditionalDownload
                ("Next page of unsorted playlist (#" <> showT n <> ")")
                [playlistItemsUrl 50 (Just npt) playlistId]
                (combineDownloadsBS $ \ [d] -> fromMaybe (PRError "Invalid JSON") $ do
                    JSON.Object xs' <- decodeJson d
                    is' <- playlistItems xs'
                    return $ loop (n+1) xs' (is' <> acc))
            | otherwise =
                batch $ take 20 $ sortIs acc
    is <- playlistItems xs
    -- Бывает, что старое видео публикуется не сразу и свежее видео на сайта
    -- оказывается далеко не на первом мести внутри плейлиста YouTube.
    -- Для таких неотсортированных плейлистов вытаскиваем еще 100 видео,
    -- сортируем и обрабатываем последние 20.
    return $ if sortIs is == is then batch is else loop 0 xs is

playlistItems xs = do
    JSON.Array (V.toList -> is) <- HM.lookup "items" xs
    let timeVid = withObj $ \ i -> do
           cd <- obj "contentDetails" i
           vid <- str "videoId" cd
           time <-str "videoPublishedAt" cd
           return (readRfc3339 $ T.unpack time, vid)
    return $ mapMaybe timeVid is


youtubeChannelUrl u =
    T.concat [ytBaseUrl, "/channels?part=id%2C+contentDetails&id="
             , u, "&", googleApiKey]
youtubeUserUrl u
    | regexTest "^UC[A-Za-z0-9_\\-]{22}$" u = youtubeChannelUrl u
    | otherwise =
        T.concat [ytBaseUrl, "/channels?part=id%2C+contentDetails&forUsername="
                 , u, "&", googleApiKey]

transformYoutubeChannel u
    | [[_,id]] <- regexGet "youtube\\.com/channel/([^&=?/]+)" u =
        youtubeChannelUrl id
    | otherwise = u

transformYoutubeUser u
    | [[_,id]] <- regexGet "youtube\\.com/user/([^&=?/]+)" u = youtubeUserUrl id
    | Just (id,_) <- youtubeUserAndPlaylistV2 u = youtubeUserUrl id
    | otherwise = u

-- TODO: плохо это все с плейлистом -- надо объединять transform и матчинг
-- в customParsers
youtubeUserAndPlaylistV2 u
    | [[_, _, _, user, playlist]] <-
        regexGet "^https?://(www.|gdata.)?youtube.com/feeds/(api/|base/)?users/([^/?&#]+)/([^/?&#]+)" u
--    , playlist /= "newsubscriptionvideos" -- пока позволяем старым работать
    = Just (user, playlist)
    | regexTest "^https?://(www.|gdata.)?youtube.com/feeds/(api/|base/)?videos" u
    , Right qs <- urlQueryStringUtf8Only u
    , Just user <- lookup "author" qs
    = Just (user, "uploads")
    | otherwise = Nothing

urlHasV2 u
    | Right qs <- urlQueryStringUtf8Only u
    , Just v <- lookup "v" qs = v == "2"
    | otherwise = False

youtubeVideosXmlToChannel u
    | regexTest "^https?://(www.|gdata.)?youtube.com/(xml/)?feeds/videos.xml" u
    , Right qs <- urlQueryStringUtf8Only u =
        if | Just ch <- lookup "channel_id" qs -> Just (youtubeChannelUrl ch)
           | Just us <- lookup "user" qs -> Just (youtubeUserUrl us)
           | otherwise -> Nothing
    | otherwise = Nothing

-- https://gdata.youtube.com/feeds/base/users/etownradioshow/uploads?q=finale&orderby=published  -- с поиском, но как ограничить поиск автором я не знаю, так что нафиг

-- https://www.googleapis.com/youtube/v3/search?part=snippet&type=video&maxResults=20&key={YOUR_API_KEY}
-- q=query
-- relatedToVideoId=
-- regionCode
-- order= по умолчанию relevance
--   date
--   rating  -- выводит какую-то фигню
--   viewCount
--
-- Для related to, но order=date мало чего дает -- видео в разнобой,
-- но в RSS-фидах они тоже в разнобой
-- https://www.googleapis.com/youtube/v3/search?part=snippet&maxResults=20&order=date&relatedToVideoId=6_GXiuV-mlo&type=video&key={YOUR_API_KEY}

-- https://www.googleapis.com/youtube/v3/videos?part=snippet&chart=mostPopular&regionCode=RU&key={YOUR_API_KEY}
-- топовые по России. На youtube это просто плейлист, и на него можно подписаться
-- в общем все standardfeeds можно к ним привести

youtubeMostPopularUrl u
    | r@[[_, _, _, country]] <-
        regexGet "^https?://(www.|gdata.)?youtube.com/feeds/(api/|base/)standardfeeds/([A-Z][A-Z])?" u
    = Just $ T.concat
      [ ytBaseUrl, "/videos?part=id&chart=mostPopular&maxResults=20"
      , if | country /= "" ->
               "&regionCode=" <> country
           | Right (lookup "lang" -> Just lang) <- urlQueryStringUtf8Only u ->
               "&regionCode=" <> T.toUpper lang
           | otherwise ->
               ""
      , "&", googleApiKey ]
    | otherwise = Nothing

youtubeSearchUrlAndSubject u
    | [[_,_,_,q]] <- regexGet "^https?://(www.|gdata.)?youtube.com/feeds/(api/|base/)?videos/-/([^?&]+)" u
    = r (T.unwords $ map decodeURIComponentT $ T.split (== '/') q) ""
      searchSubj searchUrl
    | [[_,_,_,vid]] <- regexGet "^https?://(www.|gdata.)?youtube.com/feeds/(api/|base/)?videos/([^/]+)/related" u
    = r "" ("&relatedToVideoId=" <> vid)
      (const "Related videos")
      -- должно быть Videos related to 'Название видео', но что-то лень копаться
      (const $
       "https://www.youtube.com/results?search=related&search_query=&v="
       <> vid)
      --  ^ такого URL на youtube уже нет
    | regexTest "^https?://(www.|gdata.)?youtube.com/feeds/(api/|base/)?videos" u
    , Right qs <- urlQueryStringUtf8Only u
    , Just q <- lookup "q" qs
    = r q "" searchSubj searchUrl
    | regexTest "^https?://(www.)?youtube.com/results" u
    , Right qs <- urlQueryStringUtf8Only u
    , Just q <- lookup "search_query" qs
    = r q filters searchSubj
      ((<> maybe "" ("&filters="<>) (lookup "filters" qs)) . searchUrl)
    | otherwise = Nothing
    where r q add subj url = Just (T.concat [ytBaseUrl, "/search?part=id&maxResults=20&order=date&type=video&q=", encodeURIComponentT q, add, "&", googleApiKey], (subj q, url q))
          filters
              | Right qs <- urlQueryStringUtf8Only u
              , Just fs <- lookup "filters" qs
              = T.concat $ map (fOpt . T.toLower) $ T.split (== ',') fs
              | otherwise = ""
          fOpt "short" = "&videoDuration=short"
          fOpt "medium" = "&videoDuration=medium"
          fOpt "long" = "&videoDuration=long"
          fOpt "hd" = "&videoDefinition=hd"
          fOpt "4k" = "&videoDefinition=hd" -- в API вроде пока нет 4k
          fOpt "3d" = "&videoDimension=3d"
          fOpt "cc" = "&videoCaption=closedCaption"
          fOpt "creativecommons" = "&videoLicense=creativeCommon"
          fOpt "movie" = "&videoType=movie"
          fOpt "show" = "&videoType=episode"
          fOpt "live" = "&eventType=live"
          fOpt _ = ""
          searchSubj q = q <> " - YouTube"
--          searchSubj q = "Videos matching: " <> q
          searchUrl q = "https://www.youtube.com/results?search_query=" <> encodeURIComponentT q <> sortByDate
          sortByDate = "&search_sort=video_date_uploaded"

handleYoutubeCustomUrlChannel _ _ t = go t
    where go [] = PRError "Can’t find Youtube channel"
          go (TagOpen "meta" as : ts)
              | Just "channelId" <- lookup "itemprop" as
              , Just cid <- lookup "content" as =
                  PRRedirect $ "https://www.youtube.com/channel/" <> cid
          go (_ : ts) = go ts

handleYoutubeChannel t u j
    | Just (_, p) <- youtubeUserAndPlaylistV2 u =
--         if p == "newsubscriptionvideos" then
--             PRError "newsubscriptionvideos are no longer supported by Youtube.\nStar this issue https://code.google.com/p/gdata-issues/issues/detail?id=3946"
--         else
            handleYoutubeChannel' (Just $ youtubeGuidFormat u)
                p t u j
    | otherwise =
        handleYoutubeChannel'
            (fmap (const "yt:video:VIDEO_ID") $ youtubeVideosXmlToChannel u)
            "uploads" t u j
-- стоит собирать все video id и делать дополнительный запрос по ним
-- DRDependentDownload -- скачивание, которое не redirect-ит при подписке
-- так можно узнать video duration, а в snippet показываются channelTitle
-- с пробелами и правильное время
-- http://stackoverflow.com/questions/15596753/youtube-api-v3-how-to-get-video-durations
-- По идее, так же можно и newsubscriptionvideos обработать
-- Ограничений на youtube API практически нет, можно параллельно качать
youtubeGuidFormat u =
    if urlHasV2 u then
        "tag:youtube.com,2008:video:VIDEO_ID"
    else if ".com/feeds/base/" `T.isInfixOf` u then
        "http://gdata.youtube.com/feeds/base/videos/VIDEO_ID"
    else if ".com/feeds/api/" `T.isInfixOf` u then
        "http://gdata.youtube.com/feeds/api/videos/VIDEO_ID"
    else
        "http://gdata.youtube.com/feeds/videos/VIDEO_ID"

handleYoutubeChannel' guidFormat playlist _ u j = fromMaybe (PRError "Can’t find Youtube channel") $ do
    JSON.Object o <- return j
    (JSON.Object i:_) <- arr "items" o
    cd <- obj "contentDetails" i
    rp <- obj "relatedPlaylists" cd
    (do guard (playlist == "newsubscriptionvideos")
        ch <- str "id" i
        return $ PRAdditionalDownload "YouTube subscriptions list"
                   [youtubeSubscriptionsListUrl ch Nothing]
                   (combineDownloadsBS $ handleYoutubeSubscriptionsList guidFormat ch [])
     <|>
     do u <- str playlist rp
        return $ PRRedirect ("https://www.youtube.com/playlist?list=" <> u <>
                             maybe "" (("&bq_guid_format="<>) . encodeURIComponentT)
                                   guidFormat)
     <|>
        return (PRError $ T.concat ["Can’t find “", playlist, "” playlist"]))

youtubeSubscriptionsListUrl channelId pageToken =
    T.concat [ ytBaseUrl, "/subscriptions?part=snippet&channelId="
             , channelId, "&maxResults=50&", googleApiKey
             , maybe "" ("&pageToken="<>) pageToken ]
-- Код отсюда
-- https://github.com/ali1234/ytsubs/blob/master/ytsubs.py
handleYoutubeSubscriptionsList guidFormat ch acc dat = fromMaybe (PRError "Invalid user subscriptions JSON") $ do
    JSON.Object xs <- decodeJson $ B.concat dat
    is <- arr "items" xs
    let chans = mapMaybe chan is ++ acc
        chan = withObj $ str "channelId" <=< obj "resourceId" <=< obj "snippet"
        plUrl cs =
            T.concat
            [ ytBaseUrl, "/channels?part=contentDetails&id="
            , encodeURIComponentT $ T.intercalate "," cs
            , "&maxResults=50&", googleApiKey ]
    return $ case str "nextPageToken" xs of
        Just p ->
            PRAdditionalDownload "YouTube subscriptions list (next page)"
            [youtubeSubscriptionsListUrl ch (Just p)]
            (combineDownloadsBS $
             handleYoutubeSubscriptionsList guidFormat ch chans)
        Nothing ->
            PRAdditionalDownload "YouTube subscriptions playlist IDs"
            (map plUrl $ groupByN 50 $ hashSetNub chans)
            (combineDownloadsBS $
             handleYoutubeSubscriptionsPlaylistsList guidFormat)
handleYoutubeSubscriptionsPlaylistsList guidFormat dat = fromMaybe (PRError "Invalid user subscriptions playlists JSON") $ do
    let pls d = do
           JSON.Object xs <- decodeJson d
           mapMaybe pl `fmap` arr "items" xs
        pl = withObj $
             str "uploads" <=< obj "relatedPlaylists" <=< obj "contentDetails"
        playlists = hashSetNub $ concat $ mapMaybe pls dat
        pliUrl p =
            T.concat
            [ ytBaseUrl
            , "/playlistItems?part=contentDetails&maxResults=20&playlistId="
            , p, "&", googleApiKey ]
            -- maxResults 5->20 дает общее время 13->19, что не намного больше,
            -- зато гораздо меньше вероятность что-либо пропустить,
            -- а если поставить семафор на 50, а не 20, то опять те же 13
    return $
        PRAdditionalDownload "YouTube playlists"
        (map pliUrl playlists)
        (CombineDownloads $
         handleYoutubeSubscriptionsPlaylistsItems guidFormat)
handleYoutubeSubscriptionsPlaylistsItems guidFormat dat = fromMaybe (PRError "Invalid user subscriptions playlist items JSON") $ do
    let vids = either (const $ return []) $ \ d -> do
            -- у каналов без видео бывают плейлисты uploads, которые выдают
            -- HTTP 404 Not found при попытке их выкачать
            JSON.Object xs <- decodeJson d
            mapMaybe vid `fmap` arr "items" xs
        vid = withObj $ str "videoId" <=< obj "contentDetails"
        videos = hashSetNub $ concat $ mapMaybe vids dat
        feed = PRFeed "https://www.youtube.com/"
               (defaultFeedMsg
                { fmSubject = "New Subscription Videos"
                , fmLinks = [(LKLink, "http://www.youtube.com/feed/subscriptions")] })
    return $ youtubeVideosBatch guidFormat feed videos

-- Префикс
-- (www.|gdata.|)youtube.com/feeds/(api/|base/)?

-- 4 вида GUID-ов
-- GUID yt:video:VIDEO_ID  -- videos.xml
-- GUID tag:youtube.com,2008:video:VIDEO_ID    -- v=2
-- GUID http://gdata.youtube.com/feeds/base/videos/VIDEO_ID -- без v=2 с base
-- GUID http://gdata.youtube.com/feeds/api/videos/VIDEO_ID  -- без v=2 c api
-- GUID http://gdata.youtube.com/feeds/playlists/A930398A6117E70C/<длинная абракадабра ведет на URL фида>

-- если USER_NAME начинается с UC, имеет 24 символа в длину и [A-Za-z0-9_\-]
-- то это channel

-- users/USER_NAME/uploads  (/favorites|/newsubscriptionvideos)
-- favorites - playlist, может быть, может не быть

-- playlists/…?

-- videos/-/Search/Term?…
-- videos/VIDEO_ID/related  (не используются /responses|/ratings|/complaints)
-- videos?…q=query
-- videos?…author=USER_NAME

-- standardfeeds/COUNTRY_CODE/TYPE
-- standardfeeds/TYPE?…lang=COUNTRY_CODE_locase…time=TIME
-- TYPE most_viewed, most_popular, top_rated, recently_featured
-- TIME today, this_week
-- разницы между most_viewed и API v3 chart=mostPopular
-- (плейлист самое популярное) я не нашел
-- В API v3 дате не задать, ну и ладно

-- http://www.youtube.com/feeds/base/users/Vsauce/uploads?alt=rss&v=2&orderby=published&client=ytapi-youtube-profile
-- http://youtube.com/feeds/base/users/asus/uploads?alt=rss&v=2&orderby=published&client=ytapi-youtube-profile
--  уже HTTP 404, но раньше работало

-- ПОКА РАБОТАЕТ:

-- http://gdata.youtube.com/feeds/api/playlists/032BA3BCABEC4C75
-- http://gdata.youtube.com/feeds/base/playlists/689D6EE903ED5CB6?alt=rss
-- все-таки есть playlist-ы
-- Только guid-ы какие-то странные -- очень длинные
-- http://gdata.youtube.com/feeds/api/playlists/032BA3BCABEC4C75/PLH3GPrl7glkNoYmSdnXnv5Jgh7koT4lYD

-- http://gdata.youtube.com/feeds/api/videos/dN5CHD035Xc/related
-- http://gdata.youtube.com/feeds/base/videos/3QCWjW-AUbQ/related?client=ytapi-youtube-watch&v=2
-- http://gdata.youtube.com/feeds/base/users/sjefen6/favorites?orderby=published
-- GUID http://gdata.youtube.com/feeds/base/videos/VIDEO_ID
--  (http/https, api/base не влияет)

-- НЕ РАБОТАЕТ
-- (после device support у некоторых фидов уже появились новые видео)


-- http://gdata.youtube.com/feeds/api/users/UC6Dh_cRf5juNtRHy_06bPhQ/uploads
-- http://gdata.youtube.com/feeds/base/videos?author=UCD8TbGQvNQ_A34pyjh2Seaw
-- --> https://www.youtube.com/feeds/videos.xml?channel_id=CHANNEL_ID

-- http://gdata.youtube.com/feeds/api/users/USERNAME/uploads…
-- http://gdata.youtube.com/feeds/base/users/0420to/uploads…
-- http://gdata.youtube.com/feeds/base/videos?alt=rss&orderby=published&author=1BYRRER
-- http://gdata.youtube.com/feeds/users/TheQwais/uploads
-- --> https://www.youtube.com/feeds/videos.xml?user=USERNAME
-- GUID yt:video:VIDEO_ID

-- НЕ ЗНАЮ АНАЛОГОВ

-- http://gdata.youtube.com/feeds/base/users/1tomdingman/newsubscriptionvideos

-- ЧЕРЕЗ API
-- http://gdata.youtube.com/feeds/base/videos/-/Alasdair/Roberts?v=2&orderby=published&client=ytapi-youtube-rss-redirect&alt=rss
-- GUID  tag:youtube.com,2008:video:UKY3scPIMd8  (alt=rss не влияет)
-- http://gdata.youtube.com/feeds/base/videos/-/bonnie/prince/billy?v=2&alt=rss&client=ytapi-youtube-rss-redirect&orderby=published
-- тоже не работает, только вот что это? поиск? -- да, только странный какой-то
-- иногда попадаются посты только с одним словом или вообще без них

-- http://gdata.youtube.com/feeds/api/standardfeeds/most_viewed …
-- http://gdata.youtube.com/feeds/api/standardfeeds/US/most_viewed …
-- http://gdata.youtube.com/feeds/api/videos?alt=rss&q=Alexis%20Bledel&orderby=published
-- http://gdata.youtube.com/feeds/base/standardfeeds/CZ/most_viewed?client=ytapi-youtube-browse&alt=rss&time=today
-- http://gdata.youtube.com/feeds/base/standardfeeds/most_popular?client=ytapi-youtube-browse&alt=rss&time=today&lang=fi
-- http://gdata.youtube.com/feeds/base/standardfeeds/top_rated?client=ytapi-youtube-browse&alt=rss
-- http://gdata.youtube.com/feeds/base/videos?alt=rss&client=ytapi-youtube-rss-redirect&orderby=published&q=connected+home+%7C+smart+home&v=2
-- http://gdata.youtube.com/feeds/base/videos?q=chicago+education&client=ytapi-youtube-search&v=2
-- GUID http://gdata.youtube.com/feeds/api/videos/VIDEO_ID
-- даже с alt=rss

-- uploads?orderby=published
-- uploads?orderby=updated
-- ?alt=rss&amp;v=2&amp;orderby=published&amp;client=ytapi-youtube-profile
-- ?alt=rss&v=2&orderby=published&client=ytapi-youtube-profile
-- ?orderby=updated&client=ytapi-youtube-rss-redirect&alt=rss&v=2
-- много вариантов, надо честно запоминать, что был redirect и обновлять
-- исходные фиды при push
