{-# LANGUAGE MultiWayIf, ViewPatterns, OverloadedStrings, TupleSections #-}
module Parser.Facebook
    ( customParsers
    , facebookCommentsPrefix
    , parseFacebookError, isFacebookOAuthException
    , parseFacebookUsageRdr
    ) where

import qualified Data.Text as T
import Parser.Types
import Parser.Custom
import Lib.StringConversion
import Lib.ReadUtils
import Lib.UrTime
import Lib.Regex
import Config
import Data.List
import Data.Ord
import Data.Maybe
import Control.Monad
import Control.Applicative
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import Network.HTTP.Conduit.Downloader
import Data.Aeson
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Lib.Json
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Network.URL as U
import qualified Data.Map as Map
import URL

-- http://developers.facebook.com/docs/reference/plugins/comments/

customParsers :: CustomParsers
customParsers =
    mkCustomParsers ["facebook.com"]
    [ ( (facebookCommentsPrefix `T.isPrefixOf`), changeUrl transformCommentsRedirectUrl
      , CPJSON handleFacebookCommentsRedirect)
    , ( rt "facebook\\.com/?([?#].*)?$", noScan, CPTags facebookPageAddressRequired )
    , ( rt "facebook\\.com/[0-9]+$", changeUrl transformUrl, CPJSON handleFacebook )
--    , ( rt "facebook\\.com/events/[0-9]+/?$", post transformEventUrl, CPJSON handleFacebook )
-- для них больше не работает get, а feed и posts возвращают пустые данные
    , ( rt "facebook\\.com/groups/[0-9]+([/#?].*|$)",
        noScan, CPTags facebookGroupsAreNotSupported)
        -- post transformGroupUrl, CPJSON handleFacebook )
    , ( rt "facebook\\.com/groups/[A-Za-z0-9]+",
        noScan, CPTags facebookGroupsAreNotSupported)
        -- facebookGroupAliasesNotSupported)
    , ( rt "facebook\\.com/login[./?#]?", noScan, CPTags facebookLoginRequred )
    , ( rt "facebook\\.com/[0-9]+/?$", noScan, CPTags redirectWWW )
    , ( rt "facebook\\.com/[0-9]+/posts", noScan, CPTags redirectWWW )
    , ( rt "facebook\\.com/profile\\.php\\?id=[^/]+/?", changeUrl transformProfileUrl,
        CPJSON handleFacebookId )
    , ( rt "facebook\\.com/feeds/page\\.php.*id=\\+?[0-9]+", noScan,
        CPTags redirectFeed )
    , ( rt "facebook\\.com/pages/[^/]+/[0-9]+/?", noScan, CPTags redirectPageUrl )
    , ( rt "facebook\\.com/[0-9]+_[0-9]+/?$", noScan, CPTags redirectComments )
    , ( rt "facebook\\.com/[^/]+-[0-9]+(/?$|/[?#])", noScan, CPTags redirectPageIdUrl )
    , ( rt "facebook\\.com/[^/]+/?$", changeUrl transformNonIdUrl, CPJSON handleFacebookId )
    , ( rt "facebook\\.com/[^/]+/[?#]", changeUrl transformNonIdUrl, CPJSON handleFacebookId ) --  может быть ?ref=… или #… главное, что слеш один
    , ( rt "graph\\.facebook\\.com/[0-9_]+/comments\\?after=",
        noScan, CPTags $ \ _ _ _ -> PRError "Facebook comments disabled" )
--        changeUrl (fbAddVersion . (<> ("&" <> fbCommentsOpts))), CPJSON handleFacebookComments )
    , ( rt "graph\\.facebook\\.com/[0-9_]+/comments$",
        noScan, CPTags $ \ _ _ _ -> PRError "Facebook comments disabled" )
--        changeUrl (fbAddVersion . (<> ("?" <> fbCommentsOpts))), CPJSON handleFacebookComments )
    ]

urlUserId u
    | [[_, userId]] <- regexGet "facebook\\.com/([0-9]+)" u = Just userId
    | otherwise = Nothing

transformUrl u
    | Just userId <- urlUserId u =
        T.concat [ fbApiPath, "/", userId, "/posts"
                 , "?access_token=", fbSystemUserAccessToken, "&", feedParams ]
    | otherwise = u
fbProfileApiPath userId =
    T.concat [ fbApiPath, "/", userId, "?access_token=", fbSystemUserAccessToken]
transformNonIdUrl u
    | [[_, userId]] <- regexGet "facebook\\.com/([^/?#]+)/?" u =
        T.concat [ fbApiPath, "/", userId
                 , "?access_token=", fbSystemUserAccessToken ]
    | otherwise = u
transformProfileUrl u
    | [[_, userId]] <- regexGet "facebook\\.com/profile.php\\?id=([^/?#]+)/?" u =
        fbProfileApiPath userId
    | otherwise = u
redirectWWW _ u _
    | [[_, userId]] <- regexGet "facebook\\.com/([^/#]+)/?" u =
        PRRedirect $ T.concat ["https://www.facebook.com/", userId]
    | otherwise = PRError "regexGet error???"
redirectComments _ u _
    | [[_, postId]] <- regexGet "facebook\\.com/([0-9]+_[0-9]+)" u =
        PRRedirect $ T.concat ["https://graph.facebook.com/", postId, "/comments"]
    | otherwise = PRError "regexGet error???"
redirectPageUrl _ u _
    | [[_, userId]] <- regexGet "facebook\\.com/pages/[^/?#]+/([0-9]+)/?" u =
        PRRedirect $ T.concat ["https://www.facebook.com/", userId]
    | otherwise = PRError "regexGet error???"
redirectPageIdUrl _ u _
    | [[_, userId, _]] <- regexGet "facebook\\.com/[^/]+-([0-9]+)(/?$|/[?#])" u =
        PRRedirect $ T.concat ["https://www.facebook.com/", userId]
    | otherwise = PRError "regexGet error???"
redirectFeed _ u _
    | [[_, user]] <- regexGet "id=\\+?([0-9]+)" u =
        PRRedirect $ T.concat ["https://www.facebook.com/", user]
    | otherwise = PRError "regexGet error???"
facebookLoginRequred _ _ _ = PRError "Login required"
facebookGroupsAreNotSupported _ _ _ =
    PRError "Facebook groups aren’t supported"
facebookPageAddressRequired _ _ _ =
    PRError $ T.unlines
    [ "No Facebook page address specified."
    , "Please, subscribe to public or community pages with addresses like facebook.com/SomePage"
    , "Reading your home timeline is not supported, sorry."
    ]

--     как получить id группы по alias-у непонятно
--     fql больше не работает (с v2.1)
--     search выводит все группы с совпадающим названием, а не alias-ом
--     (да и все равно, ему нужен user access token, а не app)
--     а
--     <meta property="al:android:url" content="fb://group/435101343321779" />
--     <meta property="al:ios:url" content="fb://group/?id=435101343321779" />
--     прочесть нельзя, т.к. требуется логин на страничку

fbAddVersion u
    | T.isPrefixOf fb u && not (regexTest fbv u) =
        T.concat [fbApiPath, "/", T.drop (T.length fb) u]
    | otherwise = u
    where fb  = "https://graph.facebook.com/"
          fbv = "https://graph.facebook.com/v[0-9]\\.[0-9]"

------------------------------------------------------------------------------
-- FacebookComments

--testFB = parseUrlT "https://www.facebook.com/beatsbydrerussia" -- vshabanov1"
         -- "https://graph.facebook.com/comments/?ids=http://example.com/"
--         "https://graph.facebook.com/389691382139/comments?offset=900"

facebookCommentsPrefix = "https://graph.facebook.com/comments/?ids="
facebookIdUrlPrefix =
    T.concat [fbApiPath, "/?access_token=", fbAppAccessToken, "&id="]

fbCommentsOptsNoAT = "fields=parent%7Bid%7D,id,from,message,created_time,attachment&filter=stream&limit=100" -- почему-то с access_token от нового приложения "BazQux Crawler" не работает запрос ?ids, но работает со старым access_token или вообще без него
-- Если со старым добавить v2.0 тоже работает, а v2.1 уже нет.
-- Т.е. со временем перестанет работать
-- При этом комментарии на публичных страницах (не ids) пашут
fbCommentsOpts = fbCommentsOptsNoAT <> "&access_token=" <> fbAppAccessToken

transformCommentsRedirectUrl u =
    facebookIdUrlPrefix `T.append` T.drop (T.length facebookCommentsPrefix) u

handleFacebookCommentsRedirect _ u json =
    fromMaybe (PRError "Can’t get Facebook URL id") $ do
        i <- str "id" =<< obj "og_object" =<< jsonObj json
        return $ PRRedirect $
            T.concat ["https://graph.facebook.com/", i, "/comments"]

handleFacebookComments _ u json = do
    let toplevelData comments = do
          dat <- arr "data" comments
          let switch = maybe NoSwitch (Switch True) $ do
                  JSON.Object paging <- HM.lookup "paging" comments
                  JSON.String next <- HM.lookup "next" paging
                  u <- U.importURL $ T.unpack next
                  after <- lookup "after" $ U.url_params u
                  [[_,oid]] <- return $ regexGet "/([^/]+)/comments" next
                  return $ T.concat [ "https://graph.facebook.com/", oid
                                    , "/comments?after="
                                    , T.pack $ encodeURIComponent after ]
          c <- mapM msg dat
          return $ PRParsedHtml Nothing switch (catMaybes c) []
        msg o = do
          JSON.Object m <- return o
          fmap (str "id" =<< obj "parent" m,) <$> facebookMsg (const True) m
        x = do
          case () of
--               _ | JSON.Object root <- json
--                 , Just (JSON.Object o) <-
--                     HM.lookup (T.drop (T.length facebookCommentsPrefix) u) root
--                 , Just (JSON.Object comments) <- HM.lookup "comments" o
--                  -> toplevelData comments
--               _ | JSON.Object root <- json
--                 , Just (JSON.Object o) <-
--                     HM.lookup (T.drop (T.length facebookCommentsPrefix) u) root
--                 , Just _ <- HM.lookup "data" o
--                  -> toplevelData o
              _ | JSON.Object root <- json
                 -> toplevelData root
                 -- у следующих страниц уже нет url->comments->data,
                 -- а сразу идет data
              _ | JSON.Array (V.toList -> [JSON.Object root]) <- json
                 --  ^ бывает и массив [{"data":[]}]
                 -> toplevelData root
              _ -> fail ""
    case x of
        Nothing -> PRError "Can’t parse facebook comments"
        Just r -> r

data MergedTag
    = Single TURL T.Text
    | Multiple [T.Text]

-- помечает message/story ссылками из тегов
fbMarkTags defLink name m = do
    message <- str name m
    tags <- arr (name <> "_tags") m <|> return []
    let marks =
            sortBy (comparing fst) $
            concatMap mergedTagMarks $ Map.toList $
            foldl' mergeTag Map.empty $
            mapMaybe mark tags
        mergeTag m (k, (l,n)) = Map.alter (Just . ins) k m
            where ins Nothing = Single l n
                  ins (Just (Single _ s))   = Multiple [n, s]
                  ins (Just (Multiple acc)) = Multiple $ n : acc
        mergedTagMarks ((o,len), t) =
            [(o, renderTagsT [TagOpen "a" (attrs t)]), (o+len, "</a>")]
            where attrs (Single l _) = [("href", l)]
                  attrs (Multiple t) =
                      [("href", defLink)
                      ,("title", T.intercalate "\n" $ reverse t)]
        -- если много тегов на одно и то же место, заменяем их на линк
        -- поста, т.к. это теги на "friends with … and *10 other people*"
        mark js = do
            t <- jsonObj js
            id <- str "id" t
            name <- str "name" t <|> return ""
            offset <- int "offset" t
            length <- int "length" t
            guard (length > 0)
            let link = T.concat ["https://www.facebook.com/", id]
            return ((offset, length), (link, name))
--            return [(offset, link), (offset + length, "</a>")]
        go prev t [] = [t]
        go prev t ((offs, markup) : ms) =
            hd : markup : go offs tl ms
            where (hd,tl) = T.splitAt (offs - prev) t
    return $ T.concat $ go 0 message marks

fbPictureLink id = T.concat ["https://graph.facebook.com/", id, "/picture?type=large"]
fbPostLink id = T.concat [ "https://www.facebook.com/"
                         , T.replace "_" "/posts/" id ]
fbProfileLink id =
    "https://www.facebook.com/" `T.append` id

jsIdName o = do
    JSON.String id <- HM.lookup "id" o
    JSON.String name <- HM.lookup "name" o
    return (id, name)

facebookMsg checkFrom m = do
    guid <- s "id"
    time <- s "created_time"
    let (fromId, author, links) =
            case HM.lookup "from" m of
                Just (JSON.Object o)
                    | Just (id, name) <- jsIdName o ->
                        (id,
                         name,
                         [(LKAuthor, fbProfileLink id)
                         ,(LKAuthorPic, fbPictureLink id)
                         ])
                _ -> ("", "", [])
        attachments =
            T.concat <$> (mapM attachment =<< arr "data" =<< obj "attachments" m)
        attachment a = do
            a <- jsonObj a
            url <- str "unshimmed_url" a
            title <- str "title" a <|> return ""
            descr <-
                if str "description" a `elem` [s "message", s "status"] then
                    return ""
                else
                    fbMarkTags postLink "description" a <|> return ""
            mt <- str "media_type" a
            let p' f "" = ""
                p' f x  = "<p>" <> f x <> "</p>"
                p = p' id
                td x = x
                    <> p' (renderLink' [("class", "bqrShareLink")] url) title
                    <> p descr
                photo = td . p <$> mediaImage url a
                addLen = case lengthProperty of
                    Just l -> (<> videoDuration l)
                    _ -> (<> "<p></p>")
            case mt of
                "photo" ->
                    photo
                "video" ->
                    td . addLen <$> (videoFromSource a <|> videoFromUrl url a)
                "album" ->
                    album url title a
                "link" ->
                    return $ p $ T.concat
                        $ sharedLink' (mediaImage' a) url title descr
                "question" ->
                    return $ p $ renderLink postLink "View question page"
                "note" ->
                    return $ td ""
                "music" ->
                    music a =<< (photo <|> return (td ""))
                t ->
                    return $ p $ "Unsupported media_type: " <> t
        videoFromUrl url a = do
            t <- str "title" a <|> return ""
            (src, wh) <- mediaImage' a
            return $ renderTagsT $ iframe
                (++ (dataBqrVideoPoster, src):(dataBqrVideoTitle, t):wh)
                ("https://www.facebook.com/plugins/video.php?href="
                 <> encodeURIComponentT url <> "&appId=" <> fbAppId)
        videoFromSource a = do
            source <- str "source" =<< obj "media" a
            let hn = hostNameFromUrlT source
            if | "vimeo.com" `T.isSuffixOf` hn ||
                 "youtube.com" `T.isSuffixOf` hn ->
                   return $ renderTagsT $ embedIframe source
--                | "fbcdn" `T.isInfixOf` source && ".mp4" `T.isInfixOf` source ->
--                    return $ renderTagsT $
--                        embedVideo "video/mp4" Nothing
--                        ((fst <$> mediaImage' a)
--                         <|> s "full_picture" <|> s "picture") False source
--                        -- постер 130х130 замыленный, но зато показывает
--                        -- выбранный момент видео, а не первый кадр, который
--                        -- может быть черным
               | otherwise ->
                   fail "Unsupported video"
                   -- бывают ссылки на что угодно, включая сайты новостей
                   -- с видеовыпуском.
                   -- Так что будем их просто обрабатывать как ссылку и картинку
        albumPhoto x = do
            i <- jsonObj x
            link <- str "unshimmed_url" i
            img <- mediaImage link i
            return (link, T.concat ["<p>", img, "</p>"])
        mediaImage'' i =
            obj "image" =<< obj "media" i
        mediaImage' i = do
            m <- mediaImage'' i
            src <- str "src" m
            let at n = do
                    v <- int n m
                    return (n, T.pack $ show v)
            return (src, mapMaybe at ["width", "height"])
        mediaImage link i = do
            (src, wh) <- mediaImage' i
            return $ renderTagsT $ aimgTags' wh link src ""
        album url title a = do
            sa <- arr "data" =<< obj "subattachments" a
            let photos = mapMaybe albumPhoto sa
                numPhotos = length photos
                totalPhotos = fromMaybe numPhotos $ do
                    [[_,n,_]] <- regexGet "added ([0-9]+) (new )?photos" <$>
                        s "story"
                    return $ readUnsignedInt n
                more
                    | totalPhotos > numPhotos && numPhotos >= 1 =
                        ["<p>"
                        , renderLink (fst $ last photos)
                            ("+" <> T.pack (show $ totalPhotos - numPhotos)
                             <> " photos")
                        , " from "
                        , renderLink url title, " album.</p>"]
                        -- если numPhotos вдруг == 0, то все равно будет
                        -- ссылка на альбом
                    | not $ regexTest "/posts/[0-9]+" url =
                        [ "<p>", renderLink url title, " album.</p>" ]
                        -- если это не "Photos from Имярек post",
                        -- то вставляем?
                    | otherwise = []
            return $ T.concat $ map snd photos ++ more
        music a photo = do
            sa <- arr "data" =<< obj "subattachments" a
            let songs = mapMaybe song sa
                song s = do
                    s <- jsonObj s
                    u <- str "unshimmed_url" s
                    t <- str "title" s
                    return $ T.concat ["<li>", renderLink u t, "</li>"]
            return $ T.concat $
                [photo, "<ol>"] <> songs <> ["</ol>"]
        lengthProperty =
            listToMaybe . mapMaybe lengthP =<< arr "properties" m
            -- не во всех видео есть длительность
        lengthP x = do
            x <- jsonObj x
            "Length" <- str "name" x
            t <- str "text" x
            readDuration t

-- https://developers.facebook.com/docs/graph-api/reference/v10.0/page/feed
        -- status_type:
        --   added_photos
        --   added_video
        --   app_created_story
        --   approved_friend
        --   created_event
        --   created_group
        --   created_note
        --   mobile_status_update
        --   published_story
        --   shared_story
        --   tagged_in_photo
        --   wall_post
        --
        postLink = fromMaybe (fbPostLink guid) (s "permalink_url")
        story =
            fmap (\ s -> "<p class='bqrFbStory'>" <> s <> "</p>") $
            fbMarkTags postLink "story" m
        message =
            fmap (\ s -> "<p>" <> s <> "</p>") $
            fbMarkTags postLink "message" m
        preprocessText =
            editTagSoup $
            newlinesToBr .
            addHashTags 1 (T.append "https://www.facebook.com/hashtag/")
    return $
        if s "status_type" == Just "wall_post" || not (checkFrom fromId) then
            Nothing
        else
            Just $ defaultFeedMsg
            { fmGuid = guid
            , fmBody =
                preprocessText $ T.concat $ catMaybes [story, message, attachments]
            , fmAuthor = author
            , fmLinks = links
            , fmPublishedTime = readRfc3339 $ T.unpack time
            }
    where s n = do
            JSON.String r <- HM.lookup n m
            return $ T.strip r

parseFacebookError :: B.ByteString -> Either String T.Text
parseFacebookError = jsonParser parseFacebookError'

isFacebookOAuthException :: B.ByteString -> Bool
isFacebookOAuthException = (== Right "OAuthException") . jsonParser t
    where t = withObject "root object" $
              (.: "error") >=> (.: "type") >=> withText "error type" return

parseFacebookError' = withObject "root object" $
    (.: "error") >=> (.: "message") >=> withText "message text" (return . fix)
    where fix m | "Cannot query users by their username" `T.isInfixOf` m =
                  -- (#803) Cannot query users by their username (vshabanov1)
                    personal
                | "Some of the aliases you requested do not exist" `T.isInfixOf` m =
                  -- (#803) Some of the aliases you requested do not exist: vshabanov1
                    "Page either does not exists or it’s a personal page.\n"
                    <> personal
                | "Unsupported get request" `T.isInfixOf` m =
                    "Facebook page is not accessible (private or age/country restricted)."
                | otherwise = T.append "Facebook Graph API error:\n" m
          personal =
              "Personal Facebook pages are not supported.\nOnly public (brand) or community pages are supported."

parseFacebookUsage :: B.ByteString -> Either String (Int, T.Text)
parseFacebookUsage u =
    jsonParser (withObject "app usage object" $ parseFacebookUsage' u) u

parseFacebookUsage' u o = do
    let i n = withScientific (T.unpack n) (return . truncate) =<< (o .: n)
    cc <- i "call_count"
    ct <- i "total_cputime"
    tt <- i "total_time"
    return (maximum [cc, ct, tt], bst u)

-- У страниц свой отдельный rate limit
-- https://developers.facebook.com/docs/graph-api/overview/rate-limiting#pages

parseFacebookBusinessUsage :: B.ByteString -> Either String (Int, T.Text)
parseFacebookBusinessUsage u = jsonParser (parseFacebookBusinessUsage' u) u

parseFacebookBusinessUsage' u = withObject "business usage object" $ \ o ->
    go $ concat [V.toList a | JSON.Array a <- HM.elems o]
    where go [] = fail "pages usage not found"
          go (r:rs) = pageUsage r <|> go rs
          pageUsage = withObject "page usage" $ \ o -> do
              JSON.String "pages" <- o .: "type"
              parseFacebookUsage' u o

parseFacebookUsageRdr rdr = fromMaybe (Left "No Facebook usage headers found")
    $ h "X-App-Usage" parseFacebookUsage
      <|>
      h "X-Business-Use-Case-Usage" parseFacebookBusinessUsage
    where h n f = f <$> lookup n (rdrHeaders rdr)

handleFacebook _ u js = do
    let p pageId js = do
          JSON.Object root <- return js
          JSON.Array a <- HM.lookup "data" root
          forM (V.toList a) $ \ p -> do
              JSON.Object m <- return p
              fmap addLinks <$> facebookMsg (== pageId) m
              -- поскольку группы всё равно не работают, проверка на совпадение
              -- from.id и id страницы позволяет отфильтровать added_photos
              -- от сторонних пользователей (у них вообще нет from)
        addLinks fm =
             fm
             { fmLinks =
--                  (LKApiFeed, T.concat [ "https://graph.facebook.com/"
--                                       , fmGuid fm, "/comments"]) :
-- отключил комментарии, т.к. из-за новых rate limit-ов facebook-а
-- слишком долго все обновляется
                 (LKLink, fbPostLink (fmGuid fm))
                 : fmLinks fm
             }
        err :: Monad m => T.Text -> m ParseResult
        err = return . PRError
        profile = either (PRError . T.pack) id . jsonParser (\ js ->
            case JSON.parseEither parseFacebookError' js of
                Right e -> err $ T.append "Can’t get profile info:\n" e
                _ -> case jsIdName =<< jsonObj js of
                    Just (id, name) ->
                        return $
                            PRFeed u
                               (defaultFeedMsg
                                { fmSubject = name
                                , fmLinks = [(LKLink, fbProfileLink id)] })
                               []
                    _ -> err "Can’t parse profile info")
        r = case JSON.parseEither parseFacebookError' js of
              Right e -> err $ T.append "Can’t get posts list:\n" e
              _ -> do
--                   (id, name) <- jsIdName info
--                   posts <- p id postsJson
                  id <- urlUserId u
                  posts <- p id js
                  return $ case catMaybes posts of
                      ps@(x:xs) ->
                          PRFeed u
                             (defaultFeedMsg
                              { fmSubject = fmAuthor x
                              , fmLinks =
                                [(LKLink, u) | (LKAuthor, u) <- fmLinks x] })
                             ps
                      [] ->
                          PRAdditionalDownload
                             "facebook profile info for empty feed"
                             [fbProfileApiPath id]
                             (combineDownloadsBS (profile . head))
    case r of
        Nothing -> PRError "Can’t parse facebook posts"
        Just e -> e

handleFacebookId _ u js = do
    let i = do
          JSON.Object root <- return js
          JSON.String r <- HM.lookup "id" root
          return r
    case i of
        Nothing -> PRError $ "Can’t get facebook id"
        Just i -> PRRedirect $ "https://www.facebook.com/" <> i

feedParams = "limit=25&fields=id,from,created_time,message,message_tags,permalink_url,properties,status_type,story,story_tags,attachments{description,description_tags,media,media_type,title,unshimmed_url,subattachments{media,title,unshimmed_url}}"
-- если поля не указывать, то выдает урезанные данные
--
-- limit=50, чтобы не терялись посты при медленном обновлении
-- c limit=100 сильно растет total_time в x-app-usage и приложение
-- начинает ограничиваться уже по времени, а не числу запросов
-- Пришлось сделать limit=25, т.к. иначе на некоторых фидах
-- стал выдавать "Unsupported get request"
