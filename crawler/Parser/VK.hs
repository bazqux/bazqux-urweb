{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
-- | VKontakte
module Parser.VK
    ( customParsers
    ) where

import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.IntMap as IntMap
import qualified Data.Aeson as JSON
import Lib.Json
import Parser.Types
import Parser.Custom
import Lib.UrTime
import Lib.ReadUtils
import Lib.Regex
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import Text.Printf
import Control.Applicative
import Config (vkAppServiceKey)
import URL

customParsers =
    mkCustomParsers ["vk.com", "vkontakte-feed.appspot.com"]
    [ ( rt "vkontakte-feed\\.appspot\\.com/feed/([^/]+)/wall",
        changeUrl (snd . transformVKUrl), CPJSON handleVKwall )
    , ( rt "vk\\.com/([^/\\?#]+)[\\?#/]", noScan, CPTags redirectVK )
    , ( rt "vk\\.com/podcasts-[0-9]+\\.rss$", noChange, CPNone )
    , ( rt "vk\\.com/podcasts-[0-9]+$", noScan, CPTags redirectVKPodcast)
    , ( rt "vk\\.com/([^/\\?#]+)",
        changeUrl (snd . transformVKUrl), CPJSON handleVKwall )
    ]

-- idNNNN -> owner_id=NNNN
-- (club|event|public)NNNN -> owner_id=-NNNN
-- alias -> domain=alias

-- foreign_posts=1 -> filter=all (по-умолчанию у vkontakte-feed filter=owner)

data UrlType
   = Topic T.Text T.Text -- group, topic
   | Wall T.Text
   deriving Show

redirectVK  _ u _
    | [[_, _, path]] <- rg "vk\\.com/([^/\\?#]+)[\\?#/]" u =
        PRRedirect $ vkLink path
    | otherwise = PRError "regexGet error???"
redirectVKPodcast  _ u _
    | [[_, _, path]] <- rg "vk\\.com/(podcasts-[0-9]+)" u =
        PRRedirect $ vkLink $ path <> ".rss"
    | otherwise = PRError "regexGet error???"
transformVKUrl u
    | [[_, _, group, topic]] <- rg "vk\\.com/topic-([0-9]+)_([0-9]+)" u =
       (Topic group topic
       ,T.concat ["https://api.vk.com/method/board.getComments"
                 ,vkParams
                 ,"group_id=", group, "&topic_id=", topic, "&sort=desc" ])
    | [[_, _, wall]] <- rg "vk\\.com/([^/\\?#]+)" u =
        wall_get wall
    | [[_, _, wall]] <- rg "vkontakte-feed.appspot.com/feed/([^/]+)/wall" u =
        wall_get wall
    | otherwise = (Wall "", u)
    where wall_get wall =
              (Wall wall
              ,T.concat
               ["https://api.vk.com/method/wall.get"
               ,vkParams
               ,if "foreign_posts=1" `T.isInfixOf` u
                then "filter=all&" else "filter=owner&"
               ,case () of
                  _ | [[_,id]] <- regexGet "^id([0-9]+)$" wall ->
                      "owner_id=" <> id
                    | [[_,_,id]] <- regexGet "^(club|event|public)([0-9]+)$" wall ->
                      "owner_id=-" <> id
                    | otherwise ->
                      "domain=" <> wall
               ])

vkParams = T.concat
    ["?https=1&v=5.85&lang=ru&count=100&extended=1&access_token="
    ,vkAppServiceKey, "&"]

handleVKwall _ u j = fromMaybe (PRError "Invalid JSON") $ err <|> do
    JSON.Object o <- return j
    response <- obj "response" o
    items <- arr "items" response
    profiles <- arr "profiles" response
    groups <- arr "groups" response <|> Just []
    let kind = fst $ transformVKUrl u
        profileMap =
            IntMap.fromList $
            mapMaybe (profile False) profiles ++ mapMaybe (profile True) groups
        profile group j = do
            JSON.Object o <- return j
            sn <- str "screen_name" o
            name <-
                str "name" o <|>
                liftM2 (\ a b -> T.concat [a, " ", b])
                    (str "first_name" o) (str "last_name" o)
                <|> str "first_name" o <|> str "last_name" o
                <|> return sn
            id <- int "id" o
            photo <- str "photo_100" o <|> str "photo_50" o
            return (if group then -id else id,
                    (name, [(LKAuthor, vkLink sn)
                           ,(LKAuthorPic, photo)]))
        geo o = do
            g <- obj "geo" o
            c <- str "coordinates" g
            p <- obj "place" g
            t <- str "title" p
            return $ embeddedGoogleMaps "bqrVKMap" c t
        feedMsg js = do
            JSON.Object o <- return js
            date <- int "date" o
            id <- int "id" o
            fromId <- int "from_id" o
            (author, authorLinks) <- IntMap.lookup fromId profileMap
            text <- str "text" o
            let i = T.pack . show
                link = case kind of
                    Wall wall ->
                        vkLink $ T.concat
                        ["wall", maybe "" i $ int "owner_id" o,"_", i id]
                    Topic group topic ->
                        vkLink $ T.concat
                        ["topic-", group, "_", topic, "?post=", i id]
                postLink (T.concat -> inner) = case kind of
                    Wall wall ->
                        T.concat [link, "?z=", inner, "%2F"
                                 ,T.drop (T.length $ vkLink "") link]
                    Topic group topic ->
                        T.concat [link, "&z=", inner, "%2F"
                                 ,"post-", group, "_", i id]
                photo a = do
                    p <- obj "photo" a
                    photo' p
                posted_photo a = do
                    p <- obj "posted_photo" a
                    oldPhoto p
                graffiti a = do
                    p <- obj "graffiti" a
                    oldPhoto p
                oldPhoto p = do -- pre 2013
                    src <- str "photo_604" p <|> str "photo_130" p
                    return $ renderTagsT $
                        [TagOpen "p" [], TagOpen "img" [("src", src)]
                        ,TagClose "p"]
                ti = T.pack . show
                photoSrc preferred p = do
                    sizes <- arr "sizes" p
                    s <- forM sizes $ \ s -> do
                        JSON.Object o <- return s
                        t <- str "type" o
                        w <- int "width" o
                        h <- int "height" o
                        u <- str "url" o
                        return (t, w, h, u)
                    let go [] = fail "no photo src"
                        go [x] = ret x
                        go (x@(_,w,_,_):xs)
                            | w >= 700 = ret x
                            | otherwise = go xs
                        ret (_,w,h,u) =
                            return (u, [("width", ti w), ("height", ti h)])
                        goP [] = go []
                        goP (x:xs)
                            | Just x <- find (\(t,_,_,_) -> t == x) s = ret x
                            | otherwise = goP xs
                    if null preferred then
                        go $ sort
                            [(t `elem` ["o", "p", "q", "r"], w, h, u)
                             -- картинки, которые могут быть обрезаны
                             -- ставим в последнюю очередь
                            | (t, w, h, u) <- s]
                    else
                        goP preferred
                photo' p = do
                    (src, wh) <- photoSrc [] p
                    -- ["y", "x", "z", "w", "r", "q", "p", "o", "m", "s"]
                    -- https://vk.com/dev/objects/photo_sizes
                    owner_id <- int "owner_id" p
                    pid <- int "id" p
                    t <- str "text" p <|> Just ""
                    let l | owner_id == 0 || pid == 0 = link
                          | otherwise =
                              postLink ["photo", i owner_id, "_", i pid]
                        html = renderTagsT $ aimgTags' wh l src ""
                    return $ T.concat $
                        ["<p>", html ]
                        ++ (if t /= "" then ["<br/>", preprocessVKText t] else [])
                        ++ ["</p>"]
                album a = do
                    a <- obj "album" a
                    owner_id <- int "owner_id" a
                    aid <- fmap i (int "id" a) <|> str "id" a
                    t <- str "title" a <|> Just ""
                    description <- str "description" a <|> Just ""
                    size <- int "size" a
                    thumb <- obj "thumb" a
                    (src, wh) <- photoSrc [] thumb
                    let l | owner_id == 0 || aid == "0" = link
                          | otherwise =
                              T.concat [ link
                                       , "?z=album", i owner_id, "_", aid ]
                        html = renderTagsT $ aimgTags' wh l src ""
                        title = renderLink l $ T.concat
                            [ t
                            , if t /= "" then " " else "", "("
                            , T.pack $ show size, " photo"
                            , if size /= 1 then "s" else "", ")" ]
                    return $ T.concat $
                        ["<p>", html
                        ,"</p><p class=bqrCaption>", title
                        ,"</p>"]
                        ++ (if description /= "" then
                                ["</p><p>", preprocessVKText description] else [])
                        ++ ["</p>"]
                app a = do
                    a <- obj "app" a
                    src <- str "icon_150" a
                    aid <- int "id" a
                    name <- str "name" a
                    let l = vkLink ("app" <> i aid)
                        html = renderTagsT $ aimgTags' [] l src ""
                    return $ T.concat $
                        ["<p>", html ]
                        ++ (if name /= "" then ["<br/>", escapeHtmlT name]
                            else [])
                        ++ ["</p>"]
                audio a = do
                    a <- obj "audio" a
                    artist <- str "artist" a
                    title <- str "title" a
                    return $ renderTagsT
                        [TagOpen "p" []
                        ,TagText "[audio] "
                        ,TagOpen "a"
                             [("href", "https://vk.com/audio?performer=1&q="
                               <> encodeURIComponentT artist)]
                        ,TagOpen "b" []
                        ,TagText artist, TagClose "b",  TagClose "a"
                        ,TagText " - "
                        ,TagOpen "a" [("href", link)]
                        ,TagText title
                        ,TagClose "a"
                        ,TagClose "p"]
                video a = do
                    v <- obj "video" a
                    d <- int "duration" v
                    title <- str "title" v
                    owner_id <- int "owner_id" v
                    let tryWH w h = do
                            src <- str (T.append "photo_" $ ti w) v
                            return (src, []) -- [("width", ti w), ("height", ti h)])
                    (src, wh) <-
                        tryWH 800 450 <|> tryWH 640 480 <|>
                        tryWH 320 240 <|> tryWH 130 98
                        -- в API высоты нет, она фиксирована в документации,
                        -- при этом часто photo_640 содержит такой же url
                        -- как и photo_800
                        -- а может быть и photo_800 содержит нестандартный
                        -- размер 480x360
                        -- так что игнорируем размеры из документации
                    vid <- int "id" v
                    let l = postLink ["video", i owner_id, "_", i vid]
                    return $
                        renderTagsT
                        [ TagOpen "p" [], TagOpen "a" [("href", l)]
                        , TagOpen "img" $ [("src", src)] ++ wh
                        , TagClose "a", TagClose "p"
                        , TagOpen "p" [("class", "bqrCaption")]
                        , TagOpen "a" [("href", l)]
                        , TagText (T.concat [ title
                                            , " (", formatVkDuration d, ")"])
                        , TagClose "a", TagClose "p"
                        ]
                doc a = do
                    d <- obj "doc" a
                    title <- str "title" d
                    url <- str "url" d
                    size <- int "size" d
                    let photo =
                            photoSrc ["m", "s"] =<< obj "photo" =<< obj "preview" d
                    return $
                        renderTagsT $
                        [ TagOpen "p" []
                        , TagText $ if isNothing photo then "File " else ""
                        , TagOpen "a" [("href", url)] ]
                        ++ (case photo of
                              Just (src, wh) ->
                                  [ TagOpen "img" $ [("src", src)] ++ wh
                                  , TagClose "img"
                                  , TagOpen "br" []]
                              _ -> [])
                        ++
                        [ TagText (T.concat [ title
                                            , " (", formatVkSize size, ")"])
                        , TagClose "a", TagClose "p"
                        ]
                extLink name title link =
                    return $
                        renderTagsT $
                        [ TagOpen "p" [], TagText (name <> " ")
                        , TagOpen "a" [("href", link)]
                        , TagText (if title /= "" then title else "Untitled")
                        , TagClose "a", TagClose "p"
                        ]
                note a = do
                    n <- obj "note" a
                    title <- str "title" n
                    view_url <- str "view_url" n
                    extLink "Note" title view_url
                page a = do
                    n <- obj "page" a
                    title <- str "title" n
                    view_url <- str "view_url" n
                    extLink "Page" title view_url
                poll a = do
                    p <- obj "poll" a
                    question <- str "question" p
                    extLink "Poll" question link
                linkAtt a = do
                    l <- obj "link" a
                    url <- str "url" l
                    title <- str "title" l
                    description <- str "description" l
                    let img = do
                            p <- obj "photo" l
                            photoSrc ["a", "b", "m", "s"] p
                    return $
                        T.concat $ ["<p>"] ++ sharedLink' img url title description ++ ["</p>"]
                attachment a =
                    -- http://vk.com/dev/attachments_w
                    -- TODO: недостающие ^ отсюда
                    photo a <|> album a
                    <|> posted_photo a <|> graffiti a
                    <|> audio a <|> video a <|> doc a
                    <|> note a <|> poll a <|> linkAtt a
                    <|> page a <|> app a
                copy js = do
                    fm <- feedMsg js
                    insertMsg fm
            attachments <- arr "attachments" o <|> return []
            copy_history <- arr "copy_history" o <|> return []
            let atts =
                    mapMaybe (attachment <=< jsonObj) attachments
                chs = fst $ unzip $ mapMaybe copy copy_history
            return $ defaultFeedMsg
                { fmAuthor = author
                , fmPublishedTime = Just $ UrTime date 0
                , fmBody =
                    T.concat $
                    (if text /= "" then [ "<p>", preprocessVKText text, "</p>",
                                         if chs /= [] then "<hr/>" else "" ]
                     else [])
                    ++ chs ++ atts ++
                    [ fromMaybe "" (geo o) ]
                , fmGuid =
                    if vkontakte_feed then
                        "http" <> T.drop 5 link
                        -- у vkontakte-feed не было guid, но были http-ссылки
                    else
                        T.pack (show id)
                , fmLinks = (LKLink, link) : authorLinks }
        vkontakte_feed = T.isInfixOf "vkontakte-feed.appspot.com" u
    return $ case kind of
        Wall wall ->
            let (rootTitle, rootLinks) = fromMaybe (wall, []) rootProfile
                rootProfile
                    | [[_,id]] <- regexGet "^id([0-9]+)$" wall =
                        IntMap.lookup (readUnsignedInt id) profileMap
                    | [[_,_,id]] <- regexGet "^(club|event|public)([0-9]+)$" wall =
                        IntMap.lookup (- readUnsignedInt id) profileMap
                    | otherwise =
                        find (elem (LKAuthor, vkLink wall) . snd) $
                        IntMap.elems profileMap
            in
                PRFeed u
                (defaultFeedMsg
                 { fmSubject = rootTitle
                 , fmLinks = (LKLink, vkLink wall) : rootLinks })
                (mapMaybe feedMsg items)
        Topic group topic ->
            PRAdditionalDownload "topic title"
            [T.concat ["https://api.vk.com/method/board.getTopics"
            ,vkParams
            ,"group_id=", group, "&topic_ids=", topic]]
            (combineDownloadsBS $ parseTopic group topic (mapMaybe feedMsg items))
    where err = do
              JSON.Object o <- return j
              fmap PRError $ str "error_msg" =<< obj "error" o
          parseTopic group topic items ts =
              fromMaybe (PRError "Invalid Topic JSON") $ err <|> do
              t <- listToMaybe ts
              JSON.Object o <- decodeJson t
              response <- obj "response" o
              [JSON.Object item] <- arr "items" response
              title <- str "title" item
              return $
                  PRFeed u
                  (defaultFeedMsg
                   { fmSubject = title
                   , fmLinks = [(LKLink, vkLink $ T.concat ["topic-", group, "_", topic])] })
                  items

preprocessVKText =
    editTagSoup $
    newlinesToBr .
    addHashTags 2
        (\ t -> vkLink $ T.concat ["feed?q=%23", t, "&section=search"]) .
    addGroupHashTags 2
        (\ g t -> vkLink $ T.concat [g, "/", t]) .
    fixNonLinkedText
        (regexReplace'' TagText
            linkRegex
            (\ (_:b:url:_:name:_) xs ->
                 TagText b :
                 TagOpen "a" [("href", relUriNoNormalizeT url (vkLink ""))] :
                 TagText (if T.strip name /= "" then name else url) :
                 TagClose "a" : xs))

linkRegex = multilineRegex "(^|[[:space:][:punct:]«»‹“„‘‚”’¡¿])\\[([^|]+)(\\|([^]]*))?\\]"

vkLink = T.append "https://vk.com/"

formatVkDuration :: Integer -> T.Text
formatVkDuration d
    | h /= 0    = T.pack $ printf "%02d:%02d:%02d" h m s
    | otherwise = T.pack $ printf "%02d:%02d" m s
    where h = d `div` 3600
          m = (d `rem` 3600) `div` 60
          s = d `rem` 60

formatVkSize :: Integer -> T.Text
formatVkSize s
    | s >= g = r g "Gb"
    | s >= m = r m "Mb"
    | s >= k = r k "Kb"
    | otherwise = r 1 "b"
    where r d suff = T.pack $ show (s `div` d) ++ suff
          k = 1024
          m = k*k
          g = k*m
-- PRFeed _ _ (fmSummary . (!! 3) -> t) <- parseFile "" "../conf/pssh/homepage.xml"
