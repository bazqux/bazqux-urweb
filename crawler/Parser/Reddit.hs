{-# LANGUAGE ViewPatterns, OverloadedStrings, RecordWildCards, MultiWayIf #-}
-- | Reddit
module Parser.Reddit
    ( customParsers
    ) where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Lazy as HM
import URL
import Parser.Types
import Parser.Custom
import Parser.DOM
import Lib.UrTime
import Lib.Regex
import Lib.StringConversion
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import Control.Applicative

customParsers =
    mkCustomParsers ["reddit.com"]
    [ ( r "/r/[^/]+/comments/.*\\.rss$", changeUrl transformRedditUrl, CPJSON handleRedditComments)
    , ( r "/r/[^/]+/.*\\.rss", changeUrl transformRedditUrl, CPJSON $ handleReddit False)
    , ( r "/r/[^/]+\\.rss", changeUrl transformRedditUrl, CPJSON $ handleReddit True) -- бывают и без /, например, Science.rss
    , ( r "/domain/.*\\.rss$", changeUrl transformRedditUrl, CPJSON $ handleReddit False)
    , ( r "/\\.rss\\?feed=.*", changeUrl transformRedditUrl, CPJSON $ handleReddit False)
    , ( r "/\\.rss$", changeUrl transformRedditUrl, CPJSON $ handleReddit False)
    , ( r "/r/[^/]+/?$", noScan, CPTags redirectReddit)
    , ( r "/r/[^/]+/(new|hot|rising)/?$", noScan, CPTags redirectReddit)
    , ( r "/r/[^/]+/top/?(\\?.*)?$", noScan, CPTags redirectRedditTop)
    , ( r "/r/[^/]+/\\?f=flair_name%3A%22.*%22$", noScan, CPTags redirectRedditFlair)
    , ( rt "old\\.reddit\\.com/", noScan, CPTags redirectWww )
    ]
    where r x = rt $ "www\\.reddit\\.com" <> x

transformRedditUrl = httpToHttps . T.replace ".rss" ".json"

redirectWww _ u _
    | [[_, p]] <- regexGet "old\\.reddit\\.com/(.*)" u =
        PRRedirect $ T.concat [r "/", p]
    | otherwise = PRError "regexGet error???"

redirectReddit _ u _
    | [[_, sr]] <- regexGet "www\\.reddit\\.com/r/([^/]+)" u =
        PRRedirect $ T.concat [r "/r/", T.toLower sr, "/.rss"]
    | otherwise = PRError "regexGet error???"

redirectRedditTop _ u _
    | [[_, sr, args]] <- regexGet "www\\.reddit\\.com/r/([^/]+)/top/?(\\?.*)?$" u =
        PRRedirect $ T.concat [r "/r/", T.toLower sr, "/top.rss", args]
    | otherwise = PRError "regexGet error???"

redirectRedditFlair _ u _
    | [[_, sr, flair]] <- regexGet "www\\.reddit\\.com/r/([^/]+)/\\?f=flair_name(%3A%22.*%22)$" u =
        PRRedirect $ T.concat [r "/r/", T.toLower sr, "/search.rss?q=flair", flair, "&restrict_sr=on&sort=new&t=all"]
    | otherwise = PRError "regexGet error???"

redditData kind x = do
    JSON.Object o <- return x
    k <- str "kind" o
    guard (k == kind)
    JSON.Object d <- HM.lookup "data" o
    return d
redditListing x = do
    d <- redditData "Listing" x
    jsonArray =<< HM.lookup "children" d
redditTime d = do
    JSON.Number c <- HM.lookup "created_utc" d
    return $ Just $ UrTime (truncate c) 0
    <|> return Nothing
checkRedditError what js r =
    fromMaybe (PRError e) $
        do JSON.Object root <- return js
           JSON.Number (truncate -> code) <- HM.lookup "error" root
           return $ case code of
               304 -> PRError (e <> ": Not modified")
               429 -> PRError (e <> ": Rate limit.<br/>Sometimes it happends for some reddits, retry in few minutes.")
               _ -> PRError (e <> ": HTTP " <> T.pack (show code))
       <|> r
    where e = "Can’t parse reddit " <> what

imgUrl :: Regex
imgUrl = "https://i\\.redd\\.it/[0-9a-z]+\\.(gif|jpg|png)"

handleReddit linkGuid _ u js = checkRedditError "JSON" js $ do
    l <- redditListing js
    return $
        PRFeed
        u
        (defaultFeedMsg
            { fmSubject = name
            , fmLinks = [(LKLink, subredditUrl name)] })
        (mapMaybe linkOrComment $ take 25 l)
        -- был случай, когда API выдало не 25 постоа, а больше
        -- и старые добавились как новые. Так что ограничиваем число постов.
    where name = maybe "reddit" T.toLower $
              do [[_, name]] <- return $ regexGet "reddit.com/r/([^/.]+)" u
                 return name
              <|>
              do [[_, name]] <- return $ regexGet "reddit.com/domain/([^/]+).rss" u
                 return name
          linkOrComment x = (snd <$> link x) <|> comment x
          link = link' <=< redditData "t3"
          link' :: JSON.Object -> Maybe ((TURL, Text), FeedMsg)
          link' d = do
              author <- str "author" d
              title <- str "title" d
              url <- unescapeHtmlT <$> str "url" d
              domain <- str "domain" d
              permalink <- str "permalink" d
              thumbnail <- str "thumbnail" d
              subreddit <- T.toLower <$> str "subreddit" d
              JSON.Bool self <- HM.lookup "is_self" d
              body <- str "selftext_html" d <|> return ""
              guid0 <- str "id" d
              time <- redditTime d
              crossPostsList <- arr "crosspost_parent_list" d <|> return []
              redditMedia <- bool "is_reddit_media_domain" d
              let crossPosts = mapMaybe (crossPost <=< jsonObj) crossPostsList
                  link = r permalink
                  guid
                      | linkGuid =
                          T.concat [ if "https" `T.isPrefixOf` u
                                     then "https:" else "http:"
                                   , "//www.reddit.com"
                                   , permalink ]
                      | otherwise = guid0
                  linkHtml = if self || ignored || redditMedia then "" else
                      T.concat ["<p>", renderLink url (shorten url), "</p>"]
                  ignored =
                      hostNameFromUrlT url == "v.redd.it"
                      -- v.redd.it перенаправляет на сам пост,
                      -- убираем такие ссылки
                  shorten u = domain <> path <> if T.null suff then "" else "…"
                      where (domain, path0) = T.span (/= '/')
                                $ rmWwwPrefix $ rmHttpPrefix u
                            (path, suff) = T.splitAt 7 path0
              return
                 ((url, subreddit)
                 ,defaultFeedMsg
                  { fmGuid = guid
                  , fmSubject = title
                  , fmBody =
                      (if | not (null crossPosts) -> ""
                          | Just e <- embed d <|> media d
                              <|> gallery d <|> preview url d -> linkHtml <> e
                          | Just i <- imageFromLink crossPosts url -> i
                          | T.isPrefixOf "https://" thumbnail ->
                              -- thumbnail может быть "self", "default", ""
                              T.concat $ twoColumns (renderTagsT $ aimgTags'
                                  (fromMaybe [] $ thumbWH d)
                                  url thumbnail "bqrRedditThumb") linkHtml
                          | otherwise ->
                              linkHtml)
                      <>
                      unescapeHtmlT body
                      <>
                      poll d
                      <>
                      T.concat (map snd crossPosts)
                  , fmAuthor = author
                  , fmPublishedTime = time
                  , fmLinks =
                        [(LKAuthor, userUrl author)
                        ,(LKLink, link), (LKApiFeed, link <> ".rss") ]
                  , fmTags =
                      (if bool "is_original_content" d == Just True
                       then ["OC"] else [])
                      <>
                      maybeToList (str "link_flair_text" d)
                      <>
                      (if subreddit /= name then [subreddit] else [])
                  })
          comment x = do
              (d, fm) <- commentFeedMsg x
              t <- str "link_title" d
              return $ fm { fmSubject = t }
          fixTweet h
              | Just s <- T.stripPrefix "<blockquote class=\"twitter-video\">" h
              = "<blockquote class=\"twitter-tweet\">" <> s
              | otherwise = h
          embed x = do
              e <- obj "secure_media_embed" x
              h <- fixTweet . unescapeHtmlT <$> str "content" e
              return $ h <> "<p></p>"
          media x = do
              v <- obj "reddit_video" =<< obj "secure_media" x
              vu <- unescapeHtmlT <$> str "fallback_url" v
              w <- int "width" v
              h <- int "height" v
              (JSON.Object i:_) <- arr "images" =<< obj "preview" x
              (_,_,poster) <- previewImageUrl i
              return $ renderTagsT $ paragraph $
                  embedVideoWH w h "video/mp4" (Just poster) False vu
          thumbWH d = do
              h <- int "thumbnail_height" d
              return [("width", "140"), ("height", showT h)]
--               (JSON.Object i:_) <- arr "images" =<< obj "preview" d
--               d <- obj "source" i
--               whScaledByMaxWidth truncate 140 d
              -- reddit почему-то делает не round, а truncate
          imageFromLink crossPosts u = do
              guard (u `notElem` (map fst crossPosts) && regexTest imgUrl u)
              -- в репостах может не быть preview, но если ссылка указывает
              -- на картинку, то ее и вставляем
              return $ renderTagsT $ paragraph $ aimgTags u u ""
          preview l d = do
              p <- obj "preview" d
              guard =<< bool "enabled" p
              is <- arr "images" p
              T.concat . map (renderTagsT . paragraph)
                  <$> mapM (previewImage l) [i | JSON.Object i <- is]
          previewImage link i = do
              p@(_,_, poster) <- previewImageUrl i
              (do (w,h,vu) <- variant "mp4" i
                  return $ embedVideoWH w h "video/mp4" (Just poster) True vu)
               <|> (image link <$> variant "gif" i)
               <|> (return $ image link p)
          previewImageUrl = previewImageUrl' "source" "resolutions" resolution
          previewImageUrl' source resolutions r i = do
              r0 <- r =<< obj source i
              rs <- mapM (r <=< jsonObj) =<< arr resolutions i
              let rs' = r0 : rs
                  -- для небольших изображений source может быть
                  -- больше остальных resolutions
                  p   | s@(_:_) <- filter (\(w,h,_) -> w < 1000 && h < 1000) rs'
                      = maximum s
                      | otherwise = minimum rs'
              return p
          variant what i =
              resolution =<< obj "source" =<< obj what =<< obj "variants" i
          image link (w,h,u) = awhImgTags w h link u
          resolution r =
              (,,) <$> int "width" r
                  <*> int "height" r
                  <*> (unescapeHtmlT <$> str "url" r)
          resolutionXYU r =
              (,,) <$> int "x" r
                  <*> int "y" r
                  <*> (unescapeHtmlT <$> str "u" r)
          gallery x = do
              md <- obj "media_metadata" x
              is <- arr "items" =<< obj "gallery_data" x
              fmap T.concat $ forM is $ \ i -> do
                  mid <- str "media_id" =<< jsonObj i
                  m <- obj mid md
                  sUrl <- fmap unescapeHtmlT $ str "u" =<< obj "s" m
                  renderTagsT . paragraph . image sUrl
                      <$> previewImageUrl' "s" "p" resolutionXYU m
          poll x = fromMaybe "" $ do
              p <- obj "poll_data" x
              total <- int "total_vote_count" p
              oa <- arr "options" p
              os <- forM oa $ \ o -> do
                  o <- jsonObj o
                  text <- str "text" o
                  return (text, int "vote_count" o)
              let t x = [TextNode x]
                  table
                      | all (isNothing . snd) os =
                          node "ul" [] $ map (node "li" [] . t . fst) os
                      | otherwise =
                          pollTable $ map (pollRow . row) os
                  row (text, fromMaybe 0 -> v) =
                      [pollOptionPercent $ t $
                          (if v == maxV then "✓\xA0" else "") <> showVotes v
                      ,pollOptionValue
                          [pollOptionText $ t text
                          ,pollOptionBar $ fromInteger v / fromInteger maxV
                          ]]
                  maxV = max 1 $ fromJust $ maximum $ map snd os
              return $ T.concat
                  ["<p>", showVotes total, " votes</p>"
                  ,renderTagsT $ forestToTags [table]]
          crossPost x = do
              ((u, sub), FeedMsg {..}) <- link' x
              link <- lookup LKLink fmLinks
              return (u, T.concat
                  ["<hr/><header class=bqrRepostHeader>"
                  ,"<div class=bqrRepostHeaderSubject>"
                  ,renderLink link fmSubject
                  ,"</div>"
                  ,"<div class=bqrRepostHeaderFrom>"
                  ,"<span class=bqrRepostHeaderFromFrom>from </span>"
                  ,subredditLink [("class", "bqrRepostHeaderFromFeed")] sub
                  ,"</div>"
                  ,"<div class=bqrRepostHeaderAuthor>"
                  ,renderLink (userUrl fmAuthor) fmAuthor
                  ,"</div>"
                  ,case fmPublishedTime of
                      Just t ->
                          "<div class=bqrRepostHeaderTime>"
                          <> sharedPostTime link t <> "</div>"
                      Nothing -> ""
                  ,"</header>"
                  ,fmBody
                  ])
          subredditUrl s = T.concat [r "/r/", s, "/"]
          userUrl u = T.concat [r "/user/", u, "/"]
          subredditLink a s = renderLink' [] (subredditUrl s) s

commentFeedMsg x = do
    d <- redditData "t1" x
    author <- str "author" d
    body <- str "body_html" d
    guid <- str "id" d
    permalink <- str "permalink" d
    time <- redditTime d
    return
        (d
        ,defaultFeedMsg
            { fmGuid = guid
            , fmBody = unescapeHtmlT body
            , fmAuthor = author
            , fmPublishedTime = time
            , fmLinks =
                [(LKAuthor
                 ,T.concat [r "/user/", author, "/"])
                ,(LKLink, r permalink)]
            })

r x = "https://www.reddit.com" <> x

handleRedditComments _ u js = checkRedditError "comments JSON" js $ do
    l <- jsonArray js
    c <- fmap concat $ mapM (comments . return) l
    return $ PRParsedHtml Nothing NoSwitch c []
    where comment x = do
              (d, fm) <- commentFeedMsg x
              pid <- str "parent_id" d
              r <- comments $ HM.lookup "replies" d
              return (( if "t1_" `T.isPrefixOf` pid
                        then Just (T.drop 3 pid)
                        else Nothing
                       , fm)
                      : r)
              <|>
              more x
          more x = do
              d <- redditData "more" x
--              l <- jsonArray =<< HM.lookup "children" d
--              return [(Nothing, defaultFeedMsg) | _ <- l]
              -- по-умолчанию reddit выдает 200 комментов
              -- причем не по порядку, а топовые (что хорошо)
              -- и для поста в 339 комментов выдает еще 40more
              --
              -- more -- обычно непосредственно коммент, а не список
              -- возможно, их можно как-нить сливать
              -- проблема здесь в том, что если у ЖЖ мы можем проверить,
              -- появились ли новые сообщения в цепочке, то здесь нет
              -- а сканировать каждый раз на каждом большом посте кучу more
              -- очень накладно, так что пока more просто игнорируются
              --
              -- Видимо, стоит потом договориться с reddit-ом, чтобы давали
              -- комментов побольше
              return []
          comments x = (fmap concat . mapM comment =<< redditListing =<< x)
                       <|> return []
          page = T.take (T.length u - 4) u -- отрезаем .rss

showVotes n
    | n < 1000 = showT n
    | otherwise = showT k <> "." <> showT r <> "k"
    where (k, r) = round (fromInteger n / 100) `quotRem` 10
