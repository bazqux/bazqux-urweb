{-# LANGUAGE ViewPatterns, BangPatterns, MultiWayIf, RecordWildCards,
             OverloadedStrings #-}
-- | Разбор HTML, RSS и Atom
module Parser.Default
    ( defaultParser, metaRedirectUrl
    ) where

import Control.Monad
import Data.Maybe
import Data.List
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashSet as HS
import Parser.Types
import Parser.Custom
import Lib.UrTime
import Lib.ReadUtils
import Lib.Regex
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import Control.Applicative
import Lib.FastChar (emptyText)
import Parser.Disqus hiding (customParsers)
import Parser.LiveJournal hiding (customParsers)
import qualified Parser.Facebook
import URL

setGuid    !a = \ m -> m { fmGuid = a }
setAuthor  !a = \ m -> m { fmAuthor = a }
setAuthor' False !a = \ m -> m { fmAuthor = a }
setAuthor' True !a = \ m -> if fmAuthor m /= "" then m else m { fmAuthor = a }
setAuthorEmail !a = \ m -> m { fmAuthorEmail = a }
setAuthorEmail' False !a = \ m -> m { fmAuthorEmail = a }
setAuthorEmail' True !a = \ m -> if fmAuthorEmail m /= "" then m else m { fmAuthorEmail = a }
setSubject !a = \ m -> m { fmSubject = a }
setPublishedTime  !a  = \ m -> m { fmPublishedTime = a }
setUpdatedTime    !a  = \ m -> m { fmUpdatedTime   = a }
setSummary !a = \ m -> m { fmSummary = a }
setBody    !a = \ m -> m { fmBody = a }
setDuration !a = \ m -> m { fmDuration = Just a }
addLink k (T.strip -> u)
    | u /= "" = \ m -> m { fmLinks = (k, T.strip u) : fmLinks m }
    | otherwise = \ m -> m -- игнорируем <link></link>
addLink' False k u    = addLink k u
addLink' True k u     = \ m -> if isJust $ lookup k $ fmLinks m then m else addLink k u m
rmLink  k             = \ m -> m { fmLinks = filter ((/= k) . fst) $ fmLinks m }
dbg        _ = id
addTag     !a = \ m -> m { fmTags = a : fmTags m }
addTags    !a = \ m -> m { fmTags = reverse a ++ fmTags m }
addImage   !a = \ m -> m { fmImages = a : fmImages m }
addCommaTags = addTags . map T.strip . T.split (== ',')
setCommentsCount !a = \ m -> m { fmCommentsCount = tryReadUnsignedInt a }
modMedia f !m = m { fmMedia = f $ fmMedia m }
addEnclosure !u !a !m = m { fmEnclosures = (u,a) : fmEnclosures m }
setEnclosureOrigLink u m =
    case fmEnclosures m of
        (u0,a) : es -> m { fmEnclosures = (u,a) : es
                         , fmEnclosureLinks = (u0, u) : fmEnclosureLinks m }
        _ -> m
addMThumbnail !t !m = m { mThumbnails = t : mThumbnails m }
addMGroup !g !m = m { mGroups = g : mGroups m }
addMContent !g !m = m { mContents = g : mContents m }
setMTitle !t !m = m { mTitle = Just t }
setMDescription !d !m = m { mDescription = Just d }

defaultParser url = start
    where start tags@(TagOpen name atts : ts)
             | "?" `T.isPrefixOf` name = start ts
             | "!" `T.isPrefixOf` name = start ts
             | "doctype" `T.isPrefixOf` name = start ts
             | Just go <- checkFeedTag url name atts ts = go
             | otherwise = crawlHtml url tags
--                      PRError $ "Invalid first tag " <>
--                                 T.encodeUtf8 (TL.toStrict name)
          start (_ : ts) = start ts
          start [] = PRError "Can’t detect feed format"

checkFeedTag url name atts ts = case name of
--     "html" -> crawlHtml url ts
--     "body" -> crawlHtml url ts
    "feed" -> Just $ crawlAtom (baseUri atts) ts
    -- "rss" | ("version", "2.0") `elem` atts ->
    "rss" -> Just $ crawlRSS (baseUri atts) ts
    -- 2.0 и 0.9 вроде одна фигня
    ((`elem` ["rdf", "rdf:rdf"]) -> True)
        | Just _ <- lookup "xmlns:rss" atts ->
            Just $ crawlRSS (baseUri atts) $ map derss ts
        | otherwise ->
            Just $ crawlRSS (baseUri atts) ts
            -- что-то не уловил особой разницы между rdf и rss
    _ -> Nothing
--         PRError $ "Invalid first tag " <>
--                    T.encodeUtf8 (TL.toStrict name)
    where baseUri atts
              | Just b <- lookup "xml:base" atts
              , correctUrl (T.unpack b) = b
              | otherwise = url
          derss (TagOpen t a) | "rss:" `T.isPrefixOf` t =
              TagOpen (T.drop 4 t) a
          derss (TagClose t) | "rss:" `T.isPrefixOf` t =
              TagClose (T.drop 4 t)
          derss x = x

-- | Для html нам необходимо определить redirect-ы на фиды и
-- использование javascript движков комментирования
crawlHtml :: TURL -> [Tag Text] -> ParseResult
crawlHtml url tags = go "" dzero [] [] tags
    where go g d f j [] =
              PRHtml
              { prHtmlFeeds = feeds
              , prCommentFeeds = selectCommentsFeed g url feeds
              , prJsComments =
                  if isJust (dForum d) && dForum d /= Just "cnn"
                     && dFoundEmbedJs d then
                      (JSCSupported $ disqusUrl $
                       d { dUrl = dUrl d <|> Just url })
                      : reverse j
                  else
                      reverse j
              }
              where feeds =
                        map (fromRelUrl . T.unpack . snd) $
                        sortBy (comparing fst) $
                        reverse f
          go !g !d !f !j (TagOpen t atts : ts)
              | Just n <- checkFeedTag url t atts ts = n
          go !g !d !f !j (TagOpen t atts : ts) = case t of
              "link"
                  | Just feed <- linkFeedAttr atts ->
                      go g d ((0,feed) : f) j ts
              "script"
                  | Just src <- lookup "src" atts
                  , any (`T.isSuffixOf` hostNameFromUrlT src)
                    [ "js-kit.com", "www.intensedebate.com"
                    , "pub.widgetbox.com", "letsgodynamic.com"
                    , "livefyre.com"
                    ] -- && not ("count.js" `T.isInfixOf` src)
--                     ||
--                     any (`T.isInfixOf` src)
--                     [ "echo" -- как-то слишком резко
--                     ]
                      ->
                      go g d f (JSCUnsupported src : j) ts
                  | TagText s : TagClose "script" : ts' <- ts
--                            , "comment_module" -- style.com (FeatureFeed)
                            ->
                      go g (inSrc d atts `dplus` findDisqusInScript s) f j ts'
                  | otherwise ->
                      go g (inSrc d atts) f j ts
              "fb:comments"
                  | Just href <- lookup "href" atts ->
                      fbComments href g d f j ts
              "div"
                  | Just "fb:comments" <- lookup "class" atts
                  , Just href <- lookup "data-href" atts ->
                      fbComments href g d f j ts
              "div"
                  | Just "fb-comments" <- lookup "class" atts
                  , Just href <- lookup "data-href" atts ->
                      fbComments href g d f j ts
              "a"
                  | Just (TagText (T.toLower -> "rss")) <- find textTag ts
                    -- теоретически можно еще script/style отфильтровывать
                  , Just href <- lookup "href" atts ->
--                  , "/cgi-bin/openforum/rss_forum.cgi?forum" `T.isPrefixOf` href ->
                      go g d ((1,href):f) j ts
                      -- для opennet, а надо ли?
              "a" | Just href <- lookup "href" atts
                  , Just p <- feedPriority url (T.unpack href) ->
                      go g d ((p,href):f) j ts
              "meta"
                  | Just u <- metaRedirectUrl url atts ->
                      PRRedirect u
                  | Just (T.toLower -> "generator") <- lookup "name" atts
                  , Just (T.toLower -> content) <- lookup "content" atts ->
                      go content d f j ts
                         -- TODO: может все-таки сделать unescape автоматом?
                         -- парсинг теперь жрет мало, а вот глюков можно
                         -- насажать
                  | Just "twitter:site" <- lookup "property" atts
                  , Just "@livejournal" <- lookup "content" atts
                  , [[_,_,arg]] <-
                      regexGet "https://(.+)/[0-9]+.html(\\?.*)?$" url
                  ->
                      if ljParam `T.isSuffixOf` arg then
                          case preprocessLJ url $ parseLJ url tags of
                              PRParsedHtml { .. } ->
                                  PRParsedHtml
                                  { prThreadLinks =
                                      [(p
                                       ,T.replace "&nojson" ("&" <> ljParam) u
                                       ,g)
                                      |(p, u, g) <- prThreadLinks]
                                  , ..
                                  }
                              r -> r
                      else
                          PRRedirect (urlAddParamT url ljParam)
              _ | Just "disqus_thread" <- lookup "id" atts ->
                  go g (d { dFoundEmbedJs = True }) f j ts
              _ ->
                  go g d f j ts
          go !g !d !f !j (_ : ts) = go g d f j ts
          ljParam = "nojs=1&format=light"
          fromRelUrl r = T.pack $ relUri r (T.unpack url)
          textTag (TagText t) = not (emptyText t)
          textTag _ = False
          fbComments href g d f j ts =
              go g d f (JSCUnsupported (T.append Parser.Facebook.facebookCommentsPrefix href) : j) ts
          inSrc d atts
              | Just s <- lookup "src" atts =
                  d { dFoundEmbedJs = dFoundEmbedJs d
                                      || "embed.js" `T.isInfixOf` s }
                  `dMplusForum` disqusUrlForum s
              | otherwise = d

-- Comment widget-ы
-- <script src="http://js-kit.com/for/имя_сайта/comments.js"></script>
-- или
-- <script type="text/javascript" src="http://disqus.com/forums/имя_сайта/embed.js"></script>
-- т.е. в тегах script проверять домен домен src на
-- js-kit.com, disqus.com, www.intensedebate.com, pub.widgetbox.com,
-- letsgodynamic.com
-- есть <script> var disqus_shortname = 'brandon-si' … disqus_identifier

-- Для htmlcommentbox надо внутри тега script искать такой текст:
-- setAttribute("src", "http://www.htmlcommentbox.com/jread?page="
-- наверное есть еще какие нить widget-ы

-- | Максимально консервативное определение того, что ссылка является фидом
-- данного сайта.
feedPriority root href
    | h `elem` ["feeds.feedburner.com", "feeds2.feedburner.com"] = Just 3
    | h == hostNameFromUrlT root
    , Just (T.pack -> p) <- urlPathAndQuery u
    , regexTest feedPath p || p == "/feeds/posts/default" = Just 2
    | otherwise = Nothing
    where h = hostNameFromUrlT $ T.pack u
          u = relUri href (T.unpack root)

feedPath = "^/(feed|rss|rss2|rss20|atom|rdf)(\\.xml)?$"

selectCommentsFeed generator url feeds =
    checkFeedUrl $
    process True  url id list $
    process False generator id generatorList $
    process True url not blackList $
    feeds
    where feedUrl =
              T.concat [url, if T.last url == '/' then "" else "/", "feed"]
          checkFeedUrl next = case filter (feedUrl `T.isPrefixOf`) feeds of
              [] -> next
              f -> f
          process allowEmpty what filtCond l next = go l
              where go [] = next
                    go ((regex, feedRegex) : xs)
                        | regexTest regex what
                        , f <- filter (filtCond . regexTest feedRegex) feeds
                        , not (null f) || allowEmpty
                            = f
                        | otherwise = go xs

          h a b = (httpRegex a, httpRegex b)
          list =
              [ h "blogspot\\.com/[0-9]+/[0-9]+/.*"
                  -- http://vshabanov-ru.blogspot.com/2006/10/blog-post_25.html
                  "blogspot\\.com/feeds/[0-9]+/comments/default"
                  -- http://vshabanov-ru.blogspot.com/feeds/4978599246845228377/comments/default
              , h "juick\\.com/.+/[0-9]+"
                  -- http://juick.com/dmz/1483437
                  "juick\\.com/[0-9]+"
                  -- http://rss.juick.com/1483437
              , h "wordpress\\.com/[0-9]+/[0-9]+/[0-9]+/.*"
                  -- http://codemate.wordpress.com/2008/12/11/dbextvim-cheat-sheet/
                  "wordpress\\.com/[0-9]+/[0-9]+/[0-9]+/.*/feed"
                  -- http://codemate.wordpress.com/2008/12/11/dbextvim-cheat-sheet/feed/
              ]
          generatorList =
              [ ( "blogger"
                , "feeds/[0-9]+/comments/default" )
                -- http://blog.johantibell.com/feeds/531175054778293901/comments/default
                -- все эти feed это тупо url+"feed/"
              , ( "wordpress"
                , "[0-9]+/[0-9]+/.*/feed" )
                -- http://coder.bsimmons.name/blog/2011/08/a-brief-tutorial-introduction-to-fclabels-1-0/feed/
              , ( "wordpress"
                , "[0-9]+/.*/feed" )
                -- http://comonad.com/reader/2011/what-constraints-entail-part-1/feed/
              , ( "wordpress"
                , "archives/[0-9]+/feed" )
                -- http://therning.org/magnus/archives/1003/feed
              , ( "wordpress"
                , "\\?feed=rss2&p=[0-9]+$" )
                -- http://blog.felipe.lessa.nom.br/?feed=rss2&p=68
              ]
          -- тут мы вырезаем фиды на блоги из постов
          blackList =
              [ h "tumblr.com/post/[0-9]+/.*"
                  -- http://controlflow.tumblr.com/post/2362831115/ilmerge
                  "tumblr.com/rss"
                  -- http://controlflow.tumblr.com/rss
              ]


linkFeedAttr atts = listToMaybe $ map snd $
--    sortBy (comparing fst) -- сортируем, чтобы atom был в начале?
    [ (t, feed) |
      ("rel", rel) <- atts
    , rel == "alternate" || rel == "alternative" || rel == "replies"
    , ("type", t) <- atts
    , t == "application/rss+xml" || t == "application/atom+xml"
    , ("href", feed) <- atts
    ]
linkSelfAttr (filter ((`elem` ["rel", "type", "href"]) . fst) -> atts)
    | length atts >= 3 = self >> okType >> href
    | length atts == 2 = self >> href
    | otherwise = Nothing
    where href = lookup "href" atts
          self = guard $ ("rel", "self") `elem` atts
          okType = guard $ ("type", "application/rss+xml") `elem` atts
             || ("type", "application/atom+xml") `elem` atts
linkHtmlAttr (filter ((`elem` ["rel", "type", "href"]) . fst) -> atts)
    | length atts >= 3 = alternate >> okType >> href
    | length atts == 2 = (alternate >> href) <|> (okType >> href)
    | length atts == 1 = href
    | otherwise = Nothing
    where href = lookup "href" atts
          alternate = guard $ ("rel", "alternate") `elem` atts
          okType = guard $ ("type", "text/html") `elem` atts
             || ("type", "application/xhtml+xml") `elem` atts


--process = start
--    where start (TagOpen ""

-- судя по примерам, достаточно в первых 2-3 TagOpen поискать html
-- и с фидами примерно то же самое -- их можно по feed посмотреть
-- для начала можно html определять и не разгребать -- уже легче будет

-- после всех ?xml…
-- и всех !… (включая doctype)
-- первый тег
--   html -> понятно что
--   feed -> Atom http://hackage.haskell.org/packages/archive/feed/0.3.8/doc/html/src/Text-Atom-Feed-Import.html#elementFeed
--   rss -> RSS 2.0 0.9 http://hackage.haskell.org/packages/archive/feed/0.3.8/doc/html/src/Text-RSS-Import.html#elementToRSS
--   RDF -> RSS1 http://hackage.haskell.org/packages/archive/feed/0.3.8/doc/html/src/Text-RSS1-Import.html#elementToFeed

-- readTestTags = fmap (take 5 . go . lines) $ readFile "test_feed_tags.txt"
--     where go [] = []
--           go (_:f:tags) = (f, map (read Ttake 7 tags) : go (drop 7 tags)

-- testTags = forM_ testFeedURLs $ \ u -> do
--     et <- downloadURL u
--     case et of
--         Right t -> do
--             putStrLn "======================="
--             putStrLn u
--             mapM_ print $ take 7 $ parseTags t
--         Left _ -> return ()
-- x = fmap (take 5 . parseXML . either id id) $ downloadURL
--     "http://caml.inria.fr/news.en.rss"

-- toUnicode s = TL.unpack $ ensureUtf8Xml s

crawlRSS :: TURL -> [Tag Text] -> ParseResult
crawlRSS baseUri = go defaultFeedMsg [] . dropWhile (not . channel)
    where channel (TagOpen c _) = c == "channel" || c == "chanel"
          channel _ = False
          go !f !m tags = case tags of
              TagOpen "item" _  : tl -> item f m defaultFeedMsg tl
              (channel -> True) : tl -> go f m tl
              TagOpen "image" _ : tl -> image m f tl
              _ | Just (tag, tags, a, dat, tl) <- tagOpen tags ->
                  go (processTag tag tags a dat f) m tl
              (_ : tl)                 -> go f m tl
              []                       -> PRFeed baseUri f (reverse m)

          -- TODO: что-то очень похоже с go
          item !f !m !ef tags = case tags of
              TagClose "item" : tl      -> exit tl
              tl@(TagOpen "item" _ : _) -> exit tl  -- а вдруг
              _ | Just (tag, tags, a, dat, tl) <- tagOpen tags ->
                  item f m (processTag tag tags a dat ef) tl
              (_ : tl) -> item f m ef tl
              [] -> exit []
              where exit = go f (ef : m)

          -- куча копипасты
          image !m !ef tags = case tags of
              TagClose "image" : tl -> go ef m tl
              TagClose "item" : tl      -> exit tl
              tl@(TagOpen "item" _ : _) -> exit tl  -- а вдруг
              _ | Just (tag, _, a, dat, tl) <- tagOpen tags ->
                  image m (processImageTag tag a dat ef) tl
              (_ : tl) -> image m ef tl
              [] -> exit []
              where exit = go ef m

          processImageTag tag a dat =
              case tag of
                  "url"  -> link LKAuthorPic dat
                  _ -> dbg tagMsg
              where tagMsg =
                      "RSS image tag " <> tag <> " " <> showAttrs a
                                      <> " = " <> dat
                    link k l = handled $ addLink k l
                    handled x =
                        dbg ("<font color=\"lightgray\">"
                              <> tagMsg <> "</font>") . x

          processTag tag tags a dat = do
              case tag of
                  "guid" -> handled $ setGuid dat
                  "dc:identifier" -> handled $ setGuid dat
                  "title" -> setSubject dat
                  "author"
                      | Just p <- lookup "flickr:profile" a ->
                          setAuthor dat .
                          flickBuddyicon p .
                          link LKAuthor p
                  "author" -> setAuthor dat
                  "dc:creator" -> setAuthor dat
                  "dublincore:creator" -> setAuthor dat
                  "itunes:author" -> setAuthor dat
                  "lj:journal" -> setAuthor dat
                  "dc:date" ->
                      handled $ setPublishedTime $ readRfc3339 $ T.unpack dat
                  "dublincore:date" ->
                      handled $ setPublishedTime $ readRfc3339 $ T.unpack dat
                  "pubdate" ->
                      handled $ setPublishedTime $ readRfc822 $ T.unpack dat
                  "lastbuilddate" ->
                      handled $ setUpdatedTime $ readRfc822 $ T.unpack dat
                  "description" -> setSummary dat
                  "content:encoded" -> setBody dat
--                  "itunes:summary" -> setSummary dat
-- может затереть нормальный summary
                  "category"
                      | not ("http://gdata.youtube.com" `T.isPrefixOf` dat)
                          -> handled $ addTag dat
                  ((`elem` ["dc:subject", "dublincore:subject"]) -> True)
                      | "feeds.pinboard.in/" `T.isInfixOf` baseUri ->
                          handled $ addTags $ T.words dat
                          -- у pinboard сразу все теги и через пробелы
                      | otherwise -> handled $ addTag dat
                  "comments" ->
                      link LKCommentsHtml dat
                  "wfw:commentrss" ->
                      link LKFeed dat
                  a | a `elem` [ "slash:comments", "lj:reply-count"
                               , "thr:total"] ->
                      handled $ setCommentsCount dat
                  "link" ->
                      link LKLink dat
--                   "atom:link" | Just f <- linkSelfAttr a ->
--                       link LKLink f
-- в rss смотрим только на wfw:commentsRss, линк на фид не нужен
                  "yablogs:author"
                      | Just url <- lookup "url" a ->
                          link LKAuthor url .
                          (if dat == "" then id else setAuthor dat)
                  "yablogs:more" ->
                      link LKNext $ dat -- B.replace "search.xml" "search.rss" dat
                      -- TODO: перевести на bytestring
                  "yablogs:journal"
                      | Just url <- lookup "url" a ->
                          -- TODO: здесь я пользуюсь тем, что этот тег
                          -- идет после wfw:commentrss, но может идти
                          -- и раньше
                          -- TODO: использовать также как source в
                          -- atom для отсекания постов-комментов
                          handled $ rmLink LKFeed
                          -- удаляем wfw:commentrss т.к. он ведет на
                          -- яндексовский поиск по комментам, а я
                          -- лучше по link-у посмотрю (по guid нельзя,
                          -- там может tag:blogger.com … вылезти)
                  l | ":origlink" `T.isSuffixOf` l ->
--                      setURL dat
                      link LKOrigLink dat
                  l | "link" `T.isSuffixOf` l
                    , Just "hub" <- lookup "rel" a
                    , Just url <- lookup "href" a ->
                      link LKHub url
                  _ ->
                      processMediaTag tag tags a dat
                      -- dbg tagMsg
              where tagMsg =
                        T.concat ["RSS tag ", tag, " ", showAttrs a, " = ", dat]
                    link k l = handled $ addLink k l
                    handled x =
                        dbg ("<font color=\"lightgray\">"
                              <> tagMsg <> "</font>") . x

processMediaTag tag tags a dat =
    case tag of
        "media:keywords" -> addCommaTags dat
        "itunes:keywords" -> addCommaTags dat
        "itunes:duration" -> setDuration dat
        "media:title" -> modMedia $ setMTitle dat
        "media:description" -> modMedia $ setMDescription $
            if | Just "plain" <- lookup "type" a -> htmlfyYoutubeDescription id dat
               | Nothing <- lookup "type" a -> htmlfyYoutubeDescription id dat
               | otherwise -> dat
        "itunes:category"
            | Just c <- lookup "text" a ->
                addTags [c]
        "itunes:image"
            | Just u <- href -> addImage u
        "image"
            | TagOpen "url" _ : TagText u : _ <- filter notSpace tags
                -> addImage u
            | otherwise -> addImage dat
        "media:category"
            | Just "urn:flickr:tags" <- lookup "scheme" a ->
                addTags $ T.words dat
        "media:content"
            | Just g <- gravatar -> g
             -- оно есть у 340 из 350 wordpress блогов
        "media:thumbnail"
            | Just g <- gravatar -> g
             -- github использует media:thumbnail для картинок пользователей
        "enclosure"
            | Just g <- gravatar -> g
             -- даже enclosure бывает с gravatar (когда кросспостят с wordpress)
        "media:content"
            | Just u <- url -- игнорируем все, кроме url.
            -> modMedia $ addMContent (u, noUrlA, childMedia)
        "media:thumbnail"
            | Just u <- url
            , not (looksLikeLogo u)
            -> modMedia $ addMThumbnail (u, noUrlA)
        "media:group" -> modMedia $ addMGroup childMedia
        "link"
            | Just "enclosure" <- lookup "rel" a
            , Just u <- href
            -> processMediaTag "enclosure" tags (("url",u):a) dat
        "enclosure"
            | Just u <- url
            , u `notElem` ["/assets/images/icon-stub.png"]
              -- у 3dnews первая картинка заглушка, вырезаем
              -- (не очень хорошо, но несколько картинок как newsblur показывать
              -- не хочется, т.к. они часто дублируются, а определять,
              -- где есть картинка, а где нет трудоемко)
            -> addEnclosure u noUrlA
        "feedburner:origenclosurelink" ->
            setEnclosureOrigLink dat
        "icon" ->
            addLink LKAuthorPic dat
        "webfeeds:icon" ->
            addLink LKAuthorPic dat
        _ -> id
    where urlLookup n = case lookup n a of
                          Just (T.strip -> u)
                              | u /= "" -> Just u
                          _ -> Nothing
          noUrlA = filter ((/= "url") . fst) a
          url = urlLookup "url"
          href = urlLookup "href"
          gravatar
              | Just u <- url
              , [[_,id]] <-
                  regexGet "gravatar.com/avatar/([0-9a-fA-F]{32})" u
              = Just $ addLink LKAuthorPic $ T.concat
                [ "https://secure.gravatar.com/avatar/", id, "?d=404&r=g" ]
              | Just u <- url
              , regexTest "avatars.*\\.githubusercontent\\.com" u
              = Just $ addLink LKAuthorPic $ T.replace "s=30" "s=96" u
              | otherwise = Nothing
          childMedia = go defaultFeedMsg tags
              where go !m [] = fmMedia m
                    go !m tags
                        | Just (t, tags, a, txt, tl') <- tagOpen tags =
                            go (processMediaTag t tags a txt m) tl'
                    go !m (_:tl) = go m tl

looksLikeLogo u =
    path /= "" && (HS.fromList parts `HS.difference` ignored == HS.empty)
    where path = T.toLower $ maybe u T.pack $ urlPathAndQuery $ T.unpack u
          parts = T.split (`elem` ("/.-_?=&" :: [Char])) path
          ignored =
              HS.fromList
              [ "", "assets", "images", "files", "uploads"
              , "logo", "rss", "stub", "icon", "favicon"
              , "png", "jpg", "jpeg", "gif", "webp"
              ]

flickBuddyicon p
    | [[_,uid]] <- regexGet "people/([0-9]+@N[0-9]+)" p =
        addLink LKAuthorPic $ T.concat ["https://www.flickr.com/buddyicons/", uid, ".jpg"]
    | [[_,uid]] <- regexGet "people/([^/]+)" p =
        addLink LKAuthorPic $ T.concat ["https://www.flickr.com/buddyicons/", uid]
    | otherwise = id

showAttrs :: [(Text, Text)] -> Text
showAttrs = T.concat . intersperse ", " .
            map (\ (a,v) -> T.concat ["(\"", a, "\", \"", v, "\")"])

crawlAtom :: TURL -> [Tag Text] -> ParseResult
crawlAtom baseUri = go defaultFeedMsg []
    where go !f !m tags = case tags of
              TagOpen "entry" a : tl ->
                  entry f m (addBaseUri a defaultFeedMsg) tl
                        -- TODO: для корня тоже надо автора обрабатывать
              TagOpen "author" _ : tl    -> author id False True f m f tl
              _ | Just (tag, tags, a, dat, tl) <- tagOpen tags ->
                  go (processTag tag tags a dat f) m tl
              (_ : tl) ->
                  go f m tl
              [] ->
                  PRFeed
                  (maybe baseUri (`relUriNoNormalizeT` baseUri)
                   $ lookup LKBaseUri (fmLinks f))
                  f (reverse m)

          addBaseUri a m
              | Just uri <- lookup "xml:base" a = addLink LKBaseUri uri m
              | otherwise = m

          -- TODO: что-то очень похоже с go
          entry !f !m !ef tags = case tags of
              TagClose "entry" : tl      -> exit tl
              tl@(TagOpen "entry" _ : _) -> exit tl  -- а вдруг
              TagOpen "author" _ : tl    -> author id False False f m ef tl
              _ | Just (tag, tags, a, dat, tl) <- tagOpen tags ->
                  if tag == "source" then
                      let htmls = [ f | TagOpen "link" a <- tags
                                  , Just f <- [linkHtmlAttr a] ]
                          (guid, title, html) = go "" "" "" tags
                          go !i !t !h ts0 = case ts0 of
                              [] -> (i,t,h)
                              (TagOpen "link" a : ts)
                                  | h == ""
                                  , Just f <- linkHtmlAttr a ->
                                      go i t f ts
                              (TagOpen "id" _ : ts)
                                  | (txt, _, ts') <- tagData "id" [] ts ->
                                      go txt t h ts'
                              (TagOpen "title" _ : ts)
                                  | (txt, _, ts') <- tagData "title" [] ts ->
                                      go i txt h ts'
                              (_:ts) -> go i t h ts
                          ex = if html /= "" then
                                   addLink' True LKAuthor html
                               else
                                   id
                      in
--                      error $ show htmls
                      author ex True False f m
                          (ef { fmSource = Just (guid, title, html) }) $
                          takeWhile notTagCloseAuthor
                          (drop 1 $ dropWhile notTagOpenAuthor tags)
                          ++ (TagClose "author" : tl)
                      -- вырезаем author
                  else
                      entry f m (processTag tag tags a dat ef) tl
              (_ : tl) -> entry f m ef tl
              [] -> exit []
              where exit = go f (ef : m)
                    notTagOpenAuthor (TagOpen "author" _) = False
                    notTagOpenAuthor _ = True
                    notTagCloseAuthor (TagClose "author") = False
                    notTagCloseAuthor _ = True

          -- куча копипасты
          author ex !noOver !root !f !m !ef tags = case tags of
              TagClose "author" : tl
                  | root -> go (ex ef) m tl
                  | otherwise -> entry f m (ex ef) tl
              TagClose "entry" : tl      -> exit tl
              tl@(TagOpen "entry" _ : _) -> exit tl  -- а вдруг
              _ | Just (tag, _, a, dat, tl) <- tagOpen tags ->
                  author ex noOver root f m (processAuthorTag noOver tag a dat ef) tl
              (_ : tl) -> author ex noOver root f m ef tl
              [] -> exit []
              where exit | root = go (ex ef) m
                         | otherwise = go f (ex ef : m)

          processAuthorTag noOver tag a dat =
              -- author может быть в <source>, а source менее приоритетный
              -- в нем может быть name пустой например
              case tag of
                  "name" -> handled $ setAuthor' noOver dat
                  "lj:journal" -> setAuthor dat
                  "uri"  -> link LKAuthor dat
                  "email" -> handled $ setAuthorEmail' noOver dat
                  "gd:image" | Just src <- lookup "src" a ->
                                           link LKAuthorPic src
                  "flickr:buddyicon" -> link LKAuthorPic dat
                  "link" | Just (T.toLower -> "image") <- lookup "rel" a
                         , Just href <- lookup "href" a ->
                                        link LKAuthorPic href
                  _ -> dbg tagMsg
              where tagMsg =
                      "Atom author tag " <> tag <> " " <> showAttrs a
                                      <> " = " <> dat
                    link k l = handled $ addLink' noOver k l
                    handled x =
                        dbg ("<font color=\"lightgray\">"
                              <> tagMsg <> "</font>") . x

          processTag tag tags a dat =
              case tag of
                --  "atom:source" -- если фид откуда-нить скопирован
                --  то будет инфа об исходном фиде, тады стоит покравлить html
                --  в russian lambda planet/ocaml planet такое соблюдается

                -- а в rss с этим лажа -- haskell planet
                -- но, в принципе если идет линк на другой сайт,
                -- и своих комментов нет, то можно брать комменты с него
                  "id"
                      | Just guid <- lookup "gr:original-id" a ->
                          handled $ setGuid dat . addLink LKGROriginalId guid
                      | otherwise ->
                          handled $ setGuid dat
                  "title" -> setSubject dat
                  "published" -> handled $ setPublishedTime $
                                 readRfc3339 $ T.unpack dat
                  "updated" -> handled $ setUpdatedTime $
                               readRfc3339 $ T.unpack dat
                  "content" -> setBody dat
                  "summary" -> setSummary dat
                  "category"
                      | Just c <- lookup "term" a
                      , not ("http://gdata.youtube.com" `T.isPrefixOf` c)
                      -> handled $ addTag c
                  a | ":origlink" `T.isSuffixOf` a ->
--                      setURL dat
                      link LKOrigLink dat
                  "link" | Just f <- linkFeedAttr a ->
                      link LKFeed f
                  "link" | Just f <- linkHtmlAttr a ->
--                      setURL f
                      link LKLink f
                      -- был LKCommentsHtml, но мы не можем здесь отличить
                      -- comments от ссылки на сам коммент
                  "link" | Just f <- linkSelfAttr a ->
                      link LKBaseUri f
                  "link" | Just "next" <- lookup "rel" a
                         , Just "application/atom+xml" <- lookup "type" a
                         , Just u <- lookup "href" a ->
                      link LKNext u
                  l | "link" `T.isSuffixOf` l
                    , Just "hub" <- lookup "rel" a
                    , Just url <- lookup "href" a ->
                      link LKHub url
                  a | a `elem` [ "thr:total"
                               , "slash:comments", "lj:reply-count"] ->
                      handled $ setCommentsCount dat
                  "lj:journal"
                      | Just author <- lookup "username" a ->
                          handled $ setAuthor author
                  "lj:poster" ->
                      findUpic $ parseTagsT $ T.encodeUtf8 dat
                  _ ->
                      processMediaTag tag tags a dat
                      -- dbg tagMsg
              where findUpic [] = id
                    findUpic (TagOpen "name" _ : TagText a : xs) =
                        link LKAuthor
                        (if "_" `T.isPrefixOf` a then
                             T.concat ["https://users.livejournal.com/", a, "/"]
                        else
                             T.concat ["https://", a, ".livejournal.com/"]) .
                        setAuthor a . findUpic xs
                    findUpic (TagOpen "lj:userpic" _ : TagText p : xs) =
                        link LKAuthorPic p . findUpic xs
                    findUpic (_:xs) = findUpic xs
                    tagMsg =
                      T.concat ["Atom tag ", tag, " ", showAttrs a, " = ", dat]
                    link k l = handled $ addLink k l
                    handled x =
                        dbg ("<font color=\"lightgray\">"
                              <> tagMsg <> "</font>") . x

tagOpen (TagOpen t a : tl) | t /= "br" = Just (t, tags, a, txt, tl')
    where (txt, tags, tl')
              | Just "xhtml" <- lookup "type" a =
                  let (x, tc) = span (/= TagClose t) tl in
                  (renderTagsT x, x, if null tc then [] else tail tc)
              | otherwise = tagData t [] tl
tagOpen _ = Nothing

tagData tag = go []
    where go acc tags _ | acc `seq` tags `seq` False = undefined
          go acc tags (tag@(TagText t) : ts) = go (t : acc) (tag:tags) ts
          go acc tags (t@(TagOpen n _) : ts) =
--              go (("&lt;" <> n <> "&gt;") : acc) ts
              go (renderTagsT [t] : acc) (t:tags) ts
          go acc tags (t@(TagClose n) : ts)
             | n == tag = ret acc tags ts
--              | otherwise = go (("&lt;/" <> n <> "&gt;") : acc) ts
             | otherwise = go (renderTagsT [t] : acc) (t:tags) ts
          go acc tags (t : ts) = go acc (t:tags) ts
          go acc tags [] = ret acc tags []
          ret acc tags ts = (T.concat $ reverse acc, reverse tags, ts)


--     -- даты обычно одинаковые
--     -- и в разных форматах:
--     --   Mon, 08 Mar 2010 17:07:45 -0500  (ltu)
--     --   Sat, 06 Feb 2010 11:03:53 GMT (rsdn, lj)
--     --   Tue, 05 Jan 2010 23:08:31 +0000 (conal) rfc822
--     --   2008-07-03T04:26:58.117+04:00 (blogger) rfc3339/8601 Atom
--     --   2010-03-09T23:43:19.821Z (blogger)
--     --   2010-03-08T13:54:36.750562-07:00-0700

getAhrefs tags = [ href | TagOpen "a" atts <- tags
                        , ("href", href) <- atts ]
