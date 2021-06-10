-- | Утилиты, использующиеся в custom parser-ах
{-# LANGUAGE TupleSections, BangPatterns, ViewPatterns, RecordWildCards,
             OverloadedStrings, LambdaCase
#-}
module Parser.Custom where

import Data.List
import Data.Function
import Data.Either
import Lib.UrTime
import Lib.Regex
import Lib.FastChar (emptyText)
import Text.HTML.TagSoup hiding (parseTags, renderTags, (~/=), (~==))
import qualified Text.HTML.TagSoup as OrigTagSoup
import Text.HTML.TagSoup.Fast
import Parser.Types
import URL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Lib.Json
import qualified Data.Vector as V
import Data.Char
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Lib.StringConversion
import Control.Monad
import Parser.DOM

import qualified Data.CSS.Syntax.Tokens as CSS

data CustomParser
    = CPTags (UrTime -> TURL -> [Tag T.Text] -> ParseResult)
    | CPBS (UrTime -> TURL -> B.ByteString -> ParseResult)
    | CPJSON (UrTime -> TURL -> JSON.Value -> ParseResult)
    | CPNone

type TransformedUrl =
    ( T.Text
    , Maybe ( T.Text
            , B.ByteString -> B.ByteString -> B.ByteString)
      -- u2 используется в transformGooglePlusPosts
    , Maybe B.ByteString)
type GetEnv = T.Text -> IO (Maybe T.Text)

-- TODO: сделать [TURL -> Maybe (IO TransformedUrl, CustomParser)],
-- а то везде regex-ы по два раза повторяются
type CustomParsers =
    ([THostName]
    ,[(TURL -> Bool, TURL -> GetEnv -> IO TransformedUrl, CustomParser)])

mkCustomParsers a b = (a, b) :: CustomParsers
noChange u = const $ return (u, Nothing, Nothing)
noScan u = const $ return ("", Nothing, Nothing)
changeUrl f u = const $ return (f u, Nothing, Nothing)
changeUrlEnv f u e = do
    u' <- f u e
    return (u', Nothing, Nothing)
changeUrlIO f u = const $ fmap (, Nothing, Nothing) $ f u
post f u = const $ return (u', Nothing, p)
    where (u', p) = f u
mkHttps u = changeUrl httpToHttps u

setEnv [] pr = pr
setEnv e (PRSetEnv e0 pr) = setEnv (e <> e0) pr
setEnv e pr = PRSetEnv (nubBy ((==) `on` fst) e) pr

combineDownloadsBS f = CombineDownloads $ \ drs ->
    case partitionEithers drs of
        ([], d) -> f d
        ((e:_), _) -> PRError e

------------------------------------------------------------------------------
-- JSON утилиты

jsonParser :: (JSON.Value -> JSON.Parser a) -> B.ByteString -> Either String a
jsonParser f dat = do
    x <- eitherDecodeJson dat
    JSON.parseEither f x

jsonArray x = do
    JSON.Array (V.toList -> l) <- return x
    return l
arr n x = jsonArray =<< HM.lookup n x
int n x = do
    JSON.Number (truncate -> i) <- HM.lookup n x
    return i
bool n x = do
    JSON.Bool b <- HM.lookup n x
    return b
str n x = do
    JSON.String s <- HM.lookup n x
    return s
obj n x = do
    JSON.Object o <- HM.lookup n x
    return o
jsonObj x = do
    JSON.Object o <- return x
    return o
withObj f x = f =<< jsonObj x

whScaledByMaxWidth roundFunc maxWidth d = do
    w <- int "width" d
    h <- int "height" d
    let r :: Int -> Double
        r = fromIntegral
        ti = T.pack . show
    return
        [("width", ti maxWidth)
        ,("height", ti $ min maxWidth $ roundFunc $ r maxWidth / r w * r h)]

------------------------------------------------------------------------------
-- утилиты

dropLast [] = []
dropLast [_] = []
dropLast (x:xs) = x : dropLast xs

dropLastTwo [] = []
dropLastTwo [_,_] = []
dropLastTwo (x:xs) = x : dropLastTwo xs

hashSetNub' f xs = go HS.empty xs
    where go acc [] = []
          go acc (x:xs)
              | HS.member k acc = go acc xs
              | otherwise = x : go (HS.insert k acc) xs
              where k = f x
hashSetNub xs = hashSetNub' id xs

groupByN n [] = []
groupByN n is = batch : groupByN n next
    where (batch, next) = splitAt n is

editTagSoup f = renderTagsT . f . parseTagsT . tbs

addHashTags' rule minLength hashTagLink = fixNonLinkedText $
    regexReplace'' TagText rule
    (\ (all:b:t:_) xs ->
         let (hd,tl) = T.span (\ c -> isAlphaNum c || c == '_') t in
         if T.any isAlpha hd && T.length hd >= minLength then
             TagText b : TagOpen "a" [("href", hashTagLink hd)] :
             TagText "#" : TagText hd : TagClose "a" : TagText tl : xs
         else
             TagText all : xs)

addHashTags = addHashTags'
    "(^|[[:space:][:punct:]])#(([^#[:space:][:punct:]]|_)+)"
-- instagram позволяет #делать#хештеги#без#пробелов
addHashTagsInstagram = addHashTags'
    "()#(([^#[:space:][:punct:]]|_)+)"

addUsernames minLength usernameLink = fixNonLinkedText $
    regexReplace'' TagText
    "(^|[[:space:][:punct:]])@(([^@#[:space:][:punct:]]|[_.])+)"
    (\ (all:b:t:_) xs ->
         let (hd,tl) = T.span (\ c -> isAscii c && (isAlphaNum c || c == '_' || c == '.')) t in
         if T.any isAlpha hd && T.length hd >= minLength then
             TagText b : TagOpen "a" [("href", usernameLink hd)] :
             TagText "@" : TagText hd : TagClose "a" : TagText tl : xs
         else
             TagText all : xs)

-- относится только к VK, но код очень похожий, пусть рядом лежит
addGroupHashTags minLength groupHashTagLink = fixNonLinkedText $
    regexReplace'' TagText
    "(^|[[:space:][:punct:]])#(([^#@[:space:][:punct:]]|_)+)@(([^#@[:space:][:punct:]]|[_.])+)"
    (\ (all:b:t:_:g:_) xs ->
         let (tag,tag') = T.span (\ c -> isAlphaNum c || c == '_') t
             (group,group') = T.span (\ c -> isAscii c && (isAlphaNum c || c == '_' || c == '.')) g
             ok x = T.any isAlpha x && T.length x >= minLength
         in
         if ok tag && ok group && tag' == "" then
             TagText b : TagOpen "a" [("href", groupHashTagLink group tag)] :
             TagText (T.concat ["#", tag, "@", group]) :
             -- группа ВК подсвечивается только если существует,
             -- даже если там только английские буквы, так что пока забьём
             -- и будем подсвечивать любые группы
             TagClose "a" : TagText group' : xs
         else
             TagText all : xs)

-- этот код работает только с корректным HTML, где все теги закрываются
-- в реальности может быть что угодно:
--   <a href=a><a href=b>тут ссылка на b</a> а тут обычный текст </a>
--   <div><a href=x>ссылка на x</div> ссылка на х на новой строке!</a>
-- логика такая, что ссылка заканчивается по первому </a>, независимо от
-- остальных тегов
fixNonLinkedText replace = go []
    where go _ [] = []
          go cl (t@(TagOpen n a) : ts)
              | ignoreIn n a = t : go (n:cl) ts
          go (c:cs) (t@(TagClose n):ts)
              | c == n = t : go cs ts
          go [] (TagText t : ts) =
              replace t ++ go [] ts
          go cl (t:ts) = t : go cl ts
          ignoreIn "a" _ = True
          ignoreIn _ (lookup "class" -> Just c) =
              c `elem` ["bqrShareCaption", "bqrShareDescription"]
          ignoreIn _ _ = False

fixTagText replace = go
    where go [] = []
          go (TagText t:ts) = replace t ++ go ts
          go (t:ts) = t : go ts

newlinesToBr =
    merge . fixTagText (regexReplace'' TagText "\n" $ \ _ xs -> br : xs)
    where merge [] = []
          merge (TagOpen "br" [] : ts) = mergeBr False ts
          merge (t : ts) = t : merge ts
          mergeBr dbr = \ case
              TagOpen "br" [] : ts -> mergeBr True ts
              TagText t : ts | emptyText t -> mergeBr dbr ts
              ts -> (if dbr then (br:) else id) $ br : merge ts
          br = TagOpen "br" []

htmlfyYoutubeDescription f =
    editTagSoup (newlinesToBr . f) .
    regexReplace "\n([ \t\v]*\n)+" "\n\n" .
    T.replace "\r" "\n" .
    T.replace "\r\n" "\n"
--    T.replace "<" "&lt;" . T.replace ">" "&gt;"
    -- youtube не позволяет угловые скобки, но мы используем этот же код в
    -- media:description
    -- renderTagsT сделает правильный html escaping

notSpace (TagText t) = not $ emptyText t
notSpace _ = True

(~/=) :: Tag T.Text -> Tag T.Text -> Bool
(~/=) = (OrigTagSoup.~/=)

(~==) :: Tag T.Text -> Tag T.Text -> Bool
(~==) = (OrigTagSoup.~==)

------------------------------------------------------------------------------
-- Значения по-умолчанию

defaultMedia =
    Media [] [] [] Nothing Nothing

defaultFeedMsg =
    FeedMsg
    { fmGuid = ""
    , fmAuthor  = ""
    , fmAuthorEmail = ""
    , fmSubject = ""
    , fmPublishedTime = Nothing
    , fmUpdatedTime = Nothing
    , fmSummary = ""
    , fmBody = ""
    , fmTags = []
    , fmLinks = []
    , fmCommentsCount = Nothing
    , fmEnclosures = []
    , fmEnclosureLinks = []
    , fmImages = []
    , fmMedia = defaultMedia
    , fmDuration = Nothing
    , fmSource = Nothing
    , fmLinkNotUnique = False
    }

------------------------------------------------------------------------------
-- Утилиты для вставки shared video/img

sharedLink imgSrc = sharedLink' (fmap (,[]) imgSrc)

sharedLink' imgSrc url title description = twoColumns image text
    where image
              | Just (i, wh) <- imgSrc =
                  renderTagsT $ aimgTags' wh url i "bqrShareImage"
              | otherwise = ""
          text =
              renderTagsT
              [ TagOpen "a" [("href", url)
                            ,("class", "bqrShareLink")]
              , TagText title
              , TagClose "a"
              , TagOpen "br" []
              , TagOpen "a" [("href", url)
                            ,("class", "bqrShareCaption")]
              , TagText (rmWwwPrefix $ T.pack $ hostNameFromUrl $ T.unpack url)
              , TagClose "a"
              ]
              <>
              T.concat ["<div class='bqrShareDescription'>"
                       ,description, "</div>"]

embeddedGoogleMaps cls (encodeURIComponentT -> c) addr =
    T.concat
    ["<iframe class='",cls,"' frameborder='0' scrolling='no' marginheight='0' marginwidth='0' src='https://maps.google.com/?q=", c, "&amp;ie=UTF8&amp;t=m&amp;z=15&amp;output=embed'></iframe><br /><a href='https://maps.google.com/?q="
    , c, "&amp;ie=UTF8&amp;t=m&amp;z=15&amp;source=embed' class='bqrMapAddressLink'>"
    , if addr /= "" then addr
      else "<small>View Larger Map</small>", "</a>"]

onFaviconError :: T.Text
onFaviconError = "this.src='http://bazqux.com/images/default_favicon.png';"

habrahabrToggleSpoiler :: T.Text
habrahabrToggleSpoiler = "bqrHabrahabrToggleSpoiler(this)"

twoColumns a b =
    ["<table cellpadding=0 cellspacing=0><tbody><tr>"
    ,"<td valign='top'>"
    ,a, "</td><td valign='top'>", b, "</td></tr></tbody></table>"]


aimgTags' :: [(Text, Text)] -> TURL -> TURL -> Text -> [Tag Text]
aimgTags' wh href src cls =
    [ TagOpen "a" [("href", href)]
    , TagOpen "img" ([("src", src), ("class", cls)] ++ wh)
    , TagClose "img", TagClose "a" ]

aimgTags = aimgTags' []

awhImgTags w h href src =
    aimgTags' [("width", showT w), ("height", showT h)] href src ""
wrapTag :: Text -> [(Text, Text)] -> [Tag Text] -> [Tag Text]
wrapTag n a x = TagOpen n a : x <> [TagClose n]
paragraph = wrapTag "p" []

renderLink = renderLink' []
renderLink' attrs link title =
    renderTagsT [ TagOpen "a" (("href", link):attrs), TagText title, TagClose "a" ]

editFirstTagAttrs f (TagOpen t as : ts) = TagOpen t (f as) : ts
editFirstTagAttrs _ ts = ts

removeAttr n as = filter ((/= n) . fst) as
addOrReplaceAttr n v as = (n,v) : removeAttr n as
makeFirstAttr n as = case lookup n as of
    Just v -> addOrReplaceAttr n v as
    _ -> as
addClass c = addOrModifyAttr "class" c add
    where add c0
              | c `elem` T.words c0 = c0
              | otherwise = T.concat [c, " ", c0]
removeClass c = mapMaybe $ \ case
    ("class", filter (/= c) . T.words -> cs)
        | null cs -> Nothing
        | otherwise -> Just ("class", T.unwords cs)
    a -> Just a
hasClass c as
    | Just cls <- lookup "class" as = c `elem` T.words cls
    | otherwise = False
addStyle "" = id
addStyle s =
    addOrModifyAttr "style" s (\ s0 -> T.concat [s0, "; ", s])
addStylePre "" = id
addStylePre s =
    addOrModifyAttr "style" s (\ s0 -> T.concat [s, "; ", s0])
addAttrIfNotExists a x = addOrModifyAttr a x id
addOrModifyAttr a x f as = go [] as
    where go _   [] = (a, x) : as
          go acc (x@(n, v):xs)
              | n == a    = reverse acc ++ (a, f v) : xs
              | otherwise = go (x:acc) xs

findCssUrl = go
    where go [] = Nothing
          go (CSS.Url u : ts) = Just u
          go (CSS.Function fn : CSS.String u : CSS.RightParen : ts)
              | T.toLower fn == "url" = Just u
          go (t : ts) = go ts

fixCssUrl f = go
    where go [] = []
          go (CSS.Url u : ts) =
              CSS.Url (f u) : go ts
          go (CSS.Function fn : CSS.String u : CSS.RightParen : ts)
              | T.toLower fn == "url" =
                  CSS.Url (f u) : go ts
          go (t : ts) = t : go ts

scalableStyle =
    "display: block; width: 100%; height: 0; \
    \position: relative; overflow: hidden"
innerStyle =
    "display: block; border: none; \
    \top: 0; left: 0; width: 100%; height: 100%; position: absolute"
aspectRatioPaddingBottom :: Double -> T.Text
aspectRatioPaddingBottom aspectRatio =
    T.concat ["padding-bottom: ", T.pack (show $ aspectRatio * 100), "%"]

withScalableStyle aspectRatio (TagOpen t as : ts) =
    [TagOpen "span"
        [("style", scalableStyle <> "; " <> aspectRatioPaddingBottom ar)]]
    ++ (TagOpen t (addStyle innerStyle as) : ts)
    ++ [TagClose "span"]
    where ar = fromMaybe (9/16) $ do
              r <- aspectRatio
              guard (validPaddingPercentage $ r * 100)
              return r
withScalableStyle _ ts = ts

validPaddingPercentage n = n >= 20 && n <= 200

-- Теги для вставки масштабируемых видео/iframe
embedVideo mime aspectRatio poster loop src =
    withScalableStyle aspectRatio $ embedVideo' [] mime poster loop src
embedVideoWH width height =
    embedVideo' [("width", showT width), ("height", showT height)]
embedVideo' attrs mime poster loop src =
    [ TagOpen "video" $
        [("controls","")]
        ++
        catMaybes
        [guard loop >> return ("loop","")
        ,("poster",) <$> poster]
        ++
        attrs
    , TagOpen "source"
        [("src", src), ("type", mime)]
    , TagClose "source"
    , TagClose "video"]
embedIframe src =
    withScalableStyle Nothing $ iframe id src
iframe f src =
    [TagOpen "iframe" $ f
     [("src", src)
     ,("allow", "autoplay; fullscreen; encrypted-media")
     ,("sandbox", "allow-same-origin allow-scripts allow-forms allow-popups allow-presentation")
      -- чтобы YouTube мог открыть видео в новой вкладке,
      -- нужен allow-popups
      -- allow-presentation -- для презентации на проекторах
     ,("allowfullscreen", "true")]
    ,TagClose "iframe"]

instagramPostId u
    | [[_,_,v]] <- rg "instagram\\.com/p/([0-9a-zA-Z_\\-]+)" u
    = Just v
    | otherwise = Nothing

youtubeVideoIdRE1 = rg "youtube(-nocookie)?\\.com/(watch\\?v=|embed/|v/)([0-9a-zA-Z_\\-]+)"
youtubeVideoIdRE2 = rg "youtu\\.be/([0-9a-zA-Z_\\-]+)"

youtubeVideoId u
    | [[_,_,_,_,v]] <- youtubeVideoIdRE1 u
      -- почему-то [\\-_] не обрабатывает '-', а [_\\-] обрабатывает
    = Just v
    | [[_,_,v]] <- youtubeVideoIdRE2 u
    = Just v
    | otherwise = Nothing

vimeoVideoIdRE = rg "vimeo\\.com/(channels/[^/]+/|video/|moogaloop\\.swf\\?clip_id=)?([0-9]+)"

vimeoVideoId u
    | [[_,_,_,v]] <- vimeoVideoIdRE u
    = Just v
    | otherwise = Nothing

youtubeEmbedUrl videoId =
    T.concat ["https://www.youtube.com/embed/", videoId]

embedYoutube videoId =
    renderTagsT $ embedIframe $ youtubeEmbedUrl videoId

vimeoEmbedUrl videoId = T.concat ["https://player.vimeo.com/video/", videoId]

embedVimeo videoId =
    renderTagsT $ embedIframe $ vimeoEmbedUrl videoId

dataBqrVideoPoster = "data-bqr-video-poster" :: T.Text
dataBqrVideoTitle  = "data-bqr-video-title" :: T.Text

embedInstagram link inner =
    T.concat
    [ "<blockquote class='instagram-media' data-instgrm-captioned data-instgrm-version='6' style='width:658px;padding:0;margin:0;color:inherit;border:none'>"
    , inner
    , renderTagsT [TagOpen "a" [("href", link)], TagClose "a"]
      -- instagram работает по последней ссылке,
      -- по-этому дублируем
    , "</blockquote>" ]

insertMsg fm = do
    authorPic <- lookup LKAuthorPic (fmLinks fm)
    authorLink <- lookup LKAuthor (fmLinks fm)
    link <- lookup LKLink (fmLinks fm)
    time <- fmPublishedTime fm
    -- TODO: нормально оформить
    let t = twoColumns
            (renderTagsT
             [ TagOpen "img"
               [("src", authorPic)
               ,("class", "bqrVKSharedPostAvatar")]
             , TagClose "img"])
            (renderTagsT
             [ TagOpen "a" [("href", authorLink)
                           ,("class", "bqrSharedPostAuthorLink")]
             , TagText (fmAuthor fm)
             , TagClose "a"
             , TagOpen "br" []
             ] <> sharedPostTime link time)
    return (T.concat $
            ["<p>"] ++ t ++ ["</p><p>", fmBody fm, "</p>"]
           ,fmEnclosures fm)

sharedPostTime link time@(UrTime sec _) =
    renderTagsT
    [ TagOpen "a" [("href", link)
                  ,("class", "bqrVKPostTime")
                  ,("data-time", T.pack $ show sec)]
    , TagText (T.pack $ formatUrTimeRfc822 time)
    , TagClose "a"
    ]

videoDuration duration =
    renderTagsT
    [TagOpen "br" []
    ,TagOpen "span" [("class", ytClass)], TagClose "span"
    ,TagOpen "br" []]
    where ytClass = T.intercalate "-"
                    ["bqrYouTube", T.pack (show duration)]
                    -- , views, likes, dislikes]

node n a ch = Node n a BlockLevel ch

pollTable = node "table" [("style", "width: 100%; border-collapse: collapse; border: none; margin-top: 0.5em"), ("border", "0")]
pollRow = node "tr" []
pollOptionPercent = node "td" [("class", "bqrPollOptionPercent"), ("style", "vertical-align: top; text-align: right; padding-right: 0.75em; font-weight: bold")]
pollOptionValue = node "td" []
pollOptionText = node "div" []
pollOptionBar' s = node "div" [("class", "bqrPollOptionBar"), ("style", "margin-top: 0.25em; margin-bottom: 0.55em; min-width: 4px; background: currentColor; border-radius: 2px; height: 4px;" <> s)] []
pollOptionBar p = pollOptionBar' $ "width: " <> showT (p * 100) <> "%"
