{-# LANGUAGE BangPatterns, ViewPatterns, RecordWildCards, OverloadedStrings,
             LambdaCase, TupleSections, MultiWayIf, PatternSynonyms #-}
-- | Обработка текста сообщений
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Preprocess
    ( fixMessageBody, firstWords, firstParagraph, firstWordsX, xmlText
    , xmlLinksImgsVideosAndText, fixHref, normalizeMsgLink
    , PreprocessSettings(..), fixMessageContent
    , proxyUrl, ProxyType(..), thumbnailWidth
    , newlinesToBr, editTagSoup
    , mimeByUrl, mimeByUrl'
--    , perfTest
    , hyphenateHtml, softHyphen, textToHyphenate
    ) where

import Data.Char
import Data.Ord
import Data.List
import Data.Maybe
import Control.Monad
import Control.Arrow (first, second)
import Control.Monad.State
import Control.Applicative
import Lib.ReadUtils
import Lib.StringConversion
import Lib.Regex
import Lib.Log
import Lib.FastChar (emptyText, emptyTextL)
import qualified Lib.LanguageDetector as LD
import qualified Lib.FastChar as FC
import Text.Printf
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as T (Text(..))
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.ICU.Normalize as ICU
import qualified Text.Hyphenation as Hyph
import Generated.DataTypes
import Riak (textKey)
import Resolvables ()
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import Parser.Custom -- (onFaviconError, habrahabrToggleSpoiler, httpToHttps, rmHttpPrefix, rmHttpPrefix', embedVideo, editTagSoup, newlinesToBr, vimeoVideoId, youtubeVideoId, youtubeEmbedUrl, vimeoEmbedUrl)
import qualified Data.CSS.Syntax.Tokens as CSS
import URL
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Network.URL as U
import qualified Data.PublicSuffix.Rules as PublicSuffix
import Network.Mime
-- import qualified Data.Attoparsec.Text as A
-- import qualified Hasmin.Parser.Internal as Hasmin
-- import qualified Hasmin.Class as Hasmin
-- import qualified Hasmin.Types.Declaration as Hasmin
--  ^ не схлопывает border, только border-style и пр., не анализирует display
--    на невалидные данные, неоптимизированный -- для offline обработки
import Lib.Hash (urlSafe_sha1)
import Parser.DOM
import Parser.CSS

fixMessageBody :: Maybe TURL -> URL -> B.ByteString -> [Tag T.Text]
fixMessageBody msgLink baseUri = fixAnchors . go . parseTagsT
    where -- превращаем несуществующие локальные #-ссылки во внешние
          fixAnchors t
              | HS.null ids = t
              | otherwise   = fix t
              where ids = HS.fromList
                        [a | TagOpen _ as <- t, (n, a) <- as
                        ,n == "id" || n == "name"]
                    fix [] = []
                    fix (TagOpen t as : ts) =
                        TagOpen t (fixHash as) : fix ts
                    fix (t : ts) = t : fix ts
                    fixHash = map $ \ case
                        (a, h)
                            | a == "href" || a == "usemap"
                            , Just i <- T.stripPrefix "#" h
                            , not (HS.member (decodeURIComponentT i) ids)
                            -> (a
                               ,fromMaybe (T.pack baseUri) msgLink <> "#" <> i)
                        x -> x
          small a atts = isJust $ do
              x <- fmap readLengthAttr $ lookup a atts
              guard (x `elem` [LAPixels 0, LAPixels 1, LAPercentage 0])
          go [] = []
          go (TagText t : ts) = TagText (normalizeUnicode t) : go ts
          go (TagOpen "script" _ : ts) = skipTag "script" ts
          go (TagOpen "style" _ : ts) = skipTag "style" ts
          go (TagOpen "head" _ : ts) = skipTag "head" ts
          go (TagOpen "img" atts : ts)
              | small "width" atts && small "height" atts =
                  go ts
              -- кроме скриптов заодно убираем логилки просмотров страниц
          go (TagOpen "img" atts : ts)
              | Just src <- lookup "src" atts
              , adsImage src
              = go ts
                -- отрезаем картинку "comments: 123" -- у нас свои есть
          go (TagOpen "input" (lookup "type" -> Just (T.toLower -> t)) : ts)
              | t == "hidden" || t == "submit" =
                  go ts
                  -- skipTag "input" ts
                  -- input -- void element, не бывает </input>
          go (TagOpen "textarea" _ : ts) = TagOpen "div" [] : go ts
          go (TagClose "textarea"  : ts) = TagClose "div" : go ts
          go (TagOpen "body" _ : ts) = go ts
          go (TagClose "body" : ts) = go ts
          go (TagOpen "link" _ : ts) = go ts
          go (TagClose "link" : ts) = go ts
          go (TagOpen "html" _ : ts) = go ts
          go (TagClose "html" : ts) = go ts
          go (TagOpen "meta" _ : ts) = go ts
          go (TagClose "meta" : ts) = go ts
--           go (TagOpen "noscript" _ : ts) = go ts
--           go (TagClose "noscript" : ts) = go ts
             -- а вот noscript оставляем
          go (TagOpen "form" _ : ts) = go ts
          go (TagClose "form" : ts) = go ts
          go (TagOpen "wbr" _ : ts) = go ts
          go (TagClose "wbr" : ts) = go ts
             --  ^ вырезаем, т.к. жж любит вставлять wbr в длинные слова,
             -- что портит ссылки. У нас все равно есть word-wrap: break-word;
             -- так что длинные слова нам нипочем.
             -- Единственный возможный косяк -- таблицы с длинными словами,
             -- но они попадаются крайне редко.
--          go (t@(TagOpen "code" [("class", cls)]) : ts) = t : go ts
          go (TagOpen n atts : ts) =
              TagOpen n (fixAtts n atts) : go ts
          go (t : ts) = t : go ts
          skipTag _ [] = []
          skipTag tag (TagClose t : ts) | t == tag = go ts
          skipTag tag (_ : ts) = skipTag tag ts
          allowedClass c =
              "bqr" `T.isPrefixOf` c
              || c == "instagram-media" || c == "twitter-tweet"
              || c == webfeedsFeaturedVisual
          fromRelUri u =
              T.pack $ relUriNoNormalize (T.unpack $ T.strip u) baseUri
          fixAtts tag = f
              where f [] = []
                    f (a@(n,v):as)
                        | n == "class" && tag == "code"
                        = (n, T.unwords $ map (("language-" <>) . stripLang)
                            $ T.words v) : f as
                        | ("on" `T.isPrefixOf` n && v /= onFaviconError
                           && v /= habrahabrToggleSpoiler) ||
                          (n == "class" && not (allowedClass v)) ||
                          n == "target" || n == "autoplay" = f as
                        | (n == "src" || n == "href") && adsLink (T.strip v) =
                            f as
                        | n == "href"
                        , Just i <- T.stripPrefix "#" (T.strip v) =
                            (n, "#" <> encodeURIComponentT (decodeURIComponentT i))
                            : f as
                        | n == "src" || n == "href"
                          || (n == "data" && tag == "object") =
                            (n
                            ,fixHref
                             $ (if n /= "href" then fixNonHTTPSUrl else id)
                             $ fromRelUri v)
                            : f as
                        | n == "srcset" || n == "data-lazy-srcset" =
                            (n, editSrcset (map $ first
                                (fixHref . fixNonHTTPSUrl . fromRelUri)) v)
                            : f as
                        | n == "id" || n == "name" =
                            if not (T.null v) then
                                (n, v) : f as
                            else
                                f as
                        | n == "style"
                        , isJust $ T.find (== '(') v =
                            (n, CSS.serialize $ fixCssUrl fromRelUri
                                $ CSS.tokenize v) : f as
                        | otherwise = a : f as

webfeedsFeaturedVisual = "webfeedsFeaturedVisual"

-- uw_debug((() => { var r = []; for (var l of bq.hljs.listLanguages()) { r.push(l); const a = bq.hljs.getLanguage(l).aliases; if (a) for (var al of a) r.push(al) }; return `["${r.join('", "')}"]` })())
supportedHljsLanguages :: HS.HashSet Text
supportedHljsLanguages = HS.fromList ["apache", "apacheconf", "cpp", "c", "cc", "h", "c++", "h++", "hpp", "hh", "hxx", "cxx", "xml", "html", "xhtml", "rss", "atom", "xjb", "xsd", "xsl", "plist", "bash", "sh", "zsh", "clean", "clean", "icl", "dcl", "clojure", "clj", "clojure-repl", "coffeescript", "coffee", "cson", "iced", "coq", "cs", "csharp", "c#", "css", "d", "markdown", "md", "mkdown", "mkd", "dart", "diff", "patch", "elixir", "elm", "ruby", "rb", "gemspec", "podspec", "thor", "irb", "erlang-repl", "erlang", "erl", "fsharp", "fs", "go", "golang", "haml", "haskell", "hs", "http", "https", "ini", "toml", "java", "jsp", "javascript", "js", "jsx", "json", "kotlin", "kt", "lisp", "lua", "makefile", "mk", "mak", "perl", "pl", "pm", "nginx", "nginxconf", "objectivec", "mm", "objc", "obj-c", "ocaml", "ml", "php", "php", "php3", "php4", "php5", "php6", "php7", "prolog", "puppet", "pp", "python", "py", "gyp", "ipython", "rust", "scala", "scheme", "scss", "smalltalk", "st", "sql", "stylus", "styl", "swift", "yaml", "yml", "YAML", "yaml", "tcl", "tk", "tex", "typescript", "ts", "verilog", "v", "sv", "svh"]
{-# NOINLINE supportedHljsLanguages #-}
stripLang l0
    | Just l <- T.stripPrefix "language-" l0 = l
    | Just l <- T.stripPrefix "lang-" l0 = l
    | otherwise = l0

-- | Разбор width/height атрибутов также, как это делает Firefox
-- (берет целую часть и ищет процент в любом месте)
-- Safari пытается вытащить дробную часть, разгребает width=.5foo,
-- но не разгребает width=.5.foo, не нужно нам это.
readLengthAttr a
    | T.null x         = LAError
    | T.any (== '%') u = LAPercentage l
    | otherwise        = LAPixels l
    where (x, u) = T.span isDigit $ T.strip a
          l = readUnsignedInt x

data LengthAttr
    = LAPixels Int
    | LAPercentage Int
    | LAError
    deriving (Eq, Show)


adsImage (rmHttpPrefix -> src) =
    adsLink src
    || "feeds.wordpress.com/1.0/" `T.isPrefixOf` src
    || "feeds.feedburner.com/~f" `T.isPrefixOf` src
    || "assets.feedblitz.com/i/" `T.isPrefixOf` src
    || "www.google-analytics.com" `T.isPrefixOf` src
    || "rss.buysellads.com/img" `T.isPrefixOf` src
    || "feedads" `T.isInfixOf` src
    || ".ads." `T.isInfixOf` src
    || "//ads." `T.isInfixOf` src
adsLink (rmHttpPrefix -> src) =
    "feedads.g.doubleclick.net" `T.isPrefixOf` src
    || "rss.buysellads.com/click" `T.isPrefixOf` src

fixNonHTTPSUrl url
    | Just (_, [[_,v]]) <- fixNonHTTPSUrl_ljToys url =
        youtubeEmbedUrl v
--     | [[_,_,v]] <- rg "lj-toys\\.com/.*source=youtube.*vid=([^&]+).*" url =
--         youtubeEmbedUrl v
    | "https:" `T.isPrefixOf` url = url
    | httpsDomain url = httpToHttps url
    | otherwise = url

fixNonHTTPSUrl_ljToys =
    pathGet "lj-toys.com" "/.*source=youtube.*vid=([^&]+).*"

httpsDomain = testDomains
    ["youtube.com", "vimeo.com", "staticflickr.com"
    ,"media.tumblr.com"
    ,"ted.com", "img-fotki.yandex.ru"
    ,"bp.blogspot.com", "ggpht.com", "googleusercontent.com"
    ,"wordpress.com"
    ]

-- Использовать его для msgLink не стоит, т.к. неправильно обрезать отслеживалки
-- издателя (даже треш от google news -- все равно очень быстро открывается).
-- А вот для hot links, конечно, надо убирать.
-- Также нельзя заменять при использовании link вместо guid.
fixHref (rmIgnoredUrlQuery -> url)
    | T.isInfixOf "://news.google.com/" url
    , [[_,v]] <- regexGet googleNewsRegex url
        = fixHref $ decodeURIComponentT v
    | n <- normalizeMsgLink url
    , n /= url
        = fixHref n
--     | [[_,v]] <- regexGet
--                  "^(.*techcrunch.*)\\?ncid=rss$" url = v
    | otherwise = url

googleNewsRegex = "^https?://news\\.google\\.com/news/url\\?.*&url=([^&=]+)"

-- У bing всё время меняется ID внутри ссылки, а guid нет
-- Используем нормализацию, чтобы не плодить новые посты
normalizeMsgLink url
    | T.isInfixOf "://www.bing.com" url
    , [[_,v]] <- regexGet bingApiClickRegex url
        = decodeURIComponentT v
    | T.isInfixOf "jsessionid" url
    , [[_,p,_,s]] <- regexGet jsessionidRegex url
        = p <> s
    | otherwise = url

bingApiClickRegex = "^https?://www\\.bing\\.com/news/apiclick\\.aspx?\\?.*&url=([^&=]+)"
jsessionidRegex = caseInsensitiveRegex
    "^(https?://[^?&#;]+);([a-z][a-z0-9_]*)?jsessionid=[a-z0-9!-]+([?&#].*)?$"


xmlLinksImgsVideosAndText :: [Tag T.Text] -> ([(TURL, T.Text)], [TURL], [TURL], T.Text)
xmlLinksImgsVideosAndText = go False [] [] [] []
    where go a l i e acc _
             | a `seq` l `seq` i `seq` e `seq` acc `seq` False = undefined
          go a l i e acc [] = (l, i, e, T.concat $ reverse acc)
          go a l i e acc (TagOpen "iframe" (lookup "src" -> Just s) : ts)
              = go a l i (s:e) ("\n" : acc) ts
          go a l i e acc (TagOpen "embed" (lookup "src" -> Just s) : ts)
              = go a l i (s:e) ("\n" : acc) ts
          go a l i e acc (TagOpen "object" (lookup "data" -> Just s) : ts)
              = go a l i (s:e) ("\n" : acc) ts
          go a l i e acc (TagOpen "a" (lookup "href" -> Just s) : TagText t : ts)
              = go True ((s,t):l) i e (t : "\n" : acc) ts
          go a l i e acc (TagOpen "a" (lookup "href" -> Just s) : ts)
              = go True ((s,""):l) i e ("\n" : acc) ts
          go a l i e acc (TagClose "a" : ts)
              = go False l i e acc ts
          go False l i e acc (TagText t : ts)
              | lt <- linkifyTagText t
              = go False (reverse [(link, link) |
                                   TagOpen "a" [("href", link)] <- lt] ++ l)
                   i e (t:acc) ts
          go a l i e acc (TagOpen "img" (lookup "src" -> Just s) : ts)
              | not (buttonSrc s)
                  = go a l (s:i) e acc ts
          go a l i e acc (tag : ts) = go a l i e
              (case tag of
                  TagText !t -> t : acc
                  TagOpen (spaceTag -> True) _ -> "\n" : acc
                  TagClose (spaceTag -> True) -> "\n" : acc
                  _ -> acc
              )
              ts
          buttonSrc src = any (`T.isPrefixOf` rmHttpPrefix src)
              [ "feeds.feedburner.com/"
              , "feeds.wordpress.com/"
              , "api.flattr.com/button"
              ]
-- не считаем за картинки кнопки
-- http://api.flattr.com/button/flattr-badge-large.png
-- http://feeds.feedburner.com/~ff/TomMoertelsBlog?d=yIl2AUoC8zA
-- http://feeds.wordpress.com/1.0/comments/jeltsch.wordpress.com/13/
-- http://feeds.wordpress.com/1.0/delicious/lukepalmer.wordpress.com/1988/
-- http://feeds.wordpress.com/1.0/facebook/lukepalmer.wordpress.com/1988/
-- http://feeds.wordpress.com/1.0/twitter/lukepalmer.wordpress.com/1988/

-- | Текст первого параграфа из HTML-кода
firstParagraph maxChars = firstParagraph' maxChars . forestTextL

firstParagraph' maxChars t = case paragraphs t of
    [] -> ""
    p:ps0 ->
        let go ps n acc []
                | n > 0 && maxChars - n < maxChars `div` 3
                , (p':ps') <- ps
                = go ps' n acc (sentences p')
                | otherwise = ret acc
--                 | null ps = ret acc
--                 | otherwise = T.append (ret acc) "\8194…"
                -- &ensp; перед …, если есть еще параграф
                --  ^ непонятна разница между узким и широким пробелом
                --  лучше никак не показывать наличие следующего параграфа,
                --  чем так
            go ps n acc (s:ss) = case firstWords' (not $ null ss) n s of
                (Just n', s') -> go ps (n'-1) (s':acc) ss
                (Nothing, s') ->
                    if n < maxChars `div` 3 then
                        -- стараемся не добавлять обрезанное предложение,
                        -- если только у нас не слишком мало текста
                        ret (ellipsis:acc)
                    else
                        ret (s':acc)
            ret acc = TL.toStrict . TL.unwords $ reverse acc
        in
            go ps0 maxChars [] (sentences p)

paragraphs = filter (not . emptyTextL) . TL.lines

sentences :: TL.Text -> [[TL.Text]]
sentences = go [] . TL.words
    where go [] [] = []
          go acc [] = [reverse acc]
          go acc (x:xs)
              | TL.last x `elem` (".!?…" :: String) = reverse (x:acc) : go [] xs
              | otherwise = go (x:acc) xs

ellipsis = "…"

-- | Начало текста из HTML-кода
firstWordsX n = firstWords n . buildForest . parseTagsT . tbs
firstWords n =
    TL.toStrict . snd . firstWords' False n . TL.words . forestTextL

firstWords' hasNext n ws = go n [] ws
    where -- обрезание предложения, по возможности не обрезает слова
          -- если последнее слово в один символ, то можно его включать?
          go _ []  [] = (Just n, "")
          go n acc [] = (Just (n+1), ret acc)
          go n acc (w:ws)
              | l < n = go (n - l - 1) (w:acc) ws
              | l > maxWordLength =
                (Nothing
                ,ret $
                 TL.dropWhileEnd (== softHyphen)
                 (TL.take (max maxWordLength n) w) `TL.append` ellipsis : acc)
                -- если какая-то кривость и очень большое слово
                -- то также ограничиваем его длину 50 символами
              | otherwise =
                (Nothing
                ,ret $
                 if not (null ws) || hasNext then ellipsis:w:acc else w:acc)
              where l = max (TL.length w `div` 2 + 1)
                        (TL.length $ TL.filter FC.isAdvanceInPrint w)
                        -- игнорируем SOFT HYPHEN и NonSpacingMark при подсчете
                        -- длины слова (но с ограничением для кривых слов)
                    maxWordLength = 50
          ret acc = TL.unwords $ reverse acc

isSkipTag "script" = True
isSkipTag "style" = True
isSkipTag _ = False

xmlTextL = TL.concat . go . parseTagsT
    where go [] = []
          go (TagOpen (spaceTag -> True) _ : ts) = "\n" : go ts
          go (TagClose (spaceTag -> True) : ts) = "\n" : go ts
          go (TagOpen (isSkipTag -> True) _ : TagText _ : ts) = go ts
          go (TagText !t : ts) = TL.fromStrict t : go ts
          go (_:ts) = go ts
xmlText = T.concat . go . parseTagsT . tbs
    where go [] = []
          go (TagOpen (spaceTag -> True) _ : ts) = "\n" : go ts
          go (TagClose (spaceTag -> True) : ts) = "\n" : go ts
          go (TagOpen (isSkipTag -> True) _ : TagText _ : ts) = go ts
          go (TagText !t : ts) = t : go ts
          go (_:ts) = go ts

forestTextL = TL.concat . go [] . clearEmptyTags
    where go [] [] = []
          go ((b,add):bs) [] = add $ go bs b
          go p (n:ns) = case n of
              Node {..}
                  | name `elem` ["script", "style", "noscript"] ->
                      go p ns
                  | name `elem` ["audio", "video", "iframe", "svg"] ->
                      " " : go p ns
                  | Just cs <- T.words <$> lookup "class" attrs
                  , elem "iframe_placeholder" cs || elem "nonProxied" cs ->
                      " " : go p ns
                  | Just c <- lookup "class" attrs
                  , c == bqrYoutubeDurationClass
                  , [_, n] <- children ->
                      -- Time: NN:NN => [NN:NN]
                      "[" : go ((ns, ("]\n":)) : p) [n]
                  | otherwise -> case newlineNode name attrs of
                      Newline -> "\n" : go ((ns, ("\n":)) : p) children
                      Text -> " " : go ((ns, (" ":)) : p) children
                      Empty -> go ((ns, id) : p) children
              VoidElement {..} -> case newlineVoidElement name attrs of
                  Newline -> "\n" : go p ns
                  Text -> " " : go p ns
                  Empty -> go p ns
              TextNode t -> TL.fromStrict t : go p ns

data JSFixes
    = JSFixes
      { addTwitter :: !Bool
      , addInstagram :: !Bool
      , addVideo :: !Bool
      , highlightCode :: !Bool
      , fixVKPostTime :: !Bool
      , habrSpoilerNum :: !Int
      , restoreLoadedImgSizes :: !Bool
      , setProxiedAttrs :: !Bool
      }
    deriving (Eq, Show)

defaultJSFixes =
    JSFixes
    { addTwitter = False
    , addInstagram = False
    , addVideo = False
    , highlightCode = False
    , fixVKPostTime = False
    , habrSpoilerNum = 0
    , restoreLoadedImgSizes = False
    , setProxiedAttrs = False
    }

data PreprocessSettings
   = PreprocessSettings
     { -- | Имя хоста, чтобы добавить к нему www. и использовать для загрузки
       -- изображений с отдельного домена.
       psHostName :: Maybe THostName
       -- | используется для Google Translate, который не дружит с нашими
       -- iframe_placeholder-ами, а также не дает доступа к CSS/localStorage
       -- (т.к. другой hostname), что ломает код в utils.js
     , psJavaScriptEnabled :: Bool
       -- | Заголовок Accept-Language (используется для выбора орфографии
       -- переносов)
     , psAcceptLanguage :: T.Text
     , psMaxMsgTextLength :: Maybe Int
     }
   deriving Show

defaultPreprocessSettings = PreprocessSettings (Just "") True "" Nothing

ui = isJust . psHostName
js = psJavaScriptEnabled

-- есть fixMessageBody, а здесь то, что было обнаружено позже
fixMessageContent
    :: PreprocessSettings -> (Bool -> Text -> Forest -> Text) -> Msg -> Msg
fixMessageContent ps shortTextFunc m =
    m { msgText =
          case psMaxMsgTextLength ps of
              Just 0 -> ""
              Just l -> firstWords (fromIntegral l) forest
              Nothing -> renderTagsT text
      , msgAttachments = attachments ++ thumbnnail
      , msgSubject = hyph $ subj
--      , msgAuthor = hyphenate notInAuthorLink $ msgAuthor m
      , msgAuthorPic = fmap (proxyUrlPS PTAvatar ps) (msgAuthorPic m)
--      , msgTags = map hyph $ msgTags m
      , msgShortText = hyph $ shortTextFunc False subj forest
      , msgShorterText = hyph $ shortTextFunc True subj forest
      }
    where hyph = hyphenate (const True) (Detected lang, psAcceptLanguage ps)
          authorLinkWords =
              HS.fromList
              $ maybe [] (T.split (not . FC.isAlphaMark) . T.toLower)
              $ msgAuthorUri m
          notInAuthorLink w =
              not $ HS.member (T.toLower $ T.pack w) authorLinkWords
          (text, (lang, thumbnnail, forest)) = fixTags' (msgSubject m) (msgKey m) (msgLink m) ps $
              (parseTagsT (tbs $ msgText m)) :
              if null aTags then [] else
                  (if emptyText (msgText m) then
                       -- не нужен разделитель, если нет текста
                      id
                   else
                      ([TagOpen "p" [("style", "clear:both")]] :))
                  aTags
          (catMaybes -> attachments, map aText . filter (/= []) -> aTags) =
              unzip $ map (attachment m ps) $ msgAttachments m
          showCommentSubj (topLevelHostName . hostNameFromUrlT -> h) =
              h == "livejournal.com" || h == "lambda-the-ultimate.org"
          bfu = msgKeyBlogFeedUrl $ msgKey m
          subj
              | isJust $ msgKeyCommentGuid $ msgKey m
              , not (showCommentSubj bfu ||
                     maybe False showCommentSubj (msgAuthorPic m))
              = ""
              | otherwise = msgSubject m

aText x = TagOpen "p" [("data-bqr-info", "attachment")] : x

attachment
    :: Msg -> PreprocessSettings -> Attachment
    -> (Maybe Attachment, [Tag T.Text])
attachment _ ps a@(AImage {..}) =
    (Nothing, [TagOpen "img" $
        ("src", aUrl) : catMaybes
        [(("width",) . T.pack . show) <$> aWidth
        ,(("height",) . T.pack . show) <$> aHeight
        ,("alt",) <$> aTitle]])
attachment m ps a@(AIframe {..})
    | isJust $ youtubeVideoId aUrl <|> vimeoVideoId aUrl
    = (Nothing, parseTagsT $ tbs aXml)
    | Just i <- flickrVideoId aUrl
    , as:_ <- [as | TagOpen "img" as <- parseTagsT (tbs $ msgText m)]
    = attachment m ps $
        AVideo
        { aUrl         =
            "https://www.flickr.com/video_download.gne?id=" <> i
        , aMime        = "video/mp4"
        , aFileSize    = Nothing
        , aDuration    = Nothing
        , aTitle  = Nothing
        , aWidth  = tryReadUnsignedInt =<< lookup "width" as
        , aHeight = tryReadUnsignedInt =<< lookup "height" as
        , aPoster = resizedImgSrc True True <$> lookup "src" as
          -- True, чтобы взял большое изображение как для thumbnail
        }
    | otherwise =
        -- AIframe появляется только для youtube/vimeo видео,
        -- и .swf-фалов
        -- swf -- это дыра в безопасности и производительности,
        -- плюс Chrome зачем-то пытается качать <iframe>-ы c swf-кой.
        -- Есть еще неправильно указанные
        -- application/x-shockwave-flash
        -- для нормальных ссылок (типа тех же youtube),
        -- получается, что мы пока их тоже вырезаем
        (Nothing, [])
attachment _ ps a@(AAudio {..}) =
    (guard (not $ ui ps) >> pure a
    ,tags
     <> downloadLink ps aUrl aTitle aFileSize aDuration
         (if ui ps then
              Just ("audio", 0, fixNode ps "audio" tags)
          else Nothing))
    where tags =
              [TagOpen "audio" [("controls", "")]
              ,TagOpen "source" [("src", aUrl), ("type", aMime)]
              ,TagClose "audio"]
attachment _ ps a@(AVideo2 {..}) =
    (guard (not $ ui ps) >> pure a
    ,tags
     <> downloadLink ps aUrl aTitle aFileSize aDuration
         (if ui ps then
              Just ("video", fromMaybe (9/16) aspectRatio, fixNode ps "video" tags)
          else Nothing))
    where tags = embedVideo aMime aspectRatio aPoster aLoop aUrl
          aspectRatio =
              (/) <$> (fromIntegral <$> aHeight) <*> (fromIntegral <$> aWidth)
attachment m ps (AVideo {..}) = attachment m ps (AVideo2 {..})
    where aLoop = False
attachment _ ps a@(AOther {..}) =
    (guard (not $ ui ps) >> pure a
    ,downloadLink ps aUrl Nothing aFileSize Nothing Nothing)
attachment _ ps a@(AGrOrigin {}) =
    (guard (ui ps) >> pure a, [])
attachment _ ps a@(AThumbnail {}) =
    (guard (ui ps) >> pure a, [])

fixNode ps name tags =
    forestToTags $
    maybe [TextNode $ "no <" <> name <> "> after fixTags ?"] (: []) .
    findNodeByName name $ (\(_,_,f) -> f) $ snd $
    fixTags' "" emptyMsgKey Nothing (ps { psJavaScriptEnabled = False }) [tags]

findNodeByName :: Text -> Forest -> Maybe Node
findNodeByName nm c = foldr (<|>) Nothing $ flip map c $ \ case
    n@(Node {..})
        | name == nm -> Just n
        | otherwise -> findNodeByName nm children
    _ -> Nothing

span1 a c = span_ a [c]
span_ a c = Node "span" a InlineLevel c

-- Удаляем <source> из <picture>. Они нам не нужны, т.к. мы берем избражение
-- с самым большим разрешением из <img srcset> и используем масштабирование
-- на сервере через image proxy.
-- Для <img src="data:..."> пытаемся взять srcset из <source>
fixPicture msgLink ps = map $ \ case
    n@(Node {..})
        | name == "svg" || name == "math" -> n -- ничего не трогаем внутри svg
        | name == "picture" ->
            Node { children = go [] children, .. }
        | otherwise ->
            Node { children = fixPicture msgLink ps children, .. }
    n -> n
    where go _ [] = []
          go acc (n:ns) = case n of
              VoidElement { name = "source", .. }
                  | not (badType attrs)
                  , Just ss <- lookup "srcset" attrs
                  -> go (parseSrcset ss : acc) ns
                  | otherwise
                  -> go acc ns
              VoidElement { name = "img", .. }
                  | Just s <- lookup "src" attrs
                  , "data:" `T.isPrefixOf` s
                  , ss <- concat $ reverse acc
                  , not $ null ss
                  -> VoidElement
                    { name = "img"
                    , attrs = fixImgAttrs msgLink ps
                        $ ("srcset", renderSrcset ss) : attrs }
                    : go acc ns
              _ ->
                  n : go acc ns
          badType (lookup "type" -> Just t) =
              T.isPrefixOf "image/webp" (T.toLower t)
              -- Safari до сих пор не поддерживает
          badType _ = False
          source _ = False

-- Удаляем <svg><use ...></use></svg>, т.к. они пустые и используют href
-- с сайта, которого у нас нет
fixSvg = concatMap $ \ case
    n@(Node {..})
        | name == "svg" ->
            if all ignored children then [] else [n]
        | otherwise ->
            [Node { children = fixSvg children, .. }]
    n -> [n]
    where ignored (TextNode t) = emptyText t
          ignored Node { name = "use", .. } = True
          ignored _ = False

addOnLoad = map $ \ case
    n@(VoidElement {..})
        | hasBg attrs -> n
        | name == "img" -> VoidElement { attrs = withOnload attrs, .. }
    n@(Node {..})
        | hasBg attrs || name == "math" -> n
        | name == "svg" -> Node { attrs = withOnload attrs, .. }
        | otherwise ->
            Node { children = addOnLoad children, .. }
    n -> n
    where withOnload = (<> [("onload", "bq.imgOnLoad(this)")])
          hasBg a
              | Just s <- lookup "style" a = "background" `T.isInfixOf` s
              | otherwise = False

-- добавление заглушек с кнопками play для video/audio и youtube/vimeo iframe
addPlaceholders ps = map $ \ case
    n@(Node {..})
        | name == "svg" || name == "math" -> n -- ничего не трогаем внутри svg
        | name == "audio" -> audioPlaceholder n
        | name == "video" -> videoPlaceholder n
        | name == "iframe"
        , Just r <- tryReplaceIframe ps attrs
        -> r
        | otherwise ->
            Node { children = addPlaceholders ps children, .. }
    n -> n

audioPlaceholder node =
    span1 [("class", "bqrMsgAudio" <> proxyCls), ("style", "display:block")] $
    --  ^ внешний div, для задания минимальной высоты,
    --    чтобы после включения audio не менялось расположение
    --    последующих  элементов
    placeholder
        [("class","bqrMsgAudioInner")]
        node'
        [span_ [("class","iconPlayAudio")] []]
    where proxyCls
              | hasClass proxiedClass attrs = " " <> proxiedClass
              | hasClass nonProxiedClass attrs = " " <> nonProxiedClass
              | otherwise = ""
          (node', attrs) = editAttrs' addAutoplay node

videoPlaceholder = videoPlaceholder' "poster" addAutoplay

videoPlaceholder' posterAttr editAttr node =
    placeholder
        [("class", "videoPlaceholder")
        ,("style", fromMaybe "" (poster <$> lookup posterAttr (attrs node)))]
        (editAttrs editAttr node)
        [span_ [("class","iconPlayVideo")] []]
    where poster u = "; background-image: " <> CSS.serialize [CSS.Url u]
              <> "; background-size: contain; background-position: center; background-repeat: no-repeat"

audioStyle = "display: block; width: 100%"
addAutoplay = addOrReplaceAttr "autoplay" ""
addSrcAutoplay src =
    addOrReplaceAttr "src" (urlAddOrReplaceParam "autoplay" "1" src)

placeholder attrs replacement inner =
    span_
        (maybe id addClass (lookup "class" ia) $
         maybe id addStyle (lookup "style" ia) $
         attrs
         ++ [("data-bqr-html", renderTagsT $ forestToTags [replacement])
            ,("onclick", "uw_event=event;bq.replaceWithHtml(this, this.getAttribute('data-bqr-html'))")])
        inner
    where ia = snd $ editAttrs' id replacement

downloadLink :: PreprocessSettings
    -> TURL -> Maybe Text -> Maybe Int -> Maybe Int
    -> Maybe (Text, Double, [Tag Text]) -> [Tag Text]
downloadLink ps url title0 fileSize0 duration popup
    | isJust (psHostName ps) =
        downloadLink' nonProxiedClass url title0 fileSize0 duration popup
        <>
        downloadLink' proxiedClass (proxyUrlPS PTStreamingContent ps url)
            title0 fileSize0 duration popup
    | otherwise =
        downloadLink' "" url title0 fileSize0 duration popup
downloadLink' cls url title0 fileSize0 duration popup =
    link
    ++ (case popup of
        Just (t, ar, x) ->
            [TagText ", "
            ,TagOpen "a" $
                [("data-bqr-popup-type", t)
                ,("data-bqr-popup-aspect-ratio", showT ar)
                ,("data-bqr-popup-html"
                 ,renderTagsT $ editFirstTagAttrs addAutoplay x)
                ,("data-bqr-popup-link", renderTagsT link)
                ,("href", url)
                ,("onclick", "uw_event=event;bq.enclosurePopup(this)")]
            ,TagText "popup", TagClose "a"]
        Nothing -> [])
    ++
    [TagClose "span"]
    where link =
              [TagOpen "span" [("class", "downloadLink " <> cls)]
              ,TagOpen "a" [("href", url), ("title", url)]
              ,TagText t0, TagClose "a"]
              ++
              if details /= "" then
                  [TagText $ " (" <> details <> ")"]
              else
                  []
          t0 | Just t <- title
             , t /= "" = t
             | t <- T.takeWhileEnd (/= '/') $
                 T.takeWhile (\ c -> c /= '#' && c /= '?') url
             , t /= "" = decodeURIComponentT t
             | otherwise = url
          details
             | fs /= "" && d /= "" = d <> ", " <> fs
             | fs /= "" = fs
             | otherwise = d
          d  | Just d <- duration
             , d > 1 = formatDuration d
             | otherwise = ""
          fs | Just s <- fileSize
             , s > 1 = formatFileSize s
             | otherwise = ""
          (title, fileSize)
             | Just m <- T.stripPrefix "magnet:" url
             , qs <- parseQueryStringUtf8Only m
             =  ((decodeURIComponentT <$> lookup "dn" qs) <|> title0
                ,(tryReadUnsignedInt =<< lookup "xl" qs) <|> fileSize0)
             | otherwise = (title0, fileSize0)

formatDuration :: Int -> T.Text
formatDuration duration
    | h > 0 = T.pack $ printf "%d:%02d:%02d" h m s
    | otherwise = T.pack $ printf "%d:%02d" m s
    where h = duration `div` 3600
          m = duration `rem` 3600 `div` 60
          s = duration `rem` 60

formatFileSize :: Int -> T.Text
formatFileSize s@(toEnum -> fs)
    | fs > g = toFixed (fs / g) <> "GB"
    | fs > 10*m = i (round (fs / m)) <> "MB"
    | fs > m = toFixed (fs / m) <> "MB"
    | fs > k = i (round (fs / k)) <> "kB"
    | s == 1 = "1 byte"
    | otherwise = i s <> " bytes"
    where k = 1024 :: Double
          m = k*k
          g = k*m
          toFixed = T.pack . printf "%.1f"

i = T.pack . show
-- toFixed i x
--     | i <= 0 = i $ round x
--     | l > i = let (a,b) = T.splitAt (l-i) in a <> "." <> b
--     | l == i = "0." <> r
--     | l <= i = "0." <> T.replicate (i-l) "0" <> r
--     where r = i $ round $ x * 10^i
--           l = T.length r

-- magnet:?xt=urn:btih:8a89b8a94aa587d52de819cdeadc83ac198f258b&dn=debian-9.2.0-amd64-netinst.iso

flickrVideoRegex =
    pathGet "flickr.com" "/apps/video/stewart.swf.*photo_id=([0-9]+)"
flickrVideoId u
    | Just (_, [[_,i]]) <- flickrVideoRegex u = Just i
    | otherwise = Nothing

fixTags msgSubject msgKey msgLink ps =
    renderTagsT . fst . fixTags' msgSubject msgKey msgLink ps
fixTags' msgSubject msgKey msgLink ps ts =
    ((if not (ui ps) then replaceClassesByStyles else id) $
     (if ui ps && js ps then addJSFixes defaultJSFixes else id) $
     forestToTags fixedForest
    ,(lang, thumnnail, fixedForest))
    where (lang, thumnnail, fixedForest) =
              fixForest msgLink msgSubject ps $
              concatMap (buildForest . go . addLinks False . fixIds) ts
          unprocessed c as =
              addOrReplaceAttr "class" (T.append c "Unprocessed") as
          idPrefix = "article-" <> urlSafe_sha1 (textKey msgKey) <> "-"
          wrapId i = idPrefix <> i
          fixIds = map $ \ case
              TagOpen t as -> TagOpen t $ flip map as $ \ case
                  (n, i)
                      | n `elem` ["id", "name"] -> (n, wrapId i)
                  (n@((`elem` ["href", "usemap"]) -> True)
                   ,T.stripPrefix "#" -> Just i)
                      | not (ui ps) && n == "href"
                      , Just l <- msgLink ->
                          (n, l <> "#" <> i)
                      | otherwise ->
                          (n, "#" <> wrapId i)
                  a -> a
              x -> x
          addJSFixes !f _ | False = undefined
          addJSFixes f@(JSFixes {..}) []
              | f /= defaultJSFixes =
                  TagOpen "script" [] :
                  TagText (T.intercalate ";\n" $
                              ["bq.addTwitter()" | addTwitter]
                           ++ ["bq.addInstagram()" | addInstagram]
                           ++ ["bq.addVideo()" | addVideo]
                           ++ ["bq.highlightCode()" | highlightCode]
                           ++ ["bq.fixVKPostTime()" | fixVKPostTime]
                           ++ ["bq.fixHabrSpoilers()" | habrSpoilerNum > 0]
                           ++ ["bq.restoreLoadedImgSizes()" | restoreLoadedImgSizes]
                           ++ ["bq.setProxiedAttrs()" | setProxiedAttrs]
                          ) :
                  TagClose "script" : []
              | otherwise = []
          addJSFixes f (t@(TagOpen n as) : ts)
              | n == "embed" || n == "object" =
                  t : addJSFixes (f { setProxiedAttrs = True }) ts
          addJSFixes f (t@(TagOpen n as@(lookup "class" -> Just c)) : ts)
              | has "twitter-tweet" =
                  TagOpen n (unprocessed c as)
                  : addJSFixes (f { addTwitter = True }) ts
              | has "instagram-media" =
                  TagOpen n (unprocessed c as)
                  : addJSFixes (f { addInstagram = True }) ts
              | has "bqrHabr" =
                  t
                  : addJSFixes (f { highlightCode = True }) ts
              | n == "code"
              , c' <- T.unwords
                  $ filter
                      (\ l -> stripLang l `HS.member` supportedHljsLanguages)
                      (T.words c)
              , not (T.null c')
              =   TagOpen n (addOrReplaceAttr "class" c' as)
                  : addJSFixes (f { highlightCode = True }) ts
              | has "bqrVKPostTime" =
                  TagOpen n (unprocessed c as)
                  : addJSFixes (f { fixVKPostTime = True }) ts
              | has "bqrUnknownImgSize" =
                  TagOpen n (unprocessed c as)
                  : addJSFixes (f { restoreLoadedImgSizes = True }) ts
              | has "bqrHabr_spoiler_title" =
                  (TagOpen n $
                   addOrReplaceAttr "data-spoiler-num"
                                    (showT $ habrSpoilerNum f) $
                   unprocessed c as)
                  : addJSFixes (f { habrSpoilerNum = habrSpoilerNum f + 1 }) ts
              | has "iframe_placeholder" =
                  t : addJSFixes (f { addVideo = True }) ts
              where has = (`elem` T.words c)
          addJSFixes f (t:ts) = t : addJSFixes f ts
          addLinks _ [] = []
          addLinks _ (t@(TagOpen "a" _) : ts) = t : addLinks True ts
          addLinks _ (t@(TagClose "a") : ts) = t : addLinks False ts
          addLinks False (TagText t : ts) =
              linkifyTagText t ++ addLinks False ts
          addLinks inLink (t:ts) = t : addLinks inLink ts
          go (TagText t : ts) = TagText (normalizeUnicode t) : go ts
          go (TagOpen t as : ts)
              | t `elem` ["button", "input", "select", "textarea"] =
                  TagOpen t (addTabindex $ map fixA as) : go ts
          go (TagOpen "iframe" as : ts) =
              fixIframe ps (map fixA as) go ts
          go (TagOpen "br" _ :
              TagOpen "span" [("class",c)] : TagClose "span" :
              TagOpen "br" _ : ts)
              | ("bqrYouTube":xs) <- T.split (== '-') c
              -- , [d,v,l,dl] <- mapMaybe tryReadUnsignedInt xs =
              --    embedYoutubeStats d v l dl (go ts)
              , (d:_) <- mapMaybe tryReadUnsignedInt xs =
                  embedYoutubeStats d 0 0 0 (go ts)
          go (TagOpen "img" [("src", "https://www.reddit.com/self"), ("class", "bqrRedditThumb")] : TagClose "img" : ts) =
              -- кривой reddit-овский "self" thumbnail
              go ts
          go (TagOpen "img" as : ts) =
              fixImg msgLink ps (map fixA as) $ go ts
          go (TagOpen "font" as : ts) =
              go $ TagOpen "span" (
                  maybe id (addStylePre . ("font-family: " <>))
                      (lookup "face" as)
                  $ removeAttr "face" $ removeAttr "size" as) : ts
          go (TagOpen "basefont" as : ts) = go (TagOpen "font" as : ts)
          go (TagClose "basefont" : ts) = TagClose "span" : go ts
          go (TagClose "font" : ts) = TagClose "span" : go ts
          go (TagOpen "foreignobject" as : ts) =
              TagOpen "foreignobject"
                  (addAttrIfNotExists "height" "100%" $
                   addAttrIfNotExists "width" "100%" as)
                   -- без width/height Safari/FF/Chrome
                   -- не показывают foreignObject
                  : go ts
          go (s@(TagOpen "span" [("class", "bqrRedditDomain")]) :
              p@(TagText "(") :
              TagOpen "a" as : ts)
             | Just h <- lookup "href" as
             , [[_,subreddit]] <- regexGet "^https://www\\.reddit\\.com/domain/self\\.([^.]+)/" h
             , href' <- T.concat ["https://www.reddit.com/r/", subreddit, "/"]
             = s : p : go (TagOpen "a" [("href", href')] : ts)
          go (TagOpen "a" atts : ts)
              | Just h <- lookup "href" atts
              , protocol <- T.takeWhile (/= ':') $ T.toLower h =
                  -- не подсвечиваем ссылки без адреса
                  -- (это может быть просто anchor <a name="chapter1">)
                  if protocol `elem` ["https", "http", "ftp"] then
                      TagOpen "a"
                      (("target", "_blank") : ("rel", "noopener") :
                       map fixA (filter ((/= "target") . fst) atts)) : go ts
                  else if protocol == "javascript" then
                      -- вырезаем
                      go ts
                  else
                      TagOpen "a"
                      (map fixA (filter ((/= "target") . fst) atts)) : go ts
                      -- magnet и остальные протоколы
                      -- (mailto?, локальные ссылки)
                      -- без target=_blank, чтобы в Chrome
                      -- не открывалось лишних вкладок
          go (TagOpen "video" as : TagOpen "source" ss : TagClose "source" : TagClose "video" : ts)
              | Just "bqrMsgVideo" <- lookup "class" as
              , Just src <- lookup "src" ss
              , Just mime <- lookup "type" ss
                -- совместимость со старым embedVideo
                -- кроме как для .mp4 video в facebook .bqrMsgVideo
                -- использовался для вставки embed/img в Google+ (что
                -- уже устарело) и для iframe -- по-умолчанию и так
                -- делаются во всю ширину
              = go $ embedVideo mime Nothing Nothing False src ++ ts
          go (TagOpen t as : ts)
              | t `elem` ["video", "audio"]
              = TagOpen t (fixVideoAttrs as) : go ts
          go (TagOpen "embed" as : ts)
              | Nothing <- lookup "type" as
              , Just s <- lookup "src" as
              = go $ TagOpen "embed" (as ++ [("type", mimeByUrl s)]) : go ts
          go (TagOpen "embed" as : ts)
              = TagOpen "embed" (forceHttps "src" as) : go ts
          go (TagOpen "object" as : ts)
              = TagOpen "object" (forceHttps "data" as) : go ts
          go (b@(TagOpen "blockquote" (("class", "instagram-media"):_)) :
              TagOpen "p" [] : TagOpen "a" a : TagOpen "img" i : ts)
              | Just (tryReadDouble -> Just w) <- lookup "width" i
              , Just (tryReadDouble -> Just h) <- lookup "height" i
              , w /= instagramWidth =
                  b : TagOpen "p" [] : TagOpen "a" a :
                  go (TagOpen "img"
                      (addAttr "width" instagramWidth $
                       addAttr "height"
                           (instagramWidth / w * h) i) : ts)
          go (TagOpen "span" [("class", "bqrRetweetIconOuter")] :
                  TagOpen "span" [("class", "bqrRetweetIcon")] :
                  TagClose "span" :
              TagClose "span" :
              TagText "Retweeted by " :
              TagOpen "span" [("class", "bqrRetweetedByProfile")] :
                  TagText profile :
              TagClose "span" :
              ts)
              =
              TagOpen "span" [("class", "bqrRetweetIcon")] :
              TagClose "span" :
              TagOpen "span" [("class", "bqrRetweetText")] :
                  TagText "Retweeted by " :
                  TagOpen "span" [("class", "bqrRetweetedByProfile")] :
                      TagText (normalizeUnicode profile) :
                  TagClose "span" :
              TagClose "span" :
              go ts
          go (TagOpen (ignoredTag -> True) _ : ts) = go ts
          go (TagClose (ignoredTag -> True) : ts) = go ts
          go (TagOpen t as : ts) = TagOpen t (map fixA as) : go ts
          go (t:ts) = t : go ts
          go [] = []
          addAttr n i = addOrReplaceAttr n (showT (round i :: Int))
          instagramWidth = 658.0
          ignoredTag x = HS.member x ignoredTags
          forceHttps n = map $ \ case
              (an, av) | an == n -> (an, httpToHttps av)
              a -> fixA a
          fixA ("src", u) = ("src", fixNonHTTPSUrl u)
          fixA ("poster", u) = ("poster", fixNonHTTPSUrl u)
          fixA ("class", "bqrHabr_twitter-tweet") = ("class", "twitter-tweet")
          fixA ("class", "bqrHabr_instagram-media") = ("class", "instagram-media")
          fixA ("onclick", "bqrHabrahabrToggleSpoiler(this)") =
              ("onclick", "bq.habrahabrToggleSpoiler(this)")
          fixA ("tabindex", _) = ("tabindex", "-1")
          fixA ("draggable", _) = ("draggable", "false")
          fixA ("align", a)
              | T.isPrefixOf "justify" (asciiToLowerT a) =
                  ("align", "ignoring=" <> a)
          fixA (n, x)
              | n `elem` ["width", "height"] = case readLengthAttr x of
                  LAPercentage p | n == "width" && p <= 100 ->
                      (n, showT p <> "%")
                  LAPixels p | p <= maxPixels -> (n, showT p)
                  _ -> (n, "invalid=" <> x)
          fixA a = a
          addTabindex = addOrReplaceAttr "tabindex" "-1"
          fixVideoAttrs =
              addOrReplaceAttr "controls" "" .
              addOrReplaceAttr "preload" "none" .
              -- ничего не загружаем в мобильных приложениях,
              -- а в web-интерфейсе и так заглушки стоят, а при последующем
              -- autoplay preload игнорируется
              addOrReplaceAttr "playsinline" "true" .
              addOrReplaceAttr "webkit-playsinline" "true" .
              filter ((/= "autoplay") . fst) .
              addTabindex .
              map fixA
              -- у <video>
              -- preload=none
              --     ничего не грузится, ни картинка, ни длительность видео
              -- preload=metadata
              --     грузится длительность видео,
              --     картинка есть в Chrome и Firefox,
              --     но нет в Safari, прокрутка в Safari не подвисает!


-- imageResolutions = [(320, 640), (640, 1280), (960, 1440), defaultImageResolution, (1440, 1920), (1920, 2560), (2560, 2560)]
-- defaultImageResolution = (1280, 1920)
-- defaultVideoResolution = (1920, 1080)
-- thumbnailResolutions = [(360,240), (546,364)]
-- у нас thumbnail является background-image
-- (чтобы не было видно незагрузившийся thumbnail), а у него нет srcset,
-- только image-set, у которого есть только разрешение (1x, 2x), а не размер.
-- так что пока фиксируем разрешение thumbnail

data ProxyType
    = PTFavicon
    | PTThumbnail
    | PTAvatar
    | PTContent
    | PTContentStyle
      -- ^ с ограничением размера для background: url(),
      -- которые не обрабатываются на клиенте
    | PTStreamingContent
    deriving (Eq, Show)

proxyPath t = case t of
    PTFavicon -> ""
    PTThumbnail -> thumbnail thumbnailWidth thumbnailHeight
    PTAvatar -> thumbnail 100 100
    PTContent -> "/image_proxy/-/"
    PTContentStyle -> "/image_proxy/1920x2560/"
    PTStreamingContent -> "/proxy/"
    where thumbnail w h = T.concat
              ["/image_proxy/t", showT w, "x", showT h, "/"]

proxyHost t h | h == "beta.bazqux.com" || ".beta.bazqux.com" `T.isSuffixOf` h =
    T.concat ["https://", proxySubdomain t, ".beta.bazqux.com"]
proxyHost t h | h == "local.bazqux.com" || ".local.bazqux.com" `T.isSuffixOf` h =
    T.concat ["https://", proxySubdomain t, ".local.bazqux.com"]
proxyHost t h | h == "bazqux.com" || ".bazqux.com" `T.isSuffixOf` h =
    T.concat ["https://", proxySubdomain t, ".bazqux.com"]
proxyHost t h | isIPAddr h = case t of
    PTFavicon ->    "//" <> h <> ":8008"
    PTThumbnail ->  "//" <> h <> ":8080"
    _ ->            "//" <> h <> ":8081"
proxyHost _ "" = ""
proxyHost _ h = "//" <> h

proxySubdomain t = case t of
    PTFavicon -> "favicons"
    PTThumbnail -> "thumbnails"
    PTAvatar -> "avatars"
    PTContent -> "img"
    PTContentStyle -> "img"
    PTStreamingContent -> "proxy"

-- Отдельный домен для иконок, а то Safari сначала иконки грузит,
-- а потом уже ajax-запросы
-- также отдельный домен для thumbnail, чтобы грузились параллельно с иконками
-- и отдельный домен для содержимого, чтобы при разворачивании сообщения
-- не ждать дозагрузки thumbnail-ов

proxyUrlPS pt ps = proxyUrl pt (fromMaybe "" $ psHostName ps)

proxyUrl pt h url
    | "/" `T.isPrefixOf` url =
        T.concat [proxyHost pt h, url]
        -- ссылки на /img/vimeo_thumbnail, /avatar оставляем как есть
    | "data:" `T.isPrefixOf` url = url
    | otherwise = T.concat [proxyHost pt h, proxyPath pt, url]

addProxy ps = maybe id (addProxy' ps) (psHostName ps)
addProxy' ps hostName = go False
    where go inP = concatMap $ \ case
              t@(TextNode _) -> [t]
              VoidElement {..} ->
                 [VoidElement name $ case name of
                  "img" ->
                      makeFirstAttr "src" $ -- для строковых замен на клиенте
                      add PTContent "src" attrs
                  "source" ->
                      add PTStreamingContent "src" attrs
                  "embed" ->
                      proxyAttr "src" attrs
                  _ ->
                      fixStyle attrs]
              Node {..}
                  | not inP && name == "object" ->
                      [Node { attrs = proxyAttr "data" attrs, .. }]
                  | not inP && name `elem` ["audio", "video"] ->
                      -- в принципе, проверка inP необязательна,
                      -- т.к. вложенные object/audio/video всё равно
                      -- игнорируются браузерами
                      [Node { attrs = addClass nonProxiedClass attrs, .. }
                      ,Node
                       { attrs = addClass proxiedClass
                           $ makeFirstAttr "poster"
                           $ add PTContentStyle "poster"
                           $ add PTStreamingContent "src" attrs
                       , children = go True children
                       , .. }]
                  | otherwise ->
                      [Node
                       { attrs = fixStyle attrs
                       , children = go inP children, .. }]
          f t = t
          proxy t = proxyUrl t hostName
          proxyAttr a as
              | Just u <- lookup a as
              , pu <- proxy PTStreamingContent u =
                  (if js ps then addOrReplaceAttr a pu else id)
                  $ addOrReplaceAttr ("data-proxied-" <> a) pu
                  $ addOrReplaceAttr ("data-nonproxied-" <> a) u
                  $ filter ((/= a) . fst) $ fixStyle as
              | otherwise = fixStyle as
          add t attrName =
              map (\ case
                  (n, v) | n == attrName -> (n, proxy t v)
                  a -> a)
              . fixStyle
          fixStyle = map $ \ case
              ("style", s)
                  | isJust $ T.find (== '(') s ->
                      ("style", CSS.serialize $
                          fixCssUrl (proxy PTContentStyle) $ safeCssTokenize s)
              (n, v)
                  | n == dataBqrVideoPoster ->
                      (n, proxy PTContentStyle v)
              a -> a

nonProxiedClass = "nonProxied"
proxiedClass = "proxied"

apostrophize t
    | T.any (== '\'') t = TL.toStrict $ TLB.toLazyTextWith len $ go t
    | otherwise = t
    where T.Text _ _ len = t
          go t = case t of
              '\'' :. a :. '0' :. c :. ts
                  | isDigit a && (c == 's' || c == 'S') && end ts
                  -- '60s -> ’60s
                  -> s '’' <> s a <> s '0' <> s c <> go ts
              '\'' :. a :. ts
                  | isDigit a
                  , (TLB.fromText -> restA, ts') <- T.span isDigit ts
                  , end ts'
                  -> case ts' of
                      '\'' :. ts''
                          | end ts'' ->
                              s '‘' <> s a <> restA <> s '’' <> go ts''
                          | otherwise ->
                              s '\'' <> s a <> restA <> s '\'' <> go ts''
                      _ ->
                          s '’' <> s a <> restA <> go ts'
              '\'' :. a :. ts
                  | letter a
                  , (restA, ts') <- T.span letter ts
                  -- игнорируем слово, начинающееся с апострофа
                  -> s '\'' <> s a <> TLB.fromText restA <> go ts'
              c :. ts
                  | letter c -> s c <> word c ts
                  | otherwise -> s c <> go ts
              _ -> mempty
          word p t = case t of
              '\'' :. ts
                  | end ts ->
                      s (if isDigit p then '\'' else '’') <> go ts
                      -- апостроф после цифры может быть единицей измерения:
                      -- фунтом, минутой (118° 19' 43.5")
              '\'' :. c :. ts
                  | letter c ->
                      s '’' <> s c <> word c ts
              c :. ts
                  | letter c -> s c <> word c ts
                  | otherwise -> s c <> go ts
              _ -> mempty
          letter c = FC.isAlphaMark c || isDigit c || c == '-'
          end t = case T.uncons t of
              Nothing -> True
              Just (c,_) -> FC.isPunctuationOrSpace c
          s = TLB.singleton


hyphenate check l = snd . hyphenate' check l . normalizeUnicode

normalizeUnicode x
    | ICU.quickCheck ICU.NFC x == Just True = x
    | otherwise = ICU.normalize ICU.NFC x

hyphenate' check (l, acceptLanguage) (apostrophize -> t)
    | T.any alpha t =
        (lang, TL.toStrict $ TLB.toLazyTextWith (len * 2) $ go True t)
    | otherwise = (([],""), t)
    where T.Text _ _ len = t
          lang = case l of
              Detected l -> l
              Detect pre -> LD.detect $ T.unwords [pre, t]
          -- переносим только space opening_bracket? alpha+ punctuation* space
          -- причем без больших букв (кроме первой), т.к. слово может быть
          -- сокращением
          go prevIsSpace t = case T.uncons t of
              Nothing -> mempty
              Just (c, ts)
                  | alpha c && prevIsSpace -> goWord 1 [c] ts
                  | otherwise ->
                      s c <> go (space c || FC.isOpening c) ts
          goWord n acc t = case T.uncons t of
              Nothing -> hyphAcc n acc
              Just (c, ts)
                  | alpha c && not (upper c) && n < maxWordLength ->
                      goWord (n+1) (c:acc) ts
                  | space c ->
                      hyphAcc n acc <> s c <> go True ts
                  | punctuation c ->
                      goPunct n acc [c] ts
                  | otherwise ->
                      sacc (c:acc) <> go False ts
          goPunct n acc pacc t = case T.uncons t of
              Nothing -> hyphAcc n acc <> sacc pacc
              Just (c, ts)
                  | space c ->
                    hyphAcc n acc <> sacc (c:pacc) <> go True ts
                  | punctuation c ->
                    goPunct n acc (c:pacc) ts
                  | otherwise ->
                    sacc acc <> sacc (c:pacc) <> go False ts
          hyphAcc n acc
              | n < minWordLength = sacc acc
              | otherwise = hyph acc
          maxWordLength = 50
          -- "мультиинструменталист" 21
          -- "дихлордифенилтрихлорметилметан" 30
          minWordLength = 5
          sacc = TLB.fromString . reverse
          s = TLB.singleton
          alpha = FC.isAlphaMark
          upper = FC.isUpper
          space c = FC.isSpace c || c == '-' || c == '’' || c == '—'
          punctuation = FC.isPunctuation
          hyphenators =
              [(LD.alphabet l, h) | l <- fst lang
              ,Just h <- [LD.languageHyphenator l acceptLanguage]]
          hyph t =
              maybe (TLB.fromString (reverse t)) (flip hyph' t . snd) $
              find (\(a,_) -> all (FC.inAlphabet a) t)
              hyphenators
                -- english_US делает странные переносы: me­trop­o­lis,
                -- а вот english_GB нормально: met­ro­polis
                -- но у нас больше пользователей из US.
                -- надо по Accept-Language определять
          hyph' dict (reverse -> word) = TLB.fromString $
              if check word then
                  fix $ intercalate [softHyphen] $ Hyph.hyphenate dict word
              else
                  word
          -- убираем переносы в лигатурах (иначе они перестают отображаться)
          fix [] = []
          fix ('f':h:c:xs) | h == softHyphen && FC.inAlphabet ligatureChar c =
              'f' : fix (c:xs)
          fix (x:xs) = x : fix xs

ligatureChar = FC.mkAlphabet "filjt"


softHyphen = '\xAD' -- &shy; SOFT HYPHEN

hyphenateHtml =
    editForest $ snd . hyphenateForest normalizeUnicode (Detected ([LD.English],""), "")

-- Чтобы правильно переность/не переносить текст, занимающий несколько span-ов
-- ("<span>foo</span>bar", "@<span>handle</span>'s")
-- мы делаем два прохода по дереву: извлекаем текст (hForestText),
-- склеиваем его, добавляем переносы, разрезаем обратно на части (hSplitText) и,
-- вторым проходом, заменяем текст в дереве (hChangeForestText).
-- Оба прохода реализованы через общую ф-ю hZipForest, чтобы гарантированно
-- иметь одинаковую последовательность действи при замене.

data HyphenateLanguage
    = Detect Text
       -- ^ текст для добавления при определении языка
    | Detected Lang
type Lang = ([LD.Language], Text)

hyphenateForest :: (Text -> Text) -> (HyphenateLanguage, Text) -> Forest -> (Lang, Forest)
hyphenateForest pre l f = (lang, hChangeForestText f $ hSplitText t ht)
    where t = map pre $ hForestText f
          (lang, ht) = hyphenate' (const True) l $ T.concat t

hSplitText [] _ = []
hSplitText (t:ts) ht = go t ht
    where go a b = case (uc a, uc b) of
              (Nothing, Just (cb, bs))
                  | cb == softHyphen
                  , not $ startsWithSHyphen ts
                  -> ret bs
              (Nothing, _) -> ret b
              (Just (ca, as), Just (cb, bs))
                  | ca == cb || ca == '\'' && (cb == '’' || cb == '‘') ->
                      go as bs
                  | cb == softHyphen
                  , Just (cb', bs') <- uc bs
                  , ca == cb' -> go as bs'
              _ ->
                  error $ "hSplitText: bad text "
                      ++ show (T.take 20 a, T.take 20 b)
          ret b
              | T.Text a o1 _ <- ht
              , T.Text _ o2 _ <- b
              = T.Text a o1 (o2 - o1) : hSplitText ts b
          startsWithSHyphen (t:ts)
              | Just (ca', _) <- uc t = ca' == softHyphen
              | otherwise = startsWithSHyphen ts
          startsWithSHyphen _ = False
          w2c = toEnum . fromEnum
          uc (T.Text a o 0) = Nothing
          uc (T.Text a o l) =
              Just (w2c $ A.unsafeIndex a o, T.Text a (o+1) (l-1))
          -- используем свой вариант T.uncons, т.к. T.uncons в конце
          -- возвращает T.empty, из-за чего ломаются смещения

textToHyphenate =
    normalizeUnicode . T.concat . hForestText . buildForest . parseTagsT . tbs

hForestText f = evalState (hZipForest retNode zipNode f) []
    where retNode node text g = (text :) <$> g
          zipNode name attrs level c ns t = do
              children <- c
              n <- ns
              return $ (t:children) ++ (t:n)
hChangeForestText f t = evalState (hZipForest retNode' zipNode' f) t
    where retNode' n _ g = do
              s <- get
              case s of
                  (t:ts) -> do
                      put ts :: State [Text] ()
                      (case n of TextNode _ -> (TextNode t:); _ -> (n:)) <$> g
                  _ ->
                      error "hChangeForestText: empty state?"
          zipNode' name attrs level c ns t = do
              modify tail :: State [Text] ()
              children <- c
              modify tail
              n <- ns
              return $ Node name attrs level children : n

hZipForest :: (Node -> Text -> State s [a] -> State s [a])
    -> (Text -> [(T.Text, T.Text)] -> NodeLevel
        -> State s [a] -> State s [a] -> Text -> State s [a])
    -> Forest -> State s [a]
hZipForest retNode zipNode = go
    where go [] = return []
          go (n:ns) = case n of
              -- за исключением monospace, код такой же как в forestTextL
              TextNode t -> retNode n t $ go ns
              VoidElement {..} -> case newlineVoidElement name attrs of
                  Newline -> retNode n "\n" $ go ns
                  Text -> retNode n " " $ go ns
                  Empty -> retNode n "" $ go ns
              Node {..}
                  | monospace name attrs ->
                      retNode n "-" $ go ns -- не переносим слитный код
                  | name `elem` ["script", "style", "noscript"] ->
                      retNode n "" $ go ns
                  | name `elem` ["audio", "video", "iframe", "svg", "math"] ->
                      retNode n " " $ go ns
                  | Just c <- lookup "class" attrs
                  , elem "iframe_placeholder" (T.words c) ->
                      retNode n " " $ go ns
                  | otherwise ->
                      zipNode name attrs level (go children) (go ns) $
                      case newlineNode name attrs of
                          Newline -> "\n"
                          Text -> " "
                          Empty -> ""
          monospace name attrs
              | monospaceTag name = True
              | Just s <- lookup "style" attrs
              = "monospace" `T.isInfixOf` T.toLower s
              | otherwise = False

-- | Добавляем стиль bqrHeader параграфам, весь текст внутри которых является
-- жирным. Часто в редакторах постов люди просто помечают параграф жирным,
-- вместо использования разметки заголовка.
-- Также длинные жирные параграфы часто бывают в интервью, их можно было бы
-- оставить обычным шрифтом, но с различием в шрифтах и дополнительным отступом
-- они смотрятся не так уж и плохо.
--
-- Еще возможны варианты:
--   <br><br><strong>...</strong><br><br>
--      ==> <p class=bqrHeader>...</p>
--   <br><br><strong>...</strong><b>...</b><br><br>
--      ==> <p class=bqrHeader><strong>...</strong><b>...</b></p>
--   <p><strong>...</strong><b>...</b><br><br>...
--      ==> <p><span class=bqrHeader><strong>...</strong><b>...</b></span>...
-- но мы такое не обрабатываем.
--
-- Одиночный <br> недостаточен -- шрифт заголовка, за которым сразу же,
-- без зазора, следует обычный текст, смотрится странно.
--
addBqrHeader = map add
    where add n = case n of
              Node {..}
                  | name `elem` stoplist ->
                      n
                  | name == "p" ->
                      if newlineNode name attrs == Newline && bold children
                      then
                          Node { attrs = addClass "bqrHeader" attrs, .. }
                      else
                          n
                  | otherwise ->
                      Node { children = addBqrHeader children, .. }
              _ -> n
          bold elems =
              foldr combine Nothing (map (bold' Nothing) elems) == Just True
          bold' pb = \ case
              TextNode t
                  | not (emptyText t) && pb == Nothing -> Just False
                  | otherwise -> pb
              VoidElement {..} -> notNewline $ newlineVoidElement name attrs
              Node {..} ->
                  notNewline (newlineNode name attrs)
                  `combine`
                  foldr combine Nothing
                  (map (bold' (if name `elem` ["strong","b"]
                               then Just True else pb)) children)
          combine Nothing      x       = x
          combine (Just False) _       = Just False
          combine (Just True)  Nothing = Just True
          combine (Just True)  x       = x
          notNewline Newline = Just False
          notNewline _ = Nothing
          stoplist =
              [ "dl", "ol", "ul", "li", "dd", "dt", "pre", "table"
              , "object"
              , "video", "audio"
              , "svg", "math" -- не ломаем ничего внутри
              ]

-- | Добавляем class=numbersOnly к <sub> и <sup>,
-- чтобы затем использовать в ридере "font-variant-position: sub/super",
-- если шрифт это поддерживает.
markNumbersOnlySubSup :: Forest -> Forest
markNumbersOnlySubSup = map $ \ case
    n@(TextNode _) -> n
    v@(VoidElement {}) -> v
    Node {..}
        | name `elem` ["sub", "sup"] && all numbersOnly children ->
           Node { attrs = addClass "numbersOnly" attrs, .. }
        | otherwise ->
           Node { children = markNumbersOnlySubSup children, .. }
    where numbersOnly = \ case
              TextNode t -> T.all (\ c -> isDigit c || FC.isSpace c) t
              VoidElement {} -> False
              Node {..} ->
                  newlineNode name attrs == Empty && all numbersOnly children

-- для внешних ридеров подставляем стили вместо классов
replaceClassesByStyles = map $ \ case
    TagOpen n as
        | Just c <- lookup "class" as
        , (c', s') <- go [] [] $ T.words c
        -> TagOpen n $
           (if null c' then filter ((/= "class") . fst)
            else addOrReplaceAttr "class" $ T.unwords c') $
           (foldr addStyle as s')
    t -> t
    where go acc s [] = (reverse acc, s)
          go acc s (c:cs) = case c of
              "scalable" -> skip -- go acc (scalableStyle : s) cs
              "inner" -> skip -- go acc (innerStyle : s) cs
              "imgScaler" -> skip -- go acc ("display: inline-block" : s) cs
              "iframeScaler" -> skip -- go acc ("display: block; width: 100% !important" : s) cs
              "bqrShareFavicon" -> go acc ("display: inline-block !important; width: 16px !important; height: 16px !important; padding-rigth: 0.5em !important" : s) cs
              "bqrShareLink" -> go acc ("font-weight: bold" : s) cs
              "bqrShareImage" -> go acc ("max-width: 150px; padding-right: 1em" : s) cs
              "bqrSharedPostAvatar" ->
                  go acc ("float: left; width: 1.5em; height: 1.5em; padding-right: 0.5em" : s) cs
              "bqrVKSharedPostAvatar" ->
                  go acc ("float: left; width: 2.3em; height: 2.3em; padding-right: 0.5em" : s) cs
              _ -> go (c:acc) s cs
              where skip = go acc s cs

ignoredTags = HS.fromList
    ["link", "frame", "frameset", "noframes", "applet", "dialog"]

dropTillIncluding x xs
    | (_:xs') <- dropWhile (/= x) xs = xs'
    | otherwise = []

-- if (width <= 320) {
--     innerHtml.push(' style="transform: scale(0.61);"');
-- } else if (width <= 640) {
--     innerHtml.push(' style="transform: scale(0.85);"');
-- }
youtubeButton = buildForest $ parseTagsT
    "<button class='ytp-large-play-button ytp-button'>\
    \<svg height='100%' version='1.1' viewBox='0 0 68 48' width='100%'>\
    \<path class='ytp-large-play-button-bg' d='m .66,37.62 c 0,0 .66,4.70 2.70,6.77 2.58,2.71 5.98,2.63 7.49,2.91 5.43,.52 23.10,.68 23.12,.68 .00,-1.3e-5 14.29,-0.02 23.81,-0.71 1.32,-0.15 4.22,-0.17 6.81,-2.89 2.03,-2.07 2.70,-6.77 2.70,-6.77 0,0 .67,-5.52 .67,-11.04 l 0,-5.17 c 0,-5.52 -0.67,-11.04 -0.67,-11.04 0,0 -0.66,-4.70 -2.70,-6.77 C 62.03,.86 59.13,.84 57.80,.69 48.28,0 34.00,0 34.00,0 33.97,0 19.69,0 10.18,.69 8.85,.84 5.95,.86 3.36,3.58 1.32,5.65 .66,10.35 .66,10.35 c 0,0 -0.55,4.50 -0.66,9.45 l 0,8.36 c .10,4.94 .66,9.45 .66,9.45 z' fill='#1f1f1e' fill-opacity='0.9'></path>\
    \<path d='m 26.96,13.67 18.37,9.62 -18.37,9.55 -0.00,-19.17 z' fill='#fff'></path>\
    \<path d='M 45.02,23.46 45.32,23.28 26.96,13.67 43.32,24.34 45.02,23.46 z' fill='#ccc'></path>\
    \</svg>\
    \</button>"

tryReplaceIframe :: PreprocessSettings -> [(T.Text, T.Text)] -> Maybe Node
tryReplaceIframe ps as = do
    src <- lookup "src" as
    tryReplaceIframeYouTube ps src as
        <|> tryReplaceIframeVimeo src as
        <|> tryReplaceFbVideo src as

tryReplaceFbVideo src as
    | Just poster <- lookup dataBqrVideoPoster as =
       Just $ videoPlaceholder' dataBqrVideoPoster (addSrcAutoplay src) $
       Node
       { name = "iframe"
       , attrs = as
       , children = []
       , level = InlineLevel }
    | otherwise = Nothing

-- все <div> заменены на <span>, т.к. div/header/h1/h2/… закрывают <p>
-- вместе со всеми предыдущими span-ами и переходят на уровень этого <p>,
-- ломая структуру DOM (ну и в принципе <iframe> - это inline элемент
-- и заменять его div-ами некорректно)
vimeoInner = buildForest $ parseTagsT
    "<span class='video-wrapper'><span class='video cover'></span></span>\
    \<span class='title' role='contentinfo'><span class='header'>\
      \<span class='portrait' aria-hidden='true'></span>\
      \<span class='headers'>\
      \<span class='h1'><a href='' target='_blank'></a></span>\
      \<span class='sub-title'><span class='h2 byline-from'>from <a href='' target='_blank'></a></span></span>\
    \</span></span></span>\
    \<span class='controls'>\
      \<button class='play rounded-box state-paused'><span class='play-icon'><svg viewBox='0 0 20 20' preserveAspectRatio='xMidYMid'><polygon class='fill' points='1,0 20,10 1,20'></polygon></svg></span></button>\
    \</span>"

tryReplaceIframeVimeo src as = do
    vId <- vimeoVideoId src
    return $ replaceWithIframe (vimeoEmbedUrl vId) as
        [span_
            [("class", "vimeo vimeo-unprocessed visibilityHidden")
            ,("data-video-id", vId)]
            vimeoInner]

tryReplaceIframeYouTube ps src as = do
    vId <- youtubeVideoId src

    return $ replaceWithIframe src as $
        [span_
            [("class", "youtube-unprocessed")
            ,("data-video-id", vId)]
            [span_
                [("class", "ytp-thumbnail")
                ]
                youtubeButton
            ,span_ [("class", "ytp-gradient-top")] []
            ,span1 [("class", "ytp-chrome-top")] $
             span1 [("class", "ytp-title")] $
             span1 [("class", "ytp-title-text")] $
             Node "a"
                [("class", "ytp-title-link")
                ,("target", "_blank"), ("rel", "noopener")
                ,("href", T.append "https://www.youtube.com/watch?v=" vId)]
                InlineLevel []
            ]]

replaceWithIframe src as inner =
    placeholder
        [("class", "iframe_placeholder")]
        (Node
         { name = "iframe"
         , attrs =
            addSrcAutoplay src $
            addOrReplaceAttr "style" "background-color: black" as
            --  ^ необходимо, т.к. стиль заданный через CSS игнорируется
            --    и фон в начале загрузки остается белым
         , children = []
         , level = InlineLevel })
        inner

fixIframe ps ia go (dropTillIncluding (TagClose "iframe") -> ts)
    -- браузеры игнорируют все, что внутри <iframe> до </iframe>
    | Just (fixIframeUrl ps -> src) <- lookup "src" ia =
      if any (`T.isPrefixOf` src)
          ["https://www.facebook.com/plugins/like.php"
          -- у нас есть share, кнопку уберем
          -- (Feedly, кстати, тоже вырезает)
          ,"https://slashdot.org/slashdot-it.pl?op=discuss"
          -- очень уж долго грузятся комменты
          -- (и Feedly их тоже режет ;)
          ]
          ||
          mimeByUrl src == "application/x-shockwave-flash"
          -- Chrome патается скачивать iframe с swf-файлом,
          -- лучше совсем их вырезать
      then
          go ts
      else if "fbcdn" `T.isInfixOf` src && ".mp4" `T.isInfixOf` src
          && lookup "class" ia == Just "bqrMsgVideo"
      then
          go $ embedVideo "video/mp4" Nothing Nothing False src ++ ts
      else
          iframe (++ filter (goodAttr . fst) ia)
              src
          ++ go ts
    | otherwise = go [] -- нет src
    where goodAttr a = a `elem` ["style", "width", "height"]
              || "data-bqr-" `T.isPrefixOf` a

-- для <iframe style='width:100%' height=50px src=http://mysite>
-- <span class=scalable style='height: 50.0px'>
--   <iframe class=inner ... height='50'>

-- <span class=iframeScaler style='--aspect-ratio: 0.5625'>
--   <span class=scalable style='padding-bottom: 56.25%'>
--     <iframe class=inner ..>
-- либо нет, либо изначально было relative+absolute
-- надо добавлять 560 x 315 (как у youtube)

-- <span class=iframeScaler style='width: 560.0px; --aspect-ratio: 0.5625'>
--   <span class=scalable style='padding-bottom: 56.25%'>
--     <iframe class=inner .. width='560' height='315'>

-- для <img style='background: black; max-width: 420px; width: 420px; height: 280px' border=2 vspace=4 width=200 height=100>
-- <span class=imgScaler style='width: 420.0px; --aspect-ratio: 0.6666666666666666; border: 2px solid black; margin-top: 4px; margin-bottom: 4px'>
--   <span class=scalable style='padding-bottom: 66.66666666666666%'>
--     <img class=inner width=200 height=100 style='background: black'>

-- | Обратная addScalables операция -- превращаем масштабируемые элементы
-- обратно в элементы с фиксированным размером и копируем обратно стили.
-- Это нужно для приложений (Reeder), которые пытаются по-своему заменить
-- youtube iframe/video, что конфликтует с нашими scalable и приводит к кривому
-- отображению видео (маленькое, или не влезает в заданную область, или остаются
-- пустые поля).
--
-- Просто убрать addScalables не получится, т.к. наши embedYoutube (и хабр)
-- делают аналог scalable на атрибутах, который всё равно надо убирать.
removeScalables :: Forest -> Forest
removeScalables = map $ \ case
    o@(Node {})
        | isScalable o
        , [i] <- children o
        , isInner i
        -> extract i (attrs o)
        | hasClass "imgScaler" (attrs o) || hasClass "iframeScaler" (attrs o)
        , [s@(Node {})] <- children o
        , isScalable s
        , [i] <- children s
        , isInner i
        -> extract i (attrs o)
    Node {..}
        -> Node { children = removeScalables children, .. }
    x -> x
    where isInner = hasClass "inner" . nodeAttrs
          isScalable = hasClass "scalable" . nodeAttrs
          extract n oa = editAttrs (addStyles oa . removeClass "inner") n
          addStyles oa = case parseCssStyle <$> lookup "style" oa of
              Just (("width", [cssPixels -> Just w]) : (ar -> Just a) : as) ->
                  addW w a as
              Just ((ar -> Just a) : as) ->
                  addW 560 a as
              Just as -> addStyle "width: 100%" . addS as
              Nothing -> id
          ar ("--aspect-ratio", [r]) = cssPixels r
          ar _ = Nothing
          addS s = addStyle (renderCssStyle s)
          addW w a s =
              addOrReplaceAttr "width" (showT $ truncate w)
              . addOrReplaceAttr "height" (showT $ truncate $ w * a)
              . addS s


-- Создаем scalable/inner из пар position:relative/position:absolute
-- или ширины/высоты для iframe, video и img.
-- Для всех остальных элементов исправляем style-атрибут, убирая position
-- и многие другие параметры.
addScalables :: PreprocessSettings -> Forest -> Forest
addScalables ps = map $ \ case
    t@(TextNode _) -> t
    VoidElement n (fixStyle -> as)
        | n == "img" -> wrapImg as
        | iframeName n -> makeScalable n as (VoidElement n as)
          -- embed является VoidElement-ом
        | otherwise -> VoidElement n as
    o@(Node {})
        | name o == "svg" || name o == "math" -> fixNodeStyles o
          -- ничего не делаем с iframe/video внутри svg,
          -- только исправляем стили
        | [i@(Node {})] <- filter (not . emptyNode) (children o)
        , iframeName (name i)
        , Just (ratio, p) <- aspectRatioPadding (attrs o) (attrs i)
          -- теряются атрибуты outer-элемента, но они терялись и до этого
        -> embedInScalable (iframeScaler ratio Nothing . scalable' p) i
        | name o == "audio" ->
           editAttrs (addStyle audioStyle . fixStyle) o
    i@(Node { name = name@(iframeName -> True), .. }) ->
        makeScalable name attrs i
    Node {..} ->
        Node
        { children = addScalables ps children
        , attrs = fixStyle attrs
        , .. }
    where makeScalable name attrs i
              -- Все iframe и видео у нас всегда на всю ширину
              -- (класс iframeScaler специально для этого имеет width:100%)
              -- youtube-видео часто ограничены по ширине 640px,
              -- iframe без параметров вообще имеет размер 300х150px.
              -- Чтобы они выглядели хорошо, всегда делаем их на всю ширину,
              -- а width/height используем исключительно для задания
              -- соотношения сторон
              | Just is <- combinedCss attrs
              , Just [CSS.Percentage {}] <- lookup "width" is
              , Just h <- pixels "height" is
              = embedInScalable (scalable' ("height: " <> showT h <> "px")) i
--                 editAttrs
--                   (addStyle $
--                    "width: 100%; height: " <> showT h <> "px; overflow: hidden")
--                   (embedInScalable id i)
                -- фиксированная высота
                -- iframe width=100% height=120px -- soundcloud
                -- и прочие плееры звуков, резиновые по ширине, ограниченные
                -- по высоте
              | Just (w, h) <- pixelWH attrs
                -- конкретная, не процентная высота/ширина
              = whIframe w h i
              | name == "iframe"
              , Just is <- combinedCss attrs
              , Just w <- pixels "width" is
              , Nothing <- lookup "height" is
              = whIframe w 150 i
                -- ширина и высота iframe по-умолчанию 300 на 150 px
              | name == "iframe"
              , Just is <- combinedCss attrs
              , Nothing <- lookup "width" is
              , Just h <- pixels "height" is
              = whIframe 300 h i
              | otherwise
              = -- ничего не определили -- используем соотношение 16:9
                embedInScalable
                (iframeScaler (9/16) Nothing . scalable (9/16)) i
          fixStyle = mapMaybe $ \ case
              ("style", s) -> case fixCssStyle ps s of
                  "" -> Nothing
                  s' -> Just ("style", s')
              a -> Just a
          fixStyles = map fixNodeStyles
          fixNodeStyles = \ case
              t@(TextNode _) -> t
              VoidElement n as -> VoidElement n (fixStyle as)
              Node {..} ->
                  Node { children = fixStyles children
                       , attrs = fixStyle attrs, .. }
          iframeName n = n `elem` ["iframe", "video", "embed", "object"]
          combinedCss = fmap reverse . combinedCssStyle
          emptyNode (TextNode t) = emptyText t
          emptyNode _ = False
          iframeScaler = limitWidth "iframeScaler"
          whIframe w h i
              | w <= 1 || h <= 1 =
                  TextNode ""
                  -- отслеживалка??
              | otherwise =
                  embedInScalable
                  (iframeScaler (h / w) (Just w) . scalable (h / w)) i

-- aspectRatioPadding outerElementAttributes innerElementAttributes
aspectRatioPadding oa ia
    -- может быть padding-bottom: N%; и padding-top: 38px
    -- чтобы панель управления slideshare сохранилась
    -- по-этому, возвращаем и padding-top и padding-bottom как есть,
    -- если хотя бы один из них %.
    | Just os <- parseCssStyle <$> lookup "style" oa
    , Just is <- parseCssStyle <$> lookup "style" ia
    , Just "relative" <- cssPosition os
    , Just "absolute" <- cssPosition is
    , r <- filter
        ((`elem` ["padding-top", "padding-bottom", "padding"]) . fst) os
    , (p:_) <- mapMaybe validPadding $ map snd r
    = Just (p, renderCssStyle r)
    | otherwise = Nothing
    where validPadding (CSS.Percentage p (cssNV -> n):pr)
              | validPaddingRest pr && validPaddingPercentage n = Just (n / 100)
          validPadding _ = Nothing
          validPaddingRest [] = True
          validPaddingRest
              -- "padding: 50% 0 0 0;"
              [CSS.Whitespace, (cssPixels -> Just 0.0)
              ,CSS.Whitespace, (cssPixels -> Just 0.0)
              ,CSS.Whitespace, (cssPixels -> Just 0.0)] = True
          validPaddingRest _ = False

pixelWH :: [(T.Text, T.Text)] -> Maybe (Double, Double)
pixelWH ia = do
    (reverse -> is) <- combinedCssStyle ia
    w <- pixels "width" is
    h <- pixels "height" is
    guard (w > 0 && h > 0)
    return (w, h)

pixels :: T.Text -> [(T.Text, [CSS.Token])] -> Maybe Double
pixels n is
    | Just [cssPixels -> p] <- lookup n is = p
    | otherwise = Nothing

limitWidth c ar w inner =
    -- важно именно span, т.к. div разрывает параграф <p> даже если он
    -- display:inline-block
    Node "span"
        (("class", c)
         : ("style", T.concat $
            maybe [] (\ px -> ["width: ", showT px, "px; "]) w
            <>
            ["--aspect-ratio: ", showT ar])
            -- по-идее, хорошо бы еще и --vertical-padding в пикселах,
            -- чтобы iframe-ы с комбинированным padding тоже отрабатывать,
            -- но они редки, так что пока не заморачиваемся
         : filter (== noTranslate) ia)
        InlineLevel [inner']
    where (inner', ia) = editAttrs' (filter (/= noTranslate)) inner
          -- выносим translate=no на уроветь img/iframeScaler

-- Чтобы Google Translate не ломал наши scaler-ы, добавляем атрибут translate=no,
-- но он все равно переводит iframe-ы (даже если самому iframe установить
-- translate=no и class=notranslate) и убирает span-ы вокруг iframe.
-- Оставим, вдруг починят.
noTranslate = ("translate", "no")

-- TODO: объединить limitWidth, scalable и embedInScalable
-- отдельный элемент limitWidth необязателен
scalable aspectRatio inner =
    scalable' (aspectRatioPaddingBottom aspectRatio) inner
scalable' padding inner =
    Node "span" [("class", "scalable"), ("style", padding), noTranslate]
        InlineLevel [editAttrs (addClass "inner") inner]

-- Перенос стилей из встраимого элемента в итоговый scalable
-- В scalable переносятся margin/border/float/clear/vertical-align,
-- т.е. стили, необходимые внешнему элементу (не работают или неправильно
-- работают, если останутся в inner)
-- В inner остается только background.
-- Получается совсем жесткий вариант fixCssStyle,
-- но для video/iframe/img вставленных в scalable больше и не нужно
embedInScalable f inner = addInnerStyle $ f inner'
    where (inner', ia) = editAttrs' (mapMaybe fixIA) inner
          fixIA ("class", _) = Nothing
          fixIA ("style", s)
               | null s' = Nothing
               | otherwise = Just ("style", renderCssStyle s')
               where s' = filter (T.isInfixOf "background" . fst) $
                         parseCssStyle s
          fixIA a = Just a
          ok p =
              p `elem` ["float", "clear", "vertical-align"] ||
              any (`T.isInfixOf` p) ["margin", "border"]
          addInnerStyle
              | Just s <- parseCssStyle <$> lookup "style" ia
              , s' <- filter (ok . fst) s
              , not $ null s'
              = editAttrs (addStyle $ renderCssStyle s')
              | otherwise = id

editAttrs f = fst . editAttrs' f
editAttrs' f = \ case
    Node {..} -> (Node { attrs = f attrs, .. }, attrs)
    VoidElement {..} ->
        (VoidElement { attrs = f attrs, .. }, attrs)
    t -> (t, [])
nodeAttrs = \ case
    Node {..} -> attrs
    VoidElement {..} -> attrs
    t -> []

-- проверить ссылку внутри div во flickr?
-- проверить производительность прокрутки на фидах Indexed, Digital Photography School

-- Если img задать max-width: 100%, но при этом у него задан height,
-- то картинка будет сжата по ширине, но не по высоте.
-- Если же поставить height: auto !important
-- (important, чтобы style="height:123px" тоже перезаписывал),
-- то, пока картинка не загрузилась, браузер задает ей нулевую высоту,
-- что вызывает дополнительные раздвигания постов по мере загрузки.
-- Также могут быть проблемы с картинками, где высота специально установлена
-- (так, как она установлена у ljpoll для полосок).
-- По-этому, вставляем такой же scaler, как для iframe
-- и оставляем max-width: 100% без height:auto
wrapImg as =
    -- height в % вырезается в fixCssStyle
    -- получается
    --  - width px, height px  -- обрабатываем, масштабируем
    --  - width %, height px -- ограничивать max-width:100%
    --  - no width, height px -- тоже max-width:100%
    --  - no width, no height -- тоже max-width:100%,
    -- height auto нигде не нужен
    case pixelWH as of
        Just (w, h) ->
            embedInScalable
                (limitWidth (withImgClass "imgScaler") (h/w) (Just w)
                 . scalable (h / w)) $
                VoidElement "img" as
                -- (if w * h > 1000*700 then map cdnSrc else id) $
                -- на картинке размером 5760х3840 (22M)
                -- Google CDN выдает "The image is too large to resize"
        _ | isNothing $ lookup "class" as ->
            -- если class уже есть, то и размер известен
            -- (например, bqrSharedPostAvatar)
            VoidElement "img" (("class", "bqrUnknownImgSize") : as)
            -- для restoreLoadedImgSizes
          | otherwise ->
            VoidElement "img" as
    where withImgClass c
              | Just orig <- lookup "class" as = T.unwords [c, orig]
              | otherwise = c

fixImg msgLink ps as0 xs
    | Just s <- lookup "src" as
    , correctSrc s =
         TagOpen "img" as : xs
    | otherwise = xs
    where as = fixImgAttrs msgLink ps as0
          correctSrc s =
              any (`T.isPrefixOf` s) ["https://", "http://", "data:"]
              && not (adsImage s)

resizedImgSrc ui thumbnail src
    | Just (prefix, [[_,pp,size,suffix]]) <- resizedImgSrc_blogspot src
    , Just w <- tryReadUnsignedInt =<< width size
    , w > maxWidth || thumbnail
    = T.concat [prefix, pp, "s" <> maxWidthT, suffix]
    | Just (prefix, [[_,pp,size]]) <- resizedImgSrc_blogspot src
    , Just w <- tryReadUnsignedInt size
    , w > maxWidth || thumbnail
    = T.concat [prefix, pp, "w=" <> maxWidthT]
    | thumbnail
    , Just (prefix, [[_,pp]]) <- resizedImgSrc_flickr src
    = T.concat [prefix, pp, "z.jpg"]
    | thumbnail
    , Just (prefix, [[_,pp]]) <- resizedImgSrc_youtube src
    = T.concat [prefix, pp, "0.jpg"]
    | otherwise = src
    where maxWidth = if thumbnail then thumbnailWidth else if ui then 1600 else 700
          maxWidthT = T.pack (show maxWidth)
          width s
              | [[_, w]] <- regexGet "^s([0-9]+)$" s = Just w
              | [[_, w]] <- regexGet "^w([0-9]+)-h[0-9]+$" s = Just w
              | otherwise = Nothing

thumbnailWidth = 480
thumbnailHeight = 320

resizedImgSrc_blogspot =
    pathGet' ["bp.blogspot.com", "ggpht.com", "googleusercontent.com"]
    "(/[^/]+/[^/]+/[^/]+/[^/]+/)([^/]+)(/.*)"
--     | [[_,prefix,_,size,suffix]] <- regexGet "^(https?://[^/]+\\.(bp\\.blogspot|ggpht|googleusercontent)\\.com/[^/]+/[^/]+/[^/]+/[^/]+/)([^/]+)(/.*)" src
resizedImgSrc_wordpress =
    pathGet' ["files.wordpress.com"] "(/.*\\?).*w=([0-9]+)"
resizedImgSrc_flickr =
    pathGet' ["staticflickr.com"] "(/[0-9]+/[0-9]+_[0-9a-f]+_)[mstzb]\\.jpg"
    -- https://farm{farm-id}.staticflickr.com/{server-id}/{id}_{secret}_[mstzb].jpg
    -- s        small square 75x75
    -- q        large square 150x150
    -- t        thumbnail, 100 on longest side
    -- m        small, 240 on longest side
    -- n        small, 320 on longest side
    -- -        medium, 500 on longest side
    -- z        medium 640, 640 on longest side
resizedImgSrc_youtube =
    pathGet' ["img.youtube.com"] "(/vi/[0-9a-zA-Z_\\-]+/)[0-9]+.jpg"

findThumbnail ps = go []
    where ret fn =
              [AThumbnail $ proxyUrlPS PTThumbnail ps $
               resizedImgSrc True True fn]
          go [] [] = []
          go acc [] = ret $ snd $ maximumBy (comparing fst) acc
              -- без reverse, т.к. maximumBy вернет последний из одинаковых
              -- элементов
          go !acc (n:ns) = case n of
              TextNode _ -> go acc ns
              VoidElement "img" as
                  | Just src <- lookup "src" as
                  , not (avatar as) && isHttp src ->
                      if  | Just c <- lookup "class" as
                          , webfeedsFeaturedVisual `elem` T.words c
                          -> ret src
                          | Just w <- tryReadUnsignedInt =<< lookup "width" as
                          , Just h <- tryReadUnsignedInt =<< lookup "height" as
                          -> if w >= 450 && h >= 450 then
                                 ret src
                             else if w > 30 && h > 30 && (w > 40 || h > 40)
                             then
                                 go ((w*h, src) : acc) ns
                             else
                                 go acc ns
                          | otherwise ->
                             go ((0, src) : acc) ns
              VoidElement "embed" as
                  | Just s <- videoPreviewImg =<< lookup "src" as
                  ->  ret s
              VoidElement {} -> go acc ns
              Node {..}
                  | name `elem` ["svg", "math", "audio", "object"] -> go acc ns
                  | name == "iframe"
                  , Just s <- lookup dataBqrVideoPoster attrs
                  , isHttp s
                  ->  ret s
                  | name == "iframe"
                  , Just s <- videoPreviewImg =<< lookup "src" attrs
                  ->  ret s
                  | name == "video"
                  , Just s <- lookup "poster" attrs
                  , isHttp s
                  ->  ret s
                  | name == "a"
                  , Just s <- videoPreviewImg =<< lookup "href" attrs
                  ->  go (bg attrs $ (-1, s) : acc) (children ++ ns)
                  | otherwise ->
                      go (bg attrs acc) (children ++ ns)
          bg as acc
              | Just s <- lookup "style" as
              , T.length s < 10000
                --  ^ в принципе, стили уже исправлены в addScalables
              , isJust $ T.find (== '(') s
              , Just i <- findUrl $ safeCssTokenize s
              =  ((-2, i) : acc)
                 -- приоритет background-image даже меньше ссылки на youtube
              | otherwise = acc
          findUrl [] = Nothing
          findUrl (CSS.Url u : _) = Just u
          findUrl (CSS.Function f : CSS.String u : CSS.RightParen : _)
              | T.toLower f == "url" && isHttp u = Just u
          findUrl (_ : ts) = findUrl ts
          avatar as
              | Just c <- lookup "class" as =
                  T.isInfixOf "Avatar" c || T.isInfixOf "Favicon" c
              | otherwise = False
          videoPreviewImg u
              | Just i <- youtubeVideoId u =
                  Just $ T.concat ["https://img.youtube.com/vi/", i, "/0.jpg"]
              | Just i <- vimeoVideoId u =
                  -- для vimeo необходимо делать дополнительные запросы
                  Just $ T.concat ["/img/vimeo_thumbnail/", i]
              | otherwise = Nothing

fixImgAttrs msgLink ps as = go [] [] as
    where go [] a [] = reverse a
          go s  a [] = reverse $ ("style", T.intercalate "; " $ reverse s) : a
          go s a ((n,v):xs) = case n of
              "vspace" ->
                  go (T.concat ["margin-top: ", v, "px; "
                               ,"margin-bottom: ", v, "px"] : s) a xs
              "hspace" ->
                  go (T.concat ["margin-left: ", v, "px; "
                               ,"margin-right: ", v, "px"] : s) a xs
              "border" ->
                  go (T.concat ["border: ", v, "px solid black"] : s) a xs
              "align"
                  | lv `elem` ["left", "right"] ->
                      go (T.concat ["float: ", lv] : s) a xs
                  | lv `elem` ["middle", "top", "bottom"] ->
                      go (T.concat ["vertical-align: ", lv] : s) a xs
                  | otherwise -> go s a xs
              "style" -> go (v:s) a xs
              "data-lazy-srcset" -> go s a (("srcset", v):xs)
              "srcset"
                  | srcset@(_:_) <- parseSrcset v
                  , (m, _) <- maximumBy (comparing snd) $ reverse srcset
                  -> go s (("data-orig-srcset", v) : rmSrc a)
                      (("src", fromRelUri m) : rmSrc xs)
                  | otherwise
                  -> go s (("data-error-orig-srcset", v) : a) xs
              "sizes" -> go s a xs
              -- srcset не работают под Chrome и Firefox
              "src" -> go s ((n, resizedImgSrc (ui ps) False v):a) xs
              _ -> go s ((n,v):a) xs
              where lv = T.toLower v
                    rmSrc = filter ((/= "src") . fst)
          fromRelUri u
              | Just baseUri <- msgLink = relUriNoNormalizeT u baseUri
              | otherwise = u

data Size
    = W Int
    | X Double T.Text
    deriving (Eq, Ord, Show)

-- fromRelUri msgLink

editSrcset f = renderSrcset . f . parseSrcset

renderSrcset = T.intercalate ", " . map r
    where r (u, W w) = u <> " " <> showT w <> "w"
          r (u, X _ t) = u <> " " <> t <> "x"

parseSrcset :: T.Text -> [(TURL, Size)]
parseSrcset = filter (not . T.isPrefixOf "data:" . fst) . go
    where go = url . T.dropWhile (\ c -> FC.isSpace c || c == ',')
          url (T.break FC.isSpace -> (u, s))
              | T.null u = []
              | T.last u == ',' = x1 (T.dropWhileEnd (== ',') u) : go s
              | otherwise = size u (dropWS s)
          size u s0@(T.span isDigit -> (d, s))
              | T.null d = skipComma (x1 u) s
              | otherwise = case s of
                  'w' :. s' -> skipComma (u, W (readUnsignedInt d)) s'
                  'x' :. s' -> skipComma (u, X (toEnum $ readUnsignedInt d) d) s'
                  '.' :. (T.span isDigit -> (d', 'x' :. s'))
                      | t <- d <> "." <> d' ->
                          skipComma (u, X (read $ T.unpack t) t) s'
                  _ ->
                      go $ T.dropWhile (/= ',') s
          skipComma r (dropWS -> s)
              | T.null s = [r]
              | Just s' <- T.stripPrefix "," s = r : go s'
              | otherwise = go $ T.dropWhile (/= ',') s
          dropWS = T.dropWhile FC.isSpace
          x1 u = (u, X 1.0 "1")

-- | Разделение атрибутов для table_wrapper и table
splitTableAttrs as = go [] [] as
    where go [] t [] = ([], reverse t)
          go w t [] = ([("style", renderCssStyle $ reverse w)], reverse t)
          go w t (a@(n,v):xs) = case n of
              "align"
                  | lv `elem` ["left", "right"] ->
                      -- align center игнорируем
                      go (("float", [CSS.Ident lv]):w) t xs
                  | otherwise -> go w t xs
              "width"
                  | T.isInfixOf "%" v
                  -> go w t xs
              -- "width" -> go (("min-width", [width v]):w) t xs
              -- min-width, чтобы позволять table-у растягивать table_wrapper
              -- оставлять width table-у нельзя, т.к. он может быть в процентах
              -- от ширины wrapper-а, у которого никакой ширины нет.
              "style" ->
                  s w [] (parseCssStyle v)
              _ -> go w (a:t) xs
              where lv = T.toLower v
                    width w
                        | Just i <- tryReadUnsignedInteger w =
                            CSS.Dimension w (CSS.NVInteger $ fromInteger i) "px"
                        | otherwise = CSS.Ident w
                    s w [] [] = go w t xs
                    s w sa [] =
                        go w (("style", renderCssStyle $ reverse sa):t) xs
                    s w sa (a@(n,v):ss)
                        | n == "display" ||
                          (n == "width" && any percent v) = s w sa ss
--                        | n == "width" = s (("min-width",v):w) sa ss
                        | T.isPrefixOf "margin" n || T.isPrefixOf "padding" n
                          || n == "float"
                        = s (a:w) sa ss
                        | otherwise = s w (a:sa) ss


percent (CSS.Percentage _ _) = True
percent _ = False

fixCssStyle ps style =
    renderCssStyle $
        (if ui ps then fixFonts else id) $ filter (not . bad) params
    where params = parseCssStyle style
          fixFonts = map $ \ case
              (n@"font-family", v) -> (n, fixFontFamily v)
              (n@"font", v) -> (n, fixLineHeight $ fixSize $ fixFontFamily v)
              (n@"font-size", v) -> (n, fixSize v)
              a -> a
          fixSize [] = []
          fixSize ((cssPixels -> Just p) : ts)
              | p < 20 = CSS.Percentage "100" (CSS.NVInteger 100) : ts
              | p > 100 = CSS.Dimension "5" (CSS.NVInteger 5) "em" : ts
              | otherwise =
                  CSS.Dimension (showT s) (CSS.NVNumber $ read $ show s) "em"
                  : ts
              where s = p / 20.0
          fixSize (t : ts) = t : fixSize ts
          fixLineHeight = findSlash
              where findSlash [] = []
                    findSlash (t@(CSS.Delim '/') : ts) = t : fixLH ts
                    findSlash (t : ts) = t : findSlash ts
                    fixLH [] = []
                    fixLH (t@CSS.Whitespace : ts) = t : fixLH ts
                    fixLH (t : ts) = case t of
                        CSS.Dimension {} -> normalLH t ts
                        CSS.Percentage {} -> normalLH t ts
                        CSS.Number {} -> normalLH t ts
                        _ -> t : ts
                    normalLH t ts =
                        CSS.Function "var" : CSS.Ident "--current-line-height"
                        : CSS.Comma : t : CSS.RightParen : ts
          hasPosition = cssPosition params `notElem` [Nothing, Just "static"]
          hasPercentPaddingMargin =
              any (\(p,v) -> paddingMargin p && any percent v) params
          position _ = False
          flex [CSS.Ident i] = T.isInfixOf "flex" $ asciiToLowerT i
          flex _ = False
          justify [CSS.Ident i] = T.isPrefixOf "justify" $ asciiToLowerT i
          justify _ = False
          height = (==) "height"
          widthHeight p = p == "width" || height p
          paddingMargin p =
              "padding" `T.isPrefixOf` p || "margin" `T.isPrefixOf` p
          badSize [cssPixels -> Just p] = p > toEnum maxPixels
          badSize _ = False
          bad (p, v) =
              p `elem` ["line-height", "position", "z-index", "top", "left", "right", "bottom", "max-width", "min-width", "max-height", "min-height"]
              -- max-height иногда сочетается с max-width, если убирать
              -- только max-width могут получиться обрезанные или
              -- наезжающие на текст изображения
              ||
              (p == "display" && flex v)
              ||
              (p == "text-align" && justify v)
              ||
              (any percent v && (height p || paddingMargin p))
              ||
              ((widthHeight p || paddingMargin p))
               &&
               (hasPosition
                -- вырезаем все, что относится к position
                || badSize v)
              -- бывает реклама с атрибутом position:fixed
              -- всякие padding-bottom: 75.0019%
              ||
              (widthHeight p && hasPercentPaddingMargin)
              -- бывает "padding-bottom: 56.25%; height: 0"
              -- без position:relative

fixFontFamily = reverse . go [] False . reverse
    where go acc m (t : ts) = case t of
              CSS.Whitespace -> go (t:acc) m ts
              CSS.Comma -> go (t:acc) m ts
              CSS.String s -> go (t:acc) (m || mono s) ts
              CSS.Ident i
                  | T.toLower i `elem`
                      ["normal" -- line height
                       -- font size
                      ,"larger", "smaller", "xx-small", "x-small", "small"
                      ,"medium", "large", "x-large", "xx-large"]
                    -> add acc m (t:ts)
                  | otherwise -> go (t:acc) (m || mono i) ts
              _ -> add acc m (t:ts)
          go acc m [] = add acc m []
          mono f = T.toLower f `HS.member` monospaceFonts
          add [] m ts = add' m ts
          add (CSS.Whitespace:acc) m ts =
              reverse acc <> (CSS.Comma : add' m (CSS.Whitespace : ts))
          add acc m ts =
              reverse acc <> (CSS.Comma : add' m ts)
          add' m ts =
              CSS.RightParen
              : CSS.Ident
                  (if m then
                      "--monospace-font-family"
                   else
                      "--current-font-family")
              : CSS.Function "var" : ts

monospaceFonts = HS.fromList
    ["monospace"
    ,"andale mono", "consolas", "courier", "courier new"
    ,"fira code", "fira mono", "ibm plex mono", "input mono"
    ,"letter gothic std", "lucida console", "lucida sans typewriter"
    ,"menlo", "monaco", "ms gothic", "ms mincho", "orator std"
    ,"prestige elite std", "pt mono", "source code pro"]

bqrYoutubeDurationClass = "bqrYoutubeDuration"

embedYoutubeStats duration _views _likes _dislikes xs =
    -- Отображение числа просмотров и лайков убрал,
    -- т.к. число просмотров часто останавливается на 300,
    -- у новых видео оно мало, а у старых не обновляется.
    -- Выглядит круто, но вещь бестолковая, плюс постоянно обновляются
    -- сообщения, содержащие видео с ютуба
    TagOpen "figcaption" [("class", bqrYoutubeDurationClass)] :
    s "color:#666" : TagText "Time: " : _s :
    s "color:#000;font-weight:bold" : TagText (formatDuration duration) : _s :
    TagClose "figcaption" :
    TagOpen "p" [] : TagClose "p" :
    xs
    where s st = TagOpen "span" [("style", st)]
          _s = TagClose "span"

maxPixels = 10000
-- не позволяем размеры >10k
-- бывает <img width="840" height="2903040" …>

hasDotBetweenAlnumAndAlpha = go ' '
    where go prev t = case T.uncons t of
              Nothing -> False
              Just (c,t')
                  | c == '.'
                  , FC.isLowerAlnum prev
                  , Just (n,_) <- T.uncons t'
                  , FC.isLowerAlnum n
                  -> True
                  | otherwise -> go c t'

mergeTagText = go
    where go [] = []
          go (TagText a : ts)
              | a /= "" = collect [a] ts
              | otherwise = go ts
          go (t:ts) = t : go ts
          collect acc [] = [accTag acc]
          collect acc (TagText a : ts)
              | a /= "" = collect (a:acc) ts
              | otherwise = collect acc ts
          collect acc (t:ts) = accTag acc : t : go ts
          accTag acc = TagText $ T.concat $ reverse acc

-- wget http://tunes.org/~nef/logs/haskell/16.10.07
-- 800K текста
-- ~6-7MB/sec linkifyTagText
-- linkifyTagTextOld 5.5 slower, 3.8 more allocations
linkifyTest = do
    f <- -- fmap (T.replicate 10) $
         T.readFile "16.10.07"
    let pureLinks [] = []
        pureLinks (TagOpen "a" [("href", l)] : TagText t : ts)
            | not (any (`T.isPrefixOf` t) ["http:", "https:", "ftp:"]
                   || "mailto:" `T.isPrefixOf` l)
            = t : pureLinks ts
        pureLinks (_:ts) = pureLinks ts
        links =
            pureLinks $ linkifyTagText f
            -- [l | TagOpen "a" [("href", l)] <- linkifyTagText f]
    print (length links)
    mapM_ T.putStrLn links
    let pre x = T.concat ["<pre>", x, "</pre>"]
    T.writeFile "irc.log.html" . pre . renderTagsT . linkifyTagText =<< T.readFile "16.10.07"

linkifyTagText t
    -- на более-менее больших постах High Scalability
    -- [TagText t]  45msec
    -- с обработкой 55msec
    -- с тестом hasDotBetweenAlnumAndAlpha t
    -- становится снова 45msec
    | hasDotBetweenAlnumAndAlpha t =
        mergeTagText $
        space ' ' 0 t t
    | otherwise =
        [TagText t]
    where space prev !len start t = case T.uncons t of
              Nothing -> [TagText start]
              Just (c,t')
                  | mailChar c ->
                      TagText (T.take len start) :
                      (if prev == '/' then skipLink else id)
                      -- не подсвечиваем части пути файлов
                      -- python ./manage.py
                      (domain False False 1 t t')
                  | otherwise ->
                      space c (len+1) start t'
          skipLink (TagOpen "a" _ : ts) = skipCloseLink ts
          skipLink ts = ts
          skipCloseLink (TagClose "a" : ts) = ts
          skipCloseLink (t : ts) = t : skipCloseLink ts
          skipCloseLink [] = []
          domain pt mail !len start t = case T.uncons t of
              Nothing -> tryDomain pt mail start []
              Just (c,t')
                  | mailChar c -> domain pt mail (len + 1) start t'
                    -- '_' в домене тоже копим,
                    -- потом отфильтруем, заодно asdf.com_port
                    -- не помечается
                  | c == ' ' ->
                      -- копия inCharMask punctuationOrSpaceMask,
                      -- т.к. чаще всего в тексте идут слова и пробелы
                      tryDomain pt mail (T.take len start) $ space c 1 t t'
                  | c == '.' -> domain True mail (len + 1) start t'
                  | c == '@' ->
                      if mail then -- два раза '@'
                          space c (len+1) start t'
                      else
                          domain False True (len+1) start t'
                  | c == ':' && not mail
                  , Just ('/', t') <- T.uncons t'
                  , Just ('/', t') <- T.uncons t'
                  , prefix <- T.take (len+3) start
                  , goodPrefix prefix ->
                      url "" (len+6) start c (len+3) (len+3) t'
                  | c == '/' ->
                      if not mail && validDomain (T.take len start) then
                          url "http://" (len+1) start c (len+1) (len+1) t'
                      else
                          space c (len+1) start t'
                          -- не подсвечиваем e-mail, если за ним идет слеш
                          -- asdf@gmail.com/some/path
                          -- оставляем как есть.
                          --   <asdf@gmail.com>/some/path
                          --   и
                          --   asdf@<gmail.com/some/path>
                          -- одинаково плохи
                  | FC.isPunctuationOrSpace c ->
                      tryDomain pt mail (T.take len start) $ space c 1 t t'
                  | otherwise ->
                      space c (len+1) start t'
          url prefix minLen start prev prevEnd !len t = case T.uncons t of
              Nothing -> tryUrl prefix minLen prev prevEnd len start
              Just (c,t')
                  | urlEndChar c ->
                      url prefix minLen start c (len+1) (len+1) t'
                  | urlChar c ->
                      url prefix minLen start c prevEnd (len+1) t'
                  | otherwise ->
                      tryUrl prefix minLen prev prevEnd len start
          tryUrl prefix minLen prev prevEnd len start
              | prevEnd >= minLen =
                  let (l,ls) = T.splitAt prevEnd start in
                  link (T.append prefix l) l $
                  space prev (len-prevEnd) ls (T.drop len start)
              | otherwise =
                  space prev len start (T.drop len start)
          goodPrefix "http://" = True
          goodPrefix "https://" = True
          goodPrefix "ftp://" = True
          goodPrefix _ = False
          tryDomain pt mail domain xs
              | pt && validDomain domain =
                link (T.append (prefix mail) domain) domain xs
              | otherwise = TagText domain : xs
          prefix mail
              | mail = "mailto:"
              | otherwise = "http://"
          link l t xs =
              TagOpen "a" [("href", l)] : TagText t : TagClose "a" : xs
          validDomain domain
              | d <- T.takeWhileEnd (/= '@') domain
              , T.length d <= 253
              , (tld:sld:other) <- reverse $ T.split (== '.') d
              , isValidDomain tld -- не делаем T.toLower
              , if T.length sld == 1 then
                    isValidSingleLetterDomain $ T.concat [sld, ".", tld]
                else
                    validLabel sld
              , all validLabel other
              = True
                --  test_analysis.py -- подчеркивание валидно? -- нет
                --  и может внутри pre/code не подсвечивать?
                -- .py -- Paraguay
              | otherwise = False
          validLabel l =
              T.length l >= 1 && T.length l <= 63 &&
              T.head l /= '-' && T.last l /= '-' &&
              T.all FC.isDomainLabel l
              -- не подсвечиваем домены с большой буквы
              -- а то всякие Data.Map, TweenMax.to начинают светиться
          mailChar = FC.isMail
          urlChar = FC.isUrl
          urlEndChar = FC.isUrlEnd

validDomains = HS.fromList $
    map (T.pack . head . PublicSuffix.ruleLabels) $
    filter ((== 1) . length . PublicSuffix.ruleLabels) PublicSuffix.rules
{-# NOINLINE validDomains #-}

isValidDomain d = HS.member d validDomains

-- vk.com, fb.com -- все 2 letter .com зарегистрированы
-- все gTLD (Generic TLD) могут регать 2 letter/digit домены,
--  кроме доменов, совпадающих с кодом страны
-- ok.ru
-- Так что, 2 буквы надо позволять всегда
--
-- А вот однобуквенных резольвится достаточно много,
-- какие из них реально существуют понять сложно.
-- Ограничиваемся перечисленными в Wikipedia и publicsuffix.
--
-- https://en.wikipedia.org/wiki/Single-letter_second-level_domain
--
validSingleLetterDomains = HS.fromList $
    [ T.pack $ sl ++ "." ++ tl
    | (tl:sl:_) <- map PublicSuffix.ruleLabels PublicSuffix.rules
    , length sl == 1 && sl /= "*" ]
    ++ T.words "3.dk a.co t.co g.co w.org i.net q.com q.net x.com x.org z.com"
{-# NOINLINE validSingleLetterDomains #-}

isValidSingleLetterDomain d = HS.member d validSingleLetterDomains

-- countryDomains =
--     [ c
--     | [c] <- map PublicSuffix.ruleLabels PublicSuffix.rules
--     , length c <= 2, all (\ x -> x >= 'a' && x <= 'z') c ]
--     ++ ["com", "org", "net"]

-- findSingleCharacterDomains = withDnsCache $ \ c ->
--     forM_ countryDomains $ \ tld ->
--        forM_ (['a'..'z'] ++ ['0'..'9']) $ \ sld -> do
--            let d = sld : '.' : tld
--            r <- resolveA c d
--            case r of
--                Right addr ->
--                    putStrLn $ d ++ "\t" ++ showHostAddress addr
--                _ ->
--                    return ()

-- https://tools.ietf.org/html/rfc3986 -- слишком сложно,
-- Network.URI так работает, Network.URLb работает по упрощенной схеме.
-- Можно regexp ниже ручками имитировать, чтобы еще национальные домены
-- обрабатывать (.рф).
-- Изначальный код из
-- http://stackoverflow.com/questions/37684/how-to-replace-plain-urls-with-links
-- | linkify работает с текстом внутри TagText, т.е. видимым пользователю,
-- а наружу выдает cписок тегов. Работать с отрендеренным HTML неправильно,
-- т.к. в URL могут попасть &lt, &gt, &apos
-- Также неправильно работать с текстом и пытаться его потом парсить --
-- те же <> в TagText могут превратиться в теги
linkifyTagTextOld t
    | -- T.length t < 10000 &&
      hasDotBetweenAlnumAndAlpha t
    =
        -- тормозная ф-я. Если хабр грузить (с длинными постами),
        -- то среднее время запроса 15 постов из 30 мсек становится 60 мсек.
        -- с проверкой становится где-то 30-40.
        -- с подсветкой любых доменов опять 60 мсек
        -- с проверкой hasDotBetweenAlnumAndAlpha даже меньше 30-40
        -- ?не подсвечивать внутри <code> и <pre>
        regexReplace'' TagText
        "\\b((https?|ftp)://[-A-Za-z0-9+&@#/%?=~_|!:,.;]*[-A-Za-z0-9+&@#/%=~_|]\
        \|www\\.[^[:space:]]+(\\b|$)\
        \|[-A-Za-z0-9_.]+@[a-z_]+(\\.[a-z]{2,6})+\\b\
        \|([a-z][-a-z0-9_]+\\.)+([a-z]{2,18})(\\b|$|[/?]|[/?][-A-Za-z0-9+&@#/%?=~_|!:,.;]*[-A-Za-z0-9+&@#/%=~_|]))"
        -- не подсвечиваем домены с большой буквы
        -- а то всякие Data.Map, TweenMax.to начинают светиться
        -- 18 -- пока самый длинный TLD
        (\ (u:_:_:_:mt:_:d:_) xs ->
             -- 6-я группа -- это домен
             let withMailTo
                     | mt /= "" = T.append "mailto:" u
                     | otherwise = u
                 link l =
                     TagOpen "a" [("href", l)] :
                     TagText u : TagClose "a" : xs
             in
--                  error (show l) :
                 if d /= "" then
                     if isValidDomain (T.toLower d) then
                         link $ T.append "http://" u
                     else
                         TagText u : xs
                 else
                     link withMailTo) t
    | otherwise = [TagText t]

fixIframeUrl ps =
    httpToHttps . -- iframe-ы работают только через https
    checkParams .
    fixLinks .
    fixNonHTTPSUrl . -- lj-toys
    fixQMark
    where fixLinks u
              | Just vId <- vimeoVideoId u
              , "moogaloop.swf" `T.isInfixOf` u
                -- заменяем возможный moogaloop.swf
              = vimeoEmbedUrl vId
              | Just vId <- youtubeVideoId u
              = urlAddParamsT
                  (either (const []) (filter ((/= "v") . fst))
                   (urlQueryStringUtf8Only u))
                  (youtubeEmbedUrl vId)
              | otherwise = u
          checkParams u
              | Just u <- U.importURL (T.unpack u)
              , U.Absolute (U.Host { U.host = host, .. })
                           <- U.url_type u
              = T.pack $ U.exportURL $
                u { U.url_params =
                     (if host == "cdn.embedly.com" then map fixSrc else id)
                     $ nub
                     $ filter (not . badParam) (U.url_params u)
                       ++ hostParams host }
              | otherwise = u
          badParam ("","") = True
          badParam (map toLower -> "autoplay", v) =
              not $ v `elem` ["0", "false"]
          badParam (p,_) =
              map toLower p `elem` ["autoplay", "autohide", "enablejsapi"]
          hostParams "www.youtube.com" =
              [("autoplay", "0")
              ,("fs","1")
              ,("rel","0")
              --  ^ не показываем related видео,
              --  чтобы пользователи смотрели то, что собирались, а не то,
              --  что их заставит смотреть youtube.
              ,("playsinline","1")
              ,("modestbranding","1")
              ,("origin","https://bazqux.com")]
          hostParams "brightcove.com" = [("autoStart", "false")]
          hostParams "clips.twitch.tv" = [("autoplay", "false")]
          hostParams "gfycat.com" = [("autoplay", "0"), ("controls", "1")]
          hostParams _ = []
          fixSrc ("src", x) = ("src", T.unpack $ fixIframeUrl ps $ T.pack x)
          fixSrc p = p
          fixQMark u
              -- иногда приходит url вида
              --   http://www.youtube.com/v/ShbC5yVqOdI&hl=en&fs=1&
              --                                       ^ не '?'
              -- и итоговый ролик:
              --   http://www.youtube.com/embed/ShbC5yVqOdI&hl=en&fs=1&
              -- не работает
              | Just ampPos <- T.findIndex (== '&') u
              , qpos <- fromMaybe (T.length u) $ T.findIndex (== '?') u
              , qpos > ampPos = T.take ampPos u <> "?" <> T.drop (ampPos+1) u
              | otherwise = u

mimeByUrl = mimeByUrl' "application/octet-stream"
mimeByUrl' def u =
    bst $ mimeByExt defaultMimeMap (tbs def)
    (T.takeWhile (`notElem` ("?#" :: [Char])) u)

------------------------------------------------------------------------------
-- Разбивка HTML на параграфы с отступами и изображения на всю ширину экрана
-- мобильного устройства

data Newline
    = Newline -- ^ block-level element, расположен на новой строке,
    | Text    -- ^ inline-level element, занимающий место сам по себе,
              --   даже без внутренних элементов
    | Empty   -- ^ inline-level element, не занимающий места сам по себе
    deriving (Eq, Show)

seqNl a Empty = a
seqNl _ b = b

fixForest msgLink msgSubject ps forest =
    (lang
    ,if ui ps && js ps then findThumbnail ps forest' else []
    ,(if js ps then addPlaceholders ps . addOnLoad else id) (addProxy ps forest'))
    where (lang, forest') = withBqrHabr
              ((if ui ps then
                   hyphenateForest id (Detect msgSubject, psAcceptLanguage ps)
                   . addBqrHeader . markNumbersOnlySubSup . addSections
                   . fixPicture msgLink ps
                else
                   (([],""),)
                   . removeScalables) .
               fixSvg .
               addScalables ps .
               fixInstagram .
               clearEmptyTags . clearEmptySpaceAtTheEnd)
              forest
          withBqrHabr f (Node {..} : _)
              | Just cls <- lookup "class" attrs
              , cls `elem` ["bqrHabr", "bqrTelegram"]
              , (lang, c') <- f children =
                  (lang
                  ,[Node
                   { name = "span"
                   , children = c', .. }])
                  -- не трогаем bqrHabr -- не двигаем параграфы выше него
                  -- и игнорируем всё, что вне bqrHabr (возможен мусор в конце)
                  -- также не трогаем bqrTelegram, чтобы pre было внутри
                  -- и не плодились лишние bqrTelegram
          withBqrHabr f xs = f xs
          addSections =
              map addDirAuto . markVspace . joinSections . moveUp [] [] [] .
              snd . go Newline [] [] [] .
              addTableWrappers
          fixInstagram [Node {..}]
              | not (ui ps) && name == "blockquote"
              , ("class", "instagram-media") `elem` attrs =
                 [Node { name = "span", .. }]
                 -- чтобы сторонние приложения не показывали единичный embed
                 -- как blockquote (с меньшей шириной и полоской слева)
                 -- Заменить везде на span не получится, т.к. перестает
                 -- работать instagram embed
          fixInstagram f = f
          addTableWrappers = map $ \ case
              Node {..}
                  | name == "table"
                  , (wa, ta) <- splitTableAttrs attrs ->
                      Node "div" (("class", "table_wrapper"):wa) BlockLevel
                          [Node { attrs = ta, .. }]
                          -- вложенные table не трогаем
                  | otherwise ->
                      Node { children = addTableWrappers children, .. }
              n -> n
          go nl p mf f [] = (nl, reverse $ margin f ++ mf)
          go nl p mf f (n:ns) =
              let onSeparateLine = nl /= Text && isNl p ns /= Text
                  onSeparateLineOrWide =
                      onSeparateLine || wide (attrs n)
                  -- figcaption рядом с изображениями попадают внутрь
                  -- <div class=i> чтобы сохранить маленький отступ у подписи,
                  -- а также убирается пустой текст вокруг <img>
                  spanFC acc [] = (acc, [])
                  spanFC acc l@(n:ns) = case n of
                      Node "figcaption" _ _ _ -> spanFC (n:acc) ns
                      TextNode t | emptyText t -> spanFC acc ns
                      _ -> (acc, l)
                  addImage n =
                      go Newline p (image (reverse ffc) ++ margin f' ++ mf) [] ns'
                      where (nfc, ns') = spanFC [] ns
                            (ffc, f') = spanFC (n : reverse nfc) f
                  hasAny c a = any (`elem` a) c
              in
              case n of
              TextNode t
                  | emptyText t -> go nl p mf (n:f) ns
                  | otherwise -> go Text p mf (n:f) ns
              VoidElement {..}
                  | floatElement name attrs ->
                      -- float картинки не трогаем
                      go nl p mf (n:f) ns
                  | name == "img" && onSeparateLineOrWide ->
                      addImage n
                  | otherwise ->
                      go (nl `seqNl` newlineVoidElement name attrs)
                         p mf (n:f) ns
              Node {..}
                  | floatElement name attrs ->
                      -- float элементы не трогаем, внутрь не заходим
                      go nl p mf (n:f) ns
                  | Just (T.words -> cls) <- lookup "class" attrs
                  , cls `hasAny` ["iframeScaler", "scalable"] ||
                    -- <div class=scalable> сам по себе может быть
                    -- только у iframe и attachment
                    ("imgScaler" `elem` cls && onSeparateLineOrWide)  ->
                      addImage n
                  | Just (T.words -> cls) <- lookup "class" attrs
                  , "imgScaler" `elem` cls ->
                      -- imgScaler не на новой строке не трогаем,
                      -- внутрь не заходим
                      go nl p mf (n:f) ns
                  | name == "video" && onSeparateLineOrWide ->
                      addImage n
                  | ([("class", "table_wrapper")], as) <-
                      partition ((==) "class" . fst) attrs
                  ->
                      go Newline p
                          (Node
                           { attrs =
                               ("class", "table") : as
                           , children =
                               [Node "span" [("class", "table_padding")]
                                    InlineLevel
                                    children]
                           , ..
                           }
                           : (margin f ++ mf)) [] ns
                  | name == "pre" ->
                      go Newline p
                          (Node
                           { attrs =
                               ("class", "pre") : attrs
                           , children =
                               [Node "span" [("class", "pre_padding")]
                                    InlineLevel
                                    children]
                           , ..
                           }
                           : (margin f ++ mf)) [] ns
                  | scalableFrame name attrs ->
                      addImage $ n { attrs = addClass "iframeScaler" attrs }
                  | name `elem` stoplist ||
                    lookup "class" attrs == Just "bqrHabr_spoiler" ->
                      go Newline p mf (n:f) ns
                  | otherwise ->
                      let nln = newlineNode name attrs
                          nl' = nl `seqNl` nln
                          style =
                              fromMaybe [] $
                              parseCssStyle <$> lookup "style" attrs
                          ignored =
                              case parseDisplay style of
                                  Just (Ignored _) -> True
                                  _ -> False
                              ||
                              any (badProp . fst) style
                          badProp n =
                              any (`T.isPrefixOf` n) ["border", "background"]
                          -- обновляем nl только по закрытию тега,
                          -- чтобы <a padding=1em><img>
                          -- также выносил img наверх
                          -- а <a padding=1em /><img>
                          -- уже нет
                      in
                      if not ignored && (nl' == Newline || onSeparateLine) then
                        case go Newline ((ns,nln):p) [] [] children of
                          -- было nl'
                          ((`seqNl` nln) -> nlg, r)
                              | Just c <- singleMargin r ->
                                  go nlg p mf (n { children = c }:f) ns
                              | otherwise ->
                                  go nlg p (n { children = r } : margin f
                                            ++ mf) [] ns
                      else
                        -- если это Text/Empty нода после/до Text,
                        -- то нет смысла в нее заходить
                        go nl' p mf (n:f) ns
          wide as = isJust $ do
              [w] <- lookup "width" =<< combinedCssStyle as
              p <- cssPixels w
              guard (p >= 320)
          scalableFrame name as = isJust $ do
              s <- combinedCssStyle as
              "relative" <- cssPosition s
              -- мы убираем любой position, все оставшиеся relative -- наши
              -- замены полноэкранных видео
              [CSS.Percentage {}] <- lookup "width" s
              guard (newlineNode name as == Newline)
          singleMargin [] = Nothing
          singleMargin [Node "div" [("class", "p")] _ c] = Just c
          singleMargin _ = Nothing
          isNl [] [] = Empty
          isNl ((f,Newline):p) [] = Newline
          isNl ((f,nl):p) [] = -- nl `nlSeq`
              -- текстовые ноды-родители игнорируем, если за ними не следует
              -- другого текста
              isNl p f
          isNl p  (n:ns) = case n of
              TextNode t
                  | not (emptyText t) -> Text
                  | otherwise -> isNl p ns
              VoidElement {..} ->
                  newlineVoidElement name attrs `nlSeq`
                  isNl p ns
              Node {..} ->
                  let nl = newlineNode name attrs in
                  nl `nlSeq` isNl ((ns,nl):p) children
          nlSeq Newline _ = Newline
          nlSeq Text _ = Text
          nlSeq Empty b = b
          margin = wrap "p"
          image  = wrap "i"
          wrap n [] = []
          wrap n f  = [Node "div" [("class", n)] BlockLevel $ reverse f]
          stoplist =
              [ "blockquote", "dl", "ol", "ul", "li", "dd", "dt", "pre", "table"
              , "object"
                --  ^ не делаем полную ширину, т.к. не факт, что это видео
                --  может быть что-то, захватывающее управление пальцем
                --  и мешающее прокрутке
              , "video", "audio"
              , "svg", "math" -- не ломаем ничего внутри
              ]
          -- превращаем
          --   <div foo><div class=i><img></div><div class=p>text</div></div>
          -- в
          --   <div class=i><div foo><img></div></div>
          --   <div class=p><div foo>text</div></div>
          -- Т.е. вытаскиваем наши параграфы наверх, чтобы различные
          -- <div align=center> были внутри
          moveUp
              :: Forest
              -> [(T.Text, [(T.Text, T.Text)], NodeLevel, Forest, Forest, Bool)]
              -> Forest -> Forest -> Forest
          moveUp acc [] f [] = reverse $ f ++ acc
          moveUp acc ((n,a,l,f',xs,True):p') [] [] =
              moveUp acc p' f' xs
              -- не добавляем пустую ноду,
              -- если после slice в нее ничего не добавилось
          moveUp acc ((n,a,l,f',xs,_):p') f [] =
              moveUp acc p' (Node n a l (reverse f) : f') xs
          moveUp acc p f (x:xs) = case x of
              Node n as BlockLevel ns
                  | Just c <- lookup "class" as
                  , preprocessedClass c ->
                      let (p', f') = slice (notNull xs) [] (reverse f ++ ns) p
                      in
                          moveUp (Node n as BlockLevel f' : acc) p' [] xs
              Node {..} ->
                  moveUp acc
                      ((name, attrs, level, f, xs, False) : p)
                      [] children
              _ ->
                  moveUp acc p (x:f) xs
          slice _ pacc f [] = (reverse pacc, f)
          slice hasNext pacc f ((n,a,l,f',xs,added):p') =
              slice (hasNext || notNull xs) ((n,a,l,[],xs,True):pacc)
                  (reverse f' ++ [Node n (addClass className a) l f]) p'
              where className
                        | added == False && not hasNext = "in" -- однократный
                        | added == False = "inF" -- first
                        | not hasNext = "inL" -- last
                        | otherwise = "inM" -- middle

          -- превращаем
          --  <div class=p><p>foo</p></div><div class=p>bar</div>
          -- которые могут появиться после moveUp, в
          --  <div class=p><p>foo</p>bar</div>
          joinSections [] = []
          joinSections (Node "div" [("class", "p")] _ ch : xs) = spanP [ch] xs
          joinSections (x : xs) = x : joinSections xs
          spanP acc (Node "div" [("class", "p")] _ ch : ns) = spanP (ch:acc) ns
          spanP acc xs =
              Node "div" [("class", "p")] BlockLevel (concat $ reverse acc)
              : joinSections xs
          notNull = not . null
          addDirAuto (Node "div" [("class", "p")] l ch) =
              Node "div" [("class", "p"), ("dir", "auto")] l
                  (map addDirAutoInner ch)
          addDirAuto (Node "div" as@[("class", "table")] l ch) =
              Node "div" as l (map addDirAutoInner ch)
          addDirAuto n = n
          addDirAutoInner (Node n as l ch)
              | Just (T.words -> c) <- lookup "class" as
              , any (`elem` dirAutoClasses) c =
                  Node n (as ++ [("dir", "auto")]) l ch
                  -- внутрь уже не заходим
              | otherwise = Node n as l (map addDirAutoInner ch)
          addDirAutoInner n = n
          dirAutoClasses =
              ["bqrShareLink", "bqrShareDescription", "downloadLink"]

floatElement name as
    | Just s <- fmap parseCssStyle $ lookup "style" as
    , Just f <- lookupCssIdent "float" s
    = f `elem` ["left", "right"]
    | name == "table"
    , Just a <- lookup "align" as
    = T.toLower a `elem` ["left", "right"] -- еще есть center
      -- а у p/div align -- это text-align, а не позиционирование
    | otherwise = False

data Display
    = Block
    | Inline
    | InlineBlock
    | None
    | Ignored Bool
      -- ^ Не заходим внутрь таких элементов (table, list-item, ruby, …).
      --   Флаг показывает, Block или Inline элемент сам по себе
    deriving (Show, Eq)

displayValues =
    HM.fromList
    -- <display-outside> values
    [ ("block", Block)
    , ("inline", Inline)
--    , ("run-in", Inline) -- не совсем правильно
    -- <display-inside> values
    , ("flow", Inline) -- не поддерживается в Chrome, равно как и inline flow-root
    , ("flow-root", Block)
    , ("table", Ignored True)
    , ("flex", Ignored True)
    , ("grid", Ignored True)
    , ("ruby", Ignored False)
    , ("subgrid", Ignored True)
    -- <display-listitem> values
    , ("list-item", Ignored True)
    -- <display-internal> values
    , ("table-row-group", Ignored True)
    , ("table-header-group", Ignored True)
    , ("table-footer-group", Ignored True)
    , ("table-row", Ignored True)
    , ("table-cell", Ignored False)
    , ("table-column-group", Ignored True)
    , ("table-column", Ignored False)
    , ("table-caption", Ignored True)
    , ("ruby-base", Ignored False)
    , ("ruby-text", Ignored False)
    , ("ruby-base-container", Ignored False)
    , ("ruby-text-container", Ignored False)

    -- <display-box> values
    , ("contents", Inline)
    , ("none", None)

    -- <display-legacy> values
    , ("inline-block", InlineBlock)
    , ("inline-list-item", Ignored True)
    , ("inline-table", Ignored True)
    , ("inline-flex", Ignored True)
    , ("inline-grid", Ignored True)]

displayNoneOrVisibilityHidden as
    | Just s <- parseCssStyle <$> lookup "style" as
    = parseDisplay s == Just None
      || lookupCssIdent "visibility" s == Just "hidden"
    | otherwise = False

parseDisplay s =
    listToMaybe [d | ("display", parseDisplayValue -> Just d) <- reverse s]
parseDisplayValue d = go [] d
    where go acc [] = p $ reverse acc
          go acc (CSS.Ident w : ds) = go (T.toLower w : acc) ds
          go acc (CSS.Whitespace : ds) = go acc ds
          go _ _ = Nothing
          -- https://developer.mozilla.org/en-US/docs/Web/CSS/display
          p [x] = HM.lookup x displayValues
          p ["inline", "flow-root"] = Just InlineBlock
          p [outside -> o@(Just _), inside -> True] = o
          p l@[_,_]
              | o <- listToMaybe $ mapMaybe outside l
              , any li l && (isJust o || any insideLI l) =
                  o <|> Just (Ignored True)
          p l@[_,_,_]
              | any li l && any insideLI l
              , [o] <- mapMaybe outside l = Just o
          p _ = Nothing
          outside "block" = Just (Ignored True)
          outside "inline" = Just (Ignored False)
          outside _ = Nothing
          inside' = HS.fromList
              [ "flow", "flow-root", "table", "flex", "grid", "ruby", "subgrid"]
          inside x = HS.member x inside'
          insideLI x = x == "flow" || x == "flow-root"
          li x = x == "list-item"

newlineNode n a = newlineAttrOr a $
    if blockElement n then Newline else
    if visibleInlineElement n then Text else Empty
newlineVoidElement n a
    | n == "br" =
      -- if displayNone a then Empty else
      --  ^  в clearEmptyTags уже удалены все display:none
      Newline -- неважно, что в style, все равно будет <br>
    | otherwise =
        newlineAttrOr a $
        if n == "hr" then Newline else
        if visibleVoidElement n then Text else Empty
newlineAttrOr as o
    | Just d <- parseDisplay =<< style
    = case d of
        Block -> Newline
        Ignored True -> Newline
        Ignored False -> Text
        Inline
            | o == Text -> Text
            | otherwise -> textOrEmpty False style
        InlineBlock
            | o == Text -> Text
            | otherwise -> textOrEmpty True style
        None -> Empty
--     | otherwise = o
    | o == Text || o == Newline = o
    | otherwise = textOrEmpty False style
    where style = parseCssStyle <$> lookup "style" as

textOrEmpty checkWH style
    | Just s <- style
    , any (visibleProp checkWH) s
    = Text
    | otherwise = Empty
    where visibleProp checkWH (n, vs) =
              ((if checkWH then n == "width" || n == "height" else False)
               || any (`T.isPrefixOf` n) ["margin", "padding", "border"])
              && any visibleVal vs
          visibleVal v
              | Just p <- cssPixels v = p /= 0
              | CSS.Ident i <- v = i `notElem` ["none", "auto"]
              | otherwise = True

clearEmptySpaceAtTheEnd = go . reverse
    where go [] = []
          go f@(n:ns) = case n of
              TextNode t
                  | emptyText t -> go ns
              VoidElement {..}
                  | not (visibleVoidElement name) ||
                    name == "br" || name == "hr" -> go ns
              Node {..}
                  | not (keepNode name attrs) ->
                      case go (reverse children) of
                          [] -> go ns
                          ch -> reverse (Node { children = ch, .. } : ns)
              _ -> reverse f

preprocessedClass c =
    c `elem` ["p", "i", "pre", "table", "table_wrapper", "vspace", "in"]

-- instagram/twitter и прочие ноды, внутри которых ничего трогать не надо
externalNode (lookup "class" -> Just c) = not $ preprocessedClass c
externalNode _ = False

-- Удаляем любые пустые невидимые элементы <span><a></a><i></i></span>
-- В том числе удаляем <sup></sup> которые не видны, но портят форматирование.
-- При этом <table></table> или <div></div> не удаляются, т.к. без них
-- могут исчезнуть важные переносы на следующую строку.
-- Также удаляем невидимые элементы (слежение?)
clearEmptyTags = go
    where go [] = []
          go (n:ns) = case n of
              VoidElement {..}
                  | displayNoneOrVisibilityHidden attrs -> go ns
                  --  | newlineVoidElement name attrs == Empty -> go ns
                  -- void-элементы важны сами по себе, к примеру <source>
                  -- внутри video или <param>, по-этому мы их не удаляем.
                  -- также они не влияют на форматирование (в отличие
                  -- от <sup></sup>)
              Node {..}
                  | displayNoneOrVisibilityHidden attrs -> go ns
                  | not (keepNode name attrs) ->
                      case go children of
                          [] | newlineNode name attrs == Empty ->
                              go ns
                          ch ->
                              Node { children = ch, .. } : go ns
              _ -> n : go ns

keepNode name attrs =
    visibleInlineElement name || externalNode attrs
    || any (\(n,_) -> n == "name" || n == "id") attrs
    || (case parseCssStyle <$> lookup "style" attrs of
            Just s | any (T.isPrefixOf "background" . fst) s ->
                textOrEmpty True (Just s) == Text
            _ -> False)

-- ломает "PgDOWN … <br><br><br><br><br><img>"
-- и "<br><img><br><img>" -- без зазора
-- но дает однообразный вид на мобиле, что гораздо важнее
markVspace = map mark
    where mark n@(Node "div" [("class", "p")] a ch)
              | v = vspace n
              | otherwise =
                  n { children = snd $ go True [] $ reverse ch' }
              where (v, ch') = go False [] ch
          mark n = n
          end True  acc f = (False, reverse f ++ acc)
          end False acc f = (False, reverse acc ++ f)
          go True  acc [] = (True, acc)
          go False acc [] = (True, reverse acc)
          go r acc f@(n:ns) = case n of
              TextNode t
                  | emptyText t -> go r acc ns
                  | otherwise -> end r acc f
              VoidElement {..}
                  | name == "br" -> go r (vspace n : acc) ns
                  | newlineVoidElement name attrs == Empty ->
                      go r acc ns
                  | otherwise -> end r acc f
              Node {..}
                  | newlineNode name [] == Text
                    -- проверка без атрибутов, не является ли нода видима
                    -- сама по себе (с атрибутами может стать newline)
                    || newlineNode name attrs == Text
                    || ("class", "attachmentsLabel") `elem` attrs
                    -> end r acc f
                  | otherwise ->
                      case go r [] $ (if r then reverse else id) children of
                          (True, _) ->
                              go r (vspace n : acc) ns
                          (False, ch) ->
                              end r (n { children = ch } : acc) ns
          vspace n = n { attrs = addClass "vspace" (attrs n) }

checkForest = renderForest . buildForest . parseTagsT
checkForestF = renderForest . (\(_,_,f) -> f) .
    fixForest Nothing "" defaultPreprocessSettings . buildForest . parseTagsT
editForest f = editTagSoup (forestToTags . f . buildForest)

test = do
    print $ f ""
    test
        [ "" ==> ""
        , "<br><p> <div>" ==> ""
        , "a" ==> "<div class=p>a</div>"
        , "<br>a<br>" ==> "<div class=p><br class=vspace>a</div>"
        , "<a><img></a><img>" -- без i
          ==>
          "<div class=p><a><img></a><img></div>"

        , "hello, <span style='display:none'>invisible man"
          ==>
          "<div class=p>hello, </div>"

        , "foo<span class=scalable></span>bar"
          ==>
          "<div class=p>foo</div>\
          \<div class=i><span class=scalable></span></div>\
          \<div class=p>bar</div>"

          -- игнорируем border/background
        , "<p style='background:url(bg.png)'><img>\
          \<p style='border-radius:1px'><img>"
          ==>
          "<div class=p><p style='background: url(/image_proxy/1920x2560/bg.png)'><img></p><p style='border-radius: 1px'><img></p></div>"

          -- margin нам не мешает
        , "<a style='margin:1px'><img></a>"
          ==>
          "<div class=i><a class=in style='margin: 1px'><img></a></div>"

          -- разбиение на параграфы
        , "<div><a style='margin:1px'><img></a></div>a"
          ==>
          "<div class=i><div class=in><a class=in style='margin: 1px'><img></a></div></div>\
          \<div class=p>a</div>"

        , "asdf<div><img>" ==>
          "<div class=p>asdf</div>\
          \<div class=i><div class=in><img></div></div>"

        , "<p>asdf<div><img>"
          ==>
          "<div class=p><p>asdf</p></div>\
          \<div class=i><div class=in><img></div></div>"

          -- figure/figcaption
        , "<figure> <img> <figcaption>asdf"
          ==>
          "<div class=i><figure class=in><img><figcaption>asdf</figcaption></figure></div>"

        , "<figure> <figcaption>asdf</figcaption> <img>"
          ==>
          "<div class=i><figure class=in><figcaption>asdf</figcaption><img></figure></div>"

--         , "<figure><a><img></a><figcaption>asdf</figcaption></figure>"
--           ==>
--           "<div class=i><figure class=in><a class=in><img></a><figcaption>asdf</figcaption><img></figure></div>"

          -- разные сочетания in/inF/inL/inM
        , "<br>a<br><img><br><p><br><img>"
          ==>
          "<div class=p><br class=vspace>a<br class=vspace></div>\
          \<div class=i><img></div>\
          \<div class='vspace p'><br><p class=inF><br></p></div>\
          \<div class=i><p class=inL><img></p></div>"

        , "<p><br><img><br><img>"
          ==>
          "<div class='vspace p'><p class=inF><br></p></div>\
          \<div class=i><p class=inM><img></p></div>\
          \<div class='vspace p'><p class=inM><br></p></div>\
          \<div class=i><p class=inL><img></p></div>"

        , "<div><span><img><br><img><br>asdf"
          ==>
          "<div class=i><div class=inF><span class=inF><img></span></div></div>\
          \<div class='vspace p'><div class=inM><span class=inM><br></span></div></div>\
          \<div class=i><div class=inM><span class=inM><img></span></div></div>\
          \<div class=p><div class=inL><span class=inL><br class=vspace>asdf</span></div></div>"

        , "<div><span><img><br></span><br>asdf"
          ==>
          "<div class=i><div class=inF><span class=inF><img></span></div></div>\
          \<div class=p><div class='vspace inM'><span class=inL><br></span></div>\
          \<div class=inL><br class=vspace>asdf</div></div>"

        , "<div><span><img></span><br>asdf"
          ==>
          "<div class=i><div class=inF><span class=in><img></span></div></div>\
          \<div class=p><div class=inL><br class=vspace>asdf</div></div>"

          -- scaler
        , "<svg><img width=10 height=10>"
          -- не трогаем img внутри
          ==>
          "<div class=p><svg><img width=10 height=10></svg></div>"

        , "<img width=10001 height=10001></div>"
          ==>
          "<div class=i><img width='invalid=10001' height='invalid=10001'></div>"
        , "<span width=123%><span height=10%><span width=10001 height=.5em><span width=123.456foo% height=01><span width=12.3456foo%>x"
          ==>
          "<div class=p><span width='invalid=123%'><span height='invalid=10%'><span width='invalid=10001' height='invalid=.5em'><span width='invalid=123.456foo%' height=1><span width=12%>x</span></span></span></span></span></div>"

        , "<img style='background: black; max-width: 420px; width: 420px; height: 280px' border=2 vspace=4 width=200 height=100>"
          ==>
          "<div class=i><span class=imgScaler style='width: 420.0px; --aspect-ratio: 0.6666666666666666; border: 2px solid black; margin-top: 4px; margin-bottom: 4px'><span class=scalable style='padding-bottom: 66.66666666666666%'><img class=inner width=200 height=100 style='background: black'></span></span></div>"

        , "<p>text</p>\
          \<p><a><img width=100 height=100></a></p>\
          \<p><a><img></a></p>\
          \<p>more text</p>"
          ==>
          "<div class=p><p>text</p></div>\
          \<div class=i><p class=in><a class=in><span class=imgScaler style='width: 100.0px; --aspect-ratio: 1.0'><span class=scalable style='padding-bottom: 100.0%'><img class=inner width=100 height=100></span></span></a></p></div>\
          \<div class=i><p class=in><a class=in><img></a></p></div>\
          \<div class=p><p>more text</p></div>"

          -- iframe
        , "<iframe>" ==> ""

        , "<iframe src=http://mysite>"
          ==>
          "<div class=i><span class=iframeScaler style='--aspect-ratio: 0.5625'><span class=scalable style='padding-bottom: 56.25%'>\
          \<iframe class=inner src=https://mysite/ allow='autoplay; fullscreen; encrypted-media' sandbox='allow-same-origin allow-scripts allow-forms allow-popups allow-presentation' allowfullscreen=true>\
          \</iframe></span></span></div>"

        , "<iframe width=560 height=315 src='http://mysite'></iframe>"
          ==>
          "<div class=i><span class=iframeScaler style='width: 560.0px; --aspect-ratio: 0.5625'><span class=scalable style='padding-bottom: 56.25%'><iframe class=inner src='https://mysite/' allow='autoplay; fullscreen; encrypted-media' sandbox='allow-same-origin allow-scripts allow-forms allow-popups allow-presentation' allowfullscreen='true' width='560' height='315'></iframe></span></span></div>"

        , "<iframe width=100% height=50px src=http://mysite>"
          ==>
          "<div class=i><span class=scalable style='height: 50.0px'>\
          \<iframe class=inner src='https://mysite/' allow='autoplay; fullscreen; encrypted-media' sandbox='allow-same-origin allow-scripts allow-forms allow-popups allow-presentation' allowfullscreen='true' width='100%' height='50'>\
          \</iframe></span></div>"

        , "<iframe style='width:100%' height=50px src=http://mysite>"
          ==>
          "<div class=i><span class=scalable style='height: 50.0px'>\
          \<iframe class=inner src='https://mysite/' allow='autoplay; fullscreen; encrypted-media' sandbox='allow-same-origin allow-scripts allow-forms allow-popups allow-presentation' allowfullscreen='true' height='50'>\
          \</iframe></span></div>"

        , "<iframe src=http://mysite/foo.swf>" ==> ""

          -- youtube/vimeo
        , "<span class=scalable></span><br><span class='bqrYouTube-123'></span><br>"
          ==>
          "<div class=i><span class=scalable></span><figcaption class=bqrYoutubeDuration><span style='color: #666'>Time: </span><span style='color: #000; font-weight: bold'>2:03</span></figcaption></div>"

        , "<iframe width=560 height=315 src='https://www.youtube.com/embed/guls1CGQQ2A' frameborder=0 gesture=media allow=encrypted-media allowfullscreen></iframe>"
          ==>
          "<div class='i'><span class='iframeScaler' style='width: 560.0px; --aspect-ratio: 0.5625'><span class='scalable' style='padding-bottom: 56.25%'><span style='background-color: black' class='inner iframe_placeholder' data-bqr-html='&lt;iframe src=&quot;https://www.youtube.com/embed/guls1CGQQ2A?fs=1&amp;amp;rel=0&amp;amp;playsinline=1&amp;amp;origin=https%3A%2F%2Fbazqux.com&amp;amp;autoplay=1&quot; style=&quot;background-color: black&quot; class=&quot;inner&quot; allow=&quot;autoplay; fullscreen; encrypted-media&quot; sandbox=&quot;allow-same-origin allow-scripts allow-forms allow-popups allow-presentation&quot; allowfullscreen=&quot;true&quot; width=&quot;560&quot; height=&quot;315&quot;&gt;&lt;/iframe&gt;' onclick='uw_event=event;bq.replaceWithHtml(this, this.getAttribute(&apos;data-bqr-html&apos;))'><span class=youtube-unprocessed data-video-id=guls1CGQQ2A><span class='ytp-thumbnail'><button class='ytp-large-play-button ytp-button'><svg height='100%' version='1.1' viewbox='0 0 68 48' width='100%'><path class='ytp-large-play-button-bg' d='m .66,37.62 c 0,0 .66,4.70 2.70,6.77 2.58,2.71 5.98,2.63 7.49,2.91 5.43,.52 23.10,.68 23.12,.68 .00,-1.3e-5 14.29,-0.02 23.81,-0.71 1.32,-0.15 4.22,-0.17 6.81,-2.89 2.03,-2.07 2.70,-6.77 2.70,-6.77 0,0 .67,-5.52 .67,-11.04 l 0,-5.17 c 0,-5.52 -0.67,-11.04 -0.67,-11.04 0,0 -0.66,-4.70 -2.70,-6.77 C 62.03,.86 59.13,.84 57.80,.69 48.28,0 34.00,0 34.00,0 33.97,0 19.69,0 10.18,.69 8.85,.84 5.95,.86 3.36,3.58 1.32,5.65 .66,10.35 .66,10.35 c 0,0 -0.55,4.50 -0.66,9.45 l 0,8.36 c .10,4.94 .66,9.45 .66,9.45 z' fill='#1f1f1e' fill-opacity='0.9'></path><path d='m 26.96,13.67 18.37,9.62 -18.37,9.55 -0.00,-19.17 z' fill='#fff'></path><path d='M 45.02,23.46 45.32,23.28 26.96,13.67 43.32,24.34 45.02,23.46 z' fill='#ccc'></path></svg></button></span><span class='ytp-gradient-top'></span><span class='ytp-chrome-top'><span class='ytp-title'><span class='ytp-title-text'><a class='ytp-title-link' target='_blank' rel='noopener' href='https://www.youtube.com/watch?v=guls1CGQQ2A'></a></span></span></span></span></span></span></span></div>"

        , "<iframe src=https://player.vimeo.com/video/137464081 width=640 height=360 frameborder=0 webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>"
          ==>
          T.concat ["<div class=i><span class=iframeScaler style='width: 640.0px; --aspect-ratio: 0.5625'><span class=scalable style='padding-bottom: 56.25%'>", vimeoInner " width=&quot;640&quot; height=&quot;360&quot;", "</span></span></div>"]

        , "<div style='padding: 51.04% 0 0 0; position: relative;'><iframe allowfullscreen='' frameborder='0' mozallowfullscreen='' src='https://player.vimeo.com/video/137464081' style='height: 100%; left: 0; position: absolute; top: 0; width: 100%;' webkitallowfullscreen=''></iframe></div><script src='https://player.vimeo.com/api/player.js'></script>"
          ==>
          T.concat ["<div class='i'><span class=iframeScaler style='--aspect-ratio: 0.5104'><span class='scalable' style='padding: 51.04% 0 0 0'>", vimeoInner "", "</span></span></div>"]

          -- tables
        , "<table width=100 align=left style=padding:1em>asdf"
          ==>
          "<div class=p><div class=table_wrapper style='float: left; padding: 1em'><table width=100>asdf</table></div></div>"
        , "<table style='width:100px !important; padding:1em'>asdf"
          ==>
          "<div class=table style='padding: 1em'><span class=table_padding><table style='width: 100px'>asdf</table></span></div>"
        , "asdf<table width=100>qwer<table>zxcv"
          ==>
          "<div class=p>asdf</div>\
          \<div class=table><span class=table_padding><table width=100>qwer<table>zxcv</table></table></span></div>"

        , "<img src=https://1.bp.blogspot.com/-zKIkHjamSJo/Vtje90iOU0I/AAAAAAAABOk/R2dx_hhOUPg/s1600/bazqux-reader-list-view-before-group-by-feed.png>"
          ==>
          "<div class=i><img src=/image_proxy/-/https://1.bp.blogspot.com/-zKIkHjamSJo/Vtje90iOU0I/AAAAAAAABOk/R2dx_hhOUPg/s1600/bazqux-reader-list-view-before-group-by-feed.png></div>"
          -- fixMessageBody
        , "<a href='  # some id%20 ' id=x name=' some id '>"
          `fmb`
          "<a href='#%20some%20id%20' id=x name=' some id '>"

        , "<a href='  # some id ' id=' some id ' name >"
          `fmb`
          "<a href=http://x#%20some%20id id=' some id '>"

          -- video
        , "<video poster=foo loop>"
          ==>
          "<div class='i'><span class=iframeScaler style='--aspect-ratio: 0.5625'><span class='scalable' style='padding-bottom: 56.25%'><span class='nonProxied inner videoPlaceholder' style='; background-image: url(foo); background-size: contain; background-position: center; background-repeat: no-repeat' data-bqr-html='&lt;video autoplay class=&quot;nonProxied inner&quot; controls preload=&quot;none&quot; playsinline=&quot;true&quot; webkit-playsinline=&quot;true&quot; tabindex=&quot;-1&quot; poster=&quot;foo&quot; loop&gt;&lt;/video&gt;' onclick='uw_event=event;bq.replaceWithHtml(this, this.getAttribute(&apos;data-bqr-html&apos;))'><span class='iconPlayVideo'></span></span><span class='proxied inner videoPlaceholder' style='; background-image: url(/image_proxy/1920x2560/foo); background-size: contain; background-position: center; background-repeat: no-repeat' data-bqr-html='&lt;video autoplay poster=&quot;/image_proxy/1920x2560/foo&quot; class=&quot;proxied inner&quot; controls preload=&quot;none&quot; playsinline=&quot;true&quot; webkit-playsinline=&quot;true&quot; tabindex=&quot;-1&quot; loop&gt;&lt;/video&gt;' onclick='uw_event=event;bq.replaceWithHtml(this, this.getAttribute(&apos;data-bqr-html&apos;))'><span class='iconPlayVideo'></span></span></span></span></div>"

        , "<span style='display: block; width: 100%; height: 0; position: relative; overflow: hidden; padding-bottom: 56.25%'><video style='display: block; border: none; overflow: hidden; top: 0; left: 0; width: 100%; height: 100%; position: absolute; background-color: red'><source src='https://example.com/foo.mp4' type='video/mp4'></source></video></span>"
          ==>
          "<div class='i'><span class=iframeScaler style='--aspect-ratio: 0.5625; border: none'><span class='scalable' style='padding-bottom: 56.25%'><span class='nonProxied inner videoPlaceholder' style='; background-color: red' data-bqr-html='&lt;video autoplay class=&quot;nonProxied inner&quot; controls preload=&quot;none&quot; playsinline=&quot;true&quot; webkit-playsinline=&quot;true&quot; tabindex=&quot;-1&quot; style=&quot;background-color: red&quot;&gt;&lt;source src=&quot;https://example.com/foo.mp4&quot; type=&quot;video/mp4&quot;&gt;&lt;/video&gt;' onclick='uw_event=event;bq.replaceWithHtml(this, this.getAttribute(&apos;data-bqr-html&apos;))'><span class='iconPlayVideo'></span></span><span class='proxied inner videoPlaceholder' style='; background-color: red' data-bqr-html='&lt;video autoplay class=&quot;proxied inner&quot; controls preload=&quot;none&quot; playsinline=&quot;true&quot; webkit-playsinline=&quot;true&quot; tabindex=&quot;-1&quot; style=&quot;background-color: red&quot;&gt;&lt;source src=&quot;/proxy/https://example.com/foo.mp4&quot; type=&quot;video/mp4&quot;&gt;&lt;/video&gt;' onclick='uw_event=event;bq.replaceWithHtml(this, this.getAttribute(&apos;data-bqr-html&apos;))'><span class='iconPlayVideo'></span></span></span></span></div>"

        , "<audio style='background-color:green'>"
          ==>
          "<div class='p'><span class='bqrMsgAudio nonProxied' style='display:block'><span style='background-color: green; display: block; width: 100%' class='nonProxied bqrMsgAudioInner' data-bqr-html='&lt;audio autoplay class=&quot;nonProxied&quot; controls preload=&quot;none&quot; playsinline=&quot;true&quot; webkit-playsinline=&quot;true&quot; tabindex=&quot;-1&quot; style=&quot;background-color: green; display: block; width: 100%&quot;&gt;&lt;/audio&gt;' onclick='uw_event=event;bq.replaceWithHtml(this, this.getAttribute(&apos;data-bqr-html&apos;))'><span class='iconPlayAudio'></span></span></span><span class='bqrMsgAudio proxied' style='display:block'><span style='background-color: green; display: block; width: 100%' class='proxied bqrMsgAudioInner' data-bqr-html='&lt;audio autoplay class=&quot;proxied&quot; controls preload=&quot;none&quot; playsinline=&quot;true&quot; webkit-playsinline=&quot;true&quot; tabindex=&quot;-1&quot; style=&quot;background-color: green; display: block; width: 100%&quot;&gt;&lt;/audio&gt;' onclick='uw_event=event;bq.replaceWithHtml(this, this.getAttribute(&apos;data-bqr-html&apos;))'><span class='iconPlayAudio'></span></span></span></div>"

        , "<img usemap=#mymap><map name=mymap>"
          ==>
          "<div class=i><img usemap='#article-fDOO0oQNK_Vfn15O7QT2bICEDrM-mymap'></div><map name='article-fDOO0oQNK_Vfn15O7QT2bICEDrM-mymap'></map>"

        , "<img usemap=#mymap><map name=mymap><area>"
          ==>
          "<div class=i><img usemap='#article-fDOO0oQNK_Vfn15O7QT2bICEDrM-mymap'></div>\
          \<div class='vspace p'><map name='article-fDOO0oQNK_Vfn15O7QT2bICEDrM-mymap'><area></map></div>"
          --  ^ почему для просто <map></map> не добавляется <div class=p> ?

        , "<a href=#id>X</a> <a name=id>"
          ==>
          "<div class=p><a href=#article-fDOO0oQNK_Vfn15O7QT2bICEDrM-id>X</a><a class=vspace name=article-fDOO0oQNK_Vfn15O7QT2bICEDrM-id></a></div>"

        , "<embed src='my.pdf' width='800px' height='500px'>"
          ==>
          "<div class='i'><span class='iframeScaler' style='width: 800.0px; --aspect-ratio: 0.625'><span class='scalable' style='padding-bottom: 62.5%'><embed src='/proxy/my.pdf' data-proxied-src='/proxy/my.pdf' data-nonproxied-src='my.pdf' class='inner'  width='800' height='500' type='application/pdf'></span></span></div>"

        , "<strong>H</strong>"
          ==>
          "<div class=p><strong>H</strong></div>"

        , "<p><strong>H</strong> <b>B</b></p>asdf"
          ==>
          "<div class=p><p class=bqrHeader><strong>H</strong> <b>B</b></p>asdf</div>"

        , "<p><strong>H</strong>asdf</p>"
          ==>
          "<div class=p><p><strong>H</strong>asdf</p></div>"

        , "asdf qwer theoretically hello@support #метро (parentheses) the real english text is here for the language detector"
          ==>
          "<div class=p>asdf qwer the\173o\173ret\173i\173cally hello@support #метро (paren\173the\173ses) the real eng\173lish text is here for the lan\173guage de\173tec\173tor</div>"

        , "<span style='font: italic small-caps bold 16px/2 times new roman'>text"
          ==>
          "<div class=p><span style='font: italic small-caps bold 100%/var(--current-line-height,2) var(--current-font-family),times new roman'>text</span></div>"

        , "<span style='font-family: monospace, \"Menlo\", fira sans, cursive'>text"
          ==>
          "<div class=p><span style='font-family: var(--monospace-font-family),monospace, \"Menlo\", fira sans, cursive'>text</span></div>"

        , "<font face=' times new roman ' size=1>text"
          ==>
          "<div class=p><span style='font-family: var(--current-font-family),times new roman'>text</span></div>"

        , "H<sub><a>2</a></sub>O <sup><span style='padding:1em'>1</span>"
          ==>
          "<div class=p>H<sub class=numbersOnly><a>2</a></sub>O <sup><span style='padding: 1em'>1</span></sup></div>"

        , "'00s '60s '70s i'm he's don't he'd it'll you've we're I'M 'n' s' '123 rock'n'roll xs' the real english text is here for the language detector"
          ==>
          "<div class=p>\
          \’00s ’60s ’70s i’m he’s don’t he’d it’ll you’ve we’re I’M 'n' s’ ’123 rock’n’roll xs’ the real eng\173lish text is here for the lan\173guage de\173tec\173tor</div>"

        , "foo<a>barbaz</a> <a>zxcv</a>'s <em>The</em><em>Atlantic</em> the real english text is here for the language detector"
          ==>
          "<div class=p>\
          \foo\173<a>bar\173baz</a> <a>zxcv</a>’s <em>The</em><em>Atlantic</em> the real eng\173lish text is here for the lan\173guage de\173tec\173tor</div>"

        , "ajoutons ici un texte en français pour vérifier la césure"
          ==>
          "<div class=p>ajou\173tons ici un texte en fran\173çais pour vé\173ri\173fier la cé\173sure</div>"

        , "добавим теперь немного русского текста для проверки переносов, нужно минимум 10 слов"
          ==>
          "<div class=p>до\173ба\173вим те\173перь немно\173го рус\173ско\173го тек\173ста для про\173вер\173ки пе\173ре\173но\173сов, нуж\173но ми\173ни\173мум 10 слов</div>"

        , "а теперь фокус: смешаем в одном месте русский текст для проверки переносов и французский - ajoutons ici un texte en français pour vérifier la césure"
          ==>
          "<div class=p>а те\173перь фо\173кус: сме\173ша\173ем в од\173ном ме\173сте рус\173ский текст для про\173вер\173ки пе\173ре\173но\173сов и фран\173цуз\173ский - ajou\173tons ici un texte en fran\173çais pour vé\173ri\173fier la cé\173sure</div>"

        , "<span align=justify style='text-align: Justify-all'>text</span>"
          ==>
          "<div class=p><span align='ignoring=justify'>text</span></div>"

        , "<picture><source/><img srcset='http://x 2x'>"
          ==>
          "<div class=i><picture class=in><img data-orig-srcset='http://x 2x'></picture></div>"

        , "<picture><source/><img srcset='http://x x'>"
          ==>
          "<div class=i><picture class=in><img data-error-orig-srcset='http://x x'></picture></div>"

        , "<picture><source/><img srcset='asdf 2x, http://qwer 3x'>"
          ==>
          "<div class=i><picture class=in><img src='/image_proxy/-/http://qwer' data-orig-srcset='asdf 2x, http://qwer 3x'></picture></div>"

        , "<picture>\
          \<source type='image/webp' media='(min-width: 650px)' srcset='2x.webp 2x, 1x.webp 1x '>\
          \<source type='image/png' media='(min-width: 650px)' srcset='2x.jpg 2x, 1x.jpg 1x '>\
          \<img src='data:image/gif;base64,R0lGODlhAQABAPAAAPLy8gAAACH5BAAAAAAALAAAAAABAAEAAAICRAEAOw=='></picture>"
          ==>
          "<div class=i><picture class=in><img src='/image_proxy/-/2x.jpg' data-orig-srcset='2x.jpg 2x, 1x.jpg 1x'></picture></div>"

        , "<svg>foo</svg><svg><use bar></svg>"
          ==>
          "<div class=p><svg>foo</svg></div>"

        , "<div style='padding-bottom:56.25%;height:0'>asdf"
          ==>
          "<div class=p><div>asdf</div></div>"

        , "<pre>foo"
          ==>
          "<pre class=pre><span class=pre_padding>foo</span></pre>"

        , "<table><pre>foo"
          ==>
          "<div class=table><span class=table_padding><table><pre>foo</pre></table></span></div>"
        ]
    where test ts = case catMaybes ts of
              [] -> putStrLn "PASSED"
              e -> do
                  mapM_ T.putStrLn e
                  fail "FAILED"
          f = -- T.replace "/image_proxy/1920x2560/" "" .
              editTagSoup (
              map rmDirAuto .
              map rmImgSrc . strip . fst
              . fixTags' "" emptyMsgKey Nothing defaultPreprocessSettings . (: [])
              . map addImgSrc . strip)
          a `fmb` b =
              renderTagsT (fixMessageBody (Just "http://x") "http://x" a) === b
          a ==> b = f a === b
          a === (renderTagsT . parseTagsT . tbs -> b)
              | a == b = Nothing
              | otherwise =
                  Just $ T.concat ["Error:\n  ", a, "\n  /=\n  ", b, "\n"]
          strip [] = []
          strip (TagOpen "script" _ : ts) =
              strip $ dropTillIncluding (TagClose "script") ts
          strip (TagOpen n as : ts) = TagOpen n (filter ok as) : strip ts
          strip (t : ts) = t : strip ts
          ok = flip notElem
              [("class", "bqrUnknownImgSizeUnprocessed")
              ,noTranslate]
          dummySrc = ("src", "http://x")
          proxiedDummySrc =
              second (proxyUrl PTContent "") dummySrc
          onload = ("onload", "bq.imgOnLoad(this)")
          addImgSrc (TagOpen "img" as)
              | Nothing <- lookup "src" as = TagOpen "img" (dummySrc:as)
          addImgSrc t = t
          rmImgSrc (TagOpen "img" as) =
              TagOpen "img" $ filter (`notElem` [proxiedDummySrc, onload]) as
          rmImgSrc (TagOpen "svg" as) =
              TagOpen "svg" $ filter (/= onload) as
          rmImgSrc t = t
          rmDirAuto (TagOpen t as) = TagOpen t (filter (/= ("dir", "auto")) as)
          rmDirAuto t = t
          vimeoInner p = "<span style='background-color: black' class='inner iframe_placeholder' data-bqr-html='&lt;iframe src=&quot;https://player.vimeo.com/video/137464081?autoplay=1&quot; style=&quot;background-color: black&quot; class=&quot;inner&quot; allow=&quot;autoplay; fullscreen; encrypted-media&quot; sandbox=&quot;allow-same-origin allow-scripts allow-forms allow-popups allow-presentation&quot; allowfullscreen=&quot;true&quot;" <> p <> "&gt;&lt;/iframe&gt;' onclick='uw_event=event;bq.replaceWithHtml(this, this.getAttribute(&apos;data-bqr-html&apos;))'><span class='vimeo vimeo-unprocessed visibilityHidden' data-video-id='137464081'><span class='video-wrapper'><span class='video cover'></span></span><span class='title' role='contentinfo'><span class='header'><span class='portrait' aria-hidden='true'></span><span class='headers'><span class='h1'><a href target='_blank'></a></span><span class='sub-title'><span class='h2 byline-from'>from <a href target='_blank'></a></span></span></span></span></span><span class='controls'><button class='play rounded-box state-paused'><span class='play-icon'><svg viewbox='0 0 20 20' preserveaspectratio='xMidYMid'><polygon class='fill' points='1,0 20,10 1,20'></polygon></svg></span></button></span></span></span>"

    -- "asdf<p><audio/><button/>as<i><sup/></i><li>df<p>as<li>df<ul><li><table><tr><div style='border: none; overflow: hidden; top: 0; left: 0; width: 100%; height: 100%; position: absolute'>"

emptyMsgKey = MsgKey "" Nothing Nothing

perfTest = do
    f <- T.readFile "./tests/preprocess/www.chrislovesjulia.com.html"
         -- тормозит style по 2.5kb со встроенным svg-изображением
    print $ T.length $ T.concat [s | TagOpen _ as <- parseTagsT $ tbs f, ("style", s) <- as]
    withLogger $ \ l -> logTime l "fixTags" $ do
        print (T.length $ fixTags "" (MsgKey "" Nothing Nothing) Nothing defaultPreprocessSettings [parseTagsT $ tbs $ T.replicate 500 f])
