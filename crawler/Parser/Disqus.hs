{-# LANGUAGE ViewPatterns, RecordWildCards, OverloadedStrings #-}
-- | Disqus
module Parser.Disqus
    ( customParsers
    , Disqus(..), dzero, dplus, dMplusForum
    , disqusUrl, disqusUrlForum, findDisqusInScript
    ) where

import Control.Concurrent
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Parser.Types
import Parser.Custom
import Lib.UrTime
import Lib.Regex
import Lib.StringConversion
import Control.Applicative
import URL
import Config (disqusApiKey)
import System.IO.Unsafe
import qualified Language.JavaScript.Parser.Lexer as JSL
--import qualified HJS.Parser.Lexer as HJS
--lexHJS = HJS.runLexer -- парсит комментарии как операторы

-- import qualified Language.ECMAScript3.Parser as EP
-- lexEP = EP.parseScriptFromString "src.js"
-- неправильно парсит многострочные строки
-- зато умеет делать unescape строке, но нет отдельного лексера
-- сразу парсит выражения и statement-ы

customParsers =
    mkCustomParsers ["disqus.com"]
    [ ( (isJust . disqusOfUrl), changeUrlIO transformDisqusUrl, CPJSON handleDisqusUrl )
    , ( (disqusUrlPostsPrefix `T.isPrefixOf`), changeUrlIO transformDisqusPostsUrl
      , CPJSON handleDisqusPosts )
    ]

disqusApiKeyN :: MVar Int
disqusApiKeyN = unsafePerformIO $ newMVar 0
{-# NOINLINE disqusApiKeyN #-}

disqusAppendApiKey t =
    modifyMVar disqusApiKeyN $ \ n -> do
        return (n+1, t `T.append` disqusApiKey n)

disqusUrlThreadsPrefix = "http://disqus.com/api/3.0/threads/list.json?forum="
disqusUrlPostsPrefix = "http://disqus.com/api/3.0/posts/list.json?forum="

transformDisqusUrl u
    | Just (Disqus { dForum = Just f, dIdentifier = Just i }) <- disqusOfUrl u =
        mk f "&thread:ident=" i
    | Just (Disqus { dForum = Just f, dUrl = Just l }) <- disqusOfUrl u =
        mk f "&thread:link=" l
    | otherwise = return u
    where e = encodeURIComponentT
          mk f what x =
              disqusAppendApiKey $
              T.concat [ disqusUrlThreadsPrefix, e f, what, e x ]

transformDisqusPostsUrl u =
    disqusAppendApiKey $ T.concat [u, "&order=asc&limit=100"]

handleDisqusUrl _ u json = do
    let x = do
          JSON.Object root <- return json
          JSON.Number (truncate -> code) <- HM.lookup "code" root
          response <- HM.lookup "response" root
          case response of
              JSON.String s -> return $ PRError $ T.concat
                  ["Disqus threads error #", T.pack (show code), ": ", s]
                  --  ^ особого смысла в этом нет,
                  --    т.к. все равно HTTP 400 BAD REQUEST выдается
              JSON.Array (V.toList -> [JSON.Object feed]) -> do
                  JSON.String forum <- HM.lookup "forum" feed
                  JSON.String id <- HM.lookup "id" feed
                  return $ PRRedirect $
                         T.concat [ disqusUrlPostsPrefix
                                  , encodeURIComponentT forum
                                  , "&thread="
                                  , encodeURIComponentT id ]
              JSON.Array (V.toList -> _) ->
                  return $ PRError "Too many results in disqus threads response"
              _ -> fail "parse error"
    case x of
        Nothing -> PRError "Can’t parse disqus threads response"
        Just r -> r

handleDisqusPosts _ u json = do
    let posts acc [] = return $ reverse acc
        posts acc (JSON.Object post : ps) = do
            p <- HM.lookup "parent" post
            JSON.String guid <- HM.lookup "id" post
            JSON.String time <- HM.lookup "createdAt" post
            JSON.String body <- HM.lookup "message" post
            JSON.Object a <- HM.lookup "author" post
            JSON.String author <- HM.lookup "name" a
            JSON.String alink <- HM.lookup "profileUrl" a
            JSON.Object avatar <- HM.lookup "avatar" a
            JSON.String apic <- HM.lookup "permalink" avatar
            posts
                ((case p of
                      JSON.Number (truncate -> n) -> Just (T.pack $ show n)
                      JSON.String s -> Just s
                      _ -> Nothing
                 , defaultFeedMsg
                   { fmGuid = guid
                   , fmAuthor = author
                   , fmPublishedTime = readRfc3339 $ T.unpack time
                   , fmBody = body
                   , fmLinks =
                       [(LKAuthor, alink), (LKAuthorPic, apic)]
                       -- (LKLink, T.concat [url, "#comment_", guid])
                   }) : acc) ps
        posts acc (_ : ps) = posts acc ps
        x = do
          JSON.Object root <- return json
          JSON.Number (truncate -> code) <- HM.lookup "code" root
          response <- HM.lookup "response" root
          case response of
              JSON.String s -> return $ PRError $ T.concat
                  ["Disqus posts error #", T.pack (show code), ": ", s]
                  --  ^ особого смысла в этом нет,
                  --    т.к. все равно HTTP 400 BAD REQUEST выдается
              JSON.Array (V.toList -> p) -> do
                  ps <- posts [] p
                  return $
                      PRParsedHtml Nothing
                      (maybe NoSwitch (Switch True) $
                       do JSON.Object cursor <- HM.lookup "cursor" root
                          JSON.Bool True <- HM.lookup "hasNext" cursor
                          JSON.String c <- HM.lookup "next" cursor
                          return $ head (T.splitOn "&cursor=" u)
                                     <> "&cursor=" <> c
                      )
                      ps []
              _ -> fail "parse error"
    case x of
        Nothing -> PRError "Can’t parse disqus posts response"
        Just r -> r


lexJS src = JSL.runAlex src (lex [])
    where lex acc = JSL.lexCont (lexToken acc)
          lexToken acc t@(JSL.EOFToken {..}) = return $ reverse (t:acc)
          lexToken acc t@(JSL.TailToken {..}) = return $ reverse (t:acc)
          lexToken acc t = lex (t:acc)

data Disqus
    = Disqus
      { dForum :: Maybe Text
      , dIdentifier :: Maybe Text
      , dUrl :: Maybe Text
      , dFoundEmbedJs :: Bool
      }
    deriving (Eq, Show)
-- TODO: по-хорошему надо проверять наличие строки с embed.js
-- или script src с embed.js
-- А также unescape-ить жабаскрипт строки:
-- https://developer.mozilla.org/en-US/docs/JavaScript/Guide/Values,_variables,_and_literals#String_literals

disqusUrl d = T.pack $ "http://disqus.com/?f=" ++ e dForum ++ "&i=" ++ e dIdentifier ++ "&u=" ++ e dUrl
    where e f = maybe "" (encodeURIComponent . T.unpack) $ f d
disqusOfUrl u
    | "http://disqus.com/?f=" `T.isPrefixOf` u
    , Right qs <- urlQueryStringUtf8Only u
    , Just dForum <- l "f" qs
    , Just dIdentifier <- l "i" qs
    , Just dUrl <- l "u" qs
    , dFoundEmbedJs <- True = Just (Disqus {..})
    | otherwise = Nothing
    where l k qs =
              fmap (\ x -> if x == "" then Nothing else Just x) $ lookup k qs

dzero = Disqus Nothing Nothing Nothing False
dplus a b = Disqus (m dForum) (m dIdentifier) (m dUrl)
                   (dFoundEmbedJs a || dFoundEmbedJs b)
    where m f = f a <|> f b
dMplusForum d f = d { dForum = dForum d <|> f }

jslStringToken :: JSL.Token -> Maybe Text
jslStringToken (JSL.StringToken { tokenLiteral = (T.pack -> s) })
    | T.length s > 2
    , h <- T.head s
    , l <- T.last s
    , h == l && (h == '"' || h == '\'')
    -- удаляем кавычки
    = JSON.decode (BL.fromStrict $ tbs $ "\"" <> (T.tail $ T.init s) <> "\"")
      -- обрабатываем escaping \/\/ => //
    | otherwise = Just s
jslStringToken _ = Nothing

findDisqusInScript s
    | "disqus" `T.isInfixOf` s || "embed.js" `T.isInfixOf` s =
        either (const dzero) (go dzero) (lexJS (T.unpack s))
    | otherwise = dzero
    where go d [] = d
          go d (JSL.IdentifierToken { tokenLiteral = i } :
                JSL.SimpleAssignToken {} :
                (jslStringToken -> Just s) : ts)
              | i == "disqus_shortname" =
                  st s (d `dMplusForum` Just s) ts
              | i == "disqus_identifier" =
                  st s (d { dIdentifier = dIdentifier d <|> Just s }) ts
              | i == "disqus_url" =
                  st s (d { dUrl = dUrl d <|> Just s }) ts
              | i == "disqus_blogger_current_url" =
                  st s (d { dUrl = dUrl d <|> Just s }) ts
              | i == "src" =
                  st s (d `dMplusForum` disqusUrlForum s) ts
          go d ((jslStringToken -> Just "disqusShortname") :
                JSL.ColonToken {} :
                (jslStringToken -> Just s) : ts) =
              st s (d `dMplusForum` Just s) ts
          go d (JSL.IdentifierToken { tokenLiteral = "disqus_config" } :
                JSL.SimpleAssignToken {} :
                JSL.FunctionToken {} : ts) =
             go (d `dplus` findDisqusInConfig ts) ts
          go d (JSL.IdentifierToken { tokenLiteral = "jQuery" } :
                JSL.DotToken {} :
                JSL.IdentifierToken { tokenLiteral = "extend" } :
                JSL.LeftParenToken {} :
                JSL.IdentifierToken { tokenLiteral = "Drupal" } :
                JSL.DotToken {} :
                JSL.IdentifierToken { tokenLiteral = "settings" } : ts) =
              go (d `dplus` findDisqusInDrupal ts) ts
          go d ((jslStringToken -> Just s) : ts) =
              st s d ts
          go d (_:ts) = go d ts
          st s d ts
              | "embed.js" `T.isInfixOf` s
                || ".disqus.com/blogger_item.js" == s =
                  go (d { dFoundEmbedJs = True }) ts
              | otherwise = go d ts

findDisqusInDrupal = findD
    where findD [] = dzero
          findD ((jslStringToken -> Just "disqus") :
                 JSL.ColonToken {} : JSL.LeftCurlyToken {} : ts) =
              go (dzero { dFoundEmbedJs = True }) ts
          findD (_:ts) = findD ts
          go d [] = d
          go d ((jslStringToken -> Just p) :
                JSL.ColonToken {} :
                (jslStringToken -> Just s) : ts)
              | p == "shortname" =
                  go (d `dMplusForum` Just s) ts
              | p == "identifier" =
                  go (d { dIdentifier = dIdentifier d <|> Just s }) ts
              | p == "url" =
                  go (d { dUrl = dUrl d <|> Just s }) ts
          go d (JSL.RightCurlyToken {} : ts) =d
          go d (_:ts) = go d ts

findDisqusInConfig = go dzero
    where go d [] = d
          go d (JSL.ThisToken {} :
                JSL.DotToken {} :
                JSL.IdentifierToken { tokenLiteral = "page" } :
                JSL.DotToken {} :
                JSL.IdentifierToken { tokenLiteral = p } :
                JSL.SimpleAssignToken {} :
                (jslStringToken -> Just s) : ts)
              | p == "shortname" =
                  go (d `dMplusForum` Just s) ts
              | p == "identifier" =
                  go (d { dIdentifier = dIdentifier d <|> Just s }) ts
              | p == "url" =
                  go (d { dUrl = dUrl d <|> Just s }) ts
          go d (JSL.RightCurlyToken {} : ts) = d
          go d (_:ts) = go d ts

disqusUrlForum u
    | [[_,f]] <- regexGet "https?://disqus.com/forums/([^/]+)/embed.js" u
        = Just f
    | [[_,f]] <- regexGet "https?://([^\\.]+).disqus.com/embed.js" u
        = Just f
    | otherwise = Nothing
-- http://disqus.com/forums/controlflow/embed.js
-- http://cnn.disqus.com/embed.js

-- testErr = do
--     [(UUtf8 u,_)] <- return $ parseQueryString $ T.pack fn
--     print $ disqusOfUrl u
--     parseUrlTF (T.unpack u) fn
--     where fn = "http%3A%2F%2Fwww.reddit.com%2Fr%2Fprogramming%2F.rss"

-- testDisqus = mapM_ test urls
--     where test u = do
--               putStrLn u
--               r <- parseUrl u
--               let [s] = [s | JSCSupported s <- prJsComments r]
--               (tu,_,_) <- transformURL s
--               T.putStrLn tu
--               PRRedirect pu_ <- parseTrUrl s tu
--               (pu,_,_) <- transformURL pu_
--               T.putStrLn pu
--               print =<< parseTrUrl pu_ pu
--               putStrLn ""
--           urls =
--               [ "http://lisp-univ-etc.blogspot.ru/2012/10/slava-akhmechet-published-several.html"
--                -- "http://www.npr.org/2012/10/24/163547250/stephen-colberts-most-meaningful-musical-moments?ft=1&f=13"
-- --               , "http://scrollingtext.org/project-euler-problem-14"
-- --               , "http://www.yesodweb.com/blog/2012/08/joining-forces-advance-haskell"
-- --               , "http://www.geekwire.com/2011/experiments-video-game-economics-valves-gabe-newell/"
-- --               , "http://edition.cnn.com/2012/08/13/sport/olympics-eddie-izzard-multicultural-britain/index.html?hpt=hp_c1"
-- --               , "http://www.cucirca.com/2012/01/31/house-season-8-episode-10-runaways/"
-- --               , "http://macradar.ru/hardware/magic-mouse-review/"
-- --               , "http://www.engadget.com/2012/08/12/crackle-arrives-on-windows-phone/"
-- --               , "http://www.selectism.com/2012/08/12/schott-nyc-collection-for-fall-winter-2012/"
--               ]
