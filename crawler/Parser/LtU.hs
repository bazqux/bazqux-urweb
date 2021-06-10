{-# LANGUAGE OverloadedStrings #-}
-- | lambda-the-ultimate.org
module Parser.LtU
    ( customParsers
    ) where

import Data.Char
import qualified Data.Text as T
import Parser.Types
import Parser.Custom
import Lib.UrTime
import Lib.Regex
import Lib.ReadUtils
import Text.HTML.TagSoup hiding (parseTags, renderTags, (~/=))
import Text.HTML.TagSoup.Fast

customParsers =
    mkCustomParsers ["lambda-the-ultimate.org"]
    [(rt "lambda-the-ultimate\\.org/node/[0-9]+", noChange, CPTags parseLTU)]

parseLTU _ url = go [] []
    where go c m [] =
              PRParsedHtml Nothing NoSwitch
                           [cmt | (depth, cmt) <- reverse c]
                           [(Nothing, more, Nothing) | more <- reverse m]
          go c m (margin : _ : TagOpen "a" [("id", guid)] : ts)
              | "comment-" `T.isPrefixOf` guid
              , d <- depth margin
              , (_ : TagOpen "a" _ : TagText subject : ts) <-
                  dropWhile (~/= TagOpen "h3" [("class","title")]) ts
              , (_ : ts) <-
                  dropWhile (~/= TagOpen "div" [("class", "content")]) ts
              , (comment, ts) <- span (~/= TagOpen "div" [("class","links")]) ts
              , (_ : TagText "By " : TagOpen "a" (("href", user):_) :
                 TagText author : TagClose "a" : TagText dateText : ts) <- ts
              , Just time <- parseLTUTime $ T.unpack dateText =

                  go (( d
                      , ( fmap (fmGuid . snd) $ lookup (d-1) c
                        , defaultFeedMsg
                          { fmGuid = guid
                          , fmAuthor = author
                          , fmSubject = subject
                          , fmPublishedTime = Just time
                          , fmBody = renderTagsT comment
                          , fmLinks =
                              [(LKAuthor, "http://lambda-the-ultimate.org/"
                                            <> user)
                              ,(LKLink, T.concat [url, "#", guid])]
                          })) : c) m ts
          go c m (TagOpen "a" [("href", more)] : TagText "next page" : ts) =
              go c ("http://lambda-the-ultimate.org/" <> more : m) ts
          go c m (_ : ts) = go c m ts
          depth (TagOpen "div" [("style",s)])
                   -- "margin-left:25px;"
              | "margin-left:" `T.isPrefixOf` s
                  = maybe 0 (`div` 25) $ tryReadUnsignedInt $ T.filter isDigit s
          depth _ = 0
          -- есть проблема, что на следующей странице комменты продолжаются
          -- из середины нити, а у нас они будут от корня.
          -- с другой стороны редко бывает >200 комментов,
          -- а зацикливания у нас невозможны по построению
          --
          -- вроде можно добавить ?comments_per_page=5000 и игнорировать
          -- следующие страницы

parseLTUTime = tryParseTime
    ["at %a, %Y-%m-%e %k:%M |"] -- " at Sat, 2012-04-21 10:01 | "
