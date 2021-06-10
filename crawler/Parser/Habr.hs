{-# LANGUAGE ViewPatterns, BangPatterns, OverloadedStrings #-}
-- | Хабрахабр
module Parser.Habr
    ( customParsers
    ) where

import Data.List
import qualified Data.Text as T
import Parser.Types
import Parser.Custom
import Lib.UrTime
import Lib.Regex
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import Control.Applicative

customParsers =
    mkCustomParsers
    ["habr.com", "habrahabr.ru", "geektimes.com", "geektimes.ru", "megamozg.ru"]
    [ ( rt "habr\\.com/.*post/[0-9]+/", noChange, CPTags parseHabrahabr )
    , ( rt "habr\\.com/.*news/t/[0-9]+/", noChange, CPTags parseHabrahabr )
    , ( rt "habr\\.com/.*company/.+/blog/[0-9]+/", noChange, CPTags parseHabrahabr )
      -- habr.com/ru/post/…
      -- старые сайты Хабра теперь все перенаправляются на habr.com,
      -- но оставим, на всякий случай
    , ( rt "habrahabr\\.ru/.*post/[0-9]+/", noChange, CPTags parseHabrahabr )
    , ( rt "habrahabr\\.ru/.*company/.+/blog/[0-9]+/", noChange, CPTags parseHabrahabr )
    , ( rt "geektimes\\.ru/.*post/[0-9]+/", noChange, CPTags parseHabrahabr )
    , ( rt "geektimes\\.ru/.*company/.+/blog/[0-9]+/", noChange, CPTags parseHabrahabr )
    , ( rt "geektimes\\.com/.*post/[0-9]+/", noChange, CPTags parseHabrahabr )
    , ( rt "geektimes\\.com/.*company/.+/blog/[0-9]+/", noChange, CPTags parseHabrahabr )
    , ( rt "megamozg\\.ru/.*post/[0-9]+/", noChange, CPTags parseHabrahabr )
    , ( rt "megamozg\\.ru/.*company/.+/blog/[0-9]+/", noChange, CPTags parseHabrahabr )]


parseHabrahabr _ url = findMsgText
    where bqrClass ("class", cls) =
              ("class", T.unwords $ map (T.append "bqrHabr_") $ T.words cls)
          bqrClass a = a
          pp (TagOpen t [("class", "spoiler_title")])
              = TagOpen t [("class", "bqrHabr_spoiler_title")
                          ,("onclick", habrahabrToggleSpoiler)]
          pp (TagOpen t a)
              | t /= "code" = TagOpen t (map bqrClass a)
          pp t = t
          bqrHabr = TagOpen "div" [("class","bqrHabr")]
          preprocess msg = renderTagsT $ bqrHabr : map pp msg
          dlTags (TagOpen "dl" (lookup "class" -> Just cls))
              = T.isInfixOf "tags" cls
          dlTags _ = False
          content (TagOpen "div" (lookup "class" -> Just (T.words -> c))) =
              "post__text" `elem` c
          content _ = False
          findMsgText xs
              | (_:xs') <- dropWhile (not . content) xs
              , (content, next) <- span (not . dlTags) xs'
              , next /= []
              = go (Just $ preprocess content) []  (filter notSpace next)
              | otherwise = go Nothing [] $ filter notSpace xs
          err :: String -> Maybe a
--          err w = error ("get " ++ w ++ " failed")
          err _ = Nothing
          get :: String -> ([Tag T.Text] -> Maybe (a, [Tag T.Text]))
              -> [Tag T.Text] -> Maybe (a, [Tag T.Text])
          get w f = foldr (<|>) (err w) . map f . tails
          go mt !acc xs
              | Just (parent, xs) <- get "parent" parent xs
              , Just (guid, xs) <- get "guid" guid xs
              , Just (alink, xs) <- get "authorLink" authorLink xs
              , Just (pic, xs) <- get "pic" pic xs
              , Just (author, xs) <- get "author" author xs
              , Just (time, xs) <- get "time" time xs
              , Just (msg, xs) <- msg xs
              = go mt
                  (( parent
                   , defaultFeedMsg
                     { fmGuid = guid
                     , fmAuthor = author
                     , fmPublishedTime = time
                     , fmBody = preprocess msg
                     , fmLinks =
                         [(LKAuthor, alink)] ++ pic ++
                         [(LKLink, T.concat [url, "#comment_", guid])]
                     }) : acc)
                  xs
              | otherwise = PRParsedHtml mt NoSwitch (reverse acc) []
              where
                  parent (TagOpen "span" [("class", "parent_id"),
                                          ("data-parent_id", p)] : xs)
                      = Just (if p == "0" then Nothing else Just p, xs)
                  parent _ = Nothing
                  guid (TagOpen "div" [("class", "comment"),
                                       ("id", T.stripPrefix "comment_" -> Just g)]
                        : xs) = Just (g, xs)
                  guid _ = Nothing
                  authorLink (TagOpen "a" a : xs)
                      | Just alink <- lookup "href" a
                      , Just (T.isInfixOf "user-info" -> True) <- lookup ("class") a
                      = Just (alink, xs)
                  authorLink _ = Nothing
                  pic (TagOpen "img" (lookup "src" -> Just s) : xs)
                      = Just ([(LKAuthorPic, s)], xs)
                  pic xs@(TagOpen "span" _ : _) = Just ([], xs)
                  pic _ = Nothing
                  author (TagOpen "span"
                             [("class", T.isInfixOf "user-info__nickname" -> True)] :
                             TagText a : xs) = Just (a, xs)
                  author _ = Nothing
                  time (TagOpen "time" _ : TagText t : xs)
                      = Just (parseHabrahabrTime $ T.unpack t, xs)
                  time _ = Nothing
                  msg xs = case span notReply $ dropWhile notMsg xs of
                      (_ : m, xs) -> Just (dropLast m, xs)
                      _ -> err "msg"
                  notMsg (TagOpen "div" (("class", c):_))
                      = not ("comment__message" `T.isPrefixOf` c)
                  notMsg _ = True
                  notReply (TagOpen "div" (("class", c):_))
                      = not ("comment__reply" `T.isPrefixOf` c ||
                             "comment__footer" `T.isPrefixOf` c)
                  notReply _ = True

parseHabrahabrTime = tryParseTime
    ["%e %B %Y %k:%M %Z"] .
                         T.unpack . r . T.pack -- 1 августа 2012 в 10:59
    where r = (`T.append` " +0300") .
              T.replace "в" "" .
              T.replace "января" "January" .
              T.replace "февраля" "February" .
              T.replace "марта" "March" .
              T.replace "апреля" "April" .
              T.replace "мая" "May" .
              T.replace "июня" "June" .
              T.replace "июля" "July" .
              T.replace "августа" "August" .
              T.replace "сентября" "September" .
              T.replace "октября" "October" .
              T.replace "ноября" "November" .
              T.replace "декабря" "December"
