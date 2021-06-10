{-# LANGUAGE ViewPatterns, OverloadedStrings, LambdaCase, RecordWildCards,
             TupleSections #-}
module Parser.Telegram
    ( customParsers
    , telegramError
    ) where

import Data.Maybe
import qualified Data.Text as T
import Parser.Types
import Parser.Custom
import Parser.DOM
import Parser.CSS
import Lib.UrTime
import Lib.Regex
import Lib.StringConversion
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import qualified Data.CSS.Syntax.Tokens as CSS
import Control.Applicative
import Lib.FastChar (emptyText)

customParsers :: CustomParsers
customParsers =
    mkCustomParsers ["t.me", "telegram.me"]
    [ ( rt "t\\.me/?([?#].*)?$", noScan, CPTags channelAddressRequired )
    , ( rt "telegram\\.me/?([?#].*)?$", noScan, CPTags channelAddressRequired )
    , ( regexTest "https://t\\.me/([^/?#]+)$", changeUrl transformUrl, CPTags parseChannel)
--    , ( regexTest "https://t\\.me/s/", noScan, CPTags redirectS)
--    , ( regexTest "https://t\\.me/", noScan, CPTags invalidChannelAddress)
    , ( rt "t\\.me/", noScan, CPTags redirectTME )
    , ( rt "telegram\\.me/", noScan, CPTags redirectTME )
    ]

tme x = "https://t.me/" <> x

transformUrl u
    | [[_, channel, _]] <- regexGet "https://t\\.me/([^/?#]+)([?#].*)?$" u =
        "https://t.me/s/" <> channel
    | otherwise = u
redirectTME _ u _
    | [[_, _, path]] <- regexGet "https?://[^/]+/(s/)?@*([^/?#]*)" u =
        PRRedirect $ tme path
    | otherwise = PRError "regexGet error???"
channelAddressRequired _ _ _ =
    PRError $ T.unlines
    [ "No Telegram chanel address specified."
    , "Please, subscribe to channels with addresses like t.me/SomeChannel"
    ]
invalidChannelAddress _ _ _ =
    PRError $ T.unlines
    [ "Invalid Telegram channel name."
    , "Please, subscribe to channels with addresses like t.me/SomeChannel"
    ]

telegramError u =
    "Not a public Telegram channel. It’s either does not exists, not a channel or not available on the t.me/s/… web mirror).\nIf you see messages on " <> transformUrl u <> " website then mail support@bazqux.com so I could check why it’s not working."

parseChannel _ u t = title t
    where title [] = PRError "No title?"
          title (TagOpen "title" _ : TagText t : ts)
              | Just (info:ts') <- tillClass "tgme_channel_info" ts =
                  msgs t [] ts
              | otherwise =
                  PRError $ telegramError u
          title (_ : ts) = title ts
          channelName t
              | [[_, n]] <- regexGet "^(.+) . Telegram$" t = n
              | otherwise = t
          channelLink
              | [[_, c]] <- regexGet "/s/(.+)$" u = tme c
              | otherwise = u
          msgs t acc ts
              | Just photo <- tillClass "tgme_widget_message_user_photo" ts
              , (TagOpen _ ownerHrefAs : ownerTags0)
                  <- dropWhile (not . cls "tgme_widget_message_owner_name") photo
              , Just authorLink <- lookup "href" ownerHrefAs
              , (ownerTags, owner) <- span (/= TagClose "a") ownerTags0
              , (_:text0) <- dropWhile (/= TagClose "div") owner
              , (text, footer)
                  <- span (not . cls "tgme_widget_message_footer") text0
              , (TagOpen _ hrefAs : TagOpen _ dateAs : buttons)
                  <- dropWhile (not . cls "tgme_widget_message_date") footer
              , Just link <- lookup "href" hrefAs
              , Just time <- readRfc3339 . T.unpack =<< lookup "datetime" dateAs
              , (buttonTags, next)
                  <- span (not . cls "tgme_widget_message_wrap") buttons
              = msgs t
                  (defaultFeedMsg
                   { fmAuthor = T.concat
                       [t | TagText t <- takeWhile (not . verifiedIcon) ownerTags]
                   , fmBody = renderTagsT
                       $ (TagOpen "div" [("class","bqrTelegram")] :)
                       $ map prefixClass $ forestToTags
                       $ (map (preprocess []) $ rmEmptyText $ buildForest text)
                           <> (findButtons $ buildForest buttonTags)
                   , fmPublishedTime = Just time
                   , fmGuid = link
                   , fmLinks = [(LKLink, link), (LKAuthor, authorLink)]
                       <> authorPic photo
                   } : acc)
                  next
              | otherwise =
                  PRFeed u
                  (defaultFeedMsg
                   { fmSubject = channelName t
                   , fmLinks = [(LKLink, channelLink)] })
                  acc -- без reverse, т.к. последние посты внизу
          authorPic (TagOpen "img" (lookup "src" -> Just p) : _) =
              [(LKAuthorPic, p)]
          authorPic _ = []
          verifiedIcon (TagOpen _ as) = hasClass "verified-icon" as
          verifiedIcon _ = False
          tillClass c [] = Nothing
          tillClass c (TagOpen _ as : ts)
              | hasClass c as = Just ts
          tillClass c (_ : ts) = tillClass c ts
          cls c (TagOpen _ as) = hasClass c as
          cls _ _ = False
          prefixClass (TagOpen t as) = TagOpen t (map prefixClass' as)
          prefixClass t = t
          prefixClass' ("class", c) =
              ("class", T.unwords
               $ map (\ c -> if T.isPrefixOf "bqr" c then c else "bqrTelegram_" <> c)
               $ T.words c)
          prefixClass' a = a

isTelegramFeed = T.isPrefixOf (tme "")

hr = VoidElement "hr" []

preprocess href = \ case
    Node {..}
        | hasClass "tgme_widget_message_text" attrs ->
            node "p" [] (fixText children)
        | hasClass "tgme_widget_message_service_photo" attrs ->
            node "p" [] (fixText children)
        | hasClass "message_media_not_supported_wrap" attrs ->
            if hasClass "media_not_supported_cont" attrs then
                TextNode ""
            else
                node "div" [] (map (notSupported []) children)
        | hasClass "tgme_widget_message_sticker_wrap" attrs
        , [Node "a" as _ ch] <- children
        , Just img@(Node _ ias _ _)
          <- findNodeByClassName "tgme_widget_message_sticker" ch ->
            node "div" [("style", "width: 256px")]
            [node "a" (filterHref as)
                [VoidElement "img" (findSrc ias ++ wh ias (Just img))]]
        | hasClass "tgme_widget_message_link_preview" attrs ->
            node "div" [] $
            map (link $ filterHref attrs) children
        | hasClass "tgme_widget_message_video_player" attrs ->
            video Nothing (filterHref attrs)
                "tgme_widget_message_video_" children
        | hasClass "tgme_widget_message_voice_player" attrs ->
            audio (filterHref attrs)
                "tgme_widget_message_voice_" children
        | hasClass "tgme_widget_message_photo_wrap" attrs ->
            node "p" []
            [VoidElement "img"
                (findSrc attrs ++ wh attrs
                 (findNodeByClassName "tgme_widget_message_photo" children))]
        | hasClass "tgme_widget_message_grouped_wrap" attrs ->
            node "div" [] $ group children
        | hasClass "tgme_widget_message_reply" attrs ->
            node "blockquote" [] $
            map (preprocess $ filterHref attrs) children
        | hasClass "tgme_widget_message_author" attrs ->
            -- автор внутри reply
            node "div" [] [node "a" href [text children]]
        | hasClass "tgme_widget_message_metatext" attrs ->
            -- текст внутри reply
            node "div" [] (fixText children)
        | hasClass "tgme_widget_message_reply_thumb" attrs ->
            thumb "left" href attrs
        | hasClass "tgme_widget_message_poll" attrs ->
            node "div" [] (map poll children)
    n -> n
    where fixText = map $ \ case
              Node {..}
                  | hasClass "emoji" attrs -> text children
                    -- убираем Telegram emoji, оставляем оригинал
                  | name == "pre" ->
                      Node { name = "code", .. }
              n -> n
          link href = \ case
              n@(Node {..})
                  | hasClass "link_preview_video_player" attrs ->
                      video Nothing
                          (filterHref attrs) "link_preview_video_" children
                  | hasClass "link_preview_site_name" attrs ->
                      node "div" (("class", "bqrShareCaption") : href)
                      [text children]
                  | hasClass "link_preview_title" attrs ->
                      node "a" (("class", "bqrShareLink") : href)
                      [text children]
                  | hasClass "link_preview_description" attrs ->
                      node "div" [("class", "bqrShareDescription")]
                      [text children]
                  | hasClass "link_preview_right_image" attrs ->
                      thumb "right" href attrs
                  | hasClass "link_preview_image" attrs ->
                      node "div" []
                      [node "a" href [VoidElement "img" (findSrc attrs)]]
                      -- есть только padding, width нет
              n -> n
          group = concatMap $ \ case
              Node {..}
                  | hasClass "tgme_widget_message_photo_wrap" attrs ->
                      [node "p" []
                       [node "a" (filterHref attrs)
                        [VoidElement "img" (findSrc attrs ++ ratioWH attrs)]]]
                  | hasClass "tgme_widget_message_video_player" attrs ->
                      [node "p" []
                       [video (ratio attrs) (filterHref attrs)
                            "tgme_widget_message_video_" children]]
                  | otherwise ->
                      group children
              _ -> []
          ratioWH attrs
              | Just r <- ratio attrs =
                  [("width" , showT $ round (if r > 1 then 800 else 800*r))
                  ,("height", showT $ round (if r <= 1 then 800 else 800/r))]
              | otherwise = attrs
          ratio attrs =
              (read . T.unpack) <$> lookup "data-ratio" attrs
          thumb float href attrs =
              node "a" href
              [VoidElement "img" (("style", "float: " <> float <> "; margin-right: 1em; width: 4em; height: 4em; object-fit: cover"):findSrc attrs)]

          poll = \ case
              Node {..}
                  | hasClass "tgme_widget_message_poll_question" attrs ->
                      node "div" [] [node "b" [] [text children]]
                  | hasClass "tgme_widget_message_poll_type" attrs ->
                      grayDiv [text children]
                  | hasClass "tgme_widget_message_poll_options" attrs ->
                      pollTable $ concatMap pollOption children
              n -> n
          pollOption = \ case
              Node {..}
                  | hasClass "tgme_widget_message_poll_option" attrs ->
                      [pollRow $ concatMap pollOption' children]
              n -> []
          pollOption' = \ case
              Node {..}
                  | hasClass "tgme_widget_message_poll_option_percent" attrs ->
                      [pollOptionPercent [text children]]
                  | hasClass "tgme_widget_message_poll_option_value" attrs ->
                      [pollOptionValue $ concatMap pollOptionValue' children]
              _ -> []
          pollOptionValue' = \ case
              Node {..}
                  | hasClass "tgme_widget_message_poll_option_text" attrs ->
                      [pollOptionText [text children]]
                  | hasClass "tgme_widget_message_poll_option_bar" attrs
                  , Just s <- lookup "style" attrs ->
                      [pollOptionBar' s]
              _ -> []
          wh attrs paddingNode
              | Just s <- parseCssStyle <$> lookup "style" attrs
              , Just [cssPixels -> Just w] <- lookup "width" s
              , Just (Node _ as _ _) <- paddingNode
              , Just ps <- parseCssStyle <$> lookup "style" as
              , Just [CSS.Percentage _ (cssNV -> padding)] <-
                  lookup "padding-top" ps <|> lookup "padding-bottom" ps
              , ws <- maybeWS ps
              = [("width", showT (round w))
                ,("height", showT (round (w*ws*padding/100)))]
              | otherwise
              = []
          maybeWS ps
              | Just [CSS.Percentage _ (cssNV -> ws)] <- lookup "width" ps
              = 100 / ws
              | otherwise = 1

findButtons f
    | null b = []
    | otherwise = hr : b
    where b = go f
          go [] = []
          go (n:ns) = case n of
              Node {..}
                  | hasClass "tgme_widget_message_inline_button" attrs ->
                      node "div" []
                          [node "a" (filterHref attrs) [text children]]
                      : go ns
                  | otherwise ->
                      go children ++ go ns
              _ -> go ns

grayDiv = node "div" [("style", "color: #777")]

notSupported href = \ case
    Node {..}
        | hasClass "message_media_not_supported_label" attrs ->
            grayDiv [text children]
        | hasClass "message_media_view_in_telegram" attrs ->
            node "a" (if null href then filterHref attrs else href)
                [text children]
        | otherwise ->
            Node
            { attrs = removeAttr "class" attrs
            , children = map (notSupported href) children
            , .. }
    VoidElement {..} ->
        VoidElement { attrs = removeAttr "class" attrs, .. }
    TextNode t -> TextNode t

text = TextNode . T.concat . text'
text' = concatMap $ \ case
    TextNode t -> [t]
    Node {..} -> text' children
    _ -> []

video ratio href prefix = go Nothing Nothing
    where go ns _ [] = fromMaybe (TextNode "Can’t extract video?") ns
          go ns p (c:cs) = case c of
              Node {..}
                  | hasClass (prefix <> "thumb") attrs ->
                      go ns (findUrl attrs) cs
                  | hasClass "message_media_not_supported_wrap" attrs ->
                      go (Just $ node "div" []
                             (map (notSupported href) children)) p cs
                  | hasClass (prefix <> "wrap") attrs
                  , [Node "video" vas l ch] <- children ->
                     node "span"
                         (addStylePre scalableStyle
                          $ maybe
                             (filterAttr "style" attrs)
                             (\ r ->
                                 [("style", aspectRatioPaddingBottom (1/r))])
                             ratio)
                         [Node "video"
                             (maybe id (addAttrIfNotExists "poster") p
                              $ addStyle innerStyle
                              $ removeAttr "class" vas) l ch]
              _ -> go ns p cs

audio href prefix = node "div" [] . go Nothing []
    where go ns t [] =
              maybe [TextNode "Can’t extract audio?"] (: t) ns
          go ns t (c:cs) = case c of
              Node {..}
                  | hasClass "message_media_not_supported_wrap" attrs ->
                      go (Just $ node "div" []
                             (map (notSupported href) children)) t cs
                  | hasClass (prefix <> "wrap") attrs
                  , [Node _ _ _ t@[TextNode _]] <- filter time children ->
                    go ns [node "div" [] [node "a" href t]] cs
                  | name == "audio" ->
                    go (Just c) t cs
              _ -> go ns t cs
          time (Node { name = "time", .. }) = True
          time _ = False

rmEmptyText = go
    where go [] = []
          go (n:ns) = case n of
              TextNode t
                  | emptyText t -> go ns
                  | otherwise -> TextNode t : go ns
              Node {..} -> Node { children = go children, .. } : go ns
              n@(VoidElement {}) -> n : go ns

findNodeByClassName c = foldr (<|>) Nothing . map (\ case
    n@(Node {..})
        | hasClass c attrs -> Just n
        | otherwise -> findNodeByClassName c children
    n@(VoidElement {..})
        | hasClass c attrs -> Just n
    _ -> Nothing)

filterAttr a = filter ((== a) . fst)
filterHref = filterAttr "href"
findSrc = maybe [] (\ u -> [("src", u)]) . findUrl

findUrl as
    | Just s <- lookup "style" as = findCssUrl $ CSS.tokenize s
    | otherwise = Nothing
