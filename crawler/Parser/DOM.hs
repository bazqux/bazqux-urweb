{-# LANGUAGE OverloadedStrings #-}
module Parser.DOM where

import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import qualified Data.Text as T
import qualified Data.HashSet as HS
import Lib.FastChar (emptyText)

-- https://developer.mozilla.org/en-US/docs/Web/CSS/Visual_formatting_model
--
-- Мы пытаемся имитировать работу браузера, определяя,
-- какие изображения/видео/pre находятся на отдельной строке:
-- 1. block-level элементы
-- 2. inline-level (возможно, обрамленный пустыми inline-level элементами
--    без полей/рамок/размеров или вложенный в inline-level элементы)
--    между block-level элементами
--
-- float-ы, table, list-item, blockquote игнорируем,
-- т.к. они все имеют дополнительный контент по бокам.
--
-- Также мы имитируем anonymous box-ы, в которые браузер складывает
-- inline-контент, перемежающийся с block-ами, создавая <div class=p>
-- между <div class=i> и помещая туда всё, что расположено между изображениями.
--
-- Для сохранения выравнивания и стилей, мы копируем родительские ноды
-- внутрь <div class=p> и <div class=i> (см moveUp)
--
-- Также, для создания одинаковых вертикальных полей вокруг изображений
-- мы помечаем элементы, создающие перенос на новую строку в начале и в конце
-- <div class=p> как class=vspace, чтобы потом делать им display:none
-- (см. markVspace)

type Forest = [Node]
data Node
    = Node
      { name  :: !T.Text
      , attrs :: [(T.Text, T.Text)]
      , level :: !NodeLevel
      , children :: Forest
      }
    | TextNode !T.Text
    | VoidElement
      { name  :: !T.Text
      , attrs :: [(T.Text, T.Text)]
      }
    deriving Show
data NodeLevel = BlockLevel | InlineLevel
    deriving (Show, Eq)

-- | http://w3c.github.io/html/syntax.html#void-elements
voidElements :: HS.HashSet T.Text
voidElements =
    HS.fromList
    [ "area", "base", "br", "col", "embed", "hr", "img", "input", "link"
    , "meta", "param", "source", "track", "wbr" ]
visibleVoidElements :: HS.HashSet T.Text
visibleVoidElements =
    HS.fromList
    [ "br", "embed", "hr", "img", "input" ]
visibleVoidElement = (`HS.member` visibleVoidElements)
voidElement = (`HS.member` voidElements)

-- | не block/void ноды, видимые сами по себе
-- (а не только меняющие положение других тегов),
-- которые нельзя удалять
-- также
visibleInlineElements :: HS.HashSet T.Text
visibleInlineElements = HS.fromList
    [ "iframe", "object", "video", "audio", "canvas"
    , "button", "textarea", "select", "svg"
    -- , "optgroup", "option" -- перед ними (возможно) будет </select>
    ]
visibleInlineElement = (`HS.member` visibleInlineElements)

blockElement = (`HS.member` blockElements)

-- | https://developer.mozilla.org/en-US/docs/HTML/Block-level_elements
blockElements =
    HS.fromList
    [ "address", "article", "aside", "blockquote"
    , "details", "dialog"
    , "dd", "div", "dl", "dt", "fieldset", "figcaption", "figure", "footer"
    , "form", "h1", "h2", "h3", "h4", "h5", "h6", "header", "hgroup", "hr"
    , "li", "main", "nav", "ol", "p", "pre", "section"
    , "table", "ul"
    ]

spaceTag = (`HS.member` spaceTags)

spaceTags =
    HS.fromList ["br", "iframe", "embed", "object", "td", "tr", "video"]
    `HS.union`
    blockElements

monospaceTag = (`HS.member` monospaceTags)
monospaceTags :: HS.HashSet T.Text
monospaceTags =
    HS.fromList
    ["tt", "code", "kbd", "samp", "pre", "xmp", "plaintext", "listing"]

buildForest = go [] [] . dropWhile skipAtStart
    where skipAtStart (TagText t) = emptyText t
          skipAtStart (TagClose _) = True
          skipAtStart _ = False
          block n a = Node n a BlockLevel []
          levelByName n
              | blockElement n = BlockLevel
              | otherwise        = InlineLevel
          -- go path@[(node, parentForest)] forest
          closeNode n f fp = n { children = reverse f } : fp
          go :: [(Node, Forest)] -> Forest -> [Tag T.Text] -> Forest
          go [] f [] = reverse f
          go ((n,fp):ns) f [] = go ns (closeNode n f fp) []
          go ns f (t:ts) = case t of
              TagText t -> go ns (TextNode t:f) ts
              TagOpen n as
                  | voidElement n ->
                      go ns (VoidElement n as:f) ts
              -- TagClose "br" -> go ns (VoidElement "br" []:f) ts
              --  ^ вырезается в fast-tagsoup
              TagClose n
                  | voidElement n -> go ns f ts
              TagOpen n as ->
                  open n as ns f ts
              TagClose n ->
                  close n ns f ts
              -- чтобы было без warning
              TagComment _ -> go ns f ts
              TagWarning _ -> go ns f ts
              TagPosition _ _ -> go ns f ts
          open n
              | n == "p" = tryCloseAndOpen inBlock (withName n) n
                -- новый <p> закрывает имеющийся <p> в пределах блока
                -- <a> работает вне иерархии, пока этого не учитываем
              | n == "button" = tryCloseAndOpen everywhere (withName n) n
              | header n = tryCloseAndOpen everywhere (header . name) n
                -- новый <hN> закрывает любой <hN'>
              | n == "li" = tryCloseAndOpen inLiList (withName n) n
              | ddNode n = tryCloseAndOpen inDdList (ddNode . name) n
              | n == "td" || n == "th" =
                  inTable ["tr", "tbody", "thead", "tfoot"] n
              | n == "tr" =  inTable ["tbody", "thead", "tfoot"] n
              | n == "col" = inTable ["colgroup"] n
              | n `elem` ["caption", "colgroup", "thead", "tbody", "tfoot"] =
                  inTable [] n
              | rubyNode n =
                  within "ruby" $ tryCloseAndOpen inBlockIgnoringP
                      (rubyNode . name) n
              | n == "iframe" = \ as ns f ->
                  tryCloseAndOpen (const False) (const False) n as ns f .
                  dropWhile (/= TagClose "iframe")
              | otherwise =
                  tryCloseAndOpen (const False) (const False) n
                  -- только открываем
          close n
              | n == "p" = \ ns f ts ->
                  case tryClose inBlock (withName "p") ns f of
                      Nothing -> go ns (block "p" [] : f) ts
                      -- одиночный </p> превращаем в <p></p>
                      Just (ns', f') -> go ns' f' ts
              | header n = tryCloseAndGo everywhere (header . name)
              | blockElement n = tryCloseAndGo everywhere (withName n)
              | otherwise = tryCloseAndGo inBlock (withName n)
          withName n np = name np == n
          inBlock np =
                 level np == BlockLevel
              || name np == "button"
              --  ^ не является Block-Level, но ведет себя как <div>
              -- при закрытии элементов
          inBlockIgnoringP np = name np /= "p" && inBlock np
          ddNode n = n == "dt" || n == "dd"
          inLiList np = name np `elem` ["ol", "ul", "dl", "dt", "dd"]
          inDdList np = name np `elem` ["ol", "ul", "dl", "li"]
          everywhere = const False
          header t = -- h1..h6
              T.length t == 2 &&
              T.head t == 'h' && T.last t >= '1' && T.last t <= '6'
          rubyNode n = n == "rb" || n == "rt" || n == "rtc"
          within name act = \ as ns f ts ->
              case tryClose everywhere (withName name) ns f of
                  Just _ ->
                      act as ns f ts
                  Nothing -> go ns f ts
          tryCloseAndGo stop check ns f ts
              | Just (ns', f') <- tryClose stop check ns f = go ns' f' ts
              | otherwise = go ns f ts
          tryCloseAndOpen stop check n as ns f ts
              | Just (ns', f') <- tryClose stop check ns f =
                  go ((node, f'):ns') [] ts
              | n /= "p" && blockElement n
              , Just (ns', f') <- tryClose inBlock (withName "p") ns f =
                  go ((node, f'):ns') [] ts
                  -- необходимо сначала закрыть параграф
              | otherwise =
                  go ((node, f):ns) [] ts
              where node = Node n as (levelByName n) []
          tryClose :: (Node -> Bool) -> (Node -> Bool)
              -> [(Node, Forest)] -> Forest
              -> Maybe ([(Node, Forest)], Forest)
          tryClose _ _ [] _ = Nothing
          tryClose stop check ((np,fp):nsp) f
              | check np =
                  Just (nsp, closeNode np f fp)
              | stop np = Nothing -- сначала проверяем, потом выходим <= block
              | otherwise =
                  tryClose stop check nsp (closeNode np f fp)
          inTable stop n as ns f ts = tgo ns f
              where tgo [] _ = go ns f ts -- нет <table>
                    tgo ns@((np,fp):nsp) f
                        | name np `elem` "table":stop =
                            go ((Node n as (levelByName n) [], f):ns) [] ts
                        | otherwise =
                            tgo nsp (closeNode np f fp)

renderForest = renderTagsT . forestToTags

forestToTags = concatMap go
    where go (VoidElement n a) = [TagOpen n a]
          go (TextNode t) = [TagText t]
          go (Node n a _ ch) = TagOpen n a : forestToTags ch ++ [TagClose n]
