{-# LANGUAGE LambdaCase, BangPatterns, ViewPatterns, OverloadedStrings #-}
module Parser.CSS where

import Data.Maybe
import Data.List
import qualified Data.Text as T
import qualified Data.CSS.Syntax.Tokens as CSS
import qualified Data.Scientific
import Lib.StringConversion (asciiToLowerT)

safeCssTokenize s
    | T.length s > 10000 = []
      -- на всякий пожарный не разгребаем огромные style
    | otherwise = CSS.tokenize s

parseCssStyle :: T.Text -> [(T.Text, [CSS.Token])]
parseCssStyle = go [] . safeCssTokenize
    where go acc x = case dropWS x of
              [] -> reverse acc
              CSS.Ident name : (dropWS -> (CSS.Colon : xs))
                  | (value, xs') <- span (/= CSS.Semicolon) $ dropWS xs ->
                      go ((asciiToLowerT name
                          ,fixWS $ rmImportant value) : acc) xs'
              xs ->
                  go acc $ dropWhile (== CSS.Semicolon) $
                  dropWhile (/= CSS.Semicolon) xs
          dropWS = dropWhile (== CSS.Whitespace)
          -- убираем последний и двойные пробелы из value
          fixWS [] = []
          fixWS [CSS.Whitespace] = []
          fixWS (CSS.Whitespace : xs@(CSS.Whitespace : _)) = fixWS xs
          fixWS (x:xs) = x : fixWS xs
          -- убираем !important, чтобы не ломал наши стили и pattern matching
          rmImportant = go . reverse
              where go (dropWS -> (CSS.Ident i : (dropWS -> CSS.Delim '!':xs)))
                        | T.toLower i == "important" = go xs
                    go x = reverse x

renderCssStyle :: [(T.Text, [CSS.Token])] -> T.Text
renderCssStyle =
    CSS.serialize . concat .
    intersperse [CSS.Semicolon, CSS.Whitespace] . map toTokens
    where toTokens (n,v) = CSS.Ident n : CSS.Colon : CSS.Whitespace : v

-- | Объединенные width, height и style атрибуты
combinedCssStyle :: [(T.Text, T.Text)] -> Maybe [(T.Text, [CSS.Token])]
combinedCssStyle a
    | style == "" = Nothing
    | otherwise = Just $ parseCssStyle style
    where attrStyle n
              | Just v <- lookup n a = T.concat [n, ":", v, ";"]
              | otherwise = ""
          style =
              T.concat
              [attrStyle "width", attrStyle "height"
              ,fromMaybe "" (lookup "style" a)]

lookupCssIdent :: T.Text -> [(T.Text, [CSS.Token])] -> Maybe T.Text
lookupCssIdent i = go . reverse
    where go [] = Nothing
          go ((n, [CSS.Ident p]):xs) | n == i = Just $ T.toLower p
          go (x:xs) = go xs

cssNV :: CSS.NumericValue -> Double
cssNV (CSS.NVInteger i) = fromIntegral i
cssNV (CSS.NVNumber n) = Data.Scientific.toRealFloat n

-- | Последний, синтаксически корректный position
cssPosition :: [(T.Text, [CSS.Token])] -> Maybe T.Text
cssPosition = lookupCssIdent "position"

cssPixels :: CSS.Token -> Maybe Double
cssPixels = \ case
    CSS.Number _ nv         -> Just $ cssNV nv
    CSS.Dimension _ nv unit -> Just $ cssNV nv * u (T.toLower unit)
    _ -> Nothing
    where u :: T.Text -> Double
          u "px" = 1
          u "in" = 96
          u "pt" = u "in" / 72
          u "pc" = 12 * u "pt"
          u "cm" = u "in" / 2.54
          u "mm" = u "in" / 25.4
          u "em" = 16
          u "ex" = 7.18
          u "ch" = 8
          -- https://drafts.csswg.org/css-values-4/#dimension
          -- есть еще юниты
          u _ = 1 -- vw, vh, vmin, vmax -- считаем, что окошко 100x100px
