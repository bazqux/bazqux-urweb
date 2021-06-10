module Lib.QueryParser where

import Lib.ElasticSearch
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Aeson as JSON

-- data Primitive = Term T.Text | Phrase T.Text | Range T.Text T.Text
-- data Syntax t = T t | And [Syntax t] | Or [Syntax t] | Not (Syntax t)
-- data QueryBase = Primitive Primitive | Field T.Text (Syntax Primitive)
-- data Query = Syntax QueryBase

data Q
    = Term T.Text
    | Phrase T.Text
    | Range T.Text T.Text
    | Field T.Text Q -- ошибка, если уже внутри Field
    | And [Q]
    | Or [Q]
    | Not Q

test x = BL.putStrLn $ JSON.encode $ toEs "_all" x

toEs field q = case q of
    Term t
        | T.all (`elem` "*?") t ->
            obj "match_all" $ obj' []
        | Just _ <- T.find (`elem` "*?") t ->
            obj "wildcard" $ obj stdField $ JSON.String t
        | otherwise ->
            wrap $ \ f ->
--            obj "match" $ obj f $ JSON.String t
            obj "match" $ obj f $ obj'
            [ ("query", JSON.String t)
            , ("operator", JSON.String "AND") ]
            -- поскольку analyzer может разбить на несколько термов
            -- надо указывать AND
    Phrase p ->
        wrap $ \ f ->
        obj "match_prase" $ obj f $ JSON.String p
    Range a b ->
        obj "range" $ obj field $ obj'
        [ ("gte", JSON.String a)
        , ("lte", JSON.String b) ]
    Field f q
        | field /= "_all" -> error "Field inside field?"
        | otherwise -> toEs f q
    And qs -> esMust $ map (toEs field) qs
    Or qs -> esShould $ map (toEs field) qs
    Not q -> esMustNot [toEs field q]
    where wrap f
              | stdField /= field =
                  esShould [f field, f stdField]
              | otherwise =
                  f field
          stdField
              | field `elem` ["subject", "author", "tags", "text"] =
                  T.append "std_" field
              | field == "_all" =
                  "std_all"
              | otherwise = field
