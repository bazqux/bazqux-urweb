------------------------------------------------------------------------------
-- Быстрый разборщик JSON.
--
-- Соответствует https://tools.ietf.org/html/rfc7159
--
-- Низкоуровневый код -- работает через обычные ф-ии, без абстракций
-- вроде attoparsec (но с pattern synonyms, за счет чего разбор похож на
-- работу со String).
--
-- Содержит внутри один общий буфер Text, из которого нарезаются ключи и
-- строковые значения. За счет этого мы экономим на создании мелких Text,
-- но возможно удержание в памяти большого исходного буфера.
--
-- При использовании этого разборщика вместо aeson фильтрация ускоряется
-- в 3 раза.
--
-- Работает в 2-3 раза быстрее aeson. Быстрее sajson и json-syntax почти везде:
-- - sajson быстрее только на синтетических примерах с большой вложенностью
--   или большим числом пробелов
-- - json-syntax быстрее при разборе чисел (mesh.json), но в 1.4 раза медленнее
--   разбирает twitter и в 2 раза idsQuery.
--   (у sajson тормозит не разбор чисел, а преобразование в scientific в
--   fromFloatDigits)
--
-- Проверил Z.Data.JSON -- примерно как Lib.Json, но нужно еще ключи объектов
-- сортировать. По-идее, должен был работать как json-syntax без преобразований,
-- но оказывается в 1-1.5 раза медленнее.
--
-- Возможно, json-syntax создающий структуры данных aeson напрямую, без
-- дополнительных преобразований работал бы быстрее (без преобразований
-- он на треть шустрее Lib.Json), но уж очень он навороченный
-- для такой простой задачи, не хочется разбираться и тянуть все зависимости.
-- К тому же, он неправильно разбирает UTF16 surrogate
-- https://github.com/byteverse/json-syntax/issues/3

{-# LANGUAGE OverloadedStrings, RankNTypes, PatternSynonyms, ViewPatterns,
             BangPatterns, LambdaCase #-}
module Lib.Json
    ( decodeJson, eitherDecodeJson
    ) where

import qualified Data.HashMap.Strict as HM
import Data.Scientific (Scientific,scientific)
import qualified Data.Aeson as AE
import qualified Data.Vector as V
import           Data.Char
import           Data.Text.Internal (Text(..))
import qualified Data.Text.Array as A
import qualified Data.Text as T
import qualified Data.ByteString as B
import           Control.Monad.ST (ST)
import           GHC.Base (unsafeChr)
import           Data.Word (Word16)
import           Data.Char (ord)

import Lib.StringConversion (bst)

decodeJson :: B.ByteString -> Maybe AE.Value
decodeJson = parseJsonT . bst

eitherDecodeJson :: B.ByteString -> Either String AE.Value
eitherDecodeJson = maybe (Left "Invalid JSON") Right . decodeJson

parseJsonT :: Text -> Maybe AE.Value
parseJsonT t0@(Text _ _ t0len) = snd $ A.run2 $ do
    dst <- A.new t0len
    dsta <- A.unsafeFreeze dst
    let value d = \ case
            '[' :. ts -> arrayStart d $ skipWS ts
            '{' :. ts -> objectStart d $ skipWS ts
            't' :. 'r' :. 'u' :. 'e' :. ts        -> r (AE.Bool True) d ts
            'f' :. 'a' :. 'l' :. 's' :. 'e' :. ts -> r (AE.Bool False) d ts
            'n' :. 'u' :. 'l' :. 'l' :. ts        -> r AE.Null d ts
            (parseNumber -> Just (n, ts)) -> r (AE.Number n) d ts
            '"' :. ts -> fmap (\(s, d', ts') -> (AE.String s, d', ts'))
                <$> parseString d ts
            _ -> err
        r !v d ts = return $ Just (v, d, ts)
        err = return Nothing
        arrayStart d = \ case
            ']' :. ts -> r (AE.Array V.empty) d ts
            t -> arrayValue d [] 0 t
        arrayStep d acc !len = \ case
            ',' :. ts -> arrayValue d acc len $ skipWS ts
            ']' :. ts -> r (AE.Array $ V.reverse $ V.fromListN len acc) d ts
            _ -> err
        arrayValue d acc !len t = do
            av <- value d t
            case av of
                Just (v, d', ts) -> arrayStep d' (v:acc) (len+1) $ skipWS ts
                _ -> err
        objectStart d = \ case
            '}' :. ts -> r (AE.Object HM.empty) d ts
            t -> objectMember d HM.empty t
        objectStep d acc = \ case
            ',' :. ts -> objectMember d acc $ skipWS ts
            '}' :. ts -> r (AE.Object acc) d ts
            _ -> err
        objectMember d acc = \ case
            '"' :. ts -> do
                mk <- parseString d ts
                case mk of
                    Just (k, d', skipWS -> (':' :. (skipWS -> ts'))) -> do
                        mv <- value d' ts'
                        case mv of
                            Just (v, d'', ts'') ->
                                objectStep d'' (HM.insert k v acc) $ skipWS ts''
                            _ -> err
                    _ -> err
            _ -> err
        parseString d0 = string d0
            where string d t = case t of
                      '"' :. ts -> r (-- T.copy $
                          -- копирование замедляет на 8-20%
                                      Text dsta d0 (d-d0)) d ts
                      '\\' :. ets -> case ets of
                          '"' :. ts -> put '"' d ts
                          '\\' :. ts -> put '\\' d ts
                          '/' :. ts -> put '/' d ts
                          'b' :. ts -> put '\b' d ts
                          'f' :. ts -> put '\f' d ts
                          'n' :. ts -> put '\n' d ts
                          'r' :. ts -> put '\r' d ts
                          't' :. ts -> put '\t' d ts
                          'u' :. (hex4 -> Just (h, ts))
                              | h < 0xD800 || h > 0xDFFF -> do
                                  A.unsafeWrite dst d h
                                  string (d+1) ts
                              | h <= 0xDBFF -- high surrogate
                              , ('\\' :. 'u' :. (hex4 -> Just (l, ts'))) <- ts
                              , l >= 0xDC00 && l <= 0xDFFF -- low surrogate
                              -> do
                                  A.unsafeWrite dst d h
                                  A.unsafeWrite dst (d+1) l
                                  string (d+2) ts'
                          _ -> err
                      c :. ts -> put c d ts
                      _ -> err
                  put c d t = do
                      write dst d c
                      string (d+1) t
        check (Just (v, _, ts))
            | T.all isWhitespace ts = Just v
        check _ = Nothing
    v <- value 0 $ skipWS t0
    return (dst, check v)

isWhitespace :: Char -> Bool
isWhitespace '\x20' = True
isWhitespace '\x0A' = True
isWhitespace '\x0D' = True
isWhitespace '\x09' = True
isWhitespace _      = False

isExponent :: Char -> Bool
isExponent c = c == 'e' || c == 'E'

parseNumber :: Text -> Maybe (Scientific, Text)
parseNumber = \ case
    ('-' :. t) -> start negate t
    t -> start id t
    where start sign t = case t of
              '0' :. '.' :. (digit -> Just d) :. ts -> dot sign (startIR d) (-1) ts
              '0' :. ts -> Just $ expn 0 0 ts
              (digit1_9 -> Just d) :. ts -> digits sign (startIR d) ts
              _ -> Nothing
          digits sign !c t = case t of
              '.' :. (digit -> Just d) :. ts -> dot sign (accIR c d) (-1) ts
              (digit -> Just d) :. ts        -> digits sign (accIR c d) ts
              _ -> Just $ expn (sign $ readIR c) 0 t
          dot sign !c !e t = case t of
              (digit -> Just d) :. ts        -> dot sign (accIR c d) (e-1) ts
              _ -> Just $ expn (sign $ readIR c) e t
          expn c e0 t = case t of
              x :. ts
                  | isExponent x
                  , Just r <- withSign (expStart c e0 0) ts -> r
              _ -> (scientific c e0, t)
          expStart c e0 e sign t = case t of
              (digit -> Just d) :. ts -> expDigits c e0 (e*10 + d) sign ts
              _ -> Nothing
          expDigits c e0 !e sign t = case t of
              (digit -> Just d) :. ts -> expDigits c e0 (e*10 + d) sign ts
              _ -> Just (scientific c (sign e + e0), t)
          digit :: Enum a => Char -> Maybe a
          digit c
              | isDigit c = Just (toEnum $ ord c - ord '0')
              | otherwise = Nothing
          digit1_9 :: Enum a => Char -> Maybe a
          digit1_9 c
              | c >= '1' && c <= '9' = Just (toEnum $ ord c - ord '0')
              | otherwise = Nothing
          withSign :: Num a => ((a -> a) -> Text -> Maybe (b, Text))
                   -> Text -> Maybe (b, Text)
          withSign f t = case t of
              '+' :. ts -> f id ts
              '-' :. ts -> f negate ts
              _ -> f id t

-- Idea stolen from GHC implementation of `instance Read Integer`
-- http://hackage.haskell.org/package/base-4.11.1.0/docs/src/Text.Read.Lex.html#valInteger
-- A sub-quadratic algorithm for converting digits to Integer.
-- First we collect blocks of `blockDigits`-digit Integers
-- (so we don't do anything besides simple (acc*10+digit) on most inputs).
-- Then we combine them:
-- Pairs of adjacent radix b digits are combined into a single radix b^2 digit.
-- This process is repeated until we are left with a single digit.

blockDigits :: Int
blockDigits = 40

startBase :: Integer
startBase = 10^blockDigits

-- | (num digits in current block, blocks, current block's value)
type IntegerReader = (Int, [Integer], Integer)

startIR :: Integer -> IntegerReader
startIR d = (1, [], d)

{-# INLINE startIR #-}
{-# INLINE accIR #-}
{-# INLINE readIR #-}

accIR :: IntegerReader -> Integer -> IntegerReader
accIR (n, blocks, !cd) d
    | n < blockDigits = (n+1, blocks, cd*10 + d)
    | otherwise = (1, cd:blocks, d)

readIR :: IntegerReader -> Integer
readIR (_, [], cd) = cd
readIR (n, blocks, cd) =
    go startBase ((cd * padding):blocks) `div` padding
    where padding = 10^(blockDigits-n)
          go :: Integer -> [Integer] -> Integer
          go _ [] = 0
          go _ [x] = x
          go b xs = go (b*b) (combine b xs)
          combine :: Integer -> [Integer] -> [Integer]
          combine _ [] = []
          combine _ [x] = [x]
          combine b (x0:x1:xs) = x' : combine b xs
              where !x' = x0 + x1*b

skipWS :: Text -> Text
skipWS t = case t of
    c :. ts
        | isWhitespace c -> skipWS ts
        | otherwise -> t
    ts -> ts

-------------------------------------------------------------------------------
-- Low level utilities

pattern (:.) :: Char -> Text -> Text
pattern x :. xs <- (uncons -> Just (x, xs))

infixr 5 :.

-- | uncons first Word16 from Text without trying to decode UTF-16 sequence
uncons :: Text -> Maybe (Char, Text)
uncons (Text src offs len)
    | len <= 0 = Nothing
    | otherwise =
      Just (w2c (A.unsafeIndex src offs), Text src (offs+1) (len-1))
{-# INLINE uncons #-}

-- | write 16bit character
write :: A.MArray s -> Int -> Char -> ST s ()
write dst d x = A.unsafeWrite dst d (c2w x)
{-# INLINE write #-}

-- | no-op for convenient pattern matching
w2c :: Word16 -> Char
w2c = unsafeChr . fromIntegral
{-# INLINE w2c #-}

c2w :: Char -> Word16
c2w = fromIntegral . ord
{-# INLINE c2w #-}

hex :: Char -> Maybe Word16
hex c
    | c >= '0' && c <= '9' = Just (toEnum $ ord c - ord '0')
    | c >= 'a' && c <= 'f' = Just (toEnum $ ord c - ord 'a' + 10)
    | c >= 'A' && c <= 'F' = Just (toEnum $ ord c - ord 'A' + 10)
    | otherwise            = Nothing

{-# INLINE hex #-}

hex4 :: Text -> Maybe (Word16, Text)
hex4 ((hex -> Just h3) :. (hex -> Just h2)
      :. (hex -> Just h1) :. (hex -> Just h0) :. ts)
    = Just (h3*0x1000 + h2*0x100 + h1*0x10 + h0, ts)
hex4 _ = Nothing

{-# INLINE hex4 #-}
