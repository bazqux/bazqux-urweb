------------------------------------------------------------------------------
-- https://tools.ietf.org/html/rfc7159
--
-- работает в 2-3 раза быстрее aeson, медленнее sajson,
-- примерно как json-syntax.
--
-- Работает на ByteString и, в отличии от Lib.Json, создаёт новые Text
-- для ключей и значений, что позволяет избегать удержания в памяти
-- большого исходного Text.
--
-- Общее замедление на разборе Twitter/idsQuery где-то в 1.5 раза.
-- Добавление копирования Text в Lib.Json вызывает замедление всего на 8-20%,
-- так что дело не только в создании новых Text.
-- Возможно, A.unsafeIndex работает лучше чем (inlinePerformIO . peek)).
--
-- Оставляем для справки.
--
{-# LANGUAGE OverloadedStrings, RankNTypes, PatternSynonyms, ViewPatterns,
             BangPatterns, LambdaCase #-}
module Lib.JsonB
    ( parseJson
    ) where

import System.IO.Unsafe
import qualified Data.HashMap.Strict as HM
import Data.Scientific (Scientific,scientific)
import qualified Data.Aeson as AE
import qualified GHC.Exts as Exts
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import           Data.Char (ord, isDigit)
import qualified Data.Text.Encoding as T
import Data.Word (Word8)
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Bits

parseJson :: B.ByteString -> Maybe AE.Value
parseJson t0@(B.toForeignPtr -> (fp, o, l)) = unsafePerformIO $ do
  dstFp <- B.mallocByteString $ B.length t0
  withForeignPtr fp $ \ src0 ->
   withForeignPtr dstFp $ \ dst -> do
    let value = \ case
            '[' :. ts -> array [] $ skipWS ts
            '{' :. ts -> object HM.empty $ skipWS ts
            't' :. 'r' :. 'u' :. 'e' :. ts        -> r (AE.Bool True) ts
            'f' :. 'a' :. 'l' :. 's' :. 'e' :. ts -> r (AE.Bool False) ts
            'n' :. 'u' :. 'l' :. 'l' :. ts        -> r AE.Null ts
            (parseNumber -> Just (n, ts)) -> r (AE.Number n) ts
            '"' :. ts -> fmap (\(s, ts') -> (AE.String s, ts'))
                <$> parseString ts
            _ -> err
        r !v ts = return $ Just (v, ts)
        ra acc ts = r (AE.Array $ Exts.fromList $ reverse acc) ts
        ro acc ts = r (AE.Object acc) ts
        err = return Nothing
        array acc = \ case
            ']' :. ts -> ra acc ts
            t -> arrayValue acc t
        arrayStep acc = \ case
            ',' :. ts -> arrayValue acc $ skipWS ts
            ']' :. ts -> ra acc ts
            _ -> err
        arrayValue acc t = do
            r <- value t
            case r of
                Just (v, ts) -> arrayStep (v:acc) $ skipWS ts
                _ -> err
        object acc = \ case
            '}' :. ts -> ro acc ts
            t -> objectMember acc t
        objectStep acc = \ case
            ',' :. ts -> objectMember acc $ skipWS ts
            '}' :. ts -> ro acc ts
            _ -> err
        objectMember acc = \ case
            '"' :. ts -> do
                s <- parseString ts
                case s of
                    Just (s, skipWS -> (':' :. (skipWS -> ts'))) -> do
                        r <- value ts'
                        case r of
                            Just (v, ts'') ->
                                objectStep (HM.insert s v acc) $ skipWS ts''
                            _ -> err
                    _ -> err
            _ -> err
        parseString = string True 0
            where string !a d t = case t of
                      '"' :. ts
                          | a -> r (T.decodeLatin1 $ B.fromForeignPtr dstFp 0 d) ts
                          | otherwise ->
                            case T.decodeUtf8' $ B.fromForeignPtr dstFp 0 d of
                              Left _ -> err
                              Right s -> r s ts
                      '\\' :. ets -> case ets of
                          '"' :. ts -> put a '"' d ts
                          '\\' :. ts -> put a '\\' d ts
                          '/' :. ts -> put a '/' d ts
                          'b' :. ts -> put a '\b' d ts
                          'f' :. ts -> put a '\f' d ts
                          'n' :. ts -> put a '\n' d ts
                          'r' :. ts -> put a '\r' d ts
                          't' :. ts -> put a '\t' d ts
                          'u' :. (hex4 -> Just (h, ts))
                              | h < 0xD800 || h > 0xDFFF -> do
                                  d' <- encodeChar dst d h
                                  string True d' ts
                              | h <= 0xDBFF -- high surrogate
                              , ('\\' :. 'u' :. (hex4 -> Just (l, ts'))) <- ts
                              , l >= 0xDC00 && l <= 0xDFFF -- low surrogate
                              -> do
                                  d' <- encodeChar dst d $
                                     ((h - 0xD800) `shiftL` 10) +
                                     (l - 0xDC00) + 0x10000
                                  string True d' ts'
                          _ -> err
                      c :. ts -> put (a && c < '\x80') c d ts
                      _ -> err
                  put a c d t = do
                      pokeByteOff dst d c
                      string a (d+1) t
        check (Just (r, ts))
            | BP _ 0 <- skipWS ts = Just r
        check _ = Nothing
    r <- value $ skipWS $ BP (plusPtr src0 o) l
    return $ check r

isWhitespace :: Char -> Bool
isWhitespace '\x20' = True
isWhitespace '\x0A' = True
isWhitespace '\x0D' = True
isWhitespace '\x09' = True
isWhitespace _      = False

isExponent :: Char -> Bool
isExponent c = c == 'e' || c == 'E'

encodeChar :: Ptr Word8 -> Int -> Int -> IO Int
encodeChar dst o oc
   | oc <= 0x7f = do
       put 0 oc
       return $ o+1
   | oc <= 0x7ff = do
       put 0 $ 0xc0 + (oc `shiftR` 6)
       put 1 $ 0x80 + oc .&. 0x3f
       return $ o+2
   | oc <= 0xffff = do
       put 0 $ 0xe0 + (oc `shiftR` 12)
       put 1 $ 0x80 + ((oc `shiftR` 6) .&. 0x3f)
       put 2 $ 0x80 + oc .&. 0x3f
       return $ o+3
   | otherwise = do
       put 0 $  0xf0 + (oc `shiftR` 18)
       put 1 $ 0x80 + ((oc `shiftR` 12) .&. 0x3f)
       put 2 $ 0x80 + ((oc `shiftR` 6) .&. 0x3f)
       put 3 $ 0x80 + oc .&. 0x3f
       return $ o+4
   where put offs x = pokeByteOff dst (o + offs) (toEnum x :: Word8)

parseNumber :: BP -> Maybe (Scientific, BP)
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
          withSign :: Num a => ((a -> a) -> BP
                   -> Maybe (b, BP))
                   -> BP -> Maybe (b, BP)
          withSign f t = case t of
              '+' :. ts -> f id ts
              '-' :. ts -> f negate ts
              _ -> f id t

-- Idea stolen from GHC implementation of `instance Read Integer`
-- http://hackage.haskell.org/package/base-4.11.1.0/docs/src/B.ByteString.Read.Lex.html#valInteger
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

skipWS :: BP -> BP
skipWS t = case t of
    c :. ts
        | isWhitespace c -> skipWS ts
        | otherwise -> t
    ts -> ts

-------------------------------------------------------------------------------
-- Low level utilities

pattern (:.) :: Char -> BP -> BP
pattern x :. xs <- (uncons -> Just (B.w2c -> x, xs))

data BP =
    BP
    { bpPtr :: {-# UNPACK #-} !(Ptr Word8)
    , bpLen :: {-# UNPACK #-} !Int
    }

uncons (BP a 0) = Nothing
uncons (BP a l) =
    Just (B.accursedUnutterablePerformIO $ peek a, BP (plusPtr a 1) (l-1))

{-# INLINE uncons #-}

infixr 5 :.

hex :: Char -> Maybe Int
hex c
    | c >= '0' && c <= '9' = Just (ord c - ord '0')
    | c >= 'a' && c <= 'f' = Just (ord c - ord 'a' + 10)
    | c >= 'A' && c <= 'F' = Just (ord c - ord 'A' + 10)
    | otherwise            = Nothing

{-# INLINE hex #-}

hex4 :: BP -> Maybe (Int, BP)
hex4 ((hex -> Just h3) :. (hex -> Just h2)
      :. (hex -> Just h1) :. (hex -> Just h0) :. ts)
    = Just (h3*0x1000 + h2*0x100 + h1*0x10 + h0, ts)
hex4 _ = Nothing

{-# INLINE hex4 #-}
