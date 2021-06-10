module Lib.FastChar
    ( emptyText, emptyTextL
    , isMail
    , isDomainLabel
    , isUrl
    , isUrlEnd
    , isPunctuationOrSpace
    , isPunctuation
    , isLowerAlnum
    , isUpper
    , isSpace
    , isAlpha
    , isAlphaMark
    , isAdvanceInPrint
    , isOpening
    , Alphabet, mkAlphabet, inAlphabet, intersectAlphabet, alphabetToList
    ) where

import Data.Array.Unboxed
import Data.Array.Base
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Array.ST
import Control.Monad
import Control.Arrow
import Data.Coerce
import Data.Semigroup

-- isAlphaNum осуществляет поиск делением пополам по списку Unicode-правил
-- и работает медленно. Пользуемся битовой маской.

-- | Битовая маска для всех символов [minBound..maxBound]
-- inCharMask работает без проверки диапазона -- можно применять только на
-- корректных символах.
newtype CharMask = CharMask (UArray Char Bool)

-- | Битовая маска для небольшого числа символов
newtype Alphabet = Alphabet (UArray Char Bool)

instance Show Alphabet where
    showsPrec d (Alphabet m) = showParen (d > app_prec) $
        showString "Alphabet " . shows [a | (a,True) <- assocs m]
        where app_prec = 10

inAlphabet (Alphabet m) c
    | c >= a && c <= b = unsafeAt m $ C.ord c - C.ord a
    | otherwise = False
    where (a,b) = bounds m
mkAlphabet [] = Alphabet $ listArray ('b','a') []
mkAlphabet a = Alphabet $ runSTUArray $ do
    p <- newArray (coerce $ foldMap (Min &&& Max) a) False
--     p <- newArray (minimum a, maximum a) False
    forM_ a $ \ c -> writeArray p c True
    return p
intersectAlphabet (Alphabet a) (Alphabet b) =
    [i | i <- [max aa ba..min ab bb], a ! i, b ! i]
    where (aa,ab) = bounds a
          (ba,bb) = bounds b
alphabetToList (Alphabet a) =
    [i | i <- range (bounds a), a ! i]

inCharMask (CharMask m) c = unsafeAt m (C.ord c) -- m ! c
mkCharMask f =
    CharMask $ listArray (minBound, maxBound) $ map f [minBound..maxBound]

emptyText = T.all (inCharMask spaceMask)
emptyTextL = TL.all (inCharMask spaceMask)

alnumOr :: String -> Char -> Bool
alnumOr s c = C.isAlphaNum c || c `elem` s

isMail = inCharMask mailCharMask
mailCharMask
    = mkCharMask $ alnumOr "-_"
    -- [-A-Za-z0-9_]  -- "." анализируем отдельно
isDomainLabel = inCharMask domainLabelCharMask
domainLabelCharMask
    = mkCharMask $ \ c -> C.isLower c || C.isNumber c || c == '-'
isUrl = inCharMask urlCharMask
urlCharMask
    = mkCharMask $ alnumOr "-+&@#/%=~_|?!:,.;"
    -- [-A-Za-z0-9+&@#/%=~_|?!:,.;]*
isUrlEnd = inCharMask urlEndCharMask
urlEndCharMask
    = mkCharMask $ alnumOr "-+&@#/%=~_|"
    -- [-A-Za-z0-9+&@#/%=~_|]
isPunctuationOrSpace = inCharMask punctuationOrSpaceMask
punctuationOrSpaceMask
    = mkCharMask $ \ c -> C.isPunctuation c || C.isSpace c
isPunctuation = inCharMask punctuationMask
punctuationMask
    = mkCharMask $ \ c -> C.isPunctuation c
isLowerAlnum = inCharMask lowerAlnumMask
lowerAlnumMask
    = mkCharMask $ \ c -> C.toLower c == c && C.isAlphaNum c
isUpper = inCharMask upperMask
upperMask
    = mkCharMask $ \ c -> C.isUpper c
isSpace = inCharMask spaceMask
spaceMask
    = mkCharMask C.isSpace
isAlpha = inCharMask alphaMask
alphaMask
    = mkCharMask C.isAlpha
isAlphaMark = inCharMask alphaMarkMask
alphaMarkMask
    = mkCharMask $ \ c -> C.isAlpha c || C.isMark c
isAdvanceInPrint = inCharMask advanceInPrintMask
advanceInPrintMask
    = mkCharMask $ \ c -> C.isPrint c && not (C.generalCategory c == C.NonSpacingMark)
{-# NOINLINE domainLabelCharMask #-}
{-# NOINLINE mailCharMask #-}
{-# NOINLINE urlCharMask #-}
{-# NOINLINE urlEndCharMask #-}
{-# NOINLINE punctuationOrSpaceMask #-}
{-# NOINLINE punctuationMask #-}
{-# NOINLINE lowerAlnumMask #-}
{-# NOINLINE upperMask #-}
{-# NOINLINE spaceMask #-}
{-# NOINLINE alphaMask #-}
{-# NOINLINE advanceInPrintMask #-}

-- | Открывающиеся скобки/кавычки на различных языках.
-- Слово, начинающееся с такого символа, считаем нормальным словом (без этого
-- символа) при определении языка и расстановке переносов
isOpening = inAlphabet openingAlphabet
openingAlphabet = mkAlphabet "([¡¿«‹“„‘‚”’‛»「『《〈【〔〖〘〚〝〟\"'"
{-# NOINLINE openingAlphabet #-}
