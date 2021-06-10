{-# LANGUAGE ViewPatterns, TupleSections, BangPatterns, FlexibleContexts,
             RecordWildCards, OverloadedStrings #-}
module Lib.LanguageDetector
    (Language(..), alphabet, detect, languageHyphenator, languageCode
    ,textWords, wordTrigrams, nGramToText, NGram(..))
    where

import Control.Monad

import System.IO.Unsafe
import Data.Maybe
import Data.Ord
import Data.List
import Data.Char
import Data.Compact
import Text.Printf
import qualified Data.Text.ICU.Normalize as ICU
import qualified Text.Hyphenation as Hyph
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Base (unsafeRead, unsafeWrite, unsafeAt)
import Data.Array.Unboxed
import qualified Lib.FastChar as FC
import Lib.StringConversion
import System.Directory
import Data.Bits
import Lib.Json

newtype NGram = NGram { unNGram :: Int }

trigram :: Char -> Char -> Char -> NGram
trigram a b c =
    NGram $ fromEnum a `shiftL` (21*2) + fromEnum b `shiftL` 21 + fromEnum c
bigram = trigram '\0'
unigram = bigram '\0'

isTrigram (NGram n) = n `shiftR` (21*2) > 0
isBigram (NGram n) = n `shiftR` (21*2) == 0 && n `shiftR` 21 > 0
isUnigram (NGram n) = n `shiftR` 21 == 0

textToNGram t = case T.unpack t of
    [a,b,c] -> trigram a b c
    [a,b]   -> bigram a b
    [a]     -> unigram a
    _       -> error $ "textToNGram: bad NGram " ++ show t

nGramToText (NGram n) = T.pack $ map chr $ filter (/= 0) [a,b,c]
    where a = n `shiftR` (21*2)
          b = n `shiftR` 21 .&. 0x10ffff
          c = n .&. 0x10ffff

instance Show NGram where
    show = show . nGramToText

loadLanguage l = do
    root <- doesDirectoryExist "./crawler"
    f <- B.readFile $ (if root then "./crawler" else ".") <> "/Lib/languages/"
        <> languageFile l
    let Just (JSON.Object o) = decodeJson f
        Just (JSON.Object freq) = HM.lookup "freq" o
--         Just (JSON.Array (mapMaybe i . V.toList -> [n1,n2,n3])) =
--             HM.lookup "n_words" o
        trigrams' = [(T.toLower g, f) | (g, i -> Just f) <- HM.toList freq, T.length g == 3]
        freqs = HM.fromListWith (+) trigrams'
        trigrams = HM.toList freqs
        total = HM.foldl' (+) 0 freqs
        probabilities = map prob trigrams
        triProb c f =
            fromIntegral c / fromIntegral (max c $ sum [c | (g,c) <- trigrams, f g])
--            (c, sum [c | (g,c) <- trigrams, f g])
        prob (g,c)
            | T.isPrefixOf " " g = (g, triProb c (T.isPrefixOf " "))
            | otherwise = (g, triProb c (T.isPrefixOf -- T.isSuffixOf
                                              (T.take 2 g)))
        normFreq f =
            IntMap.map (\x -> fromIntegral x / fromIntegral t) f
            where t = IntMap.foldl' (+) 0 f
        nGram n = normFreq $ IntMap.fromListWith (+)
            [(unNGram $ textToNGram g, f)
            | (T.toLower -> g, i -> Just f) <- HM.toList freq, T.length g == n]
--     print $ sum [f | (g, i -> Just f) <- HM.toList freq, T.length g == 2, T.isPrefixOf " " g]
--     print $ sum [f | (g, i -> Just f) <- HM.toList freq, T.length g == 2, T.isPrefixOf " " g]
--     print $ sum [f | (g, i -> Just f) <- HM.toList freq, T.length g == 2, T.isSuffixOf " " g]
    -- не совпадает ни количество " x" и " xx", ни количество " xx" и "xx "
--     forM_ (filter (\(_,(a,p)) -> a > p)
--                   probabilities) $ \ (g,p) ->
--         T.putStrLn $ g <> " " <> T.pack (show p)
    -- в русском: ную (9327,0)
    -- "ную" встречается, а триграмм, заканчивающихся на "ну" нет,
    -- что является явным косяком
    -- в английском вообще my  (44622,0) -- нет триграммы " my"
    return $ -- HM.fromList probabilities
       -- nGram 1 `IntMap.union` nGram 2 `IntMap.union`
       nGram 3
--         Frequencies { f1 = nGram 1 n1, f2 = nGram 2 n2, f3 = nGram 3 n3 }
    where i x = do
              JSON.Number (truncate -> i) <- return x
              return i

data Probs
    = Probs !Int (UArray Int Int) (UArray Int Double)

emptyProbs = Probs 0 (listArray (0,-1) []) (listArray (0,-1) [])
probsAssocs (Probs _ ls ps) =
    zipWith (\l p -> (toEnum l :: Language, p)) (elems ls) (elems ps)

instance Show Probs where
    show = show . probsAssocs

loadLanguages = do
    ls <- forM [minBound .. maxBound] $ \ l ->
        IntMap.map (\f -> [(l,f)]) <$> loadLanguage l
    return $ IntMap.map norm $ foldr (IntMap.unionWith (<>)) IntMap.empty ls
    where norm :: [(Language, Double)] -> Probs
          norm xs =
              Probs (length xs)
              (listArray b (map (fromEnum . fst) xs))
              (listArray b (map ((/ t) . snd) xs))
              where t = sum (map snd xs)
                    b = (0, length xs - 1)

languages = unsafePerformIO $
    getCompact <$> (compactWithSharing =<< loadLanguages)
{-# NOINLINE languages #-}

nGramsText = concatMap wordNGrams . textWords

textWords = mapMaybe w . T.words . T.toLower
    where w (skipOpening -> t)
              | not (T.null a) && T.all FC.isPunctuation p = Just a
              | otherwise = Nothing
              where (a,p) = T.span letter t
                    letter c = FC.isAlphaMark c || separator c
          skipOpening t = case T.uncons t of
              Just (o,ts) | FC.isOpening o -> ts
              _ -> t

separator c = c == '-' || c == '\'' || c == '’'

wordNGrams t = case T.uncons t of
    Just (unsep -> x, ts) -> unigram x : bigram ' ' x : go ' ' x ts
    Nothing -> []
    where go p1 p2 t = case T.uncons t of
              Just (unsep -> x, ts) ->
                  unigram x : trigram p1 p2 x : bigram p2 x
                  : go p2 x ts
              Nothing -> [trigram p1 p2 ' ', bigram p2 ' ']
          unsep c
              | separator c = ' '
              | otherwise = c

wordTrigrams t = case T.uncons t of
    Just (unsep -> x, ts) -> go ' ' x ts
    Nothing -> []
    where go p1 p2 t = case T.uncons t of
              Just (unsep -> x, ts) -> trigram p1 p2 x : go p2 x ts
              Nothing -> [trigram p1 p2 ' ']
          unsep c
              | separator c = ' '
              | otherwise = c

detect_ =
    sortBy (comparing $ Down . snd) . Map.toList
    . foldl' (Map.unionWith (+)) Map.empty
    . map (-- Map.map (const 1) .
           Map.fromList . norm . wordProbability) . textWords
    where norm [] = [(Nothing, 1.0)]
          norm xs = [(Just l, p / sum (map snd xs)) | (l,p) <- xs]

-- detectCosine t = Map.
--     where freqs = Map.fromListWith (+) $ map (,1)
--               $ filter ((==3) . T.length) . nGramsText

normP [] = []
normP [(l,_)] = [(l,1.0)]
normP xs = [(l, p / t) | (l,p) <- xs]
     where t = sum (map snd xs)

wordProbability w = case probs of
    (g:gs) -> foldl' merge g gs
    _ -> []
    where probs = map (\ g -> maybe [] probsAssocs $
                          IntMap.lookup (unNGram g) languages)
              $ wordTrigrams w
          merge [] _  = []
          merge _  [] = []
          merge al@((a,pa):as) bl@((b,pb):bs)
              | a == b = (a, pa*pb) : merge as bs
              | a < b  = merge as bl
              | otherwise = merge al bs
          -- с суммированием лучше определяет языки с редким алфавитом
          -- и хилым словарем типа венгерского, но гораздо хуже работает
          -- отсеивание языков по несуществующим в них словам
          -- (afrikaans оказывается впереди english)
          mergeU [] b  = b
          mergeU a  [] = a
          mergeU al@((a,pa):as) bl@((b,pb):bs)
              | a == b = (a, pa+pb) : mergeU as bs
              | a < b  = (a,pa) : mergeU as bl
              | otherwise = (b,pb) : mergeU al bs

d = T.putStrLn . snd . detect



detect :: T.Text -> ([Language], T.Text)
detect t = runST $ do
    p <- newArray (minBound, maxBound) 0.0 :: ST s (STUArray s Language Double)
    n <- newArray (minBound, maxBound) 0   :: ST s (STUArray s Language Int)
    let maxLang = fromEnum (maxBound :: Language)
    wLangs <- newArray (0, maxLang) 0   :: ST s (STUArray s Int Int)
    wProbs <- newArray (0, maxLang) 0.0 :: ST s (STUArray s Int Double)
    let -- go :: Int -> Int -> [T.Text] -> ST s ([Language], T.Text)
        go total bad [] = do
            gs0 <- forM alphabetGroups $ \ ls -> do
                forM ls $ \ l ->
                    (l,) <$> ((,) <$> readArray p l <*> readArray n l)
            (l,d) <- foldMap choose
                [g | g <- map (filter (\(_,(_,n)) -> n > 0)) gs0
                , not $ null g]
            return (l, T.unlines
                [d
                ,"Total      " <> T.justifyRight 5 ' ' (showT total)
                ,"Unknown    " <> T.justifyRight 5 ' ' (showT bad)])
        -- дальнейший код go является развернутым normP + wordProbability,
        -- использует массивы с текущими индексами языков и произведениями
        -- вероятностей, которые выделяются заранее,
        -- чтобы кучу не плодить списков в merge + normP
--         go !total !bad (w:ws)
--             | wp@(_:_) <- normP $ wordProbability w = do
--                 forM_ wp $ \ (l,lp) -> addProb l lp
--                 go (total+1) bad ws
--             | otherwise =
--                 go (total+1) (bad+1) ws
        go !total !bad (w:ws) = case probs of
            [Probs n ls ps] -> do
                -- не нужно нормализовывать вероятности
                let add i
                        | i == n = goGood
                        | otherwise = do
                            addProb (unsafeAt ls i) (unsafeAt ps i)
                            add (i+1)
                add 0
            (Probs na lsa psa : Probs nb lsb psb : tps) -> do
                let -- первоначальное заполнение массивов из двух списков
                    -- вероятностей
                    merge1 !ia !ib !i
                        | ia == na || ib == nb = merge i tps
                        | a == b = do
                            unsafeWrite wLangs i a
                            unsafeWrite wProbs i
                                (unsafeAt psa ia*unsafeAt psb ib)
                            merge1 (ia+1) (ib+1) (i+1)
                        | a < b  = merge1 (ia+1) ib i
                        | otherwise = merge1 ia (ib+1) i
                        where a = unsafeAt lsa ia
                              b = unsafeAt lsb ib
                merge1 0 0 0
            _ ->
                goBad
            where goGood = go (total+1) bad ws
                  goBad  = go (total+1) (bad+1) ws
                  probs = map (\ g -> IntMap.findWithDefault emptyProbs
                                  (unNGram g) languages)
                      $ wordTrigrams w
                  -- дальнейшее умножение вероятностей в массивах на вероятности
                  -- из списков (со сжатием массивов)
                  merge 0 _ = goBad
                  merge 1 [] = do
                      -- остался один язык, также не надо нормализовывать
                      l <- unsafeRead wLangs 0
                      addProb l 1.0
                      goGood
                  merge n [] = do
--                       t <- sum <$> mapM (unsafeRead wProbs) [0..n-1]
--                       forM_ [0..n-1] $ \ i -> do
--                           l <- unsafeRead wLangs i
--                           p <- unsafeRead wProbs i
--                           addProb l (p / t)
                      let total i !acc
                              | i == n = return acc
                              | otherwise = do
                                  x <- unsafeRead wProbs i
                                  total (i+1) (x+acc)
                      t0 <- unsafeRead wProbs 0
                      t1 <- unsafeRead wProbs 1
                      t <- total 2 (t0+t1)
                      let add i
                              | i == n = return ()
                              | otherwise = do
                                  l <- unsafeRead wLangs i
                                  p <- unsafeRead wProbs i
                                  addProb l (p / t)
                                  add (i+1)
                      l0 <- unsafeRead wLangs 0
                      p0 <- unsafeRead wProbs 0
                      addProb l0 (p0 / t)
                      l1 <- unsafeRead wLangs 1
                      p1 <- unsafeRead wProbs 1
                      addProb l1 (p1 / t)
                      add 2
                      goGood
                  merge n (Probs bn lsb psb:tps) = mergeA 0 0 0
                      where mergeA s ib d
                                | s == n || ib == bn = merge d tps
                                | otherwise = do
                                    a <- unsafeRead wLangs s
                                    let b = unsafeAt lsb ib
                                    if a == b then do
                                        pa <- unsafeRead wProbs s
                                        unsafeWrite wLangs d a
                                        unsafeWrite wProbs d
                                            (pa*unsafeAt psb ib)
                                        mergeA (s+1) (ib+1) (d+1)
                                    else if a < b then
                                        mergeA (s+1) ib d
                                    else
                                        mergeA s (ib+1) d
        addProb l lp = do
            p0 <- unsafeRead p l
            unsafeWrite p l (p0+lp)
            n0 <- unsafeRead n l
            unsafeWrite n l (n0+1)
        choose (sortBy (comparing $ Down . snd) -> ls) =
            case ls of
                ((_,(_,n)):_)
                    | n < minWords ->
                        -- но если соотношение с pNext > 10, то можно?
                        -- а если next нет, то 3 слова?
                        return ([], debug "Too few words to decide")
                ((l,(pTop,nTop)):(_,(pNext,nNext)):_)
                    | pTop > 3*pNext
                      && nTop*2 > nNext ->
                        return ([l], debug $ showT l)
                    | otherwise ->
                        return ([], debug $ "Probability difference is too small")
                [(l,(p,n))] ->
                    return ([l], debug $ showT l)
                [] ->
                    return ([], "No languages detected")
            where minWords = 10
                  (lsTop, lsRest) = splitAt 6 ls
                  debug cause = T.unlines $
                      [cause]
                      <>
                      [T.unwords [T.justifyLeft 10 ' ' (showT l)
                                 ,T.justifyRight 5 ' ' (showT n)
                                 ,T.pack (printf "%11.5f" p)]
                      |(l,(p,n)) <- lsTop]
                      <>
                      if not $ null lsRest then ["…"] else []
    go 0 0 $ -- take 300 $
        textWords t
    -- перевод кода на массивы вместо списков ускорил работу всего в 2 раза,
    -- причем, понадобилось переводить всё, что только можно, включая
    -- списки вероятностей триграмм.
    -- ограничение до 300 слов ускоряет еще ~1.5 раза, но ухудшает точность
    -- В общем профиле detect сократился с 40% до 20%, что не так уж и плохо

data Language
    = Afrikaans
    | Basque
    | Belarusian
    | Bengali
    | Bulgarian
    | Catalan
--    Chinese
--    Coptic
    | Croatian
    | Czech
    | Danish
    | Dutch
    | English
--     English_US
--     English_GB
--     Esperanto
    | Estonian
--    Ethiopic
    | Finnish
    | French
--     Friulan
    | Galician
--    Georgian
    | German
--     German_1901
--     German_1996
--     German_Swiss
    | Greek
     -- современный греческий -- это monotone
--     Greek_Ancient
--     Greek_Mono
--     Greek_Poly
    | Gujarati
    | Hindi
    | Hungarian
    | Icelandic
    | Indonesian
--     Interlingua
    | Irish
    | Italian
    | Kannada
--     Kurmanji
--     Latin
--     Latin_Classic
    | Latvian
    | Lithuanian
    | Malayalam
    | Marathi
--     Mongolian
      -- Bokmål использует 85-90% населения Норвегии
    | Norwegian
--     Norwegian_Bokmal
--     Norwegian_Nynorsk
--     Oriya
--     Panjabi
--     Piedmontese
    | Polish
    | Portuguese
    | Romanian
--     Romansh
    | Russian
--     Sanskrit
    | Serbian
--     Serbian_Cyrillic
--     Serbocroatian_Cyrillic
--     Serbocroatian_Latin
    | Slovak
    | Slovenian
    | Spanish
    | Swedish
    | Tamil
    | Telugu
    | Thai
    | Turkish
--     Turkmen
    | Ukrainian
--     Uppersorbian
    | Welsh
    deriving (Eq,Ix,Ord,Show,Bounded,Enum)

languageCode :: Language -> String
languageCode = takeWhile (/= '.') . languageFile

languageFile :: Language -> String
languageFile s = case s of
    Afrikaans -> "af"
    Basque -> "eu"
    Belarusian -> "be.new"
    Bengali -> "bn"
    Bulgarian -> "bg"
    Catalan -> "ca"
--    Chinese -> "zh-latn-pinyin"
--    Coptic -> "cop"
    Croatian -> "hr"
    Czech -> "cs.new"
    Danish -> "da"
    Dutch -> "nl"
    English -> "en.new"
--     English_US -> "en-us"
--     English_GB -> "en-gb"
--     Esperanto -> "eo"
    Estonian -> "et"
--    Ethiopic -> "mul-ethi"
    -- Farsi -> "fa"
    Finnish -> "fi"
    French -> "fr.new"
--     Friulan -> "fur"
    Galician -> "gl"
--    Georgian -> "ka"
    German -> "de.new"
--     German_1901  -> "de-1901"
--     German_1996  -> "de-1996"
--     German_Swiss -> "de-ch-1901"
    Greek -> "el"
    -- современный греческий -- это monotone
--     Greek_Ancient -> "grc"
--     Greek_Mono -> "el-monoton"
--     Greek_Poly -> "el-polyton"
    Gujarati -> "gu"
    Hindi -> "hi"
    Hungarian -> "hu.new"
    Icelandic -> "is"
    Indonesian -> "id"
--     Interlingua -> "ia"
    Irish -> "ga"
    Italian -> "it.new"
    Kannada -> "kn"
--     Kurmanji -> "kmr"
--     Latin -> "la"
--     Latin_Classic -> "la-x-classic"
    Latvian -> "lv"
    Lithuanian -> "lt"
    Malayalam -> "ml"
    Marathi -> "mr"
--     Mongolian -> "mn-cyrl"
    -- Bokmål использует 85-90% населения Норвегии
    Norwegian -> "no"
--     Norwegian_Bokmal  -> "nb"
--     Norwegian_Nynorsk -> "nn"
--     Oriya -> "or"
--     Panjabi -> "pa"
--     Piedmontese -> "pms"
    Polish -> "pl.new"
    Portuguese -> "pt"
    Romanian -> "ro"
--     Romansh -> "rm"
    Russian -> "ru.new"
--     Sanskrit -> "sa"
    Serbian -> "sr" -- Serbian_Cyrillic
--     Serbian_Cyrillic -> "sr-cyrl"
--     Serbocroatian_Cyrillic -> "sh-cyrl"
--     Serbocroatian_Latin -> "sh-latn"
    Slovak -> "sk"
    Slovenian -> "sl"
    Spanish -> "es.new"
    Swedish -> "sv"
    Tamil -> "ta"
    Telugu -> "te"
    Thai -> "th"
    Turkish -> "tr"
--     Turkmen -> "tk"
    Ukrainian -> "uk.new"
--     Uppersorbian -> "hsb"
    Welsh -> "cy"

languageHyphenator :: Language -> T.Text -> Maybe Hyph.Hyphenator
languageHyphenator l al = Hyph.languageHyphenator <$> toHyphLanguage l al

parseAcceptLanguage =
    map (T.toLower . T.takeWhile (/= ';') . T.strip) . T.split (== ',')

toHyphLanguage :: Language -> T.Text -> Maybe Hyph.Language
toHyphLanguage s acceptLanguage = case s of
    Afrikaans -> Just Hyph.Afrikaans
    Basque -> Just Hyph.Basque
    Belarusian -> Nothing
    Bengali -> Just Hyph.Bengali
    Bulgarian -> Just Hyph.Bulgarian
    Catalan -> Just Hyph.Catalan
--    Chinese -> Just Hyph.Chinese
--    Coptic -> Just Hyph.Coptic
    Croatian -> Just Hyph.Croatian
    Czech -> Just Hyph.Czech
    Danish -> Just Hyph.Danish
    Dutch -> Just Hyph.Dutch
    English -> Just $
        if findLang "en" == Just "en-gb"
        then Hyph.English_GB else Hyph.English_US
--     Esperanto -> Just Hyph.Esperanto
    Estonian -> Just Hyph.Estonian
--    Ethiopic -> Just Hyph.Ethiopic
    -- Farsi -> Just Hyph.Farsi
    Finnish -> Just Hyph.Finnish
    French -> Just Hyph.French
--     Friulan -> Just Hyph.Friulan
    Galician -> Just Hyph.Galician
--    Georgian -> Just Hyph.Georgian
    German -> Just $
        if findLang "de" == Just "de-ch"
        then Hyph.German_Swiss else Hyph.German_1996
    Greek -> Nothing -- Just Hyph.Greek_Mono
    -- есть еще Coptic с пересекающимся алфавитом
    -- современный греческий -- это monotone
--     Greek_Ancient -> Just Hyph.Greek_Ancient
--     Greek_Mono -> Just Hyph.Greek_Mono
--     Greek_Poly -> Just Hyph.Greek_Poly
    Gujarati -> Just Hyph.Gujarati
    Hindi -> Nothing -- Just Hyph.Hindi
    --  ^ один из десятков языков на devanagari, легко ошибиться
    Hungarian -> Just Hyph.Hungarian
    Icelandic -> Just Hyph.Icelandic
    Indonesian -> Just Hyph.Indonesian
--     Interlingua -> Just Hyph.Interlingua
    Irish -> Just Hyph.Irish
    Italian -> Just Hyph.Italian
    Kannada -> Nothing -- алфавит используется для нескольких языков Just Hyph.Kannada
--     Kurmanji -> Just Hyph.Kurmanji
--     Latin -> Just Hyph.Latin
--     Latin_Classic -> Just Hyph.Latin_Classic
    Latvian -> Just Hyph.Latvian
    Lithuanian -> Just Hyph.Lithuanian
    Malayalam -> Nothing -- Just Hyph.Malayalam
    -- тоже несколько языков использует
    Marathi -> Nothing -- индия Just Hyph.Marathi
--     Mongolian -> Just Hyph.Mongolian
    -- Bokmål использует 85-90% населения Норвегии
    Norwegian -> Nothing -- Just Hyph.Norwegian_Bokmal
    -- непонятно, какой язык у нас натренирован
--     Norwegian_Bokmal  -> Just "nb"
--     Norwegian_Nynorsk -> Just Hyph.Norwegian_Nynorsk
--     Oriya -> Just Hyph.Oriya
--     Panjabi -> Just Hyph.Panjabi
--     Piedmontese -> Just Hyph.Piedmontese
    Polish -> Just Hyph.Polish
    Portuguese -> Just Hyph.Portuguese
    Romanian -> Just Hyph.Romanian
--     Romansh -> Just Hyph.Romansh
    Russian -> Just Hyph.Russian
--     Sanskrit -> Just Hyph.Sanskrit
    Serbian -> Just Hyph.Serbian_Cyrillic
--     Serbian_Cyrillic -> Just Hyph.Serbian_Cyrillic
--     Serbocroatian_Cyrillic -> Just Hyph.Serbocroatian_Cyrillic
--     Serbocroatian_Latin -> Just Hyph.Serbocroatian_Latin
    Slovak -> Just Hyph.Slovak
    Slovenian -> Just Hyph.Slovenian
    Spanish -> Just Hyph.Spanish
    Swedish -> Just Hyph.Swedish
    Tamil -> Just Hyph.Tamil
    Telugu -> Just Hyph.Telugu
    Thai -> Just Hyph.Thai
    Turkish -> Just Hyph.Turkish
--     Turkmen -> Just Hyph.Turkmen
    Ukrainian -> Just Hyph.Ukrainian
--     Uppersorbian -> Just Hyph.Uppersorbian
    Welsh -> Just Hyph.Welsh
    where findLang l =
              find (T.isPrefixOf l) $ parseAcceptLanguage acceptLanguage

alphabet l = alphabets ! l

alphabets :: Array Language FC.Alphabet
alphabets = listArray (minBound,maxBound) $ map a [minBound..maxBound]
    where a (alphabet' -> cs) = FC.mkAlphabet $ cs ++ map toUpper cs

basicLatin = ['a'..'z']
gaj'sLatin = "abcčćddžđefghijklljmnnjoprsštuvzž"
devanagari = ['\x0900'..'\x097F'] <> ['\xA8E0'..'\xA8FF'] <> ['\x1CD0'..'\x1CFF']

-- | Группы языков с пересекающимися алфавитами
alphabetGroups :: [[Language]]
alphabetGroups = go 0 Map.empty [minBound..maxBound]
    where go _ m [] =
              map reverse $ Map.elems
              $ Map.fromListWith (<>) [(g,[l]) | (l,g) <- Map.toList m]
          go n m la@(l:ls)
              | null groups =
                  go (n+1) (add n m) ls
              | otherwise =
                  go n (add mg $
                        Map.map (\i -> if i `elem` groups then mg else i) m) ls
              where groups = mapMaybe (flip Map.lookup m) intersections
                    mg = minimum groups
                    add g m = foldl' (\m l -> Map.insert l g m) m intersections
                    intersections =
                        l :
                        filter (not . null . FC.intersectAlphabet (alphabet l)
                                . alphabet) ls


alphabet' :: Language -> String
alphabet' s = case s of
    Afrikaans -> basicLatin <> "áéèêëíîïóôúûý"
    Basque -> basicLatin <> "ñç"
    Belarusian -> "абвгдеёжзійклмнопрстуўфхцчшыьэюя"
    -- Belarusian_Latin "abcćčddzdźdžefghchijklłmnńoprsśštuŭvyzźž"
    Bengali -> ['\x0980'..'\x09FF'] -- "ঀঁংঃ঄অআইঈউঊঋঌ঍঎এঐ঑঒ওঔকখগঘঙচছজঝঞটঠডঢণতথদধন঩পফবভমযর঱ল঳঴঵শষসহ঺঻়ঽািীুূৃৄ৅৆েৈ৉৊োৌ্ৎ৏৐৑৒৓৔৕৖ৗ৘৙৚৛ড়ঢ়৞য়ৠৡৢৣ৤৥০১২৩৪৫৬৭৮৯ৰৱ৲৳৴৵৶৷৸৹৺৻ৼ৽৾৿"
    -- язык Бангладеша
    Bulgarian -> "абвгдежзийклмнопрстуфхцчшщъьюя"
    Catalan -> basicLatin <> "àéèíïóòúüç"
--    Chinese -> _
--    Coptic -> _
    Croatian -> gaj'sLatin
    Czech -> "aábcčdďeéěfghchiíjklmnňoópqrřsštťuúůvwxyýzž"
    Danish -> basicLatin <> "æøåáéíóúýǿ"
    Dutch -> basicLatin <> "ñçáâäéèêëíîïóôöúûüýÿ"
    English -> basicLatin
--     English_US -> _
--     English_GB -> _
--     Esperanto -> _
    Estonian -> "abdefghijklmnoprsšzžtuvõäöü"
--    Ethiopic -> _
    -- Farsi -> _
    Finnish -> basicLatin <> "åäöšž"
    French -> basicLatin <> "àâæçéèêëîïôœùûüÿ"
--     Friulan -> _
    Galician -> "abcdefghilmnñopqrstuvxzáïü"
--    Georgian -> _
    German -> basicLatin <> "äöüß"
--     German_1901  -> "de-1901"
--     German_1996  -> "de-1996"
--     German_Swiss -> _
    Greek -> ['\x0370'..'\x03FF'] -- greek and coptic
    -- современный греческий -- это monotone
--     Greek_Ancient -> _
--     Greek_Mono -> _
--     Greek_Poly -> _ == greekAndCoptic U+0370..U+03FF + greek extended U+1F00..U+1FFF
    Gujarati -> ['\x0A80'..'\x0AFF'] -- "઀ઁંઃ઄અઆઇઈઉઊઋઌઍ઎એઐઑ઒ઓઔકખગઘઙચછજઝઞટઠડઢણતથદધન઩પફબભમયર઱લળ઴વશષસહ઺઻઼ઽાિીુૂૃૄૅ૆ેૈૉ૊ોૌ્૎૏ૐ૑૒૓૔૕૖૗૘૙૚૛૜૝૞૟ૠૡૢૣ૤૥૦૧૨૩૪૫૬૭૮૯૰૱૲૳૴૵૶૷૸ૹૺૻૼ૽૾૿"
    -- devanagari без палочек
    Hindi -> devanagari
    Hungarian -> basicLatin <> "áéíóöőúüű"
    Icelandic -> "aábdðeéfghiíjklmnoóprstuúvxyýþæö"
    Indonesian -> basicLatin
--     Interlingua -> _
    Irish -> "aábcdeéfghiílmnoóprstuú"
    Italian -> "abcdefghilmnopqrstuvzàèéìíîòóùú"
    Kannada -> ['\x0C80'..'\x0CFF'] -- "ಀಁಂಃ಄ಅಆಇಈಉಊಋಌ಍ಎಏಐ಑ಒಓಔಕಖಗಘಙಚಛಜಝಞಟಠಡಢಣತಥದಧನ಩ಪಫಬಭಮಯರಱಲಳ಴ವಶಷಸಹ಺಻಼ಽಾಿೀುೂೃೄ೅ೆೇೈ೉ೊೋೌ್೎೏೐೑೒೓೔ೕೖ೗೘೙೚೛೜ೝೞ೟ೠೡೢೣ೤೥೦೧೨೩೪೫೬೭೮೯೰ೱೲೳ೴೵೶೷೸೹೺೻೼೽೾೿"
--     Kurmanji -> _
--     Latin -> _
--     Latin_Classic -> _
    Latvian -> "aābcčdeēfgģhiījkķlļmnņoprsštuūvzž"
    Lithuanian -> "aąbcčdeęėfghiįyjklmnoprsštuųūvzž"
    Malayalam -> ['\x0D00'..'\x0D7F'] -- "ഀഁംഃഄഅആഇഈഉഊഋഌ഍എഏഐ഑ഒഓഔകഖഗഘങചഛജഝഞടഠഡഢണതഥദധനഩപഫബഭമയരറലളഴവശഷസഹഺ഻഼ഽാിീുൂൃൄ൅െേൈ൉ൊോൌ്ൎ൏൐൑൒൓ൔൕൖൗ൘൙൚൛൜൝൞ൟൠൡൢൣ൤൥൦൧൨൩൪൫൬൭൮൯൰൱൲൳൴൵൶൷൸൹ൺൻർൽൾൿ"
    Marathi -> devanagari
--     Mongolian -> _
    -- Bokmål использует 85-90% населения Норвегии
    Norwegian -> basicLatin <> "æøåéèêóòâôüáà"
--     Norwegian_Bokmal  -> "nb"
--     Norwegian_Nynorsk -> _
--     Oriya -> _
--     Panjabi -> _
--     Piedmontese -> _
    Polish -> "aąbcćdeęfghijklłmnńoóprsśtuwyzźż"
    Portuguese -> basicLatin <> "áâãàçéêíóôõú"
    Romanian -> basicLatin <> "ăâîșț"
--     Romansh -> _
    Russian -> "абвгдеёжзийклмнопрстуфхцчшщъыьэюя"
--     Sanskrit -> _
    Serbian -> "абвгдђежзијклљмнњопрстћуфхцчџш" -- Serbian_Cyrillic
--     Serbian_Cyrillic -> _
--     Serbocroatian_Cyrillic -> _
--     Serbocroatian_Latin -> gaj'sLatin
    Slovak -> "aáäbcčdďdzdžeéfghchiíjklĺľmnňoóôpqrŕsštťuúvwxyýzž"
    Slovenian -> gaj'sLatin <> "áéêèíóôòú" -- это все варианты?
    Spanish -> basicLatin <> "ñáéíóúýü"
    Swedish -> basicLatin <> "åäö" -- diacritics только в заимствованных словах
    Tamil -> ['\x0B80'..'\x0BFF'] -- "஀஁ஂஃ஄அஆஇஈஉஊ஋஌஍எஏஐ஑ஒஓஔக஖஗஘ஙச஛ஜ஝ஞட஠஡஢ணத஥஦஧நனப஫஬஭மயரறலளழவஶஷஸஹ஺஻஼஽ாிீுூ௃௄௅ெேை௉ொோௌ்௎௏ௐ௑௒௓௔௕௖ௗ௘௙௚௛௜௝௞௟௠௡௢௣௤௥௦௧௨௩௪௫௬௭௮௯௰௱௲௳௴௵௶௷௸௹௺௻௼௽௾௿"
    Telugu -> ['\x0C00'..'\x0C7F'] -- "ఀఁంఃఄఅఆఇఈఉఊఋఌ఍ఎఏఐ఑ఒఓఔకఖగఘఙచఛజఝఞటఠడఢణతథదధన఩పఫబభమయరఱలళఴవశషసహ఺఻఼ఽాిీుూృౄ౅ెేై౉ొోౌ్౎౏౐౑౒౓౔ౕౖ౗ౘౙౚ౛౜ౝ౞౟ౠౡౢౣ౤౥౦౧౨౩౪౫౬౭౮౯౰౱౲౳౴౵౶౷౸౹౺౻౼౽౾౿"
    Thai -> ['\x0E00'..'\x0E7F'] -- "฀กขฃคฅฆงจฉชซฌญฎฏฐฑฒณดตถทธนบปผฝพฟภมยรฤลฦวศษสหฬอฮฯะัาำิีึืฺุู฻฼฽฾฿เแโใไๅๆ็่้๊๋์ํ๎๏๐๑๒๓๔๕๖๗๘๙๚๛๜๝๞๟๠๡๢๣๤๥๦๧๨๩๪๫๬๭๮๯๰๱๲๳๴๵๶๷๸๹๺๻๼๽๾๿"
    Turkish -> "abcçdefgğhıİijklmnoöprsştuüvyzâîû"
--     Turkmen -> _
    Ukrainian -> "абвгґдеєжзиіїйклмнопрстуфхцчшщьюя"
--     Uppersorbian -> _
    Welsh -> "abcchdddefffgnghijlllmnopphrrhstthuwyâêîôûŵŷ"

normalizeUnicode x
    | ICU.quickCheck ICU.NFC x == Just True = x
    | otherwise = ICU.normalize ICU.NFC x
-- [l | l <- [minBound..maxBound], Just a <- [alphabet l], let p = T.pack a, normalizeUnicode p /= p]
