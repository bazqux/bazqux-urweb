{-# LANGUAGE BangPatterns #-}
module Lib.Regex
    ( regexTest, regexGet, regexReplace, regexReplace', regexReplace''
    , httpRegex, wt, rt, rg, Regex
    , caseInsensitiveRegex, multilineRegex
    ) where

import Text.Regex.Base
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import qualified Data.Text as T
import Data.Array
import Data.IORef
import qualified Data.HashMap.Strict as HM
import System.IO.Unsafe
import Data.String

-- | Матчим http://(any-sub-domain.)?YOUR-REGEXP
httpRegex :: String -> Regex
httpRegex a = fromString $ "^https?://([^/\\.]+\\.)?" ++ a

-- | Матчим http://(www.)?YOUR-REGEXP
wwwRegex :: String -> Regex
wwwRegex a = fromString $ "^https?://(www\\.)?" ++ a

wt = regexTest . wwwRegex
rt = regexTest . httpRegex
rg = regexGet . httpRegex

caseInsensitiveRegex :: T.Text -> Regex
caseInsensitiveRegex =
    makeRegexOpts (defaultCompOpt { caseSensitive = False }) defaultExecOpt

multilineRegex :: T.Text -> Regex
multilineRegex =
    makeRegexOpts (defaultCompOpt { multiline = False }) defaultExecOpt
    -- multiline = True -- как ни странно, означает не игнорировать newline

-- rg' = regexGet . makeRegexOpts
--     (CompOption
--      { caseSensitive = True
--      , multiline = False
--      , rightAssoc = True
--      , newSyntax = True
--      , lastStarGreedy = False })
--     (ExecOption
--      {captureGroups = True })
--       -- False вернет только общий match, отдельных групп не будет
--     .
--     ("^https?://([^/\\.]+\\.)?" ++)

instance IsString Regex where
    -- fromString x = trace ("makeRegex " ++ show x) $ makeRegex $ T.pack x
    -- fromString = makeRegex . T.pack
    fromString = unsafePerformIO . getRegex

-- вот, что нам нужно
-- для httpRegex можно тоже quasiQuoter сделать
-- https://hackage.haskell.org/package/regex-tdfa-quasiquoter-0.2.1.0/docs/Text-Regex-TDFA-QuasiQuoter.html
-- только надо escape последовательности оставить как есть

regexCache :: IORef (HM.HashMap String Regex)
regexCache = unsafePerformIO $ newIORef HM.empty
{-# NOINLINE regexCache #-}

getRegex s = do
    -- putStrLn s
    c <- readIORef regexCache
    case HM.lookup s c of
        Just r -> return r
        Nothing -> do
            let !r = makeRegex s
            atomicModifyIORef' regexCache $ \ c -> (HM.insert s r c, r)


regexTest :: Regex -> T.Text -> Bool
regexTest = matchTest -- (T.encodeUtf8 t)

regexGet :: Regex -> T.Text -> [[T.Text]]
--regexGet r t = map (map bst) $ match r (T.encodeUtf8 t)
regexGet = match

regexReplace regex to = regexReplace' regex (\ _ xs -> to : xs)
regexReplace' :: Regex -> ([T.Text] -> [T.Text] -> [T.Text])
              -> T.Text -> T.Text
regexReplace' regex f t = T.concat $ regexReplace'' id regex f t

regexReplace'' :: (T.Text -> block)
               -> Regex -> ([T.Text] -> [block] -> [block])
               -> T.Text -> [block]
regexReplace'' toBlock regex f t =
    go t 0 $ map elems $ matchAll regex t
    where go t prev [] = [toBlock t]
          go t prev (p@((start,length):ps):xs) =
              let (!front, !back) = T.splitAt (start - prev) t
                  (match, !rest) = T.splitAt length back
              in
                  toBlock front :
                  f (match : [T.take l (T.drop (s-start) back) | (s,l) <- ps])
                    (go rest (start+length) xs)
          go t prev ([]:xs) = go t prev xs
