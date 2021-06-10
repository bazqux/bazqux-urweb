{-# LANGUAGE BangPatterns, ViewPatterns #-}
-- | Время, пригодное к сохранению и к работе из urweb.
module Lib.UrTime
    ( UrTime(..), urTimeMsec, urTimeUsec, urTimeNsec, showUrTime
    , urTimeFromMsec
    , getUrTime, plusUrTime, diffUrTime, urTimeAddYear
    , tryParseTime, readRfc3339, readRfc822, readIso8601Duration
    , readDuration
    , readYmdHMS, parseHttpTime
    , tryReadUsecUrTime
    , formatUrTime, formatUrTimeRfc822, formatUrTimeRfc3399, formatUrTimeRussian
    , urTimeFileName
    , hour, minute, day, week, month, year
    )
    where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Control.DeepSeq
import Data.Hashable
import Data.Binary
import Data.Ratio
import Data.Time
import Data.Time.Clock.POSIX
import Data.Char
import Data.Ord
import Data.Function
import Lib.ReadUtils
import qualified Data.Text as T

data UrTime
    = UrTime
      { urtSeconds :: {-# UNPACK #-} !Int
      , urtMicroseconds :: {-# UNPACK #-} !Int
      }
    deriving Bounded

instance Eq UrTime where
    (==) = (==) `on` urTimeUsec

instance Ord UrTime where
    compare = comparing urTimeUsec

instance Show UrTime where
    show = show . showUrTime

instance Hashable UrTime where
    hashWithSalt s (UrTime sec msec) =
        s `hashWithSalt` sec `hashWithSalt` msec

showUrTime = show . utcToLocalTime utc . urTimeToUTCTime

instance NFData UrTime where
    rnf t = t `seq` ()

-- integer, чтобы не было overflow
urTimeNsec (UrTime s u) = toInteger s * 1000000000 + toInteger u * 1000
urTimeUsec (UrTime s u) = toInteger s * 1000000    + toInteger u
urTimeMsec (UrTime s u) = toInteger s * 1000       + toInteger u `div` 1000

urTimeFromMsec ms = UrTime s us
    where (s,us) = ms `quotRem` 1000

urTimeFileName t =
    formatUrTime "%Y-%m-%d-%H.%M.%S." t ++ us ++ replicate (6 - length us) '0'
    where us = show $ urtMicroseconds t

urTimeFileDir = formatUrTime "%Y-%m-%d/%H/%M"

instance Binary UrTime where
    put (UrTime s us) = put s >> put us
    get = liftM2 UrTime get get

urTimeToUTCTime :: UrTime -> UTCTime
urTimeToUTCTime (UrTime s us) =
    posixSecondsToUTCTime $ fromIntegral s + fromIntegral us / 1000000

utcTimeToUrTime :: UTCTime -> UrTime
utcTimeToUrTime = posixTimeToUrTime . utcTimeToPOSIXSeconds

posixTimeToUrTime :: POSIXTime -> UrTime
posixTimeToUrTime x = UrTime sec (truncate $ frac * 1000000)
    where (sec, frac) = properFraction x

localTimeToUrTime :: LocalTime -> UrTime
localTimeToUrTime = utcTimeToUrTime . localTimeToUTC utc

zonedTimeToUrTime :: ZonedTime -> UrTime
zonedTimeToUrTime = utcTimeToUrTime . zonedTimeToUTC

getUrTime :: MonadIO m => m UrTime
getUrTime = posixTimeToUrTime `liftM` liftIO getPOSIXTime

diffUrTime :: UrTime -> UrTime -> Double
diffUrTime a b =
    fromIntegral (urTimeUsec a - urTimeUsec b) / 1000000

plusUrTime (UrTime a b) delta = UrTime sec usec
    where t = toRational a + toRational b / 1000000 + toRational delta
          (sec, frac) = properFraction t
          usec = truncate $ frac * 1000000

-- | С учетом високосного года.
-- 29 февраля переходит в 1 марта следующего года.
urTimeAddYear = utcTimeToUrTime . add . urTimeToUTCTime
    where add (UTCTime d dt) = UTCTime (addGregorianYearsRollOver 1 d) dt

parseHttpTime :: String -> Maybe UrTime
parseHttpTime =
    tryParseTime
    ["%e %b %Y %k:%M:%S %Z" -- 06 Nov 1994 08:49:37 GMT
    ,"%e-%b-%y %k:%M:%S %Z" -- 06-Nov-94 08:49:37 GMT
    ,"%a %b %e %k:%M:%S %Y" -- Sun Nov  6 08:49:37 1994
    ] . trimDayOfWeek

tryParseTime :: [String] -> String -> Maybe UrTime
tryParseTime formats string =
    fmap utcTimeToUrTime $
    foldr (<|>) Nothing $
    map (\ fmt -> parseTimeM True defaultTimeLocale fmt (fixm string)) formats
    where fixm (y1:y2:y3:y4:'/':m:'/':xs)
              | m >= '1' && m <= '9'= y1:y2:y3:y4:'/':'0':m:'/':xs
              -- "2015/1/20 16:29:00" -- %m требует ноль
          fixm x = x

-- | RSS использует rfc822
-- Только в любом случае разгребаем оба варианта, т.к. встречается разное
readRfc822 = tryParseTime (rfc822formats ++ rfc3399formats) . trimDayOfWeek

trimDayOfWeek x
    | (day, ',':rest) <- break (== ',') x
    , all (not . isDigit) day = fix rest
    | otherwise = fix x
    where fix = map (\ c -> if c == ',' then ' ' else c)

rfc822formats =
    [ "%e " ++ b ++ " " ++ y ++ " %k:%M:%S" ++ z
      -- "20 Nov 2012 05:50:01 +0000"
      -- "%a, " в начале нет, т.к. trimDayOfWeek и так обрезает день
    | b <- ["%b", "%B"]
    , y <- ["%y", "%Y"]
    , z <- [" %Z", "%Z", " %Z %Z"]
    ]
    ++
    [ "%a " ++ b ++ " %e %k:%M:%S %Z " ++ y
      -- "Mon Nov 20 05:50:01 +0000 2015" ???
    | b <- ["%b", "%B"]
    , y <- ["%y", "%Y"]
    ]
-- EEST не разгребает

-- | Atom использует Rfc 3339 (совместим с ISO 8601)
readRfc3339 = tryParseTime (rfc3399formats ++ rfc822formats) . trimDayOfWeek

rfc3399formats =
    [ "%Y-%m-%eT%k:%M:%S%Q%Z"   -- 2008-07-03T04:26:58.117+04:00
    , "%Y-%m-%eT%k:%M:%S%Q%Z%Z" -- 2010-03-08T13:54:36.750562-07:00-0700 ?
    , "%Y-%m-%eT%k:%M:%S%QZ"    -- 2010-03-09T23:43:19.821Z
      -- и кусок из ISO 8601
    , "%Y-%m-%eT%k:%M:%S%Z"     -- 2011-08-16T00:59:57-05:00
    , "%Y-%m-%eT%k:%M%Z"
    , "%Y-%m-%eT%k:%MZ"
    , "%Y-%m-%eT%k:%M"
    , "%Y-%m-%e"
    , "%Y-%m"
    , "%Y/%m/%e %k:%M:%S"
    , "%Y/%m/%e"
    , "%Y/%m"
    , "%Y"
    ]

readDuration ds =
    case map tryReadUnsignedInt $ T.split (== ':') ds of
        [Just s] -> Just s
        [Just m, Just s] | s < 60 -> Just $ m*60+s
        [Just h, Just m, Just s] | m < 60 && s < 60 -> Just $ h*3600+m*60+s
        _ -> Nothing

-- https://en.wikipedia.org/wiki/ISO_8601#Durations
--    PnYnMnDTnHnMnS
--    PnW
--    P<date>T<time>
readIso8601Duration = go False 0
    where go _ acc [] = Just $ truncate acc
          go _ acc ('P':xs) = go False acc xs
          go _ acc ('T':xs) = go True acc xs
          go t acc xs
              | Just (d, c:xs) <- digit xs
              , m <- mult t c
              , m /= 0
              = go t (acc + d*fromIntegral m) xs
              | otherwise = Nothing
          mult _ 'S' = 1
          mult True 'M' = minute
          mult False 'M' = month
          mult _ 'H' = hour
          mult _ 'D' = day
          mult _ 'Y' = year
          mult _ 'W' = week
          mult _ _ = 0
          digit xs = i 0 xs
          d x | x >= '0' && x <= '9' = Just (ord x - ord '0')
              | otherwise = Nothing
          i !acc (x:xs)
              | Just d <- d x = i (acc*10+d) xs
              | x `elem` (",." :: [Char]) = f acc 1 xs
          i acc xs = Just (acc%1, xs)
          f !acc !divisor (x:xs)
              | Just d <- d x = f (acc*10+d) (divisor*10) xs
          f !acc !d xs = Just (acc%d, xs)

    -- tryParseTime iso8601durationFormats

formatUrTime f = formatTime defaultTimeLocale f . urTimeToUTCTime
formatUrTimeRussian f = formatTime russianTimeLocale f . urTimeToUTCTime
formatUrTimeRfc3399 =
    formatUrTime "%Y-%m-%dT%H:%M:%SZ"
formatUrTimeRfc822 =
    formatUrTime "%a, %e %b %Y %k:%M:%S GMT"
tryReadUsecUrTime t = do
    ts <- tryReadUnsignedInt t
    let (s,us) = ts `quotRem` 1000000
    return $ UrTime s us

-- "%Y-%m-%d %H:%M:%S"
readYmdHMS t =
    utcTimeToUrTime $
    UTCTime
    { utctDay = fromGregorian (toEnum $ i 0 4) (i 5 2) (i 8 2)
    , utctDayTime =
        secondsToDiffTime $
        toEnum $ (i 11 2 * 60 + i 14 2) * 60 + i 17 2
    }
    where i s l = readUnsignedInt $ T.take l $ T.drop s t

minute, hour, halfAnHour, day, week, month, year :: Num a => a
minute = 60
hour = 3600
halfAnHour = 30*minute
day = 24 * hour
week = 7*day
month = 30*day
year = 365*day

russianTimeLocale :: TimeLocale
russianTimeLocale =
    TimeLocale
    { wDays =
        [("воскресенье" , "вс")
        ,("понедельник" , "пн")
        ,("вторник"     , "вт")
        ,("среда"       , "ср")
        ,("четверг"     , "чт")
        ,("пятница"     , "пт")
        ,("суббота"     , "сб")]
    , months =
        [("января"      , "янв")
        ,("февраля"     , "фев")
        ,("марта"       , "мар")
        ,("апреля"      , "апр")
        ,("мая"         , "май")
        ,("июня"        , "июн")
        ,("июля"        , "июл")
        ,("августа"     , "авг")
        ,("сентября"    , "сен")
        ,("октября"     , "окт")
        ,("ноября"      , "ноя")
        ,("декабря"     , "дек")]

    , amPm = ("AM", "PM")
    , dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y"
    , dateFmt = "%Y-%m-%d"
    , timeFmt = "%H:%M:%S"
    , time12Fmt = "%I:%M:%S %p"
    , knownTimeZones =
        [TimeZone 0 False "UT"
        ,TimeZone 0 False "GMT"
        ,TimeZone (3 * 60) False "MSK"
        ]
    }
