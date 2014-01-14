-- | Время, пригодное к сохранению и к работе из urweb.
module Lib.UrTime where

import Control.Monad
import Control.Monad.Trans
import Data.Binary
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Clock.POSIX
import Data.Time.Format
import System.Locale
import Data.Char
import Lib.ReadUtils

data UrTime
    = UrTime
      { urtSeconds :: {-# UNPACK #-} !Int
      , urtMicroseconds :: {-# UNPACK #-} !Int
      }
    deriving (Eq, Ord)

instance Show UrTime where
    show = show . showUrTime

showUrTime = show . utcToLocalTime utc . urTimeToUTCTime

urTimeUsec (UrTime s u) = s*1000000+u
urTimeMsec t = urTimeUsec t `div` 1000
urTimeNsec (UrTime s u) = (s*1000000+u)*1000

urTimeFileName t =
    formatTime defaultTimeLocale "%Y-%m-%d-%H.%M.%S." (urTimeToUTCTime t)
    ++ us ++ replicate (6 - length us) '0'
    where us = show $ urtMicroseconds t

urTimeFileDir t =
    formatTime defaultTimeLocale "%Y-%m-%d/%H/%M" (urTimeToUTCTime t)

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

diffUrTime a b =
    fromIntegral (urtSeconds a - urtSeconds b)
    + fromIntegral (urtMicroseconds a - urtMicroseconds b) / 1000000

plusUrTime (UrTime a b) delta = UrTime sec usec
    where t = toRational a + toRational b / 1000000 + toRational delta
          (sec, frac) = properFraction t
          usec = truncate $ frac * 1000000

tryParseTime :: [String] -> String -> Maybe UrTime
tryParseTime formats string =
    fmap utcTimeToUrTime $
    foldr mplus Nothing $
    map (\ fmt -> parseTime defaultTimeLocale fmt (trimString string)) formats
    where trimString = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | RSS использует rfc822
-- Только в любом случае разгребаем оба варианта, т.к. встречается разное
readRfc822 = tryParseTime (rfc822formats ++ rfc3399formats) . trimDayOfWeek

trimDayOfWeek x
    | ',' `elem` x = tail $ dropWhile (/= ',') x
    | otherwise    = x

rfc822formats =
    [ "%a, %e " ++ b ++ " " ++ y ++ " %k:%M:%S %Z" ++ z
    | b <- ["%b", "%B"]
    , y <- ["%y", "%Y"]
    , z <- ["", " %Z"]
    ]
    ++
    [ "%a " ++ b ++ " %e %k:%M:%S %Z " ++ y
    | b <- ["%b", "%B"]
    , y <- ["%y", "%Y"]
    ]
    ++
    [ "%e %b %Y %k:%M:%S %Z" -- 20 Nov 2012 05:50:01 +0000
    , "%e %B %Y %k:%M:%S %Z"
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
    , "%Y"
    ]

formatUrTime f = formatTime defaultTimeLocale f . urTimeToUTCTime
formatUrTimeRfc3399 =
    formatUrTime "%Y-%m-%dT%H:%M:%SZ"
formatUrTimeRfc822 =
    formatUrTime "%a, %e %b %Y %k:%M:%S GMT"
tryReadUsecUrTime t = do
    ts <- tryReadUnsignedInt t
    let (s,us) = ts `quotRem` 1000000
    return $ UrTime s us
