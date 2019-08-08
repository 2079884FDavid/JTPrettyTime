module JTPrettyTime.Convert
( unixToUTCTime
, unixToUTCDay
, unixToSpecificDay
, unixToLocalDay
) where

import Data.Time.Calendar
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

-- From https://stackoverflow.com/questions/44905138
unixToUTCTime :: Integer -> UTCTime
unixToUTCTime = posixSecondsToUTCTime . fromIntegral

unixToUTCDay :: Integer -> Day
unixToUTCDay = unixToSpecificDay utc

unixToSpecificDay :: TimeZone -> Integer -> Day
unixToSpecificDay tz = localDay . (utcToLocalTime tz) . unixToUTCTime

unixToLocalDay :: Integer -> IO Day
unixToLocalDay t = do
  tz <- getCurrentTimeZone
  return $ unixToSpecificDay tz t
