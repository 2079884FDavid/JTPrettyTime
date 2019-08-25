-- Contains functions to convert Unix-Timestamps to Haskell Time
-- library objects.
module JTPrettyTime.Convert.Convert
( unixToUTCTime
, unixToSpecificTime
, unixToLocalTime
, unixToUTCDay
, unixToSpecificDay
, unixToLocalDay
) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

-- From https://stackoverflow.com/questions/44905138
unixToUTCTime :: Integer -> UTCTime
unixToUTCTime = posixSecondsToUTCTime . fromIntegral

unixToSpecificTime :: TimeZone -> Integer -> LocalTime
unixToSpecificTime tz = utcToLocalTime tz . unixToUTCTime

unixToLocalTime :: Integer -> IO LocalTime
unixToLocalTime t = do
  tz <- getCurrentTimeZone
  return $ unixToSpecificTime tz t

unixToUTCDay :: Integer -> Day
unixToUTCDay = unixToSpecificDay utc

unixToSpecificDay :: TimeZone -> Integer -> Day
unixToSpecificDay tz = localDay . utcToLocalTime tz . unixToUTCTime

unixToLocalDay :: Integer -> IO Day
unixToLocalDay t = do
  tz <- getCurrentTimeZone
  return $ unixToSpecificDay tz t
