module JTPrettyTime.Util
( diffYears
, isAnniversary
, Iso8601
, parseIso8601
) where

import Data.Time.Calendar
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

type Iso8601 = String

-- Converts for UTC timezone
-- From https://stackoverflow.com/questions/44905138
unixToUTCDay :: Integer -> Day
unixToUTCDay = utctDay . posixSecondsToUTCTime . fromIntegral

unixToSpecificDay :: Integer -> TimeZone -> Day
unixToSpecificDay t tz = localDay l -- TODO some nicer composition like unixToUTCDay possible
  where
    utc = posixSecondsToUTCTime $ fromIntegral t
    l = utcToLocalTime tz utc

unixToLocalDay :: Integer -> IO Day
unixToLocalDay t = unixToSpecificDay t <$> getCurrentTimeZone

-- The timezone is the offset from UTC in minutes
-- For example: CET=60, PST=-480
unixToDay :: Integer -> Integer -> Day
unixToDay t tz = unixToUTCDay t'
  where
    t' = t + tz * 60

parseIso8601 :: ParseTime t => Iso8601 -> t
parseIso8601 = parseTimeOrError True defaultTimeLocale spec
  where
    spec = "%Y-%-m-%-d"

isAnniversary :: Day -> Day -> Bool
isAnniversary a b = m1 == m2 && d1 == d2
  where
    (_, m1, d1) = toGregorian a
    (_, m2, d2) = toGregorian b

diffYears :: Day -> Day -> Integer
diffYears a b = abs (y1 - y2)
  where
    (y1, _, _) = toGregorian a
    (y2, _, _) = toGregorian b
