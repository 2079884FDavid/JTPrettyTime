-- Collection of utility functions which might be handy.
module JTPrettyTime.Util
( unixDiffYears
, unixDiffLocalYears
, unixDiffDays
, unixDiffLocalDays
, isAnniversary
, isLocalAnniversary
) where

import Data.Time.Calendar
import Data.Time.LocalTime

import JTPrettyTime.Convert.Convert
import JTPrettyTime.Fetch

-- The reference timestamp is interpreted as UTC,
-- compared to a date from a specific timezone
-- (in most cases the local timezone).
unixDiffYears :: TimeZone -> Integer -> Integer -> Integer
unixDiffYears = doubleDayFunction unixDiffYears'

unixDiffLocalYears :: Integer -> IO Integer
unixDiffLocalYears = localiseFunction unixDiffYears

unixDiffYears' :: Day -> Day -> Integer
unixDiffYears' a b = y1 - y2
  where
    (y1, _, _) = toGregorian a
    (y2, _, _) = toGregorian b

unixDiffDays :: TimeZone -> Integer -> Integer -> Integer
unixDiffDays = doubleDayFunction diffDays

unixDiffLocalDays :: Integer -> IO Integer
unixDiffLocalDays = localiseFunction unixDiffDays

isAnniversary :: TimeZone -> Integer -> Integer -> Bool
isAnniversary = doubleDayFunction isAnniversary'

isLocalAnniversary :: Integer -> IO Bool
isLocalAnniversary = localiseFunction isAnniversary

isAnniversary' :: Day -> Day -> Bool
isAnniversary' a b = m1 == m2 && d1 == d2
  where
    (_, m2, d2) = toGregorian a
    (_, m1, d1) = toGregorian b

localiseFunction :: (TimeZone -> Integer -> Integer -> a)
                 -> Integer -> IO a
localiseFunction f t = do
  tz <- getCurrentTimeZone
  l <- getCurrentUnixTime
  return $ f tz l t 

doubleDayFunction :: (Day -> Day -> a)
                  -> TimeZone
                  -> Integer
                  -> Integer
                  -> a
doubleDayFunction f tz l t = f ld d
  where
    ld = unixToSpecificDay tz l
    d = unixToUTCDay t
