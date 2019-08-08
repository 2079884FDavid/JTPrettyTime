module JTPrettyTime.Util
( diffYears
, isAnniversary
) where

import Data.Time.Calendar
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

import JTPrettyTime.Local
import JTPrettyTime.Convert

-- Given timestamp is interpreted as UTC, compared with local timezone date
isAnniversary :: Integer -> IO Bool
isAnniversary t = do
  ld <- getCurrentLocalDay
  return $ isAnniversary' ld $ unixToUTCDay t

isAnniversary' :: Day -> Day -> Bool
isAnniversary' a b = (m1 == m2 && d1 == d2)
  where
    (_, m2, d2) = toGregorian a
    (_, m1, d1) = toGregorian b

getCurrentLocalDay :: IO Day
getCurrentLocalDay = do
  c <- getCurrentUnixTime
  unixToLocalDay c

diffYears :: Integer -> IO Integer
diffYears t = do
  ld <- getCurrentLocalDay
  return $ diffYears' ld $ unixToUTCDay t

diffYears' :: Day -> Day -> Integer
diffYears' a b = abs (y1 - y2)
  where
    (y1, _, _) = toGregorian a
    (y2, _, _) = toGregorian b
