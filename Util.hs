-- Collection of utility functions which might be handy.
module JTPrettyTime.Util
( unixDiffYears
, unixDiffDays
, isAnniversary
) where

import Data.Time.Calendar

import JTPrettyTime.Convert.Convert
import JTPrettyTime.Convert.Fetch

-- Given timestamp is interpreted as UTC,
-- compared with local timezone date
unixDiffYears :: Integer -> IO Integer
unixDiffYears = doubleDayFunction unixDiffYears'

unixDiffYears' :: Day -> Day -> Integer
unixDiffYears' a b = abs (y1 - y2)
  where
    (y1, _, _) = toGregorian a
    (y2, _, _) = toGregorian b

unixDiffDays :: Integer -> IO Integer
unixDiffDays = doubleDayFunction diffDays

isAnniversary :: Integer -> IO Bool
isAnniversary = doubleDayFunction isAnniversary'

isAnniversary' :: Day -> Day -> Bool
isAnniversary' a b = m1 == m2 && d1 == d2
  where
    (_, m2, d2) = toGregorian a
    (_, m1, d1) = toGregorian b

doubleDayFunction :: (Day -> Day -> a) -> Integer -> IO a
doubleDayFunction f t = do
  ld <- getCurrentLocalDay
  return $ f ld $ unixToUTCDay t
