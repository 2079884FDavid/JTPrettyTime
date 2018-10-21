module JTPrettyTime.Util
( diffYears
, isAnniversary
, Iso8601
, parseIso8601
) where

import Data.Time.Calendar
import Data.Time.Format

type Iso8601 = String

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
