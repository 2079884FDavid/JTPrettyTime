-- Functions to fetch the current time/date for Haskell Time library
-- objects.
module JTPrettyTime.Convert.Fetch
( getCurrentLocalDay
, getCurrentLocalTime
) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

getCurrentLocalDay :: IO Day
getCurrentLocalDay = localDay <$> getCurrentLocalTime

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = do
  t <- getCurrentTime
  zone <- getCurrentTimeZone
  return (utcToLocalTime zone t)
