module JTPrettyTime.Local
( getCurTimeString
, getCurDay
, getTime
, getCurrentUnixTime
) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.LocalTime

getCurTimeString :: IO String -- Eg.: "2018-10-23 12:04:03 [CET]"
getCurTimeString = do 
  time <- formatTime defaultTimeLocale specifiers <$> getTime
  zone <- show <$> getCurrentTimeZone
  return (time++" ["++zone++"]")
  where
    specifiers = "%Y-%m-%d %H:%M:%S"

getCurDay :: IO Day
getCurDay = do
  time <- getTime
  return (localDay time)

getTime :: IO LocalTime
getTime = do
  utc <- getCurrentTime
  zone <- getCurrentTimeZone
  return (utcToLocalTime zone utc)

getCurrentUnixTime :: IO Integer
getCurrentUnixTime = do
  t <- getPOSIXTime
  return $ floor t
