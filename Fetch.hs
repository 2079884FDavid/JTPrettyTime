-- Functions to fetch the current Unix-Timestamp/ISO8601 String
module JTPrettyTime.Fetch
( getCurrentUtcIso8601
, getCurrentSpecificIso8601
, getCurrentLocalIso8601
, getCurrentUnixTime
) where

import Data.Time.Clock.POSIX
import Data.Time.LocalTime

import JTPrettyTime.Format

getCurrentUtcIso8601 :: IO String
getCurrentUtcIso8601 = unixToUtcIso8601 <$> getCurrentUnixTime

getCurrentSpecificIso8601 :: TimeZone -> IO String
getCurrentSpecificIso8601 tz =
  unixToSpecificIso8601 tz <$> getCurrentUnixTime

getCurrentLocalIso8601 :: IO String
getCurrentLocalIso8601 = do
  t <- getCurrentUnixTime
  unixToLocalIso8601 t

getCurrentUnixTime :: IO Integer
getCurrentUnixTime = floor <$> getPOSIXTime
