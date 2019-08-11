-- Functions to fetch the current Unix-Timestamp/ISO8601 String
module JTPrettyTime.Fetch
( getCurrentLocalIso8601
, getCurrentSpecificIso8601
, getCurrentUnixTime
, getCurrentUtcIso8601
) where

import Data.Time.Clock.POSIX
import Data.Time.LocalTime

import JTPrettyTime.Format

getCurrentLocalIso8601 :: IO String
getCurrentLocalIso8601 = do
  t <- getCurrentUnixTime
  unixToLocalIso8601 t

getCurrentSpecificIso8601 :: TimeZone -> IO String
getCurrentSpecificIso8601 tz =
  unixToSpecificIso8601 tz <$> getCurrentUnixTime

getCurrentUnixTime :: IO Integer
getCurrentUnixTime = floor <$> getPOSIXTime

getCurrentUtcIso8601 :: IO String
getCurrentUtcIso8601 = unixToUtcIso8601 <$> getCurrentUnixTime
