-- Functions to convert Unix-Timestamps to ISO8601 strings
module JTPrettyTime.Format
( unixToLocalIso8601
, unixToSpecificIso8601
, unixToUtcIso8601
) where

import Data.Time.Format
import Data.Time.LocalTime

import JTPrettyTime.Convert.Convert

-- Eg.: "2018-10-23T12:04:03+01:00"
unixToLocalIso8601 :: Integer -> IO String
unixToLocalIso8601 t = do
  tz <- getCurrentTimeZone
  return $ unixToSpecificIso8601 tz t

unixToSpecificIso8601 :: TimeZone -> Integer -> String
unixToSpecificIso8601 tz t = tIso8601 ++ tzIso8601
  where
    local = unixToSpecificTime tz t
    tIso8601 = format "%Y-%m-%dT%H:%M:%S" local
    tzIso8601 = format "%Ez" tz

unixToUtcIso8601 :: Integer -> String
unixToUtcIso8601 = unixToSpecificIso8601 utc

format :: FormatTime t => String -> t -> String
format = formatTime defaultTimeLocale
