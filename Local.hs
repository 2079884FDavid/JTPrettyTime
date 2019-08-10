module JTPrettyTime.Local
( getCurrentLocalIso8601
, getCurrentUnixTime
) where

import Data.Time.Clock.POSIX

import JTPrettyTime.Format

getCurrentLocalIso8601 :: IO String
getCurrentLocalIso8601 = do
  t <- getCurrentUnixTime
  unixToLocalIso8601 t

getCurrentUnixTime :: IO Integer
getCurrentUnixTime = floor <$> getPOSIXTime
