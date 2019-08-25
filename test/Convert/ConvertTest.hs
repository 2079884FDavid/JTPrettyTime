module Convert.ConvertTest
( groupConvertConvert
) where

--import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework as Framework (Test, testGroup)

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

import JTPrettyTime.Convert.Convert
import TestFunction

groupConvertConvert :: Framework.Test
groupConvertConvert = 
  testGroup "JTPrettyTime.Convert.Convert" [
    testFunction
      "unixToUTCTime 31622705 should be 1971-01-02T00:05:05+00:00"
      31622705
      utc19710102
      unixToUTCTime,
    testFunction
      "unixToSpecificTime -06:30 31646105 \
      \should be 1971-01-02T00:05:05-06:30"
      31646105
      local19710102
      $ unixToSpecificTime (read "-06:30"),
    testFunction
      "unixToUTCDay 31622705 should be 1971-01-02"
      31622705
      day19710102
      unixToUTCDay,
    testFunction
      "unixToSpecificDay -06:30 31646105 should be 1971-01-02"
      31646105
      day19710102
      $ unixToSpecificDay (read "-06:30")]
  
day19710102 :: Day
day19710102 = fromGregorian 1971 01 02

seconds305 :: DiffTime
seconds305 = secondsToDiffTime 305  -- 00:05:05

utc19710102 :: UTCTime
utc19710102 = UTCTime day19710102 seconds305

local19710102 :: LocalTime
local19710102 = LocalTime day19710102 $ timeToTimeOfDay seconds305
