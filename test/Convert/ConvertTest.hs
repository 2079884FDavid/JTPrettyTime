module Convert.ConvertTest
( groupConvertConvert
) where

--import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework as Framework (Test, testGroup)

import Data.Time.Clock
import Data.Time.Calendar

import JTPrettyTime.Convert.Convert
import TestFunction

groupConvertConvert :: Framework.Test
groupConvertConvert = 
  testGroup "JTPrettyTime.Convert.Convert" [
    testFunction
      "Test unixToUTCTime"
      31622705
      utc19710102
      unixToUTCTime]
  
day19710102 :: Day
day19710102 = fromGregorian 1971 01 02

seconds0 :: DiffTime
seconds0 = secondsToDiffTime 305  -- 00:05:05

utc19710102 :: UTCTime
utc19710102 = UTCTime day19710102 seconds0
