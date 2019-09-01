module UtilTest
( groupUtil
) where

--import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework as Framework (Test, testGroup)

import JTPrettyTime.Util
import TestFunction

groupUtil :: Framework.Test
groupUtil = 
  testGroup "JTPrettyTime.Util" [
    testFunction
      "1998 is 24 years after 1974"
      132237169 -- 1974-03-11T12:32:49+00:00
      24
      -- 1998-01-01T00:00:00+00:00
      $ unixDiffYears (read "+00:00") 883612800,
    testFunction
      "2016 is 36 years after 1980"
      327715200 -- 1980-05-21T00:00:00+00:00
      36
      -- 2016-12-01T00:00:00+00:00
      $ unixDiffYears (read "+00:00") 1480550400,
    testFunction
      "2016-01-01 is 35 days before 2016-02-05"
      1451606400 -- 2016-01-01T00:00:00+00:00
      35
      -- 2016-02-05T00:00:00+00:00
      $ unixDiffDays (read "+00:00") 1454630400,
    testFunction
      "2018-01-13 is 365 days after 2017-01-13"
      1515801600  -- 2018-01-13T00:00:00+00:00
      (-365)
      -- 2017-01-13T00:00:00+00:00
      $ unixDiffDays (read "+00:00") 1484265600,
    testFunction
      "2019-04-10T23:45:00-06:00 is an anniversary for 1988-04-10"
      576633600  -- 1988-04-10T00:00:00+00:00
      True
      -- 2019-04-10T23:45:00-06:00
      $ isAnniversary (read "-06:00") 1554961500,
    testFunction
      "2019-01-01 is not an anniversary for 1992-04-10"
      702864000  -- 1992-04-10T00:00:00+00:00
      False
      -- 2019-01-01T00:00:00+00:00
      $ isAnniversary (read "+00:00") 1546300800]
