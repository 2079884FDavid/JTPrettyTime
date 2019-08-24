module FormatTest
( groupFormat
) where

--import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework as Framework (Test, testGroup)

import JTPrettyTime.Format
import TestFunction

groupFormat :: Framework.Test
groupFormat = 
  testGroup "JTPrettyTime.Format" [
    testFunction
      "Check epoch unix-timestamp"
      0
      "1970-01-01T00:00:00+00:00"
      unixToUtcIso8601,
    testFunction
      "Check arbitrary unix-timestamp"
      577119402
      "1988-04-15T14:56:42+00:00"
      unixToUtcIso8601,
    testFunction
      "Check epoch unix-timestamp specific"
      0
      "1970-01-01T01:00:00+01:00"
      $ unixToSpecificIso8601 (read "+01:00"),
    testFunction
      "Check arbitrary specifc #1"
      942176413
      "1999-11-09T15:10:13-04:30"
      $ unixToSpecificIso8601 (read "-04:30"),
    testFunction
      "Check arbitrary specifc #2"
      1179275690
      "2007-05-16T13:34:50+13:00"
      $ unixToSpecificIso8601 (read "+13:00")]
