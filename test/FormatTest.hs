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
      "Epoch unix-timestamp to +00:00"
      0
      "1970-01-01T00:00:00+00:00"
      unixToUtcIso8601,
    testFunction
      "Arbitrary unix-timestamp to +00:00"
      577119402
      "1988-04-15T14:56:42+00:00"
      unixToUtcIso8601,
    testFunction
      "Epoch unix-timestamp to +01:00"
      0
      "1970-01-01T01:00:00+01:00"
      $ unixToSpecificIso8601 (read "+01:00"),
    testFunction
      "Arbitrary unix-timestamp to -04:30"
      942176413
      "1999-11-09T15:10:13-04:30"
      $ unixToSpecificIso8601 (read "-04:30"),
    testFunction
      "Arbitrary unix-timestamp to +13:00"
      1179275690
      "2007-05-16T13:34:50+13:00"
      $ unixToSpecificIso8601 (read "+13:00")]
