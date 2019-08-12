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
      unixToUtcIso8601]
