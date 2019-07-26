module Parsec.ParseIso8601Test
( groupIso8601Parse
) where

--import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework as Framework (Test, testGroup)

import JTPrettyTime.Parsec.ParseIso8601
import Parsec.TestParserUtils

-- Create timestamps
-- date --utc -d "2019-03-10T12:36:49-02:00" +%s

-- Reverse
-- date --utc -d @1552228609

groupIso8601Parse :: Framework.Test
groupIso8601Parse = 
  testGroup "JTPrettyTime.Parsec.ParseIso8601 parse" [
      testParseIso8601Success
        "Parse extended +00:00"
        ("2019-03-10T14:36:49+00:00", 1552228609),
      testParseIso8601Success
        "Parse extended Military Time Zone (UTC)"
        ("2019-03-10T14:36:49Z", 1552228609),
      testParseIso8601Success
        "Parse basic format" 
        ("20190310T143649Z", 1552228609),
      testParseIso8601Success
        "Parse extended -02:00" 
        ("2019-03-10T12:36:49-02:00", 1552228609),
      testParseIso8601Success
        "Parse with no timezone specifier" 
        ("2019-03-10T14:36:49", 1552228609),
      testParseIso8601Success
        "Parse floating seconds" 
        ("2019-03-10T14:36:49.123+00:00", 1552228609),
      testParseIso8601Success
        "Parse +00:30 minutes" 
        ("2019-03-10T15:06:49+00:30", 1552228609),
      testParseIso8601Success
        "Parse +0030 minutes" 
        ("2019-03-10T15:06:49+0030", 1552228609),
      testParseIso8601Success
        "Parse with no minutes and seconds" 
        ("2019-03-10T14Z", 1552226400),
      testParseIso8601Success
        "Parse date only basic format" 
        ("2019-03-10", 1552176000),
      testParseIso8601Success
        "Parse year only"
        ("2019", 1546300800),
      testParseIso8601Success
        "Parse year with trailing garbage"
        ("2019garbage", 1546300800),
      testParseIso8601Success
        "Parse year and month extended format"
        ("1990-03", 636249600),
      testParseIso8601Success
        "Parse full ISO8601 with trailing garbage"
        ("2019-03-10T14:36:49ZOnce upon a time", 1552228609),
      testParseIso8601Success
        "Parse date basic format with trailing garbage"
        ("19900301Trailing", 636249600),
      testParseIso8601Success
        "Parse date with illegal month format results in year only"
        ("199003", 631152000),
      testParseIso8601Failure
        "Fail parse three digit year"
        "587",
      testParseIso8601Failure
        "Fail parse common date form"
        "04/02/2006"]
  where
    testParseIso8601Success = testParserSuccess iso8601Parser
    testParseIso8601Failure = testParserFailure iso8601Parser
