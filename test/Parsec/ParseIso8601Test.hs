module Parsec.ParseIso8601Test
( groupIso8601Parse
) where

import Test.Framework as Framework (Test, testGroup)
import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit

import JTPrettyTime.Parsec.ParseIso8601

-- Create timestamps
-- date --utc -d "2019-03-10T12:36:49-02:00" +%s

-- Reverse
-- date --utc -d @1552228609

groupIso8601Parse :: Framework.Test
groupIso8601Parse = 
  testGroup "JTPrettyTime.Parsec.ParseIso8601 parse" [
      test_parse_specific
        "Parse extended +00:00"
        ("2019-03-10T14:36:49+00:00", 1552228609),
      test_parse_specific
        "Parse extended Military Time Zone (UTC)"
        ("2019-03-10T14:36:49Z", 1552228609),
      test_parse_specific
        "Parse basic format" 
        ("20190310T143649Z", 1552228609),
      test_parse_specific
        "Parse extended -02:00" 
        ("2019-03-10T12:36:49-02:00", 1552228609),
      test_parse_specific
        "Parse with no timezone specifier" 
        ("2019-03-10T14:36:49", 1552228609),
      test_parse_specific
        "Parse floating seconds" 
        ("2019-03-10T14:36:49.123+00:00", 1552228609),
      test_parse_specific
        "Parse +00:30 minutes" 
        ("2019-03-10T15:06:49+00:30", 1552228609),
      test_parse_specific
        "Parse +0030 minutes" 
        ("2019-03-10T15:06:49+0030", 1552228609),
      test_parse_specific
        "Parse with no minutes and seconds" 
        ("2019-03-10T14Z", 1552226400),
      test_parse_specific
        "Parse date only basic format" 
        ("2019-03-10", 1552176000),
      test_parse_specific
        "Parse year only"
        ("2019", 1546300800),
      test_parse_specific
        "Parse year with trailing garbage"
        ("2019garbage", 1546300800),
      test_parse_specific
        "Parse year and month extended format"
        ("1990-03", 636249600),
      test_parse_specific
        "Parse full ISO8601 with trailing garbage"
        ("2019-03-10T14:36:49ZOnce upon a time", 1552228609),
      test_parse_specific
        "Parse date basic format with trailing garbage"
        ("19900301Trailing", 636249600),
      test_parse_specific
        "Parse date with illegal month format results in year only"
        ("199003", 631152000),
      test_parse_failure
        "Fail parse three digit year"
        "587",
      test_parse_failure
        "Fail parse common date form"
        "04/02/2006"]

test_parse_specific :: String -> (String, Int) -> Framework.Test
test_parse_specific desc (input, expect) = testCase desc t
  where
    t = case parseIso8601 input of 
          Left v -> assertFailure $ "Cannot be parsed: "++(show v)
          Right v -> v @?= expect

test_parse_failure :: String -> String -> Framework.Test
test_parse_failure desc input = testCase desc t
  where
    t = case parseIso8601 input of
          Left _ -> return ()
          Right v -> assertFailure . f $ show v
    f v = "Expected failure; was able to parse: "++input++" -> "++v
