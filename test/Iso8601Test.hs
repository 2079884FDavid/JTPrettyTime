module Iso8601Test
( groupIso8601Parse
, groupIso8601ParseTimezone
) where

import Test.Framework as Framework (Test, testGroup)
import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit

import JTPrettyTime.Iso8601

groupIso8601Parse :: Framework.Test
groupIso8601Parse = 
  testGroup "JTPrettyTime.Iso8601 parse" [
      testCase "Parse extended +00:00" 
          (test_parse_specific "2019-03-10T14:36:49+00:00" 1552228609),
      testCase "Parse extended Military Time Zone (UTC)" 
          (test_parse_specific "2019-03-10T14:36:49Z" 1552228609),
      testCase "Parse basic format" 
          (test_parse_specific "20190310T143649Z" 1552228609),
      testCase "Parse extended -02:00" 
          (test_parse_specific "2019-03-10T12:36:49-02:00" 1552228609),
      testCase "Parse with no T delimiter" 
          (test_parse_specific "2019-03-10 14:36:49+00:00" 1552228609),
      testCase "Parse with no timezone specifier" 
          (test_parse_specific "2019-03-10T14:36:49" 1552228609),
      testCase "Parse floating seconds" 
          (test_parse_specific "2019-03-10T14:36:49.123+00:00" 1552228609),
      testCase "Parse Military Time Zone (O = -02:00)" 
          (test_parse_specific "2019-03-10T12:36:49O" 1552228609),
      testCase "Parse +00:30 minutes" 
          (test_parse_specific "2019-03-10T15:06:49+00:30" 1552228609),
      testCase "Parse with no minutes and seconds" 
          (test_parse_specific "2019-03-10T14Z" 1552226400),
      testCase "Parse date only" 
          (test_parse_specific "2019-03-10" 1552176000)]

groupIso8601ParseTimezone :: Framework.Test
groupIso8601ParseTimezone = 
  testGroup "JTPrettyTime.Iso8601 parseTimezone" [
      testCase "ParseTimezone +00:00" 
          (test_parseTimezone_specific "+00:00" 0),
      testCase "ParseTimezone +01:00" 
          (test_parseTimezone_specific "+01:00" 3600),
      testCase "ParseTimezone Z" 
          (test_parseTimezone_specific "Z" 0),
      testCase "ParseTimezone F" 
          (test_parseTimezone_specific "F" 21600),
      testCase "ParseTimezone +00:30" 
          (test_parseTimezone_specific "+00:30" 1800),
      testCase "ParseTimezone +0030" 
          (test_parseTimezone_specific "+0030" 1800),
      testCase "ParseTimezone +02" 
          (test_parseTimezone_specific "+02" 7200),
      testCase "ParseTimezone -02:00" 
          (test_parseTimezone_specific "-02:00" (-7200)),
      testCase "ParseTimezone garbage" 
          (test_parseTimezone_specific "asdfasdf" 0),
      testCase "ParseTimezone none" 
          (test_parseTimezone_specific "" 0)]

test_parse_specific :: String -> Integer -> Assertion
test_parse_specific s d =
  parse s @?= d

test_parseTimezone_specific :: String -> Integer -> Assertion
test_parseTimezone_specific s d =
  parseTimezone s @?= d
