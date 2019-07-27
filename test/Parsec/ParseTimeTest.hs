module Parsec.ParseTimeTest
( groupTimeParse
) where

import Test.Framework as Framework (Test, testGroup)

import JTPrettyTime.Parsec.ParseTime
import Parsec.TestParserUtils

groupTimeParse :: Framework.Test
groupTimeParse = 
  testGroup "JTPrettyTime.Parsec.ParseTime parse" [
      testParserSuccess
        parseCombinedDelimiter
        "Time delimiter should not add time" 
        ("T", 0),
      testParserFailure
        parseCombinedDelimiter
        "Time delimiter must be 'T'"
        "A",
      testParserSuccess
        parseTime
        "Parse time '04:03:02.000'"
        ("04:03:02.000", 14582),
      testParserSuccess
        parseTime
        "Parse time '040302.000'"
        ("040302.000", 14582),
      testParserSuccess
        parseTime
        "Parse time '04:03:04.320'"
        ("04:03:04.320", 14584),
      testParserSuccess
        parseTime
        "Parse time '04:03:02'"
        ("04:03:02.000", 14582),
      testParserSuccess
        parseTime
        "Parse time '04:03'"
        ("04:03", 14580),
      testParserFailure
        parseTime
        "Parse time without 0-padding should fail"
        "4:03"]
