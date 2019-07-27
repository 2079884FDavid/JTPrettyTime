module Parsec.ParseTimezoneTest
( groupTimezoneParse
) where

import Test.Framework as Framework (Test, testGroup)

import JTPrettyTime.Parsec.ParseTimezone
import Parsec.TestParserUtils

groupTimezoneParse :: Framework.Test
groupTimezoneParse = 
  testGroup "JTPrettyTimezone.Parsec.ParseTimezone parse" [
      testParserSuccess
        parseTZ
        "Zulu timezone should be zero" 
        ("Z", 0),
      testParserSuccess
        parseTZ
        "Military timezone 'G' should be ignored" 
        ("G", 0),
      testParserSuccess
        parseTZ
        "Military timezone 'R' should be ignored" 
        ("R", 0),
      testParserSuccess
        parseTZ
        "Parse Timezone +07"
        ("+07", -25200),
      testParserSuccess
        parseTZ
        "Parse Timezone +08:15"
        ("+08:15", -29700),
      testParserSuccess
        parseTZ
        "Parse Timezone +0815"
        ("+0815", -29700),
      testParserSuccess
        parseTZ
        "Parse Timezone -02"
        ("-02", 7200),
      testParserSuccess
        parseTZ
        "Parse Timezone -0240"
        ("-0240", 9600)]
