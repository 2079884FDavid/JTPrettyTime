module Parsec.ParseDateTest
( groupDateParse
) where

import Test.Framework as Framework (Test, testGroup)

import JTPrettyTime.Parsec.ParseDate
import Parsec.TestParserUtils

groupDateParse :: Framework.Test
groupDateParse = 
  testGroup "JTPrettyTime.Parsec.ParseDate parse" [
      testParserSuccess
        parseDateTriple
        "Parse date triple extended format" 
        ("2019-03-10", 1552176000),
      testParserSuccess
        parseDateTriple
        "Parse date triple basic format" 
        ("20190310", 1552176000),
      testParserFailure
        parseDateTriple
        "Parse date triple with reduced date should fail" 
        "201903",
      testParserSuccess
        parseReducedDate
        "Parse a year only"
        ("1984", 441763200),
      testParserFailure
        parseReducedDate
        "Malformed data to the reduced parser"
        "1994-0A"]
