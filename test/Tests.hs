import Test.Framework as Framework (defaultMain, Test)

import Parsec.ParseDateTest
import Parsec.ParseIso8601Test
import Parsec.ParseTimeTest
import Parsec.ParseTimezoneTest

main :: IO ()
main = defaultMain tests

tests :: [Framework.Test]
tests = [
  groupIso8601Parse,
  groupDateParse,
  groupTimeParse,
  groupTimezoneParse]
