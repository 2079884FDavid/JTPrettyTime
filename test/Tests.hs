import Test.Framework as Framework (defaultMain, Test)

import Convert.ConvertTest
import FormatTest
import Parsec.ParseDateTest
import Parsec.ParseIso8601Test
import Parsec.ParseTimeTest
import Parsec.ParseTimezoneTest
import UtilTest

main :: IO ()
main = defaultMain tests

tests :: [Framework.Test]
tests = [
  groupConvertConvert,
  groupDateParse,
  groupFormat,
  groupIso8601Parse,
  groupTimeParse,
  groupTimezoneParse,
  groupUtil]
