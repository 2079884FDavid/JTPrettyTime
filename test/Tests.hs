import Test.Framework as Framework (defaultMain, Test)

import Parsec.ParseIso8601Test
import Parsec.ParseDateTest

main :: IO ()
main = defaultMain tests

tests :: [Framework.Test]
tests = [
  groupIso8601Parse,
  groupDateParse]
