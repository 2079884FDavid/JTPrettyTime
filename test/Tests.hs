import Test.Framework as Framework (defaultMain, Test)

--import Iso8601.ParseTest
import Iso8601.Timezone.ParseTest
import Parsec.ParseIso8601Test

main :: IO ()
main = defaultMain tests

tests :: [Framework.Test]
tests = [
  groupIso8601Parse,
  groupIso8601TimezoneParse]

