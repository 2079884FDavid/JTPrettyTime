module Parsec.TestParserUtils
( testParserSuccess
, testParserFailure
) where

import Test.Framework as Framework (Test)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Text.Parsec
import Text.Parsec.String

testParserSuccess :: Parser Int 
                  -> String 
                  -> (String, Int) 
                  -> Framework.Test
testParserSuccess pTested desc (input, expect) = testCase desc t
  where
    t = case parse pTested "Failure" input of 
          Left v -> assertFailure $ "Cannot be parsed: "++ show v
          Right v -> v @?= expect

testParserFailure :: Parser Int -> String -> String -> Framework.Test
testParserFailure pTested desc input = testCase desc t
  where
    t = case parse pTested "Failure" input of
          Left _ -> return ()
          Right v -> assertFailure . f $ show v
    f v = "Expected failure; was able to parse: "++input++" -> "++v
