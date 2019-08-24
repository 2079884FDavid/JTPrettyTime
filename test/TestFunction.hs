module TestFunction
( testFunction
) where

import Test.Framework as Framework (Test)
import Test.Framework.Providers.HUnit
import Test.HUnit

testFunction :: (Show b, Eq b) => String
                               -> a
                               -> b
                               -> (a -> b)
                               -> Framework.Test
testFunction desc input expect f = testCase desc t
  where
    t = expect @=? f input
