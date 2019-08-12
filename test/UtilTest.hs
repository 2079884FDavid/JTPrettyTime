module PROJECTTest
( groupPROJECT
) where

--import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework as Framework (Test, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import JTPrettyTime.PROJECT

groupPROJECT :: Framework.Test
groupPROJECT = 
  testGroup "JTPrettyTime.PROJECT" []
