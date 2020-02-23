module Test.Main where

import Prelude
import Effect (Effect)
import Test.HindleyMilner (testHindleyMilner)
import Test.LambdaCalculus (testLambdaCalculus)
import Test.PCF (testPCF)

main :: Effect Unit
main = do
  testLambdaCalculus
  testPCF
  testHindleyMilner
