module Test.Main where

import Prelude
import Effect (Effect)
import Test.LambdaCalculus (testLambdaCalculus)
import Test.PCF (testPCF)
import Test.PCFUnbound (testPCFUnbound)
import Test.UnionFind (testUnionFind)

main :: Effect Unit
main = do
  testLambdaCalculus
  testPCF
  testUnionFind
  testPCFUnbound
