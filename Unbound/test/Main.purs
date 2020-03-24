module Test.Main where

import Prelude
import Effect (Effect)
import Test.PropOpenClose (testOpenClose)
import Test.Unbound (testUnbound)

main :: Effect Unit
main = do
  testUnbound
  testOpenClose
