module Test.Main where

import Prelude
import Effect (Effect)
import Test.Rope (testRope)


main :: Effect Unit
main = do
  testRope
