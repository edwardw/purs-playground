module Test.Rope (testRope) where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Text.Trifecta.Delta (Delta(..), delta)
import Text.Trifecta.Internal.Rope (grabLine, grabRest, ropeS)



testRope :: Effect Unit
testRope = runTest do
  suite "Rope" do
    test "grabRest - single strand" do
      let expected = Just $ Tuple (Columns 6 6) "World\nLorem"
          res = grabRest (delta "Hello ")
                         (ropeS "Hello World\nLorem")
                         Nothing
                         (\x y -> Just $ Tuple x y)
      Assert.equal expected res
    test "grabRest - multiple strands" do
      let expected = Just $ Tuple (Columns 3 3) "loWorld"
          res = grabRest (delta "Hel")
                         (ropeS "Hello" <> ropeS "World")
                         Nothing
                         (\x y -> Just $ Tuple x y)
      Assert.equal expected res
    test "grabRest - offset too long" do
      let expected = Nothing
          res = grabRest (delta "OffsetTooLong")
                         (ropeS "Hello")
                         Nothing
                         (\x y -> Just $ Tuple x y)
      Assert.equal expected res

    test "grabLine" do
      let expected = Just $ Tuple (Columns 6 6) "World\n"
          res = grabLine (delta "Hello ")
                         (ropeS "Hello" <> ropeS " World\nLorem")
                         Nothing
                         (\x y -> Just $ Tuple x y)
      Assert.equal expected res
