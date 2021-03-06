module Test.UnionFind where

import Prelude
import Data.UnionFind (equivalent, find, fresh, isRepresentative, union)
import Data.UnionFind.Persistent as UFP
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)



testUnionFind :: Effect Unit
testUnionFind = runTest do
  suite "imperative union find" do
    test "union" do
      let p1 = fresh 10
          p2 = fresh 99
      Assert.equal 10 (find p1)
      Assert.equal 99 (find p2)
      Assert.assert "fresh is its own representative" (isRepresentative p1)
      Assert.assert "fresh is its own representative" (isRepresentative p2)
      Assert.assertFalse "two freshes should not be equivalent" (equivalent p1 p2)
      let _  = union (+) p1 p2
      Assert.equal 109 (find p1)
      Assert.equal 109 (find p2)
      Assert.assert "after being united they should be equivalent" (equivalent p1 p2)
      Assert.assert
        "after being united either one of them should not be representative any more"
        (isRepresentative p1 `xor` isRepresentative p2)


  suite "persistent union find" do
    test "union" do
      let u  = UFP.create :: UFP.Union Int
          p1 = UFP.fresh 10 u
          p2 = UFP.fresh 99 u
      Assert.equal 10 (UFP.find p1 u)
      Assert.equal 99 (UFP.find p2 u)
      Assert.assert "fresh is its own representative" (UFP.isRepresentative p1 u)
      Assert.assert "fresh is its own representative" (UFP.isRepresentative p2 u)
      Assert.assertFalse "two freshhes should not be equivalent" (UFP.equivalent p1 p2 u)
      let u' = UFP.union (+) p1 p2 u
      Assert.equal 109 (UFP.find p1 u')
      Assert.equal 109 (UFP.find p2 u')
      Assert.assert "after being united they should be equivalent" (UFP.equivalent p1 p2 u')
      Assert.assert
        "after being united either one of them should not be representative any more"
        (UFP.isRepresentative p1 u' `xor` UFP.isRepresentative p2 u')
      Assert.assertFalse
        "they are still not equivalent in the original collection"
        (UFP.equivalent p1 p2 u)
      Assert.equal 10 (UFP.find p1 u)
      Assert.equal 99 (UFP.find p2 u)


xor :: forall a. HeytingAlgebra a => a -> a -> a
xor x y = (x && not y) || (not x && y)
