module Test.HindleyMilner (testHindleyMilner) where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Map as M
import Data.Set as S
import Effect (Effect)
import HindleyMilner (MType(..), Name(..), PType(..), Subst(..), applySubst, freePType)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

testHindleyMilner :: Effect Unit
testHindleyMilner = runTest do
  suite "Hindley-Milner" do
    test "freePType" do
      let fromStr = TVar <<< Name
      let sigma = Forall (S.singleton $ Name "a") $ TFn (fromStr "a") (TFn (fromStr "b") (fromStr "c"))
      Assert.equal (S.fromFoldable [Name "b", Name "c"]) (freePType sigma)
    test "substitute PType" do
      let fromStr = TVar <<< Name
      let sigma = Forall (S.singleton $ Name "a") $ TFn (fromStr "a") (TFn (fromStr "b") (fromStr "c"))
      let subst = Subst $ M.fromFoldable [Tuple (Name "a") TNat, Tuple (Name "b") TNat]
      -- substitute `a` and `b` with `Nat` in
      --    `∀a. a -> b -> c`
      -- should yield:
      --    `∀a. a -> Nat -> c`
      let sigma' = Forall (S.singleton $ Name "a") $ TFn (fromStr "a") (TFn TNat (fromStr "c"))
      Assert.equal sigma' (applySubst subst sigma)

