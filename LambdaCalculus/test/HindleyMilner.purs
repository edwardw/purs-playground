module Test.HindleyMilner where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr)
import Data.Tuple (Tuple(..), uncurry)
import Data.Map as M
import Data.Set as S
import Effect (Effect)
import HindleyMilner (Dbi(..), Gamma(..), MType(..), Name(..), PType(..), Subst(..), Term(..), applySubst, freePType, generalize, infer, runInfer)
import Run (extract)
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
    test "type inferring" do
      Assert.equal
        "λx.x :: ∀_0. _0 -> _0"
        (showType prelude $ lambda [Name "x"] (var "x"))
      Assert.equal
        "fix succ :: ∀∅. Nat"
        (showType prelude $ app (var "fix") [var "succ"])
      Assert.equal
        "λf.λg.λx.f x(g x) :: ∀_2 _4 _5. (_2 -> _4 -> _5) -> (_2 -> _4) -> _2 -> _5"
        (showType prelude $ lambda [Name "f", Name "g", Name "x"] (app (var "f") [var "x", app (var "g") [var "x"]]))
      Assert.equal
        "succ 42 :: ∀∅. Nat"
        (showType prelude $ app (var "succ") [var "42"])
      Assert.equal
        "succ(succ 0) :: ∀∅. Nat"
        (showType prelude $ app (var "succ") [app (var "succ") [(var "0")]])
      Assert.equal
        "succ(succ(succ 0)) :: ∀∅. Nat"
        (showType prelude $ app (var "succ") [app (var "succ") [app (var "succ") [(var "0")]]])
      Assert.equal
        "let id = λx.x in id succ(id(succ(succ(succ 0)))) :: ∀∅. Nat"
        (showType prelude $ Let (Name "id") (lambda [Name "x"] (var "x")) (app (var "id") [var "succ", app (var "id") [app (var "succ") [app (var "succ") [app (var "succ") [var "0"]]]]]))



-- Some functions to help testing before the parsing and evaluation are integrated.
prelude :: Gamma
prelude = Gamma $ M.fromFoldable
  [ Tuple (Name "pred")   (Forall S.empty (TNat ~> TNat))
  , Tuple (Name "succ")   (Forall S.empty (TNat ~> TNat))
  , Tuple (Name "fix")    (Forall (S.singleton $ Name "a") ((tvar "a" ~> tvar "a") ~> tvar "a"))
  ]


tvar :: String -> MType
tvar = TVar <<< Name


lambda :: Array Name -> Term -> Term
lambda names term = foldr Lam term names


app :: Term -> Array Term -> Term
app = foldl App


var :: String -> Term
var name = Var (Name name) (Dbi 0)


fn :: MType -> MType -> MType
fn = TFn

infixr 9 fn as ~>


showType :: Gamma -> Term -> String
showType env term =
  case extract <<< runInfer <<< map (generalize (Gamma mempty) <<< uncurry applySubst) <<< infer env $ term of
    Left err -> "Erro referring type of " <> show term <> ": " <> show err
    Right ty -> show term <> " :: " <> show ty
