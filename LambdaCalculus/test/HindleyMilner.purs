module Test.HindleyMilner where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), uncurry)
import Data.Map as M
import Data.Set as S
import Data.String.Utils (lines)
import Effect (Effect)
import HindleyMilner (Dbi(..), Gamma(..), MType(..), Name(..), PCFLine(..), PType(..), Subst(..), Term(..), applySubst, freePType, generalize, infer, line, prelude, repl, runInfer, term)
import Run (extract)
import Run.Console (runConsoleAccum)
import Run.Node.ReadLine (runReadLineAccum)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Text.Parsing.Parser (runParser)

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
        (Right $ "λx.x :: ∀_0. _0 -> _0")
        (showType prelude <$> runParser "λx.x" term)
      Assert.equal
        (Right $ "fix succ :: ∀∅. Nat")
        (showType prelude <$> runParser "fix succ" term)
      Assert.equal
        (Right $ "λf.λg.λx.f@2 x(g@1 x) :: ∀_2 _4 _5. (_2 -> _4 -> _5) -> (_2 -> _4) -> _2 -> _5")
        (showType prelude <$> runParser "λf.λg.λx.f x(g x)" term)
      Assert.equal
        (Right $ "succ(42) :: ∀∅. Nat")
        (showType prelude <$> runParser "succ(42)" term)
      Assert.equal
        (Right $ "succ(succ(0)) :: ∀∅. Nat")
        (showType prelude <$> runParser "succ(succ(0))" term)
      Assert.equal
        (Right $ "let id = λx.x in id succ(id(succ(succ(succ(0))))) :: ∀∅. Nat")
        (showType prelude <$> runParser "let id = λx.x in id succ(id(succ(succ(succ(0)))))" term)

  suite "PCF" do
    test "parsing PCF line" do
      Assert.equal
        (Right $ Run (App (Lam (Name "x") (Var (Name "x") (Dbi 0))) (Var (Name "y") (Dbi 0))))
        (runParser "(λx.x)y" line)
      Assert.equal
        (Right $ TopLet (Name "true") (Lam (Name "x") (Lam (Name "y") (Var (Name "x") (Dbi 1)))))
        (runParser "true = λx y.x" line)

  suite "PCF repl" do
    test "let-polymorphism" do
      let program = """
        two = succ (succ 0)
        three = succ two
        add = fix (\f m n.ifz m then n else f (pred m) (succ n))
        mul = fix (\f t m n.ifz m then t else f (add t n) (pred m) n) 0
        add two three
        mul three three
        let id = \x.x in id succ (id three)  -- Let-polymorphism.
        """
      let res = runRepl program
      let expected = [ "[two : ∀∅. Nat]"
                     , "[three : ∀∅. Nat]"
                     , "[add : ∀∅. Nat -> Nat -> Nat]"
                     , "[mul : ∀∅. Nat -> Nat -> Nat]"
                     , "5"
                     , "9"
                     , "4"
                     ]
      Assert.equal expected res


showType :: Gamma -> Term -> String
showType env term =
  case term
        # infer env
        # map (generalize (Gamma mempty) <<< uncurry applySubst)
        # runInfer
        # extract of
    Left err -> "Erro referring type of " <> show term <> ": " <> show err
    Right ty -> show term <> " :: " <> show ty


runRepl :: String -> Array String
runRepl p =
  repl
    # runReadLineAccum (lines p)
    # runConsoleAccum
    # extract
