module Test.PCF where

import Prelude
import Control.Monad.Writer (runWriter)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst, uncurry)
import Data.Map as M
import Data.Set as S
import Data.String.Utils (lines)
import Effect (Effect)
import PCF (Gamma(..), MType(..), Name(..), PCFLine(..), PType(..), Subst(..), Term(..), applySubst, freePType, generalize, infer, line, prelude, repl, runInfer, term)
import Run (extract)
import Run.Console (runConsoleAccum)
import Run.Node.ReadLine (runReadLineAccum)
import Test.Unit (suite, test, testSkip)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Text.Parsing.Parser (ParseError, runParserT)

testPCF :: Effect Unit
testPCF = runTest do
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
        (showType prelude <$> runTerm "λx.x")
      Assert.equal
        (Right $ "fix succ :: ∀∅. Nat")
        (showType prelude <$> runTerm  "fix succ")
      Assert.equal
        (Right $ "λf.λg.λx.f@2 x(g@1 x) :: ∀_2 _4 _5. (_2 -> _4 -> _5) -> (_2 -> _4) -> _2 -> _5")
        (showType prelude <$> runTerm "λf.λg.λx.f x(g x)")
      Assert.equal
        (Right $ "succ 42 :: ∀∅. Nat")
        (showType prelude <$> runTerm "succ(42)")
      Assert.equal
        (Right $ "succ(succ 0) :: ∀∅. Nat")
        (showType prelude <$> runTerm "succ(succ(0))")
      Assert.equal
        (Right $ "let id = λx.x in id succ(id(succ(succ(succ 0)))) :: ∀∅. Nat")
        (showType prelude <$> runTerm "let id = λx.x in id succ(id(succ(succ(succ(0)))))")

  suite "PCF" do
    test "parsing PCF line" do
      Assert.equal
        (Right $ Run (App (Lam (Name "x") (Var (Name "x") 0)) (Var (Name "y") 0)))
        (runLine "(λx.x)y")
      Assert.equal
        (Right $ TopLet (Name "true") (Lam (Name "x") (Lam (Name "y") (Var (Name "x") 1))))
        (runLine "true = λx y.x")

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
    testSkip "sort" do
      let program = """
        -- Insertion sort sans fixpoint. Slow.
        pair=\x y f.f x y
        fst=\p.p(\x y.x)
        snd=\p.p(\x y.y)
        nil=\c n.n
        cons=\h t c n.c h(t c n)
        null=\l.l(\h t.0)1
        head=\l.l(\h t.h)undefined
        lt=fix(\f x y.ifz y then 0 else ifz x then 1 else f (pred x) (pred y))
        f=\x p.ifz null (fst p) then ifz lt x (head (fst p)) then pair (fst p) (cons x (snd p)) else pair nil (cons x (cons (head (fst p)) (snd p))) else pair nil (cons x (snd p))
        ins=\x l.let q = l f (pair (cons x nil) nil) in (ifz null (fst q) then (cons (head (fst q)) (snd q)) else snd q)
        sort=\l.l ins nil
        sort (cons 3(cons 1(cons 4(cons 1(cons 5 nil)))))
        """
      let res = runRepl program
      let expected = [ "[pair : _0 -> _1 -> (_0 -> _1 -> _4) -> _4]"
                     , "[fst : ((_1 -> _2 -> _1) -> _3) -> _3]"
                     , "[snd : ((_1 -> _2 -> _2) -> _3) -> _3]"
                     , "[nil : _0 -> _1 -> _1]"
                     , "[cons : _0 -> ((_0 -> _6 -> _7) -> _3 -> _6) -> (_0 -> _6 -> _7) -> _3 -> _7]"
                     , "[null : ((_1 -> _2 -> Nat) -> Nat -> _4) -> _4]"
                     , "[head : ((_1 -> _2 -> _1) -> _4 -> _5) -> _5]"
                     , "[lt : Nat -> Nat -> Nat]"
                     , "[f : Nat -> ((_5 -> _5 -> _5) -> (Nat -> Nat -> Nat) -> Nat -> Nat) -> (((Nat -> Nat -> Nat) -> Nat -> Nat) -> ((Nat -> Nat -> Nat) -> Nat -> Nat) -> _23) -> _23]"
                     , "[ins : _0 -> ((Nat -> ((_2 -> _2 -> _2) -> (Nat -> Nat -> Nat) -> Nat -> Nat) -> (((Nat -> Nat -> Nat) -> Nat -> Nat) -> ((Nat -> Nat -> Nat) -> Nat -> Nat) -> _3) -> _3) -> ((((_0 -> _14 -> _10) -> _14 -> _10) -> (_17 -> _18 -> _18) -> _7) -> _7) -> _20) -> (_43 -> _31 -> _32) -> _33 -> _32]"
                     , "[sort : ((_1 -> ((Nat -> ((_2 -> _2 -> _2) -> (Nat -> Nat -> Nat) -> Nat -> Nat) -> (((Nat -> Nat -> Nat) -> Nat -> Nat) -> ((Nat -> Nat -> Nat) -> Nat -> Nat) -> _3) -> _3) -> ((((_1 -> _4 -> _5) -> _4 -> _5) -> (_6 -> _7 -> _7) -> _8) -> _8) -> _9) -> (_10 -> _11 -> _12) -> _13 -> _12) -> (_15 -> _16 -> _16) -> _17) -> _17]"
                     , "λc:_.λn:_.c@1 1(c@1 1(c@1 3(c@1 4(c@1 5 n))))"
                     ]
      Assert.equal expected res
    test "list sum" do
      let program = """
        add=fix (\f m n.ifz m then n else f (pred m) (succ n))
        nil=\c n.n
        cons=\h t c n.c h(t c n)
        sum=\xs:(I->I->I)->I->I.xs(\h:I t:I.add h t)0
        sum (cons 1 (cons 125 (cons 27 nil)))
        """
      let res = runRepl program
      let expected = [ "[add : ∀∅. Nat -> Nat -> Nat]"
                     , "[nil : ∀_0 _1. _0 -> _1 -> _1]"
                     , "[cons : ∀_0 _3 _6 _7. _0 -> ((_0 -> _6 -> _7) -> _3 -> _6) -> (_0 -> _6 -> _7) -> _3 -> _7]"
                     , "[sum : ∀∅. ((Nat -> Nat -> Nat) -> Nat -> Nat) -> Nat]"
                     , "153"
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


runTerm :: String -> Either ParseError Term
runTerm s = fst <<< runWriter $ runParserT s term

runLine :: String -> Either ParseError PCFLine
runLine s = fst <<< runWriter $ runParserT s line


runRepl :: String -> Array String
runRepl p =
  repl
    # runReadLineAccum (lines p)
    # runConsoleAccum
    # extract
