module Test.PCF (testPCF) where

import Prelude
import Data.Tuple (Tuple(..), snd)
import Data.Either (Either(..))
import Data.Map as M
import Data.String.Utils (lines)
import Effect (Effect)
import PCF (PCFLine(..), Term(..), Type(..), program, line)
import Run (extract)
import Run.Console (runConsoleAccum)
import Run.Node.ReadLine (runReadLineAccum)
import Run.State (runState)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Text.Parsing.Parser (runParser)

testPCF :: Effect Unit
testPCF = runTest do
  suite "PCF" do
    test "parsing PCF line" do
      Assert.equal
        (Right $ Run (App (Lam (Tuple "x" (TV "_")) (Var "x" 0)) (Var "y" 0)))
        (runParser "(λx.x)y" line)
      Assert.equal
        (Right $ TopLet "true" (Lam (Tuple "x" (TV "_")) (Lam (Tuple "y" (TV "_")) (Var "x" 1))))
        (runParser "true = λx y.x" line)
  suite "PCF repl" do
    test "let-polymorphism" do
      let program' = """
        two = succ (succ 0)
        three = succ two
        add = fix (\f m n.ifz m then n else f (pred m) (succ n))
        mul = fix (\f t m n.ifz m then t else f (add t n) (pred m) n) 0
        add two three
        mul three three
        let id = \x.x in id succ (id three)  -- Let-polymorphism.
        """
      let res = runProgram program'
      let expected = [ "[two : Nat]"
                     , "[three : Nat]"
                     , "[add : Nat -> Nat -> Nat]"
                     , "[mul : Nat -> Nat -> Nat]"
                     , "5"
                     , "9"
                     , "4"
                     ]
      Assert.equal expected res
    test "sort" do
      let program' = """
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
      let res = runProgram program'
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

runProgram :: String -> Array String
runProgram p =
  program
    # runReadLineAccum (lines p)
    # runConsoleAccum
    # runState (Tuple [] M.empty)
    # extract
    # snd
