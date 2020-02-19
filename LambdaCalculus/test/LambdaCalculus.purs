module Test.LambdaCalculus (testLambdaCalculus) where

import Prelude
import Data.Either (Either(..))
import Data.String.Utils (lines)
import Data.Tuple (snd)
import Data.Map as M
import Effect (Effect)
import LambdaCalculus (Term(..), LambdaLine(..), eval, line, norm, term)
import Main (program)
import Run (extract)
import Run.Console (runConsoleAccum)
import Run.Node.ReadLine (runReadLineAccum)
import Run.State (runState)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Text.Parsing.Parser (runParser)

testLambdaCalculus :: Effect Unit
testLambdaCalculus = runTest do
  suite "lambda calculus" do
    test "parsing lambda terms" do
      Assert.equal
        (Right $ Var "x" 0)
        (runParser "x" term)
      Assert.equal
        (Right $ Lam "x" (Var "x" 0))
        (runParser "λx -> x" term)
      Assert.equal
        (Right $ App (Lam "x" (Var "x" 0)) (Var "z" 0))
        (runParser "(λx -> x)z" term)
      Assert.equal
        (Right $ Lam "x" (Lam "y" (Var "x" 1)))
        (runParser "λx y -> x" term)
      Assert.equal
        -- 2 = \f x -> f (f x)
        (Right $ Lam "f" (Lam "x" (App (Var "f" 1) (App (Var "f" 1) (Var "x" 0)))))
        (runParser "λf x -> f (f x)" term)
      Assert.equal
        -- exp = \m n -> n m
        (Right $ Lam "m" (Lam "n" (App (Var "n" 0) (Var "m" 1))))
        (runParser "λm n -> n m" term)
      Assert.equal
        -- https://en.wikipedia.org/wiki/De_Bruijn_index
        -- λz. (λy. y (λx. x)) (λx. z x) ~> λz.(λy.y(λx.x))(λx.z@1 x)
        (Right $ Lam "z"
                     (App (Lam "y"
                               (App (Var "y" 0)
                                    (Lam "x" (Var "x" 0))))
                          (Lam "x"
                               (App (Var "z" 1) (Var "x" 0)))))
        (runParser "λz. (λy. y (λx. x)) (λx. z x)" term)
      Assert.equal
        -- https://github.com/Gabriel439/Haskell-Morte-Library/issues/1
        -- plus = λm. λn. λs. λz. m s (n z s) ~> λ. λ. λ. λ. 3 1 (2 0 1)
        (Right $ Lam "m"
                     (Lam "n"
                          (Lam "s"
                               (Lam "z"
                                    (App (App (Var "m" 3)
                                              (Var "s" 1))
                                         (App (App (Var "n" 2)
                                                   (Var "z" 0))
                                              (Var "s" 1)))))))
        (runParser "λm. λn. λs. λz. m s (n z s)" term)
    test "parsing lambda lines" do
      Assert.equal
        (Right $ Let "true" (Lam "x" (Lam "y" (Var "x" 1))))
        (runParser "true = λx y -> x" line)
      Assert.equal
        (Right $ Run (App (Lam "x" (Var "x" 0)) (Var "y" 0)))
        (runParser "(λx -> x)y" line)
    test "eval and normal forms" do
      Assert.equal
        (Right $ Var "z" 0)
        (eval M.empty <$> runParser "(λx -> x)z" term)
      Assert.equal
        -- `(λf x -> f x)(λf x -> f x)` is normalized to `λx x1 -> x x1`
        (Right (Lam "x" (Lam "x" (App (Var "x" 1) (Var "x" 0)))))
        (norm M.empty <$> runParser "(λf x -> f x)(λf x -> f x)" term)
    test "plus 1" do
      -- https://github.com/Gabriel439/Haskell-Morte-Library/issues/1#issuecomment-55781605
      -- 1 = λs. λz. s z ~> λ. λ. (1 0)
      -- plus = λm. λn. λs. λz. m s (n z s) ~> λ. λ. λ. λ. 3 1 (2 0 1)
      -- plus 1 ~> λn. λs. λz. s (n z s) ~> λ. λ. λ. 1 (2 0 1)
      Assert.equal
        (Right (Lam "n"
                    (Lam "s"
                         (Lam "z"
                              (App (Var "s" 1)
                                         (App (App (Var "n" 2)
                                                   (Var "z" 0))
                                              (Var "s" 1)))))))
        (norm M.empty <$> runParser "(λm. λn. λs. λz. m s (n z s))(λs. λz. s z)" term)
  suite "lambda calculus repl" do
    test "2^3" do
      let program' = """
        2 = λf x -> f (f x)
        3 = λf x -> f (f (f x))
        exp = λm n -> n m
        exp 2 3"""

      let res = runProgram program'
      let expected = ["λx.λx.x@1(x@1(x@1(x@1(x@1(x@1(x@1(x@1 x)))))))"]
      Assert.equal expected res
    test "factorial" do
      let program' = """
        true = λx y -> x
        false = λx y -> y
        0 = λf x -> x
        1 = λf x -> f x
        succ = λn f x -> f(n f x)
        pred = λn f x -> n(λg h -> h (g f)) (λu -> x) (λu -> u)
        mul = λm n f -> m(n f)
        is0 = λn -> n (λx -> false) true
        Y = λf -> (λx -> x x)(λx -> f(x x))
        fact = Y(λf n -> (is0 n) 1 (mul n (f (pred n))))
        fact (succ (succ (succ 1)))  -- Compute 4!"""
      let res = runProgram program'
      let expected = ["λf.λx.f@1(f@1(f@1(f@1(f@1(f@1(f@1(f@1(f@1(f@1(f@1(f@1(f@1(f@1(f@1(f@1(f@1(f@1(f@1(f@1(f@1(f@1(f@1(f@1 x)))))))))))))))))))))))"]
      Assert.equal expected res
    test "quote" do
      let program' = """
        Var = λm.λa b c.a m
        App = λm n.λa b c.b m n
        Lam = λf.λa b c.c f
        quote ((λy.y) x)"""
      let res = runProgram program'
      let expected = ["λa.λb.λc.b@1(λa.λb.λc.c(λy.λa.λb.λc.a@2 y@3))(λa.λb.λc.a@2 x@6)"]
      Assert.equal expected res
    test "quoted self-interpreter and self-reducer" do
      let program' = """
        Y = λf.(λx.f(x x))(λx.f(x x))
        E = Y(λe m.m (λx.x) (λm n.(e m)(e n)) (λm v.e (m v)))
        P = Y(λp m.(λx.x(λv.p(λa b c.b m(v (λa b.b))))m))
        RR = Y(λr m.m (λx.x) (λm n.(r m) (λa b.a) (r n)) (λm.(λg x.x g(λa b c.c(λw.g(P (λa b c.a w))(λa b.b)))) (λv.r(m v))))
        R = λm.RR m (λa b.b)
        1 = λf x.f x
        succ = λn f x.f(n f x)
        E (quote (succ (succ (succ 1))))
        R (quote (succ (succ (succ 1))))"""
      let res = runProgram program'
      let expected = [ "λv.λv.v@1(v@1(v@1(v@1 v)))"
                     , "λa.λb.λc.c(λw.λa.λb.λc.c(λw.λa.λb.λc.b@1(λa.λb.λc.a@2 w@10)(λa.λb.λc.b@1(λa.λb.λc.a@2 w@13)(λa.λb.λc.b@1(λa.λb.λc.a@2 w@16)(λa.λb.λc.b@1(λa.λb.λc.a@2 w@19)(λa.λb.λc.a@2 w@15))))))"
                     ]
      Assert.equal expected res

runProgram :: String -> Array String
runProgram p =
  program
    # runReadLineAccum (lines p)
    # runConsoleAccum
    # runState M.empty
    # extract
    # snd
