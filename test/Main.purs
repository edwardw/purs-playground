module Test.Main where

import Prelude
import Data.Either (Either(..))
import Data.Map as M
import Effect (Effect)
import LambdaCalculus (Term(..), LambdaLine(..), eval, line, norm, term)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Text.Parsing.Parser (runParser)

main :: Effect Unit
main = runTest do
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
