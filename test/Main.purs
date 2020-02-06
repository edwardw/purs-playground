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
        (runParser "x" term)
        (Right $ Var "x")
      Assert.equal
        (runParser "λx -> x" term)
        (Right $ Lam "x" (Var "x"))
      Assert.equal
        (runParser "(λx -> x)z" term)
        (Right $ App (Lam "x" (Var "x")) (Var "z"))
      Assert.equal
        (runParser "λx y -> x" term)
        (Right $ Lam "x" (Lam "y" (Var "x")))
      Assert.equal
        -- 2 = \f x -> f (f x)
        (runParser "λf x -> f (f x)" term)
        (Right $ Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var "x")))))
      Assert.equal
        -- exp = \m n -> n m
        (runParser "λm n -> n m" term)
        (Right $ Lam "m" (Lam "n" (App (Var "n") (Var "m"))))
    test "parsing lambda lines" do
      Assert.equal
        (runParser "true = λx y -> x" line)
        (Right $ Let "true" (Lam "x" (Lam "y" (Var "x"))))
      Assert.equal
        (runParser "(λx -> x)y" line)
        (Right $ Run (App (Lam "x" (Var "x")) (Var "y")))
    test "evaluation and normal form" do
      Assert.equal
        (eval M.empty <$> runParser "(λx -> x)z" term)
        (Right $ Var "z")
      Assert.equal
        -- `(λf x -> f x)(λf x -> f x)` is normalized to `λx x1 -> x x1`
        (norm M.empty <$> runParser "(λf x -> f x)(λf x -> f x)" term)
        (Right (Lam "x" (Lam "x1" (App (Var "x") (Var "x1")))))
