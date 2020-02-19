module Test.PCF (testPCF) where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Effect (Effect)
import PCF (PCFLine(..), Term(..), Type(..), line)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Text.Parsing.Parser (runParser)

testPCF :: Effect Unit
testPCF = runTest do
  suite "parsing PCF" do
    test "parsing PCF line" do
      Assert.equal
        (Right $ Run (App (Lam (Tuple "x" (TV "_")) (Var "x" 0)) (Var "y" 0)))
        (runParser "(λx.x)y" line)
      Assert.equal
        (Right $ TopLet "true" (Lam (Tuple "x" (TV "_")) (Lam (Tuple "y" (TV "_")) (Var "x" 1))))
        (runParser "true = λx y.x" line)
