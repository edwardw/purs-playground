module Test.PCFUnbound (testPCFUnbound) where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff)
import PCFUnbound (PCFLine(..), Term, line, norm, red, step, tc)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Text.Parsing.Parser (runParser)
import Unbound.LocallyNameless (FreshM, aeq, runFreshM)


testPCFUnbound :: Effect Unit
testPCFUnbound = runTest do
  suite "PCFUnbound" do
    test "beta-eta reduction" do
      pcfLineAeq red "z" "(λx. x)z"
      pcfLineAeq red "x" "(λf x. f x)x"
      pcfLineAeq red "λx. x" "(λf x. f x)(λf x. f x)"
      pcfLineAeq red "λn.λs.λz. (λs. s) s(n z s)" "(λm n s z. m s (n z s))(λs z. s z)"
    test "small step reduction" do
      pcfLineAeq (tc step) "z" "(λx. x)z"
      pcfLineAeq (tc step) "λf. x f" "(λf x. f x)x"
      pcfLineAeq (tc step) "λx. (λf x. f x) x" "(λf x. f x)(λf x. f x)"
      pcfLineAeq (tc step) "λn.λs.λz. (λs. λz. s z) s(n z s)" "(λm n s z. m s (n z s))(λs z. s z)"
    test "normalization" do
      pcfLineAeq norm "z" "(λx. x)z"
      pcfLineAeq norm "x" "(λf x. f x)x"
      pcfLineAeq norm "λx. x" "(λf x. f x)(λf x. f x)"
      pcfLineAeq norm "λn.λs.λz. s(n z s)" "(λm n s z. m s (n z s))(λs z. s z)"


pcfLineAeq :: (Term -> FreshM Term) -> String -> String -> Aff Unit
pcfLineAeq f expected actual =
  case runParser expected line of
    Right (Run t1) -> case runParser actual line of
      Right (Run t2) -> do
        let t2' = runFreshM $ f t2
        Assert.assert (show t1 <> " /= " <> show t2')
                      (t1 `aeq` t2')
      _ -> Assert.assert "should not happen" false
    _ -> Assert.assert "should not happen" false
