module Test.Main where

import Prelude
import Data.Either (Either(..), fromRight)
import Data.Tuple (Tuple(..))
import Data.Map as M
import Effect (Effect)
import LambdaCalculus (Term(..), LambdaLine(..), eval, line, norm, term)
import Partial.Unsafe (unsafePartial)
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
    test "2^3" do
      -- 2 = \f x -> f (f x)
      -- 3 = \f x -> f (f (f x))
      -- exp = \m n -> n m
      -- exp 2 3  -- Compute 2^3.
      let two = unsafePartial fromRight $ runParser "λf x -> f (f x)" term
      let thr = unsafePartial fromRight $ runParser "λf x -> f (f (f x))" term
      let exp = unsafePartial fromRight $ runParser "λm n -> n m" term
      let env = M.fromFoldable [ Tuple "2" two
                               , Tuple "3" thr
                               , Tuple "exp" exp
                               ]
      Assert.equal
        (Right (Lam "x"
                    (Lam "x"
                         (App (Var "x" 1)
                              (App (Var "x" 1)
                                   (App (Var "x" 1)
                                        (App (Var "x" 1)
                                             (App (Var "x" 1)
                                                  (App (Var "x" 1)
                                                       (App (Var "x" 1)
                                                            (App (Var "x" 1)
                                                                 (Var "x" 0))))))))))))
        (norm env <$> runParser "exp 2 3" term)
    test "factorial" do
      -- true = \x y -> x
      -- false = \x y -> y
      -- 0 = \f x -> x
      -- 1 = \f x -> f x
      -- succ = \n f x -> f(n f x)
      -- pred = \n f x -> n(\g h -> h (g f)) (\u -> x) (\u ->u)
      -- mul = \m n f -> m(n f)
      -- is0 = \n -> n (\x -> false) true
      -- Y = \f -> (\x -> x x)(\x -> f(x x))
      -- fact = Y(\f n -> (is0 n) 1 (mul n (f (pred n))))
      -- fact (succ (succ 1))  -- Compute 3!
      let t    = unsafePartial fromRight $ runParser "λx y -> x" term
      let f    = unsafePartial fromRight $ runParser "λx y -> y" term
      let zero = unsafePartial fromRight $ runParser "λf x -> x" term
      let one  = unsafePartial fromRight $ runParser "λf x -> f x" term
      let succ = unsafePartial fromRight $ runParser "λn f x -> f(n f x)" term
      let pred = unsafePartial fromRight $ runParser "λn f x -> n(λg h -> h (g f)) (λu -> x) (λu ->u)" term
      let mul  = unsafePartial fromRight $ runParser "λm n f -> m(n f)" term
      let is0  = unsafePartial fromRight $ runParser "λn -> n (λx -> false) true" term
      let _Y   = unsafePartial fromRight $ runParser "λf -> (λx -> x x)(λx -> f(x x))" term
      let fact = unsafePartial fromRight $ runParser "Y(λf n -> (is0 n) 1 (mul n (f (pred n))))" term
      let env = M.fromFoldable [ Tuple "true" t
                               , Tuple "false" f
                               , Tuple "0" zero
                               , Tuple "1" one
                               , Tuple "succ" succ
                               , Tuple "pred" pred
                               , Tuple "mul" mul
                               , Tuple "is0" is0
                               , Tuple "Y" _Y
                               , Tuple "fact" fact
                               ]
      Assert.equal
        (Right (Lam "f"
                    (Lam "x"
                         (App (Var "f" 1)
                              (App (Var "f" 1)
                                   (App (Var "f" 1)
                                        (App (Var "f" 1)
                                             (App (Var "f" 1)
                                                  (App (Var "f" 1)
                                                       (Var "x" 0))))))))))
        (norm env <$> runParser "fact (succ (succ 1))" term)
