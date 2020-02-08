module Test.Main where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Tuple (Tuple(..))
import Data.Map as M
import Data.Traversable (sequence)
import Data.Unfoldable (replicate)
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
      let two = runParser "λf x -> f (f x)" term
      let thr = runParser "λf x -> f (f (f x))" term
      let exp = runParser "λm n -> n m" term
      let env = sequence $ M.fromFoldable [ Tuple "2" two
                                          , Tuple "3" thr
                                          , Tuple "exp" exp
                                          ]
      let apps = replicate 8 (App (Var "x" 1)) :: Array (Term -> Term)
      let res  = foldr identity (Var "x" 0) apps
      Assert.equal
        (Right (Lam "x" (Lam "x" res)))
        (norm <$> env <*> runParser "exp 2 3" term)
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
      -- fact (succ (succ (succ 1)))  -- Compute 4!
      let t    = runParser "λx y -> x" term
      let f    = runParser "λx y -> y" term
      let zero = runParser "λf x -> x" term
      let one  = runParser "λf x -> f x" term
      let succ = runParser "λn f x -> f(n f x)" term
      let pred = runParser "λn f x -> n(λg h -> h (g f)) (λu -> x) (λu ->u)" term
      let mul  = runParser "λm n f -> m(n f)" term
      let is0  = runParser "λn -> n (λx -> false) true" term
      let _Y   = runParser "λf -> (λx -> x x)(λx -> f(x x))" term
      let fact = runParser "Y(λf n -> (is0 n) 1 (mul n (f (pred n))))" term
      let env  = sequence $ M.fromFoldable [ Tuple "true" t
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
      let apps = replicate 24 (App (Var "f" 1)) :: Array (Term -> Term)
      let res  = foldr identity (Var "x" 0) apps
      Assert.equal
        (Right (Lam "f" (Lam "x" res)))
        (norm <$> env <*> runParser "fact (succ (succ (succ 1)))" term)
    test "quote" do
      let var = runParser "λm.λa b c.a m" term
      let app = runParser "λm n.λa b c.b m n" term
      let lam = runParser "λf.λa b c.c f" term
      let env = sequence $ M.fromFoldable [ Tuple "Var" var
                                          , Tuple "App" app
                                          , Tuple "Lam" lam
                                          ]
      let expected = norm <$> env <*> runParser "App (Lam (λy.Var y)) (Var x)" term
      let quoted = norm <$> env <*> runParser "quote ((λy.y) x)" term
      Assert.equal expected quoted
    test "quoted self-interpreter and self-reducer" do
      let _Y   = runParser "λf.(λx.f(x x))(λx.f(x x))" term
      let _E   = runParser "Y(λe m.m (λx.x) (λm n.(e m)(e n)) (λm v.e (m v)))" term
      let _P   = runParser "Y(λp m.(λx.x(λv.p(λa b c.b m(v (λa b.b))))m))" term
      let _RR  = runParser "Y(λr m.m (λx.x) (λm n.(r m) (λa b.a) (r n)) (λm.(λg x.x g(λa b c.c(λw.g(P (λa b c.a w))(λa b.b)))) (λv.r(m v))))" term
      let _R   = runParser "λm.RR m (λa b.b)" term
      let one  = runParser "λf x.f x" term
      let succ = runParser "λn f x.f(n f x)" term
      let env = sequence $ M.fromFoldable [ Tuple "Y" _Y
                                          , Tuple "E" _E
                                          , Tuple "P" _P
                                          , Tuple "RR" _RR
                                          , Tuple "R" _R
                                          , Tuple "1" one
                                          , Tuple "succ" succ
                                          ]
      let res1 = foldr identity (Var "v" 0) $
                 (replicate 4 (App (Var "v" 1)) :: Array (Term -> Term))
      let res2 = runParser "λa b c.c(λw a b c.c(λw1 a b c.b(λa b c.a w)(λa b c.b(λa b c.a w)(λa b c.b(λa b c.a w)(λa b c.b(λa b c.a w)(λa b c.a w1))))))" term
      Assert.equal
        (Right (Lam "v" (Lam "v" res1)))
        (norm <$> env <*> runParser "E (quote (succ (succ (succ 1))))" term)
      Assert.equal
        res2
        (norm <$> env <*> runParser "R (quote (succ (succ (succ 1))))" term)
