module Test.Unbound (testUnbound) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Typeable (class Typeable, mkTyRep)
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Unbound.Alpha (class Alpha, genericACompare, genericAeq, genericClose, genericFreshen, genericFvAny, genericIsPat, genericIsTerm, genericLFreshen, genericNamePatFind, genericNthPatFind, genericOpen, genericSwaps)
import Unbound.Bind (Bind)
import Unbound.Fresh (FreshM, runFreshM)
import Unbound.Name (Name, s2n)
import Unbound.Operations (bnd, unbnd)
import Unbound.Subst (class Subst, SubstName(..), genericSubst, genericSubsts, subst)


testUnbound :: Effect Unit
testUnbound = runTest do
  suite "Unbound - untyped" do
    test "Original readme example" do
      Assert.equal "Lam (<y> App (V 0@0) (Lam (<x> Lam (<y> App (V 0@0) (V 1@0)))))"
                   (show example)


--------------------------------------------------------------------------------
-- The example in original readme file -----------------------------------------
--------------------------------------------------------------------------------

type Var = Name Expr

data Expr
  = V Var
  | Lam (Bind Var Expr)
  | App Expr Expr

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show = case _ of
    V x       -> "V " <> show x
    Lam b     -> "Lam " <> show b
    App e1 e2 -> "App (" <> show e1 <> ") (" <> show e2 <> ")"

instance typeableExpr :: Typeable Expr where
  typeOf _ = mkTyRep "Unbound" "UntypedLambdaExpr"


instance alphaExpr :: Alpha Expr where
  -- don't try to be smart, for some reason the compiler really doesn't like
  -- the point-free style here.
  aeq' ctx x y         = genericAeq ctx x y
  fvAny' ctx nfn x     = genericFvAny ctx nfn x
  close ctx b x        = genericClose ctx b x
  open ctx b x         = genericOpen ctx b x
  isPat x              = genericIsPat x
  isTerm x             = genericIsTerm x
  isEmbed _            = false
  nthPatFind x         = genericNthPatFind x
  namePatFind x        = genericNamePatFind x
  swaps' ctx perm x    = genericSwaps ctx perm x
  freshen' ctx x       = genericFreshen ctx x
  lfreshen' ctx a cont = genericLFreshen ctx a cont
  acompare' ctx x y    = genericACompare ctx x y


instance substExpr :: Subst Expr Expr where
  isvar = case _ of
    V x -> Just $ SubstName identity x
    _   -> Nothing

  isCoerceVar _ = Nothing
  subst n u x   = genericSubst n u x
  substs ss x   = genericSubsts ss x


eval :: Expr -> FreshM Expr
eval = case _ of
  V x       -> unsafeThrow $ "unbound variable " <> show x
  e@(Lam _) -> pure e
  App e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    case v1 of
      Lam bnd -> do
        Tuple x body <- unbnd bnd
        let body' = subst x v2 body
        eval body'
      _ -> unsafeThrow "application of non-lambda"


example :: Expr
example =
  let x = s2n "x"
      y = s2n "y"
      e = Lam $ bnd x (Lam $ bnd y (App (V y) (V x)))
  in runFreshM $ eval (App (App e e) e)
