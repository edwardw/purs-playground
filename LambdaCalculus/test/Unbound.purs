module Test.Unbound where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Typeable (class Typeable, mkTyRep)
import Data.Typelevel.Undefined (undefined)
import Unbound.Alpha (class Alpha, genericACompare, genericAeq, genericClose, genericFreshen, genericFvAny, genericIsPat, genericIsTerm, genericLFreshen, genericNamePatFind, genericNthPatFind, genericOpen, genericSwaps)
import Unbound.Bind (Bind)
import Unbound.Fresh (FreshM, runFreshM)
import Unbound.Name (Name, s2n)
import Unbound.Operations (bnd, unbnd)
import Unbound.Subst (class Subst, SubstName(..), genericSubst, genericSubsts, subst)


type Var = Name Expr

data Expr
  = V Var
  | Lam (Bind Var Expr)
  | App Expr Expr

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show = case _ of
    V x -> "V " <> show x
    Lam b -> "Lam " <> show b
    App e1 e2 -> "App " <> show e1 <> " " <> show e2

instance typeableExpr :: Typeable Expr where
  typeOf _ = mkTyRep "Unbound" "UntypedLambdaCalcExpr"


instance alphaExpr :: Alpha Expr where
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


unboundEval :: Expr -> FreshM Expr
unboundEval = case _ of
  V x       -> undefined
  e@(Lam _) -> pure e
  App e1 e2 -> do
    v1 <- unboundEval e1
    v2 <- unboundEval e2
    case v1 of
      Lam bnd -> do
        Tuple x body <- unbnd bnd
        let body' = subst x v2 body
        unboundEval body'
      _ -> undefined


example :: Expr
example =
  let x = s2n "x"
      y = s2n "y"
      e = Lam $ bnd x (Lam $ bnd y (App (V y) (V x)))
  in runFreshM $ unboundEval (App (App e e) e)
