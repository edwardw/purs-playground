module LC where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Tuple (Tuple(..))
import Data.Typeable (class Typeable, mkTyRep)
import Effect (Effect)
import Test.Assert (assert')
import Unbound.LocallyNameless (class Alpha, class Subst, Bind, FreshM, Name, SubstName(..), aeq, bind_, fvSet, genericACompare, genericAeq, genericClose, genericFreshen, genericFvAny, genericIsPat, genericIsTerm, genericLFreshen, genericNamePatFind, genericNthPatFind, genericOpen, genericSubst, genericSubsts, genericSwaps, runFreshM, string2Name, subst, unbind_)


data Exp
  = Var (Name Exp)
  | Lam (Bind (Name Exp) Exp)
  | App Exp Exp

derive instance genericExp :: Generic Exp _

instance showExp :: Show Exp where
  show = case _ of
    Var a     -> "(Var " <> show a <> ")"
    Lam b     -> "Lam " <> show b
    App e1 e2 -> "(App " <> show e1 <> " " <> show e2 <> ")"

instance typeableExp :: Typeable Exp where
  typeOf _ = mkTyRep "LC" "Exp"

instance alphaExp :: Alpha Exp where
  aeq' ctx a b         = genericAeq ctx a b
  fvAny' ctx nfn a     = genericFvAny ctx nfn a
  close ctx b a        = genericClose ctx b a
  open ctx b a         = genericOpen ctx b a
  isPat a              = genericIsPat a
  isTerm a             = genericIsTerm a
  isEmbed _            = false
  nthPatFind a         = genericNthPatFind a
  namePatFind a        = genericNamePatFind a
  swaps' ctx perm a    = genericSwaps ctx perm a
  freshen' ctx a       = genericFreshen ctx a
  lfreshen' ctx a cont = genericLFreshen ctx a cont
  acompare' ctx a b    = genericACompare ctx a b

instance substExp :: Subst Exp Exp where
  isvar = case _ of
    Var a -> Just $ SubstName identity a
    _     -> Nothing

  isCoerceVar _ = Nothing
  subst n u a   = genericSubst n u a
  substs ss a   = genericSubsts ss a


-- | All new functions should be defined in a monad that can generate
-- | locally fresh names.
type M a = FreshM a


-- | Beta-Eta equivalence for lambda calculus terms.
-- | If the terms have a normal form then the algorithm will terminate.
-- | Otherwise, the algorithm may loop for certain inputs.
beEq :: Exp -> Exp -> M Boolean
beEq e1 e2
  | aeq e1 e2 = pure true
  | otherwise   = do
    e1' <- red e1
    e2' <- red e2
    if aeq e1' e1 && aeq e2' e2
    then pure false
    else beEq e1' e2'

infix 4 beEq as =~

-- | Parallel beta-eta reduction for lambda calculus terms.
-- | Do as many reductions as possible in one step, while still ensuring
-- | termination.
red :: Exp -> M Exp
red = case _ of
  App e1 e2 -> do
    e1' <- red e1
    e2' <- red e2
    case e1' of
      -- look for a beta-reduction
      Lam bnd -> do
        Tuple a e1'' <- unbind_ bnd
        pure $ subst a e2' e1''
      _ -> pure $ App e1' e2'

  Lam bnd -> do
    Tuple a e <- unbind_ bnd
    e' <- red e
    case e of
      -- look for an eta-reduction
      App e1 (Var b) | b == a && not (S.member a (fvSet e1)) -> pure e1
      _ -> pure $ Lam (bind_ a e')

  a@(Var _) -> pure a


x = string2Name "x" :: Name Exp
y = string2Name "y" :: Name Exp
z = string2Name "z" :: Name Exp
s = string2Name "s" :: Name Exp

lam :: Name Exp -> Exp -> Exp
lam a b = Lam (bind_ a b)

zero_  = lam s (lam z (Var z)) :: Exp
one_   = lam s (lam z (App (Var s) (Var z))) :: Exp
two_   = lam s (lam z (App (Var s) (App (Var s) (Var z)))) :: Exp
three_ = lam s (lam z (App (Var s) (App (Var s) (App (Var s) (Var z))))) :: Exp

plus :: Exp
plus = lam x (lam y (lam s (lam z (App (App (Var x) (Var s)) (App (App (Var y) (Var s)) (Var z))))))

true_ = lam x (lam y (Var x)) :: Exp
false_ = lam x (lam y (Var y)) :: Exp
if_ :: Exp -> Exp -> Exp -> Exp
if_ a b c = (App (App a b) c)


main :: Effect Unit
main = do
  -- \x.x == \y.y
  assert' "a1" $ lam x (Var x) `aeq` lam y (Var y)
  -- \x.y /= \x.x
  assert' "a2" $ not (lam x (Var y) `aeq` lam x (Var x))
  -- \x.(\y.x) (\y.y) == \y.y
  assertM "be1" $ lam x (App (lam y (Var x)) (lam y (Var y))) =~ (lam y (Var y))
  -- \x. f x === f
  assertM "be2" $ lam x (App (Var y) (Var x)) =~ Var y
  assertM "be3" $ if_ true_ (Var x) (Var y) =~ Var x
  assertM "be4" $ if_ false_ (Var x) (Var y) =~ Var y
  assertM "be5" $ App (App plus one_) two_ =~ three_
  where
  assertM msg c =
    if (runFreshM c)
    then pure unit
    else assert' msg false
