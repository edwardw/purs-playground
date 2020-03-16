module STLC where

import Prelude
import Data.Array ((:))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), lookup)
import Data.Typeable (class Typeable, mkTyRep)
import Effect (Effect)
import Test.Assert (assert')
import Unbound.LocallyNameless (class Alpha, class Subst, Bind, LFreshM, Name, SubstName(..), aeq, bind_, genericACompare, genericAeq, genericClose, genericFreshen, genericFvAny, genericIsPat, genericIsTerm, genericLFreshen, genericNamePatFind, genericNthPatFind, genericOpen, genericSubst, genericSubsts, genericSwaps, lfresh, lunbind, runLFreshM, s2n, subst)


data Ty = TInt | TUnit | Arr Ty Ty

derive instance eqTy :: Eq Ty
derive instance genericTy :: Generic Ty _

instance showTy :: Show Ty where
  show x = genericShow x

instance typeableTy :: Typeable Ty where
  typeOf _ = mkTyRep "STLC" "Ty"


data Exp
  = Lit Int
  | Var (Name Exp)
  | Lam (Bind (Name Exp) Exp)
  | App Exp Ty Exp
  | EUnit

derive instance genericExp :: Generic Exp _

instance showExp :: Show Exp where
  show exp = genericShow exp

instance typeableExp :: Typeable Exp where
  typeOf _ = mkTyRep "STLC" "Exp"


instance alphaTy :: Alpha Ty where
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


instance alphaExp :: Alpha Exp where
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


instance substTy :: Subst Exp Ty where
  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst n u x   = genericSubst n u x
  substs ss x   = genericSubsts ss x


instance substExp :: Subst Exp Exp where
  isvar = case _ of
    Var x -> Just $ SubstName identity x
    _     -> Nothing

  isCoerceVar _ = Nothing
  subst n u x   = genericSubst n u x
  substs ss x   = genericSubsts ss x


type Ctx = Array (Tuple (Name Exp) Ty)

type M a = LFreshM a


-- A typechecker for STLC terms
tc :: Ctx -> Exp -> Ty -> M Boolean
tc g (Var n) ty = case lookup n g of
  Just ty' -> pure $ ty == ty'
  Nothing  -> pure false

tc g (Lam bnd) (Arr t1 t2) = do
  lunbind bnd \(Tuple x e) ->
    tc (Tuple x t1 : g) e t2

tc g (App e1 t1 e2) t2 = do
  b1 <- tc g e1 (Arr t1 t2)
  b2 <- tc g e2 t1
  pure $ b1 && b2

tc g EUnit TUnit  = pure true
tc g (Lit _) TInt = pure true
tc g _ _          = pure false

-- beta-eta equivalence, from Karl Crary's ATTAPL chapter
-- assumes both terms type check
algeq :: Exp -> Exp -> Ty -> M Boolean
algeq e1 e2 TInt = do
  e1' <- wh e1
  e2' <- wh e2
  patheq e1' e2'

algeq e1 e2 TUnit = pure true

algeq e1 e2 (Arr t1 t2) = do
  x <- lfresh (s2n "x")
  algeq (App e1 t1 (Var x)) (App e2 t1 (Var x)) t2

-- path equivalence (for terms in weak-head normal form)
patheq :: Exp -> Exp -> M Boolean
patheq (Var x) (Var y) | x == y = pure true
patheq (Lit x) (Lit y) | x == y = pure true
patheq (App e1 ty e2) (App e1' ty' e2') | ty == ty' = do
  b1 <- patheq e1 e1'
  b2 <- algeq e2 e2' ty
  pure $ b1 && b2
patheq _ _ = pure false

wh :: Exp -> M Exp
wh (App e1 ty e2) = do
  e1' <- wh e1
  case e1' of
    Lam bnd ->
      lunbind bnd $ \(Tuple x e1'') ->
      wh (subst x e2 e1'')
    _ -> pure $ App e1' ty e2
wh e = pure e


name1 = s2n "x" :: Name Exp
name2 = s2n "y" :: Name Exp

main :: Effect Unit
main = do
  -- \x.x === \y.y
  assert' "a1" $ Lam (bind_ name1 (Var name1)) `aeq` Lam (bind_ name2 (Var name2))
  -- \x.y /= \x.x
  assert' "a2" $ not (Lam (bind_ name1 (Var name2)) `aeq` Lam (bind_ name1 (Var name1)))
  -- [] ⊢ \x. x : () -> ()
  assertM "tc1" $ tc [] (Lam (bind_ name1 (Var name1))) (Arr TUnit TUnit)
  -- [] ⊢ \x. x ()  : (Unit -> Int) -> Int
  assertM "tc2" $ tc []
    (Lam (bind_ name1 (App (Var name1) TUnit EUnit)))
    (Arr (Arr TUnit TInt) TInt)
  -- \x. x  === \x. () :: Unit -> Unit
  assertM "be1" $
    algeq (Lam (bind_ name1 (Var name1)))
          (Lam (bind_ name2 EUnit))
          (Arr TUnit TUnit)
  -- \x. f x === f :: Int -> Int
  assertM "be2" $
    algeq (Lam (bind_ name1 (App (Var name2) TInt (Var name1))))
          (Var name2)
          (Arr TInt TInt)
  where
  assertM msg c =
    if (runLFreshM c)
    then pure unit
    else assert' msg false
