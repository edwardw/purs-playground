-- From the original Unbound library examples:
--    https://github.com/sweirich/replib/blob/master/Unbound/Examples/Issue28.hs

module Issue28 where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Typeable (class Typeable, mkTyRep)
import Unbound.LocallyNameless (class Alpha, class Subst, Bind, Embed, Name, Rec, Shift(..), SubstName(..), bind_, embed, genericACompare, genericAeq, genericClose, genericFreshen, genericFvAny, genericIsPat, genericIsTerm, genericLFreshen, genericNamePatFind, genericNthPatFind, genericOpen, genericSubst, genericSubsts, genericSwaps, rec, s2n)


type Var = Name Term

data Term
  = V Var
  | Unit
  | Pi (Bind (Tuple Var (Embed Term)) Term)
  | LetRec (Bind (Rec Decl) Term)

data Decl =
  -- a recursive declaration x : A = m
  -- where x may occur in m but not in A
  Decl Var (Shift (Embed Term)) (Embed Term)

derive instance genericTerm :: Generic Term _
derive instance genericDecl :: Generic Decl _

instance showTerm :: Show Term where
  show = case _ of
    V x      -> "V " <> show x
    Unit     -> "Unit"
    Pi p     -> "Pi " <> show p
    LetRec r -> "LetRec " <> show r

instance showDecl :: Show Decl where
  show (Decl x a m) = "Decl {declVar = "
                   <> show x
                   <> ", declClass = "
                   <> show a
                   <> ", declVal = "
                   <> show m

instance typeableTerm :: Typeable Term where
  typeOf _ = mkTyRep "Issue28" "Term"

instance typeableDecl :: Typeable Decl where
  typeOf _ = mkTyRep "Issue28" "Decl"


instance alphaTerm :: Alpha Term where
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


instance alphaDecl :: Alpha Decl where
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


instance substDecl :: Subst Term Decl where
  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst n u x   = genericSubst n u x
  substs ss x   = genericSubsts ss x


instance substTerm :: Subst Term Term where
  isvar = case _ of
    V x -> Just $ SubstName identity x
    _   -> Nothing

  isCoerceVar _ = Nothing
  subst n u x   = genericSubst n u x
  substs ss x   = genericSubsts ss x



v :: Var
v = s2n "x"

letrec :: Decl -> Term -> Term
letrec d e = LetRec $ bind_ (rec d) e

decl :: Var -> Term -> Term -> Decl
decl x klass e = Decl x (Shift (embed klass)) (embed e)

m0 :: Term
m0 = letrec (decl v Unit Unit) Unit

m1 :: Term
m1 = letrec (decl v (V v) (V v)) (V v)

m2 :: Term
m2 = Pi (bind_ (Tuple v (embed Unit)) m1)


-- ```
-- > m1
-- LetRec (<[Decl {declVar = x, declClass = {{V x}}, declVal = {V 0@0}]> V 0@0)
-- > subst v Unit m1
-- LetRec (<[Decl {declVar = x, declClass = {{Unit}}, declVal = {V 0@0}]> V 0@0)
-- > m2
-- Pi (<(Tuple x {Unit})> LetRec (<[Decl {declVar = x, declClass = {{V 0@0}}, declVal = {V 0@0}]> V 0@0))
-- ```
