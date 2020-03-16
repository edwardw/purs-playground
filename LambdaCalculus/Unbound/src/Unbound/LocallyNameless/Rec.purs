module Unbound.LocallyNameless.Rec where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Monoid.Conj (Conj(..))
import Unbound.LocallyNameless.Alpha (class Alpha, close, genericACompare, genericAeq, genericClose, genericFreshen, genericFvAny, genericIsPat, genericIsTerm, genericLFreshen, genericNamePatFind, genericNthPatFind, genericOpen, genericSwaps, incrLevelCtx, initialCtx, isPat, namePatFind, nthPatFind, open, patternCtx)
import Unbound.LocallyNameless.Bind (Bind(..))


newtype Rec p = Rec p

derive instance eqRec :: Eq p => Eq (Rec p)
derive instance genericRec :: Generic (Rec p) _

instance showRec :: Show p => Show (Rec p) where
  show (Rec p) = "[" <> show p <> "]"


newtype TRec p = TRec (Bind (Rec p) Unit)

derive instance genericTRec :: Generic (TRec p) _

instance showTRec :: Show p => Show (TRec p) where
  show (TRec (B r _)) = show r


instance alphaRec :: Alpha p => Alpha (Rec p) where
  isTerm _             = Conj false
  isPat (Rec p)        = isPat p

  nthPatFind (Rec p)   = nthPatFind p
  namePatFind (Rec p)  = namePatFind p

  open ctx b (Rec p)   = Rec $ open (incrLevelCtx ctx) b p
  close ctx b (Rec p)  = Rec $ close (incrLevelCtx ctx) b p

  aeq' ctx x y         = genericAeq ctx x y
  fvAny' ctx nfn x     = genericFvAny ctx nfn x
  isEmbed _            = false
  swaps' ctx perm x    = genericSwaps ctx perm x
  freshen' ctx x       = genericFreshen ctx x
  lfreshen' ctx a cont = genericLFreshen ctx a cont
  acompare' ctx x y    = genericACompare ctx x y


instance alphaTRec :: Alpha p => Alpha (TRec p) where
  -- Try to write it in point-free style, the compiler will complain.
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


-- | Constructor for recursive patterns.
rec :: forall p. Alpha p => p -> Rec p
rec p = Rec $ close (patternCtx initialCtx) (namePatFind p) p

-- | Destructor for recursive patterns.
unrec :: forall p. Alpha p => Rec p -> p
unrec r@(Rec p) = open (patternCtx initialCtx) (nthPatFind p) p
