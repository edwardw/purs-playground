module Unbound.Rec where

import Prelude
import Data.Function (on)
import Data.Generic.Rep (class Generic, from, to)
import Data.Monoid.Conj (Conj(..))
import Data.Profunctor.Strong (first)
import Unbound.Alpha (class Alpha, class GenericAlpha, close, gacompare, gaeq, gclose, gfreshen, gfvAny, gisPat, gisTerm, glfreshen, gnamePatFind, gnthPatFind, gopen, gswaps, incrLevelCtx, initialCtx, isPat, namePatFind, nthPatFind, open, patternCtx)
import Unbound.Bind (Bind(..))


newtype Rec p = Rec p

derive instance eqRec :: Eq p => Eq (Rec p)
derive instance genericRec :: Generic (Rec p) _

instance showRec :: Show p => Show (Rec p) where
  show (Rec p) = "[" <> show p <> "]"


newtype TRec p = TRec (Bind (Rec p) Unit)

derive instance genericTRec :: Generic (TRec p) _

instance showTRec :: Show p => Show (TRec p) where
  show (TRec (B r _)) = show r


instance alphaRec
  :: (Alpha p, Generic (Rec p) rep, GenericAlpha rep)
  => Alpha (Rec p) where

  isTerm _ = Conj false
  isPat (Rec p) = isPat p

  nthPatFind (Rec p) = nthPatFind p
  namePatFind (Rec p) = namePatFind p

  open ctx b (Rec p) = Rec $ open (incrLevelCtx ctx) b p
  close ctx b (Rec p) = Rec $ close (incrLevelCtx ctx) b p

  aeq' ctx = (gaeq ctx) `on` from

  fvAny' ctx nfn = map to <<< gfvAny ctx nfn <<< from

  isEmbed _ = false

  swaps' ctx perm = to <<< gswaps ctx perm <<< from

  lfreshen' ctx a cont = glfreshen ctx (from a) (cont <<< to)

  freshen' ctx = liftM1 (first to) <<< gfreshen ctx <<< from

  acompare' ctx = gacompare ctx `on` from


instance alphaTRec
  :: (Alpha p, Generic (TRec p) rep, GenericAlpha rep)
  => Alpha (TRec p) where

  aeq' ctx = (gaeq ctx) `on` from

  fvAny' ctx nfn = map to <<< gfvAny ctx nfn <<< from

  close ctx b = to <<< gclose ctx b <<< from

  open ctx b = to <<< gopen ctx b <<< from

  isPat = gisPat <<< from

  isTerm = gisTerm <<< from

  isEmbed _ = false

  nthPatFind = gnthPatFind <<< from

  namePatFind = gnamePatFind <<< from

  swaps' ctx perm = to <<< gswaps ctx perm <<< from

  lfreshen' ctx a cont = glfreshen ctx (from a) (cont <<< to)

  freshen' ctx = liftM1 (first to) <<< gfreshen ctx <<< from

  acompare' ctx = gacompare ctx `on` from


-- | Constructor for recursive patterns.
rec :: forall p. Alpha p => p -> Rec p
rec p = Rec $ close (patternCtx initialCtx) (namePatFind p) p

-- | Destructor for recursive patterns.
unrec :: forall p. Alpha p => Rec p -> p
unrec r@(Rec p) = open (patternCtx initialCtx) (nthPatFind p) p
