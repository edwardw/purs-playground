module Unbound.Rebind where

import Prelude
import Data.Generic.Rep (class Generic, from)
import Data.Monoid.Conj (Conj(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Undefined (undefined)
import Unbound.Alpha (class Alpha, class GenericAlpha, acompare', aeq', close, freshen', fvAny', gnamePatFind, gnthPatFind, incrLevelCtx, isPat, isTermCtx, lfreshen', open, swaps')


data Rebind p1 p2 = Rebind p1 p2

derive instance eqRebind :: (Eq p1, Eq p2) => Eq (Rebind p1 p2)
derive instance genericRebind :: Generic (Rebind p1 p2) _

instance showRebind :: (Show p1, Show p2) => Show (Rebind p1 p2) where
  show (Rebind p1 p2) = "(<<" <> show p1 <> ">> " <> show p2 <> ")"


instance alphaRebind
  :: (Alpha p1, Alpha p2, Generic (Rebind p1 p2) rep, GenericAlpha rep)
  => Alpha (Rebind p1 p2) where

  isTerm _ = Conj false

  isPat (Rebind p1 p2) = isPat p1 <> isPat p2

  swaps' ctx perm (Rebind p1 p2) =
    Rebind (swaps' ctx perm p1)
           (swaps' (incrLevelCtx ctx) perm p2)

  freshen' ctx (Rebind p1 p2) =
    if isTermCtx ctx
    then undefined
    else do
      Tuple p1' perm1 <- freshen' ctx p1
      Tuple p2' perm2 <- freshen' (incrLevelCtx ctx) (swaps' (incrLevelCtx ctx) perm1 p2)
      pure $ Tuple (Rebind p1' p2') (perm1 <> perm2)

  lfreshen' ctx (Rebind p1 p2) cont =
    if isTermCtx ctx
    then undefined
    else
      lfreshen' ctx p1 $ \p1' perm1 ->
      lfreshen' (incrLevelCtx ctx) (swaps' (incrLevelCtx ctx) perm1 p2) $ \p2' perm2 ->
      cont (Rebind p1' p2') (perm1 <> perm2)

  aeq' ctx (Rebind p1 p2) (Rebind q1 q2) =
    aeq' ctx p1 q1 && aeq' (incrLevelCtx ctx) p2 q2

  fvAny' ctx nfn (Rebind p1 p2) =
    Rebind <$> fvAny' ctx nfn p1
           <*> fvAny' (incrLevelCtx ctx) nfn p2

  open ctx b (Rebind p1 p2) = Rebind (open ctx b p1) (open (incrLevelCtx ctx) b p2)
  close ctx b (Rebind p1 p2) = Rebind (close ctx b p1) (close (incrLevelCtx ctx) b p2)

  acompare' ctx (Rebind p1 p2) (Rebind q1 q2) =
    acompare' ctx p1 q1 <> acompare' (incrLevelCtx ctx) p2 q2

  isEmbed _ = false

  nthPatFind = gnthPatFind <<< from

  namePatFind = gnamePatFind <<< from
