module Unbound.LocallyNameless.Shift where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Monoid.Conj (Conj(..))
import Data.Tuple (Tuple(..))
import Effect.Exception.Unsafe (unsafeThrow)
import Unbound.LocallyNameless.Alpha (class Alpha, acompare', aeq', close, decrLevelCtx, fvAny', isEmbed, isTermCtx, isZeroLevelCtx, namePatFind, nthPatFind, open, swaps')


newtype Shift e = Shift e

derive instance functorShift :: Functor Shift

instance showShift :: Show e => Show (Shift e) where
  show (Shift e) = "{" <> show e <> "}"


instance alphaShift :: Alpha e => Alpha (Shift e) where
  isPat (Shift e) = if isEmbed e then mempty else Nothing

  isTerm _ = Conj false

  isEmbed (Shift e) = isEmbed e

  swaps' ctx perm (Shift e) = Shift $ swaps' (decrLevelCtx ctx) perm e

  freshen' ctx p =
    if isTermCtx ctx
    then unsafeThrow "freshen' called on a term"
    else pure $ Tuple p mempty

  lfreshen' ctx p cont =
    if isTermCtx ctx
    then unsafeThrow "freshen' called on a term"
    else cont p mempty

  aeq' ctx (Shift e1) (Shift e2) = aeq' ctx e1 e2

  fvAny' ctx nfn (Shift e) = Shift <$> fvAny' ctx nfn e

  close ctx b se@(Shift e) =
    if isTermCtx ctx
    then unsafeThrow "close on Shift"
    else if isZeroLevelCtx ctx
         then se
         else Shift $ close (decrLevelCtx ctx) b e

  open ctx b se@(Shift e) =
    if isTermCtx ctx
    then unsafeThrow "open on Shift"
    else if isZeroLevelCtx ctx
         then se
         else Shift $ open (decrLevelCtx ctx) b e

  nthPatFind (Shift e) = nthPatFind e
  namePatFind (Shift e) = namePatFind e

  acompare' ctx (Shift e1) (Shift e2) = acompare' ctx e1 e2
