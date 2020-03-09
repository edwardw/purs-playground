module Unbound.Embed where

import Prelude
import Data.Foldable (and)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Monoid.Conj (Conj(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Undefined (undefined)
import Unbound.Alpha (class Alpha, acompare', aeq', close, fvAny', isTerm, isTermCtx, open, swaps', termCtx)


newtype Embed t = Embed t

derive instance eqEmbed :: Eq t => Eq (Embed t)
derive instance ordEmbed :: Ord t => Ord (Embed t)
derive instance genericEmbed :: Generic (Embed t) _

instance showEmbed :: Show a => Show (Embed a) where
  show (Embed a) = "{" <> show a <> "}"


instance alphaEmbed :: Alpha t => Alpha (Embed t) where
  isPat (Embed t) = if and (isTerm t) then mempty else Nothing

  isTerm _ = Conj false

  isEmbed (Embed t) = and (isTerm t)

  swaps' ctx perm (Embed t) =
    if isTermCtx ctx
    then Embed t
    else Embed (swaps' (termCtx ctx) perm t)

  freshen' ctx p =
    if isTermCtx ctx
    then undefined
    else pure $ Tuple p mempty

  lfreshen' ctx p cont =
    if isTermCtx ctx
    then undefined
    else cont p mempty

  aeq' ctx (Embed x) (Embed y) = aeq' (termCtx ctx) x y

  fvAny' ctx nfn ex@(Embed x) =
    if isTermCtx ctx
    then pure ex
    else Embed <$> fvAny' (termCtx ctx) nfn x

  close ctx b (Embed x) =
    if isTermCtx ctx
    then undefined
    else Embed $ close (termCtx ctx) b x

  open ctx b (Embed x) =
    if isTermCtx ctx
    then undefined
    else Embed $ open (termCtx ctx) b x

  nthPatFind _ = mempty
  namePatFind _ = mempty

  acompare' ctx (Embed x) (Embed y) = acompare' (termCtx ctx) x y
