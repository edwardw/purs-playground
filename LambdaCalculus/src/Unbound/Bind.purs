module Unbound.Bind where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isNothing)
import Data.Monoid.Conj (Conj(..))
import Data.Tuple (Tuple(..))
import Effect.Exception.Unsafe (unsafeThrow)
import Unbound.Alpha (class Alpha, acompare', aeq', close, freshen', fvAny', incrLevelCtx, isPat, isTerm, lfreshen', open, patternCtx, swaps')


data Bind p t = B p t

derive instance genericBind :: Generic (Bind p t) _

instance showBind :: (Show p, Show t) => Show (Bind p t) where
  show (B p t) = "(<" <> show p <> "> " <> show t <> ")"

instance alphaBind :: (Alpha p, Alpha t) => Alpha (Bind p t) where
  aeq' ctx (B p1 t1) (B p2 t2) =
    aeq' (patternCtx ctx) p1 p2
    && aeq' (incrLevelCtx ctx) t1 t2

  fvAny' ctx nfn (B p t) =
    B <$> fvAny' (patternCtx ctx) nfn p
      <*> fvAny' (incrLevelCtx ctx) nfn t

  isPat _ = Nothing

  isTerm (B p t) = (Conj $ isNothing (isPat p)) <> isTerm t

  close ctx b (B p t) =
    B (close (patternCtx ctx) b p)
      (close (incrLevelCtx ctx) b t)

  open ctx b (B p t) =
    B (open (patternCtx ctx) b p)
      (open (incrLevelCtx ctx) b t)

  nthPatFind b = unsafeThrow $ "Binding " <> show b <> " used as a pattern"
  namePatFind b = unsafeThrow $ "Binding " <> show b <> " used as a pattern"

  swaps' ctx perm (B p t) =
    B (swaps' (patternCtx ctx) perm p)
      (swaps' (incrLevelCtx ctx) perm t)

  freshen' ctx (B p t) = do
    Tuple p' perm1 <- freshen' (patternCtx ctx) p
    Tuple t' perm2 <- freshen' (incrLevelCtx ctx) (swaps' (incrLevelCtx ctx) perm1 t)
    pure $ Tuple (B p' t') (perm1 <> perm2)

  lfreshen' ctx (B p t) cont =
    lfreshen' (patternCtx ctx) p $ \p' perm1 ->
    lfreshen' (incrLevelCtx ctx) (swaps' (incrLevelCtx ctx) perm1 t) $ \t' perm2 ->
    cont (B p' t') (perm1 <> perm2)

  acompare' ctx (B p1 t1) (B p2 t2) =
    acompare' (patternCtx ctx) p1 p2 <> acompare' (incrLevelCtx ctx) t1 t2

  isEmbed _ = false
