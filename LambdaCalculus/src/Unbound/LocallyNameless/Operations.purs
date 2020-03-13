module Unbound.LocallyNameless.Operations where

import Prelude
import Data.Array ((:))
import Data.Const (Const(..))
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid.Endo (Endo(..))
import Data.Set (Set)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Data.Typeable (class Typeable, typeOf)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))
import Unbound.LocallyNameless.Alpha (class Alpha, acompare', aeq', close, freshen', fvAny', initialCtx, lfreshen', namePatFind, nthPatFind, open, patternCtx, swaps')
import Unbound.LocallyNameless.Bind (Bind(..))
import Unbound.LocallyNameless.Embed (Embed(..))
import Unbound.LocallyNameless.Fresh (class Fresh)
import Unbound.LocallyNameless.LFresh (class LFresh)
import Unbound.LocallyNameless.Name (AnyName(..), Name)
import Unbound.PermM (Perm)
import Unsafe.Coerce (unsafeCoerce)


-- | `aeq t1 t2` returns true iff `t1` and `t2` are alpha-equivalent terms.
aeq :: forall a. Alpha a => a -> a -> Boolean
aeq = aeq' initialCtx


-- | An alpha-respecting total order on terms involving binders.
acompare :: forall a. Alpha a => a -> a -> Ordering
acompare = acompare' initialCtx


-- | Returns a fold over any names in a term `a`.
fvAny :: forall a f
       . Alpha a => Contravariant f => Applicative f
      => (AnyName -> f AnyName) -> a -> f a
fvAny = fvAny' initialCtx


-- | Returns the free `b` variables of a term `a`.
fv :: forall a f b
    . Alpha a => Typeable b => Contravariant f => Applicative f
   => (Name b -> f (Name b)) -> a -> f a
fv = fvAny <<< justFiltered f
  where
  f :: AnyName -> Maybe (Name b)
  f (AnyName (Tuple t v)) =
    if t == typeOf (Proxy :: Proxy (Name b))
    then Just $ unsafeCoerce v
    else Nothing


-- | Returns the set of free `b` variables of a term `a`.
fvSet :: forall a b. Alpha a => Typeable b => a -> Set (Name b)
fvSet = S.fromFoldable <<< toArrayOf fv


-- | Freshen a pattern by replacing all old binding names with new fresh
-- | names, returning a new pattern and a `Perm AnyName` specifying how
-- | names were replaced.
freshen :: forall p m. Alpha p => Fresh m => p -> m (Tuple p (Perm AnyName))
freshen = freshen' (patternCtx initialCtx)


-- | Locally freshen a pattern, replacing all binding names with new names that
-- | are not already in scope. The second argument is a continuation, which
-- | takes the renamed term and a permutation that specifies how the pattern has
-- | been renamed. The resulting computation will be run with the in-scope set
-- | extended by the names just generated.
lfreshen :: forall p m b. Alpha p => LFresh m => p -> (p -> Perm AnyName -> m b) -> m b
lfreshen = lfreshen' (patternCtx initialCtx)


-- | Apply the given permutation of variable names to the given term.
swaps :: forall t. Alpha t => Perm AnyName -> t -> t
swaps = swaps' initialCtx


-- | Closes over the variables of pattern `p` in the term `t`.
bind_ :: forall p t. Alpha p => Alpha t => p -> t -> Bind p t
bind_ p t = B p (close initialCtx (namePatFind p) t)


-- | `unbind b` lets you descend beneath a binder `b :: Bind p t` by returning
-- | the pair of the pattern `p` and the term `t` where the variables in the
-- | pattern have been made globally fresh in the freshness monad `m`.
unbind_ :: forall p t m. Alpha p => Alpha t => Fresh m => Bind p t -> m (Tuple p t)
unbind_ (B p t) = do
  Tuple p' _ <- freshen p
  pure $ Tuple p' (open initialCtx (nthPatFind p') t)

-- | `lunbind` opens a binding in an `LFresh` monad, ensuring that the names
-- | chosen for the binders are locally fresh.  The components of the binding
-- | are passed to a continuation, and the resulting monadic action is run in
-- | a context extended to avoid choosing new names which are the same as the
-- | ones chosen for this binding.
lunbind
  :: forall m p t c
   . LFresh m => Alpha p => Alpha t
  => Bind p t -> (Tuple p t -> m c) -> m c
lunbind (B p t) cont =
  lfreshen p $ \p' _ -> cont $ Tuple p' (open initialCtx (nthPatFind p') t)


embed :: forall e. e -> Embed e
embed e = Embed e


unembed :: forall e. Embed e -> e
unembed (Embed e) = e



--- Internal

type Getting r s a = (a -> Const r a) -> s -> Const r s

type Fold s a = forall f. Contravariant f => Applicative f => (a -> f a) -> s -> f s

getConst :: forall a b. Const a b -> a
getConst (Const x) = x

appEndo :: forall a. Endo (->) a -> a -> a
appEndo (Endo f) x = f x

toArrayOf :: forall s a. Fold s a -> s -> Array a
toArrayOf l = foldrOf l (:) []

foldrOf :: forall r s a. Getting (Endo (->) r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf l f z = map (flip appEndo z) (foldMapOf l (Endo <<< f))

foldMapOf :: forall r s a. Getting r s a -> (a -> r) -> s -> r
foldMapOf l f = getConst <<< l (Const <<< f)

justFiltered :: forall a b. (a -> Maybe b) -> Fold a b
justFiltered p bfb x =
  case p x of
    Just b  -> cmap (unsafePartial (fromJust <<< p)) (bfb b)
    Nothing -> pure x
