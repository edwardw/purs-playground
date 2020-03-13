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
import Unbound.LocallyNameless.Alpha (class Alpha, aeq', close, freshen', fvAny', initialCtx, lfreshen', namePatFind, nthPatFind, open, patternCtx)
import Unbound.LocallyNameless.Bind (Bind(..))
import Unbound.LocallyNameless.Embed (Embed(..))
import Unbound.LocallyNameless.Fresh (class Fresh)
import Unbound.LocallyNameless.LFresh (class LFresh)
import Unbound.LocallyNameless.Name (AnyName(..), Name)
import Unbound.PermM (Perm)
import Unsafe.Coerce (unsafeCoerce)

aeq :: forall a. Alpha a => a -> a -> Boolean
aeq = aeq' initialCtx

freshen :: forall p m. Alpha p => Fresh m => p -> m (Tuple p (Perm AnyName))
freshen = freshen' (patternCtx initialCtx)

lfreshen :: forall p m b. Alpha p => LFresh m => p -> (p -> Perm AnyName -> m b) -> m b
lfreshen = lfreshen' (patternCtx initialCtx)

bind_ :: forall p t. Alpha p => Alpha t => p -> t -> Bind p t
bind_ p t = B p (close initialCtx (namePatFind p) t)

unbind_ :: forall p t m. Alpha p => Alpha t => Fresh m => Bind p t -> m (Tuple p t)
unbind_ (B p t) = do
  Tuple p' _ <- freshen p
  pure $ Tuple p' (open initialCtx (nthPatFind p') t)

lunbind :: forall m p t c. LFresh m => Alpha p => Alpha t => Bind p t -> (Tuple p t -> m c) -> m c
lunbind (B p t) cont =
  lfreshen p $ \x _ -> cont $ Tuple x (open initialCtx (nthPatFind x) t)

fvAny :: forall a f
       . Alpha a
      => Contravariant f
      => Applicative f
      => (AnyName -> f AnyName) -> a -> f a
fvAny = fvAny' initialCtx

fv :: forall a f b
    . Alpha a
   => Typeable b
   => Contravariant f
   => Applicative f
   => (Name b -> f (Name b)) -> a -> f a
fv = fvAny <<< justFiltered f
  where
  f :: forall c. Typeable c => AnyName -> Maybe (Name c)
  f (AnyName (Tuple t v)) =
    if t == typeOf (Proxy :: Proxy (Name b))
    then Just $ unsafeCoerce v
    else Nothing

fvSet :: forall a b. Alpha a => Typeable b => a -> Set (Name b)
fvSet = S.fromFoldable <<< toArrayOf fv

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
