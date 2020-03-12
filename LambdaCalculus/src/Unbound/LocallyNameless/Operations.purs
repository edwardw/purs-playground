module Unbound.LocallyNameless.Operations where

import Prelude
import Data.Tuple (Tuple(..))
import Unbound.LocallyNameless.Alpha (class Alpha, close, freshen', initialCtx, namePatFind, nthPatFind, open, patternCtx)
import Unbound.LocallyNameless.Bind (Bind(..))
import Unbound.LocallyNameless.Embed (Embed(..))
import Unbound.LocallyNameless.Fresh (class Fresh)
import Unbound.LocallyNameless.Name (AnyName)
import Unbound.PermM (Perm)


freshen :: forall p m. Alpha p => Fresh m => p -> m (Tuple p (Perm AnyName))
freshen = freshen' (patternCtx initialCtx)

bind_ :: forall p t. Alpha p => Alpha t => p -> t -> Bind p t
bind_ p t = B p (close initialCtx (namePatFind p) t)

unbind_ :: forall p t m. Alpha p => Alpha t => Fresh m => Bind p t -> m (Tuple p t)
unbind_ (B p t) = do
  Tuple p' _ <- freshen p
  pure $ Tuple p' (open initialCtx (nthPatFind p') t)

embed :: forall e. e -> Embed e
embed e = Embed e

unembed :: forall e. Embed e -> e
unembed (Embed e) = e
