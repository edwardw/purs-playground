module Unbound.Operations where

import Prelude
import Data.Tuple (Tuple(..))
import Unbound.Alpha (class Alpha, close, freshen', initialCtx, namePatFind, nthPatFind, open, patternCtx)
import Unbound.Bind (Bind(..))
import Unbound.Fresh (class Fresh)
import Unbound.Name (AnyName)
import Unbound.PermM (Perm)


freshen :: forall p m. Alpha p => Fresh m => p -> m (Tuple p (Perm AnyName))
freshen = freshen' (patternCtx initialCtx)

bnd :: forall p t. Alpha p => Alpha t => p -> t -> Bind p t
bnd p t = B p (close initialCtx (namePatFind p) t)

unbnd :: forall p t m. Alpha p => Alpha t => Fresh m => Bind p t -> m (Tuple p t)
unbnd (B p t) = do
  Tuple p' _ <- freshen p
  pure $ Tuple p' (open initialCtx (nthPatFind p') t)
