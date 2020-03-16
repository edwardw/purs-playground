module Unbound.LocallyNameless.LFresh where

import Prelude
import Control.Monad.Reader (class MonadTrans, ReaderT, ask, lift, local, runReader, runReaderT)
import Data.Identity (Identity)
import Data.List.Lazy as ZL
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as S
import Data.Typeable (class Typeable)
import Data.Typelevel.Undefined (undefined)
import Unbound.LocallyNameless.Name (AnyName, Name, makeName, mkAnyName, name2String)


class Monad m <= LFresh m where
  lfresh    :: forall a. Typeable a => Name a -> m (Name a)
  avoid     :: forall a. Array AnyName -> m a -> m a
  getAvoids :: m (Set AnyName)


newtype LFreshMT m a = LFreshMT (ReaderT (Set AnyName) m a)

unLFreshMT :: forall m a. LFreshMT m a -> ReaderT (Set AnyName) m a
unLFreshMT (LFreshMT r) = r


instance functorLFreshMT :: Functor m => Functor (LFreshMT m) where
  map f (LFreshMT r) = LFreshMT (map f r)

instance applyLFreshMT :: Monad m => Apply (LFreshMT m) where
  apply = ap

instance applicativeLFreshMT :: Monad m => Applicative (LFreshMT m) where
  pure = LFreshMT <<< pure

instance bindLFreshMT :: Monad m => Bind (LFreshMT m) where
  bind (LFreshMT r) f = LFreshMT $ bind r (unLFreshMT <<< f)

instance monadLFreshMT :: Monad m => Monad (LFreshMT m)

instance monadTransLFreshMT :: MonadTrans LFreshMT where
  lift = LFreshMT <<< lift


runLFreshMT :: forall m a. LFreshMT m a -> m a
runLFreshMT m = contLFreshMT m S.empty

contLFreshMT :: forall m a. LFreshMT m a -> Set AnyName -> m a
contLFreshMT (LFreshMT m) = runReaderT m

instance lfreshLFreshMT :: Monad m => LFresh (LFreshMT m) where
  lfresh name = LFreshMT do
    let s = name2String name
    used <- ask
    case ZL.head $ ZL.filter (\nm -> not (S.member (mkAnyName nm) used))
                             (map (makeName s) nat) of
      Just nm -> pure nm
      Nothing -> undefined -- unreachable
    where
    nat = ZL.iterate (_ + 1) 0

  avoid names = LFreshMT <<< local (S.union (S.fromFoldable names)) <<< unLFreshMT

  getAvoids = LFreshMT ask


type LFreshM = LFreshMT Identity

runLFreshM :: forall a. LFreshM a -> a
runLFreshM m = contLFreshM m S.empty

contLFreshM :: forall a. LFreshM a -> Set AnyName -> a
contLFreshM (LFreshMT r) = runReader r
