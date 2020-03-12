module Unbound.LocallyNameless.Fresh where

import Prelude
import Control.Monad.Cont (class MonadCont, callCC)
import Control.Monad.State (class MonadTrans, StateT, evalState, evalStateT, get, lift, put)
import Data.Identity (Identity)
import Unbound.LocallyNameless.Name (Name(..))


class Monad m <= Fresh m where
  fresh :: forall a. Name a -> m (Name a)


newtype FreshMT m a = FreshMT (StateT Int m a)

instance functorFreshMT :: Functor m => Functor (FreshMT m) where
  map f (FreshMT s) = FreshMT (map f s)

instance applyFreshMT :: Monad m => Apply (FreshMT m) where
  apply = ap

instance applicativeFreshMT :: Monad m => Applicative (FreshMT m) where
  pure = FreshMT <<< pure

instance bindFreshMT :: Monad m => Bind (FreshMT m) where
  bind (FreshMT s) f = FreshMT $ bind s f'
    where
    f' a = case f a of FreshMT m -> m

instance monadFreshMT :: Monad m => Monad (FreshMT m)

instance monadTransFreshMT :: MonadTrans FreshMT where
  lift = FreshMT <<< lift

instance monadContFreshMT :: MonadCont m => MonadCont (FreshMT m) where
  callCC f = callCC \k -> f k

instance freshFreshMT :: Monad m => Fresh (FreshMT m) where
  fresh nm = case nm of
    Fn s _ -> FreshMT $ do
      n <- get
      put (n + 1)
      pure $ Fn s n
    Bn _ _ -> pure nm


runFreshMT :: forall m a. Functor m => FreshMT m a -> m a
runFreshMT m = contFreshMT m 0

contFreshMT :: forall m a. Functor m => FreshMT m a -> Int -> m a
contFreshMT (FreshMT s) = evalStateT s



--------------------------------------------------------------------------------
-- FreshM Monad ----------------------------------------------------------------
--------------------------------------------------------------------------------

type FreshM = FreshMT Identity

runFreshM :: forall a. FreshM a -> a
runFreshM m = contFreshM m 0

contFreshM :: forall a. FreshM a -> Int -> a
contFreshM (FreshMT s) = evalState s
