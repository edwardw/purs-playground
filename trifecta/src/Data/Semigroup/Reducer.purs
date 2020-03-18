module Data.Semigroup.Reducer where

import Prelude
import Data.Foldable
import Data.Semigroup.Foldable



class Semigroup m <= Reducer c m where
  -- | Convert a value into a `Semigroup`
  unit :: c -> m

  -- | Append a value to a `Semigroup` for use in left-to-right reduction
  -- |
  -- | The default:
  -- |    snoc m = append m <<< unit
  snoc :: m -> c -> m

  -- | Prepend a value onto a `Semigroup` for use during right-to-left reduction
  -- |
  -- | The default:
  -- |    cons = append <<< unit
  cons :: c -> m -> m



-- | Apply a 'Reducer' to a `Foldable` container, after mapping the contents into
-- | a suitable form for reduction.
foldMapReduce
  :: forall a e f m
   . Foldable f
  => Monoid m
  => Reducer e m
  => (a -> e) -> f a -> m
foldMapReduce f = foldMap (unit <<< f)


-- | Apply a `Reducer` to a `Foldable` mapping each element through `unit`.
foldReduce :: forall e f m. Foldable f => Monoid m => Reducer e m => f e -> m
foldReduce = foldMap unit


-- | Apply a `Reducer` to a `Foldable1` mapping each element through `unit`
foldReduce1 :: forall e f m. Foldable1 f => Reducer e m => f e -> m
foldReduce1 = foldMap1 unit


pureUnit :: forall e f m. Applicative f => Reducer e m => e -> f m
pureUnit = pure <<< unit
