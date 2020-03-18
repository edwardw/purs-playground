module Text.Trifecta.Internal.IntervalMap
  ( Interval(..)
  , IntervalMap(..), Nd, singleton, insert
  , search, intersections, dominators
  , offset
  , IntInterval(..)
  , fromArray
  ) where

import Prelude
import Data.Array ((:))
import Data.FingerTree (FingerTree, ViewL(..))
import Data.FingerTree as FT
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldr, foldrDefault)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndexDefault, foldrWithIndexDefault)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Lazy (force)
import Data.Semigroup.Reducer (class Reducer)
import Data.Sequence.Internal (class Measured, measure)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (Tuple3)
import Effect.Exception.Unsafe (unsafeThrow)
import Partial.Unsafe (unsafePartial)



-- | A closed interval. The lower bound should be less than or equal to the
-- | higher bound.
data Interval v = Interval v v


instance showInterval :: Show v => Show (Interval v) where
  show (Interval a b) = "Interval " <> show a <> " " <> show b


instance semigroupInterval :: Ord v => Semigroup (Interval v) where
  append (Interval a b) (Interval c d) = Interval (min a c) (max b d)


instance reducerInterval :: (Ord v, Monoid v) => Reducer v (Interval v) where
  unit v = Interval v v
  cons v (Interval a b) = Interval (v <> a) (v <> b)
  snoc (Interval a b) v = Interval (a <> v) (b <> v)


instance eqInterval :: Eq v => Eq (Interval v) where
  eq (Interval a b) (Interval c d) = a == c && b == d


instance ordInterval :: Ord v => Ord (Interval v) where
  compare (Interval a b) (Interval c d) = case compare a c of
    LT -> LT
    EQ -> compare d b -- reversed to put larger intervals first
    GT -> GT


instance functorInterval :: Functor Interval where
  map f (Interval a b) = Interval (f a) (f b)


instance foldableInterval :: Foldable Interval where
  foldMap f (Interval a b) = f a <> f b
  foldr f a b = foldrDefault f a b
  foldl f a b = foldlDefault f a b


instance traversableInterval :: Traversable Interval where
  traverse f (Interval a b) = Interval <$> f a <*> f b
  sequence a = sequenceDefault a


high :: forall v. Interval v -> v
high (Interval _ i) = i

low :: forall v . Interval v -> v
low (Interval i _) = i



data Nd v a = Nd (Interval v) a


instance functorNode :: Functor (Nd v) where
  map f (Nd i x) = Nd i (f x)


instance functorWithIndexNode :: FunctorWithIndex (Interval v) (Nd v) where
  mapWithIndex f (Nd i x) = Nd i (f i x)


instance foldableNode :: Foldable (Nd v) where
  foldMap f (Nd _ x) = f x
  foldr f a b = foldrDefault f a b
  foldl f a b = foldlDefault f a b


instance foldableWithIndexNode :: FoldableWithIndex (Interval v) (Nd v) where
  foldMapWithIndex f (Nd i v) = f i v
  foldrWithIndex f a b = foldrWithIndexDefault f a b
  foldlWithIndex f a b = foldlWithIndexDefault f a b


instance traversableNode :: Traversable (Nd v) where
  traverse f (Nd i x) = Nd i <$> f x
  sequence a = sequenceDefault a


instance traverseWithIndexNode :: TraversableWithIndex (Interval v) (Nd v) where
  traverseWithIndex f (Nd i x) = Nd i <$> f i x



-- rightmost interval (including largest lower bound) and largest upper bound.
data IntInterval v
  = NoInterval
  | IntInterval (Interval v) v


instance semigroupIntInterval :: Ord v => Semigroup (IntInterval v) where
  append NoInterval i = i
  append i NoInterval = i
  append (IntInterval _ hi1) (IntInterval i2 hi2) =
    IntInterval i2 (max hi1 hi2)


instance monoidIntInterval :: Ord v => Monoid (IntInterval v) where
  mempty = NoInterval


instance measuredNode :: Ord v => Measured (Nd v a) (IntInterval v) where
  measure (Nd i _) = IntInterval i (high i)



-- | Map of closed intervals, possibly with duplicates.
-- | The `Foldable` and `Traversable` instances process the intervals in
-- | lexicographical order.
newtype IntervalMap v a = IntervalMap (FingerTree (IntInterval v) (Nd v a))


instance functorIntervalMap :: Functor (IntervalMap v) where
  map f (IntervalMap t) = IntervalMap (map (map f) t)


instance functorWithIndexIntervalMap :: FunctorWithIndex (Interval v) (IntervalMap v) where
  mapWithIndex f (IntervalMap t) = IntervalMap (map (mapWithIndex f) t)


instance foldableIntervalMap :: Foldable (IntervalMap v) where
  foldMap f (IntervalMap t) = foldMap (foldMap f) t
  foldr f a b = foldrDefault f a b
  foldl f a b = foldlDefault f a b


instance foldableWithIndexIntervalMap :: FoldableWithIndex (Interval v) (IntervalMap v) where
  -- Can not do
  --    foldMap (foldMapWithIndex f) t
  -- A compiler bug?
  foldMapWithIndex f (IntervalMap t) = foldMap (\(Nd i v) -> f i v) t
  foldrWithIndex f a b = foldrWithIndexDefault f a b
  foldlWithIndex f a b = foldlWithIndexDefault f a b


instance traversableIntervalMap :: Traversable (IntervalMap v) where
  traverse f (IntervalMap t) = IntervalMap <$> traverse (traverse f) t
  sequence a = sequenceDefault a


instance traversabelWithIndexIntervalMap :: TraversableWithIndex (Interval v) (IntervalMap v) where
  traverseWithIndex f (IntervalMap t) =
    IntervalMap <$> traverse (traverseWithIndex f) t


instance measuredIntervalMap :: Ord v => Measured (IntervalMap v a) (IntInterval v) where
 measure (IntervalMap t) = measure t


largerError :: forall a. a
largerError = unsafeThrow "Text.Trifecta.Internal.IntervalMap.larger: the impossible happened"



-- | `O(m log (n\/m))`.  Merge two interval maps.
-- | The map may contain duplicate intervals; entries with equal intervals are
-- | kept in the original order.
union :: forall a v. Ord v => IntervalMap v a -> IntervalMap v a -> IntervalMap v a
union (IntervalMap xs) (IntervalMap ys) = IntervalMap (merge1 xs ys)

merge1
  :: forall a v
   . Ord v
  => FingerTree (IntInterval v) (Nd v a)
  -> FingerTree (IntInterval v) (Nd v a)
  -> FingerTree (IntInterval v) (Nd v a)
merge1 xs ys = case FT.viewL xs of
  NilL -> ys
  ConsL x@(Nd i _) xs' -> (force l) <> (x `FT.cons` merge2 (force xs') (force r))
    where
    larger (IntInterval k _) = k >= i
    larger _ = largerError
    Tuple l r = unsafePartial (FT.split larger ys)

merge2
  :: forall a v
   . Ord v
  => FingerTree (IntInterval v) (Nd v a)
  -> FingerTree (IntInterval v) (Nd v a)
  -> FingerTree (IntInterval v) (Nd v a)
merge2 xs ys = case FT.viewL ys of
  NilL -> xs
  ConsL y@(Nd i _) ys' -> (force l) <> (y `FT.cons` merge1 (force r) (force ys'))
    where
    larger (IntInterval k _) = k >= i
    larger _ = largerError
    Tuple l r = unsafePartial (FT.split larger xs)


instance semigroupIntervalMap :: Ord v => Semigroup (IntervalMap v a) where
  append = union


instance monoidIntervalMap :: Ord v => Monoid (IntervalMap v a) where
  mempty = IntervalMap FT.Empty



-- | `O(n)`. Add a delta to each interval in the map
offset :: forall a v. Ord v => Monoid v => v -> IntervalMap v a -> IntervalMap v a
offset v (IntervalMap m) = IntervalMap $ map (\(Nd (Interval lo hi) a) ->
  Nd (Interval (v <> lo) (v <> hi)) a) m


-- | `O(1)`. Interval map with a single entry.
singleton :: forall a v. Ord v => Interval v -> a -> IntervalMap v a
singleton i x = IntervalMap $ FT.Single (Nd i x)


-- | `O(log n)`.  Insert an interval into a map.
-- | The map may contain duplicate intervals; the new entry will be inserted
-- | before any existing entries for the same interval.
insert :: forall a v. Ord v => v -> v -> a -> IntervalMap v a -> IntervalMap v a
insert lo hi _ m | lo > hi = m
insert lo hi x (IntervalMap t) = IntervalMap $ (force l) <> (Nd i x `FT.cons` (force r))
  where
  larger (IntInterval k _) = k >= i
  larger _ = largerError
  i = Interval lo hi
  Tuple l r = unsafePartial (FT.split larger t)


-- | `O(k log (n\/k))`.  All intervals that contain the given interval, in
-- | lexicographical order.
dominators :: forall a v. Ord v => v -> v -> IntervalMap v a -> Array (Tuple (Interval v) a)
dominators i j = intersections j i


-- | `O(k log (n\/k))`.  All intervals that contain the given point, in
-- | lexicographical order.
search :: forall a v. Ord v => v -> IntervalMap v a -> Array (Tuple (Interval v) a)
search p = intersections p p


-- | `O(k log (n\/k))`.  All intervals that intersect with the given interval,
-- | in lexicographical order.
intersections :: forall a v. Ord v => v -> v -> IntervalMap v a -> Array (Tuple (Interval v) a)
intersections lo hi (IntervalMap t) = matches (takeUntil (greater hi) t)
  where
  takeUntil f m = force (fst $ unsafePartial (FT.split f m))
  dropUntil f m = force (snd $ unsafePartial (FT.split f m))
  matches xs = case FT.viewL (dropUntil (atleast lo) xs) of
    NilL -> []
    ConsL (Nd i x) xs' -> Tuple i x : matches (force xs')

  atleast k (IntInterval _ h) = k <= h
  atleast _ _ = false

  greater k (IntInterval i _) = low i > k
  greater _ _ = false


fromArray :: forall a v. Ord v => Array (Tuple3 v v a) -> IntervalMap v a
fromArray = foldr ins mempty
  where
  ins (Tuple lo (Tuple hi (Tuple n _))) = insert lo hi n
