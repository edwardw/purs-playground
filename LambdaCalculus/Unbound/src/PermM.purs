module Unbound.PermM where

import Prelude hiding (compose)
import Data.Array as A
import Data.Foldable (all, foldr)
import Data.Map (Map)
import Data.Map as M
import Data.Set (Set)
import Data.Set as S
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (Tuple(..), uncurry)


newtype Perm a = Perm (Map a a)

derive instance eqPerm :: Ord a => Eq (Perm a)

instance showPerm :: Show a => Show (Perm a) where
  show (Perm p) = show p


permValid :: forall a. Ord a => Perm a -> Boolean
permValid (Perm p) = all (\v -> M.member v p) (M.values p)

apply :: forall a. Ord a => Perm a -> a -> a
apply (Perm p) x = fromMaybe x $ M.lookup x p

single :: forall a. Ord a => a -> a -> Perm a
single x y =
  if x == y
  then Perm M.empty
  else Perm $ M.fromFoldable [Tuple x y, Tuple y x]

empty :: forall a. Perm a
empty = Perm M.empty


compose :: forall a. Ord a => Perm a -> Perm a -> Perm a
compose (Perm b) (Perm a) =
  Perm (M.union (map (\v -> fromMaybe v $ M.lookup v b) a)
                (M.difference b a))

instance semigroupPerm :: Ord a => Semigroup (Perm a) where
  append = compose

instance monoidPerm :: Ord a => Monoid (Perm a) where
  mempty = Perm M.empty


isid :: forall a. Ord a => Perm a -> Boolean
isid (Perm p) =
  all (\(Tuple k v) -> k == v) (M.toUnfoldable p :: Array (Tuple a a))


join :: forall a. Ord a => Perm a -> Perm a -> Maybe (Perm a)
join (Perm p1) (Perm p2) =
  let overlap = M.intersectionWith (==) p1 p2 in
  if all identity (M.values overlap)
  then Just <<< Perm $ M.union p1 p2
  else Nothing


support :: forall a. Ord a => Perm a -> Set a
support (Perm p) = S.filter (\k -> M.lookup k p /= Just k) (M.keys p)


restrict :: forall a. Ord a => Perm a -> Set a -> Perm a
restrict (Perm p) s = Perm $ M.filterKeys (\k -> not $ S.member k s) p


data PartialPerm a = PP (Map a a) (Map a a)

instance showPartialPerm :: Show a => Show (PartialPerm a) where
  show (PP p1 p2) = "PP " <> show p1 <> " " <> show p2


emptyPP :: forall a. PartialPerm a
emptyPP = PP M.empty M.empty

extendPP :: forall a. Ord a => a -> a -> PartialPerm a -> Maybe (PartialPerm a)
extendPP x y pp@(PP mfwd mrev)
  | Just y' <- M.lookup x mfwd = if y == y' then Just pp
                                            else Nothing
  | Just x' <- M.lookup y mrev = if x == x' then Just pp
                                            else Nothing
  | otherwise = Just $ PP (M.insert x y mfwd) (M.insert y x mrev)

ppToPerm :: forall a. Ord a => PartialPerm a -> Perm a
ppToPerm (PP mfwd mrev) = Perm $ foldr (uncurry M.insert) mfwd
                                       (S.map (findEnd &&& identity) chainStarts)
  where
  chainStarts = M.keys mfwd `S.difference` M.keys mrev
  findEnd x =
    case M.lookup x mfwd of
      Nothing -> x
      Just x' -> findEnd x'

mkPerm :: forall a. Ord a => Array a -> Array a -> Maybe (Perm a)
mkPerm xs ys
  | A.length xs /= A.length ys = Nothing
  | otherwise =
    A.zipWith extendPP xs ys
      # foldr (>=>) pure
      # (_ $ emptyPP)
      # map ppToPerm
