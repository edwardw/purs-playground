-- | A faithful (I think) implementation of persistent union find according to
-- |
-- |    A Persistent Union-Find Data Structure
-- |    by Sylvain Conchon et al.
-- |
-- | Three main differences from the traditional imperative union find IMO:
-- |
-- |    1. the presence of data structure representing the collection of points
-- |       in the API.
-- |    2. the semantic of `find` is actually `findOrInsert`.
-- |    3. the `union` returns the updated collection, thus makes it
-- |       *persistent*.
module Data.UnionFind.PersistentPaperVersion where

import Prelude
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Data.Array as A
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

data PArray a
  = Arr (Array a)
  | Diff Int a (PArrayRef a)

type PArrayRef a =
  Ref (PArray a)


instance showPArray :: Show a => Show (PArray a) where
  show = case _ of
    Arr a -> "Arr " <> show a
    Diff ix v ref -> unsafePerformEffect do
      a <- Ref.read ref
      pure $ "Diff " <> show ix <> " " <> show v <> " " <> show a


init :: forall a. Int -> (Int -> a) -> PArrayRef a
init n f = A.range 0 (n-1)
  # map f
  # Arr
  # Ref.new
  # unsafePerformEffect


get :: forall a. PArrayRef a -> Int -> a
get ref ix = case unsafePerformEffect $ Ref.read ref of
  Arr a         -> unsafePartial $ A.unsafeIndex a ix
  Diff i v ref'
    | ix == i   -> v
    | otherwise -> get ref' ix


set :: forall a. PArrayRef a -> Int -> a -> PArrayRef a
set ref ix v = unsafePerformEffect $
  Ref.read ref >>= case _ of
    Arr a -> do
      let v' = unsafePartial $ A.unsafeIndex a ix
      ref' <- Ref.new <<< Arr $ A.updateAtIndices [Tuple ix v] a
      Ref.write (Diff ix v' ref') ref
      pure ref'
    Diff _ _ _ -> Ref.new $ Diff ix v ref


type UnionFind =
  { rank::   PArrayRef Int
  , parent:: PArrayRef Int
  }


create :: Int -> UnionFind
create n =
  { rank:   init n (const 0)
  , parent: init n identity
  }


-- The signature of `find` is deviated from the paper, returning the updated
-- union find data structure in addition to the description found. It shouldn't
-- have.
--
-- It is this way mainly because I didn't like to make the UnionFind structure
-- a ref to the ref, only to realize that this is one of the main points of the
-- paper.
find :: UnionFind -> Int -> Tuple UnionFind Int
find h x = Tuple (h { parent = p }) repr
  where
  go f i = case get f i of
    fi | fi == i   -> Tuple f i
    fi | otherwise ->
      let Tuple f r = go f fi
          f' = set f i r
      in Tuple f' r

  Tuple p repr = go h.parent x


union :: UnionFind -> Int -> Int -> UnionFind
union h x y = do
  let Tuple hx cx = find h x
      Tuple hy cy = find h y
  if cx == cy then h
  else do
    let rx = get hx.rank cx
        ry = get hy.rank cy
    if rx > ry then
      h { parent = set hx.parent cy cx }
    else if ry > rx then
      h { parent = set hy.parent cx cy }
    else
      { rank:   set hx.rank cx (rx + 1)
      , parent: set hx.parent cy cx
      }
