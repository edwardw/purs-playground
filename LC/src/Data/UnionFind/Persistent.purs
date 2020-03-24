-- | A faithful (I think) implementation of persistent union find data structure
-- | according to the paper
-- |
-- |    A Persistent Union-Find Data Structure
-- |    by Sylvain Conchon et al.
-- |
-- | With regard to the API, there are three differences between this one and
-- | the traditional imperative version:
-- |
-- |    1. the presence of data structure representing the collection of points
-- |    2. a new `create` function to create an empty collection
-- |    3. the `union` returns the updated collection, thus makes it
-- |       *persistent*.
-- |
-- | It is puzzling that the paper invented its own persistent array while the
-- | persistent collection data structures are prevalent in FP languages'
-- | standard libraries. My main takeaway from the paper is the pattern of
-- | hiding a persistent data structure behind an mutable reference and:
-- |
-- |    - takes that structure as argument and updates it in place to mimic the
-- |      semantic of imperative counterpart
-- |    - or, takes the structure as argument *and* also returns an updated
-- |      version to make it persistent
-- |
-- | This could be the so-called *semi-persistent*.
module Data.UnionFind.Persistent
  ( Union
  , Info
  , Rank
  , create
  , fresh
  , find
  , union
  , equivalent
  , isRepresentative
  ) where


import Prelude
import Data.Array as A
import Data.Tuple (Tuple(..))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)


type Rank = Int
type Point a = Int
type Info a =
  { elem :: a
  , parent :: Int
  , rank :: Rank
  }
type Union a = Ref (Array (Info a))



repr :: forall a. Point a -> Union a -> Point a
repr p u = unsafePerformEffect do
  arr <- Ref.read u
  let info = unsafePartial (A.unsafeIndex arr p)
  if p == info.parent then
    pure p
  else do
    let info' = unsafePartial (A.unsafeIndex arr info.parent)
        p''   = repr info'.parent u
    -- path compression
    when (info.parent /= p'')
         (let newinfo = info { parent = p'' }
          in Ref.modify_ (\a -> A.updateAtIndices [Tuple p newinfo] a) u
         )
    pure p''



--------------------------------------------------------------------------------
-- API -------------------------------------------------------------------------
--------------------------------------------------------------------------------


create :: forall a. Union a
create = unsafePerformEffect (Ref.new [])


fresh :: forall a. a -> Union a -> Point a
fresh x u = unsafePerformEffect do
  arr <- Ref.read u
  let p = A.length arr
      info = { elem: x, parent: p, rank: 0 }
  Ref.modify_ (\a -> A.snoc a info) u
  pure p


find :: forall a. Eq a => Point a -> Union a -> a
find p u = unsafePerformEffect do
  arr <- Ref.read u
  let root = repr p u
      info = unsafePartial (A.unsafeIndex arr root)
  pure info.elem


union :: forall a. Eq a => (a -> a -> a) -> Point a -> Point a -> Union a -> Union a
union f px py u =
  if rootx == rooty then u
  else unsafePerformEffect do
    arr <- Ref.read u
    let x = unsafePartial (A.unsafeIndex arr rootx)
        y = unsafePartial (A.unsafeIndex arr rooty)
        Tuple x' y'
          = if x.rank > y.rank then
              Tuple (x { elem = f x.elem y.elem })
                    (y { parent = rootx })
            else if y.rank > x.rank then
              Tuple (x { parent = rooty })
                    (y { elem = f x.elem y.elem })
            else
              Tuple (x { elem = f x.elem y.elem, rank = x.rank + 1 })
                    (y { parent = rootx })
        arr' = A.updateAtIndices [Tuple rootx x', Tuple rooty y'] arr
    Ref.new arr'
  where
  rootx = repr px u
  rooty = repr py u


equivalent :: forall a. Eq a => Point a -> Point a -> Union a -> Boolean
equivalent px py u = (repr px u) == (repr py u)


isRepresentative :: forall a. Point a -> Union a -> Boolean
isRepresentative p u = unsafePerformEffect do
  arr <- Ref.read u
  let info = unsafePartial (A.unsafeIndex arr p)
  pure (p == info.parent)
