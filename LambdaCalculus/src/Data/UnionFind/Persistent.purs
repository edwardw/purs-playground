-- | An (untested) implementation of the paper version of persistent union find
-- | data structure.
-- |
-- | There are four differences in API between the imperative and the persistent
-- | union find data structure:
-- |
-- |    - the three differences described in the paper version
-- |    - the `fresh` takes no argument and returns an empty collection
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
module Data.UnionFind.Persistent
  ( fresh
  , findOrInsert
  , union
  , equivalent
  , isRepresentative
  ) where

import Prelude
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)



type Rank = Int
type Point a = Tuple Rank a
type Union a = Ref (Map a (Point a))



repr :: forall a. Eq a => Ord a => a -> Union a -> Maybe (Point a)
repr x u = unsafePerformEffect do
  m <- Ref.read u
  case M.lookup x m of
    p@(Just (Tuple _ y)) ->
      if x == y then pure p
      else do
        let p' = repr y u
        case p' of
          Just root@(Tuple _ y') ->
            -- path compression
            when (y /= y') (Ref.modify_ (\c -> M.insert x root c) u)
          Nothing -> pure unit
        pure p'
    Nothing -> pure Nothing


--------------------------------------------------------------------------------
-- API -------------------------------------------------------------------------
--------------------------------------------------------------------------------


fresh :: forall a. Union a
fresh = unsafePerformEffect (Ref.new M.empty)


findOrInsert :: forall a. Eq a => Ord a => a -> Union a -> a
findOrInsert x u = unsafePerformEffect $
  case repr x u of
    Just (Tuple _ y)  -> pure y
    Nothing -> do
      let px = Tuple 0 x
      Ref.modify_ (\m -> M.insert x px m) u
      pure x


union :: forall a. Eq a => Ord a => a -> a -> Union a -> Union a
union x y u = unsafePerformEffect $
  case Tuple (repr x u) (repr y u) of
    Tuple (Just rootx@(Tuple rx dx))
          (Just rooty@(Tuple ry dy)) -> do
      m <- Ref.read u
      let m' =  if rx > ry then
                  M.insert dy rootx m
                else if ry > rx then
                  M.insert dx rooty m
                else do
                  let n = M.insert dx (Tuple (rx+1) dx) m
                  M.insert dy (Tuple ry dx) n
      Ref.new m'

    _ -> pure u


equivalent :: forall a. Eq a => Ord a => a -> a -> Union a -> Boolean
equivalent x y u = case Tuple px py of
  Tuple (Just (Tuple _ x'))
        (Just (Tuple _ y')) -> x' == y'
  _                         -> false
  where
  px = repr x u
  py = repr y u


isRepresentative :: forall a. Eq a => Ord a => a -> Union a -> Boolean
isRepresentative x u = case repr x u of
  Just (Tuple _ x') -> x == x'
  Nothing           -> false
