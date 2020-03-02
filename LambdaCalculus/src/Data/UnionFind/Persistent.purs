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
  ( Point
  , fresh
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
import Data.Typelevel.Undefined (undefined)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)



type Rank = Int
data Point a
  = Info Rank a
  | Link a
type Union a = Ref (Map a (Point a))



repr :: forall a. Eq a => Ord a => a -> Union a -> Maybe (Tuple a (Point a))
repr x u = unsafePerformEffect do
  m <- Ref.read u
  case M.lookup x m of
    Just p@(Info _ _) -> pure (Just (Tuple x p))
    Just (Link y) -> do
      let res = repr y u
      case res of
        Just (Tuple y' _) ->
          -- path compression
          when (y /= y') (Ref.modify_ (\c -> M.insert x (Link y') c) u)
        Nothing -> pure unit
      pure res
    Nothing -> pure Nothing


--------------------------------------------------------------------------------
-- API -------------------------------------------------------------------------
--------------------------------------------------------------------------------


fresh :: forall a. Union a
fresh = unsafePerformEffect (Ref.new M.empty)


findOrInsert :: forall a. Eq a => Ord a => a -> Union a -> a
findOrInsert x u = unsafePerformEffect $
  case repr x u of
    Just (Tuple _ (Info _ desc)) -> pure desc
    Just (Tuple _ (Link _))      -> undefined -- unreachable
    Nothing -> do
      let px = Info 0 x
      Ref.modify_ (\m -> M.insert x px m) u
      pure x


union :: forall a. Eq a => Ord a => (a -> a -> a) -> a -> a -> Union a -> Union a
union f x y u = unsafePerformEffect $
  case Tuple (repr x u) (repr y u) of
    Tuple (Just (Tuple rootx (Info rankx descx)))
          (Just (Tuple rooty (Info ranky descy))) -> do
      m <- Ref.read u
      let m' =  if rankx > ranky then do
                  let n = M.insert rooty (Link rootx) m
                  M.insert rootx (Info rankx (f descx descy)) n
                else if rooty > rootx then do
                  let n = M.insert rootx (Link rooty) m
                  M.insert rooty (Info ranky (f descx descy)) n
                else do
                  let n = M.insert rooty (Link rootx) m
                  M.insert rootx (Info (rankx+1) (f descx descy)) n
      Ref.new m'

    _ -> pure u


equivalent :: forall a. Eq a => Ord a => a -> a -> Union a -> Boolean
equivalent x y u = case Tuple px py of
  Tuple (Just (Tuple x' _))
        (Just (Tuple y' _)) -> x' == y'
  _                         -> false
  where
  px = repr x u
  py = repr y u


isRepresentative :: forall a. Eq a => Ord a => a -> Union a -> Boolean
isRepresentative x u = unsafePerformEffect do
  m <- Ref.read u
  case M.lookup x m of
    Just (Info _ _) -> pure true
    Just (Link _)   -> pure false
    Nothing         -> pure false
