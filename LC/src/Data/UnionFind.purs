module Data.UnionFind
  ( Point
  , Link
  , fresh
  , find
  , union
  , equivalent
  , isRepresentative
  ) where

import Prelude
import Data.Tuple (Tuple(..))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)



type Weight = Int

data Link a
  = Info Weight a
  | Link (Point a)

type Point a = Ref (Link a)

instance eqLink :: Eq a => Eq (Link a) where
  eq (Info _ a) (Info _ b) = a == b
  eq (Link pa) (Link pb)   = unsafePerformEffect do
    la <- Ref.read pa
    lb <- Ref.read pb
    pure $ la == lb
  eq _ _                   = false


instance showLink :: Show a => Show (Link a) where
  show = case _ of
    Info w a -> "Info " <> show w <> " " <> show a
    Link p   -> unsafePerformEffect do
      l <- Ref.read p
      pure $ "Link -> " <> show l


fresh :: forall a. a -> Point a
fresh desc = unsafePerformEffect <<< Ref.new $ Info 1 desc


repr :: forall a. Eq a => Point a -> Point a
repr p = unsafePerformEffect do
  Ref.read p >>= case _ of
    Info _ _ -> pure p
    Link p'  -> do
      let p'' = repr p'
      l'  <- Ref.read p'
      l'' <- Ref.read p''
      when (l'' /= l') (Ref.write l' p)
      pure p''


find :: forall a. Eq a => Point a -> a
find p = unsafePerformEffect do
  Ref.read p >>= case _ of
    Info _ desc -> pure $ desc
    Link p'     -> pure $ find (repr p')


union :: forall a. Eq a => (a -> a -> a) -> Point a -> Point a -> Unit
union f p1 p2 = unsafePerformEffect do
  let p1' = repr p1
      p2' = repr p2
  l1 <- Ref.read p1'
  l2 <- Ref.read p2'
  if l1 /= l2 then
    case Tuple l1 l2 of
      Tuple (Info w1 desc1) (Info w2 desc2) ->
        if w1 >= w2 then do
          Ref.write (Link p1') p2'
          Ref.write (Info (w1+w2) (f desc1 desc2)) p1'
        else do
          Ref.write (Link p2') p1'
          Ref.write (Info (w1+w2) (f desc1 desc2)) p2'
      _ -> pure unit
  else pure unit


equivalent :: forall a. Eq a => Point a -> Point a -> Boolean
equivalent p1 p2 = unsafePerformEffect do
  l1 <- Ref.read $ repr p1
  l2 <- Ref.read $ repr p2
  pure $ l1 == l2


isRepresentative :: forall a. Point a -> Boolean
isRepresentative p = unsafePerformEffect do
  Ref.read p >>= case _ of
    Info _ _ -> pure true
    Link _   -> pure false
