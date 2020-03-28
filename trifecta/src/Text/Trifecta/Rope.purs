module Text.Trifecta.Rope
  ( Rope(..)
  , rope
  , ropeS
  , Strand(..)
  , strand
  , strands
  , grabRest
  , grabLine
  ) where

import Prelude
import Data.FingerTree (FingerTree)
import Data.FingerTree as FT
import Data.Foldable (intercalate)
import Data.Lazy (force)
import Data.List ((:))
import Data.List as L
import Data.Semigroup.Reducer (class Reducer)
import Data.Semigroup.Reducer as Reducer
import Data.Sequence.Internal (class Measured, measure)
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Text.Trifecta.Delta (class HasBytes, class HasDelta, Delta, bytes, delta)
import Text.Trifecta.Util.Combinators as Util



data Strand
  = Strand String Delta
  | Skipping Delta


instance showStrand :: Show Strand where
  show = case _ of
    Strand s d -> "Strand " <> s <> " " <> show d
    Skipping d -> "Skipping " <> show d


strand :: String -> Strand
strand s = Strand s (delta s)


instance measuredDelta :: Measured Strand Delta where
  measure = case _ of
    Strand _ d -> delta d
    Skipping d -> d


instance hasDeltaStrand :: HasDelta Strand where
  delta = measure



data Rope = Rope Delta (FingerTree Delta Strand)


instance showRope :: Show Rope where
  show (Rope d t) = "Rope " <> show d <> " " <> show t


rope :: FingerTree Delta Strand -> Rope
rope r = Rope (measure r) r


-- | Construct a 'Rope' out of a single `String` strand.
ropeS :: String -> Rope
ropeS = rope <<< FT.Single <<< strand


strands :: Rope -> FingerTree Delta Strand
strands (Rope _ r) = r


grabRest
  :: forall r
   . Delta
  -> Rope
  -> r
  -> (Delta -> String -> r)
  -> r
grabRest offset input failure success =
  trim (delta l) (bytes offset - bytes l) (L.fromFoldable r)
  where
  toStr = case _ of
    Strand s _ -> s
    Skipping _ -> ""

  go offset' s strands' = success offset' (intercalate "" (s : map toStr strands'))

  trim offset' 0 (Strand s _ : xs) = go offset' s xs
  trim _       k (Strand s _ : xs) = go offset (S.drop k s) xs
  trim offset' k (Skipping p : xs) = trim (offset' <> p) k xs
  trim _       _ L.Nil             = failure

  Tuple l r = splitRopeAt offset input


-- | Split the rope in two halves, given a 'Delta' offset from the beginning.
splitRopeAt :: Delta -> Rope -> Tuple (FingerTree Delta Strand) (FingerTree Delta Strand)
splitRopeAt splitPos input = Tuple (force l) (force r)
  where
  Tuple l r = unsafePartial (FT.split (\pos -> bytes pos > bytes splitPos) $ strands input)


-- | Grab the rest of the line at a certain offset in the input `Rope`, or
-- | return a default if there is no newline left in the input. Also see
-- | `grabRest`.
grabLine
  :: forall r
   . Delta
  -> Rope
  -> r
  -> (Delta -> String -> r)
  -> r
grabLine offset input failure success =
  grabRest offset input failure (\d -> success d <<< Util.takeLine)


instance hasBytesRope :: HasBytes Rope where
  bytes t = let x = measure t :: Delta in bytes x


instance hasDeltaRope :: HasDelta Rope where
  delta = measure


instance measuredRope :: Measured Rope Delta where
  measure (Rope d _) = d


instance semigroupRope :: Semigroup Rope where
  append (Rope mx x) (Rope my y) = Rope (mx <> my) (x <> y)


instance monoidRope :: Monoid Rope where
  mempty = Rope mempty FT.Empty


instance reducerRopeRope :: Reducer Rope Rope where
  unit = identity
  snoc m c = append m (Reducer.unit c)
  cons c m = append (Reducer.unit c) m


instance reducerStrandRope :: Reducer Strand Rope where
  unit s = rope (FT.Single s)
  cons s (Rope mt t) = Rope (delta s <> mt) (s `FT.cons` t)
  snoc (Rope mt t) s = Rope (mt <> delta s) (t `FT.snoc` s)


instance reducerStringRope :: Reducer String Rope where
  unit = Reducer.unit <<< strand
  cons = Reducer.cons <<< strand
  snoc r = Reducer.snoc r <<< strand


instance reducerCharArrayRope :: Reducer (Array Char) Rope where
  unit = Reducer.unit <<< strand <<< SCU.fromCharArray
  cons = Reducer.cons <<< strand <<< SCU.fromCharArray
  snoc r = Reducer.snoc r <<< strand <<< SCU.fromCharArray
