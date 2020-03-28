module Text.Trifecta.Delta
  ( Delta(..)
  , class HasDelta, delta
  , class HasBytes, bytes
  , prettyDelta
  , nextTab
  , rewind
  , near
  , column
  ) where

import Prelude
import Data.Enum (fromEnum)
import Data.FingerTree (FingerTree)
import Data.Foldable (foldMap)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Text.Prettyprint.Doc (Doc, annotate, pretty)
import Data.Sequence.Internal (class Measured, measure)
import Data.String as S
import Data.String.CodeUnits as SCU
import Text.Trifecta.Util.Pretty (AnsiStyle, bold, char)



class HasBytes t where
  bytes :: t -> Int


instance hasBytesString :: HasBytes String where
  bytes = S.length


instance hasBytesFingerTree :: (Monoid v, Measured a v, HasBytes v) => HasBytes (FingerTree v a) where
  bytes t = let x = measure t :: v in bytes x


data Delta
  = Columns Int Int
  | Tab Int Int Int
  | Lines Int Int Int Int
  | Directed String Int Int Int Int


derive instance ordDelta :: Ord Delta
derive instance genericDelta :: Generic Delta _


instance showDelta :: Show Delta where
  show d = genericShow d


instance hasBytesDelta :: HasBytes Delta where
  bytes = case _ of
    Columns _ b        -> b
    Tab _ _ b          -> b
    Lines _ _ b _      -> b
    Directed _ _ _ b _ -> b


instance eqDelta :: Eq Delta where
  eq = eq `on` bytes


instance semigroupDelta :: Semigroup Delta where
  append (Columns c a)        (Columns d b)         = Columns            (c + d)                            (a + b)
  append (Columns c a)        (Tab x y b)           = Tab                (c + x) y                          (a + b)
  append (Columns _ a)        (Lines l c t a')      = Lines      l       c                         (t + a)  a'
  append (Columns _ a)        (Directed p l c t a') = Directed p l       c                         (t + a)  a'
  append (Lines l c t a)      (Columns d b)         = Lines      l       (c + d)                   (t + b)  (a + b)
  append (Lines l c t a)      (Tab x y b)           = Lines      l       (nextTab (c + x) + y)     (t + b)  (a + b)
  append (Lines l _ t _)      (Lines m d t' b)      = Lines      (l + m) d                         (t + t') b
  append (Lines _ _ t _)      (Directed p l c t' a) = Directed p l       c                         (t + t') a
  append (Tab x y a)          (Columns d b)         = Tab                x (y + d)                          (a + b)
  append (Tab x y a)          (Tab x' y' b)         = Tab                x (nextTab (y + x') + y')          (a + b)
  append (Tab _ _ a)          (Lines l c t a')      = Lines      l       c                         (t + a)  a'
  append (Tab _ _ a)          (Directed p l c t a') = Directed p l       c                         (t + a)  a'
  append (Directed p l c t a) (Columns d b)         = Directed p l       (c + d)                   (t + b)  (a + b)
  append (Directed p l c t a) (Tab x y b)           = Directed p l       (nextTab (c + x) + y)     (t + b)  (a + b)
  append (Directed p l _ t _) (Lines m d t' b)      = Directed p (l + m) d                         (t + t') b
  append (Directed _ _ _ t _) (Directed p l c t' b) = Directed p l       c                         (t + t') b


-- | Increment a column number to the next tabstop.
nextTab :: Int -> Int
nextTab x = x + (8 - mod x 8)


-- | Rewind a 'Delta' to the beginning of the line.
rewind :: Delta -> Delta
rewind (Lines n _ b d)      = Lines n 0 (b - d) 0
rewind (Directed p n _ b d) = Directed p n 0 (b - d) 0
rewind _                    = Columns 0 0


-- | Should we show two things with a 'Delta' on the same line?
near :: forall s t. HasDelta s => HasDelta t => s -> t -> Boolean
near s t = rewind (delta s) == rewind (delta t)


instance monoidDelta :: Monoid Delta where
  mempty = Columns 0 0



class HasDelta t where
  delta :: t -> Delta


instance hasDeltaDelta :: HasDelta Delta where
  delta = identity


instance hasDeltaChar :: HasDelta Char where
  delta = case _ of
    '\t' -> Tab 0 0 1
    '\n' -> Lines 1 0 1 0
    c | fromEnum c <= 0x7f   -> Columns 1 1
    c | fromEnum c <= 0x7ff  -> Columns 1 2
    c | fromEnum c <= 0xffff -> Columns 1 3
    c | otherwise            -> Columns 1 4


instance hasDeltaString :: HasDelta String where
  delta = foldMap delta <<< SCU.toCharArray


instance hasDeltaFingerTree :: (Monoid v, Measured a v, HasDelta v) => HasDelta (FingerTree v a) where
  delta t = let x = measure t :: v in delta x


prettyDelta :: Delta -> Doc AnsiStyle
prettyDelta d = case d of
  Columns c _         -> go interactive 0 c
  Tab x y _           -> go interactive 0 (nextTab x + y)
  Lines l c _ _       -> go interactive l c
  Directed fn l c _ _ -> go fn l c
  where
  go
    :: String -- Source description
    -> Int    -- Line
    -> Int    -- Column
    -> Doc AnsiStyle
  go source line' column'
    = annotate bold (pretty source)
      <> char ':' <> annotate bold (pretty (line'+1))
      <> char ':' <> annotate bold (pretty (column'+1))
  interactive = "(interactive)"


-- | Retrieve the character offset within the current line from this 'Delta'.
column :: forall t. HasDelta t => t -> Int
column t = case delta t of
  Columns c _        -> c
  Tab b a _          -> nextTab b + a
  Lines _ c _ _      -> c
  Directed _ _ c _ _ -> c
