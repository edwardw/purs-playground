module Text.Trifecta.Rendering where

import Prelude
import Ansi.Codes (Color(..), GraphicsParam(..), RenderingMode(..))
import Control.Comonad (class Comonad, class Extend)
import Data.Array ((..), (:))
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Enum (fromEnum, toEnumWithDefaults)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Traversable (class Traversable, sequenceDefault)
import Data.Function (on)
import Data.Lazy (Lazy, defer, force)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semigroup.Reducer (class Reducer)
import Data.Semigroup.Reducer as Reducer
import Data.String (CodePoint, Pattern(..))
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Text.Prettyprint.Doc (Doc, align, annotate, fill, hcat, nesting, pretty, space, vsep, (<+>))
import Data.Text.Prettyprint.Doc.Render.Terminal (color, bgColor)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Exception.Unsafe (unsafeThrow)
import Text.Trifecta.Delta (class HasBytes, class HasDelta, Delta(..), bytes, column, delta, near)
import Text.Trifecta.Util.Combinators (argmax, argmin)
import Text.Trifecta.Util.Pretty (AnsiStyle, bold, char, columns, underlined)



outOfRangeEffect :: Array GraphicsParam -> Array GraphicsParam
outOfRangeEffect xs = PMode Bold : xs


gp :: Array GraphicsParam -> Doc AnsiStyle -> Doc AnsiStyle
gp xs0 = go (A.reverse xs0)
  where
  go xs0' = case A.uncons xs0' of
    Nothing -> identity
    Just { head: x, tail: xs } -> case x of
      PMode Bold      -> annotate bold <<< go xs
      PMode Underline -> annotate underlined <<< go xs
      PForeground c   -> annotate (color c) <<< go xs
      PBackground c   -> annotate (bgColor c) <<< go xs
      _               -> go xs



-------------------------------------------------------------------------------
-- GHC.Arr and GHC.Ix ---------------------------------------------------------
-------------------------------------------------------------------------------


class Ord a <= Ix a where
  -- | The array of values in the subrange defined by a bounding pair.
  range :: Tuple a a -> Array a
  -- | The position of a subscript in the subrange.
  index :: Tuple a a -> a -> Int
  -- | Returns `true` if the given subscript lies in the range defined the
  -- | bounding pair.
  inRange :: Tuple a a -> a -> Boolean
  -- | The size of the subrange defined by a bounding pair.
  rangeSize :: Tuple a a -> Int


indexError :: forall a b. Show a => Tuple a a -> a -> String -> Lazy b
indexError rng i tp = defer \_ -> unsafeThrow $
  "Ix{" <> show tp <> "}.index: Index " <> show i <> " out of range " <> show rng


hopelessIndexError :: forall a. Lazy a
hopelessIndexError = defer \_ -> unsafeThrow "Error in array index"


instance ixChar :: Ix Char where
  range (Tuple m n) = map (toEnumWithDefaults m n) (fromEnum m .. fromEnum n)

  index b@(Tuple m _) i
    | inRange b i = fromEnum i - fromEnum m
    | otherwise   = force $ indexError b i "Char"

  inRange (Tuple m n) i = m <= i && i <= n

  rangeSize b@(Tuple _ n)
    | inRange b n = index b n + 1
    | otherwise   = 0


instance ixCodePoint :: Ix CodePoint where
  range (Tuple m n) = map (toEnumWithDefaults m n) (fromEnum m .. fromEnum n)

  index b@(Tuple m _) i
    | inRange b i = fromEnum i - fromEnum m
    | otherwise   = force $ indexError b i "Char"

  inRange (Tuple m n) i = m <= i && i <= n

  rangeSize b@(Tuple _ n)
    | inRange b n = index b n + 1
    | otherwise   = 0


instance ixInt :: Ix Int where
  range (Tuple m n) = m .. n

  index b@(Tuple m _) i
    | inRange b i = i - m
    | otherwise   = force $ indexError b i "Int"

  inRange (Tuple m n) i = m <= i && i <= n

  rangeSize b@(Tuple _ n)
    | inRange b n = index b n + 1
    | otherwise   = 0


instance ixTuple :: (Ix a, Ix b) => Ix (Tuple a b) where
  range (Tuple (Tuple l1 l2) (Tuple u1 u2)) =
    Tuple <$> (range (Tuple l1 u1)) <*> (range (Tuple l2 u2))

  index b@(Tuple (Tuple l1 l2) (Tuple u1 u2)) i@(Tuple i1 i2)
    | inRange b i = index (Tuple l1 u1) i1 * rangeSize (Tuple l2 u2) + index (Tuple l2 u2) i2
    | otherwise   = force hopelessIndexError

  inRange (Tuple (Tuple l1 l2) (Tuple u1 u2)) (Tuple i1 i2) =
    inRange (Tuple l1 u1) i1 && inRange (Tuple l2 u2) i2

  rangeSize b@(Tuple _ n)
    | inRange b n = index b n + 1
    | otherwise   = 0


data IxArray i e
  = IxArray i         -- the lower bound, l
            i         -- the upper bound, u
            Int       -- A cache of (rangeSize (l,u))
            (Array e) -- The actual elements


arr :: forall i e
     . Ix i
    => Tuple i i         -- a pair of bounds (l, u)
    -> Array (Tuple i e) -- an array of associations of the form (index, value)
    -> IxArray i e
arr b@(Tuple l u) ies =
  let n = rangeSize b
  in IxArray l u n (snd $ A.unzip ies)


replace :: forall i e. Ix i => IxArray i e -> Array (Tuple i e) -> IxArray i e
replace (IxArray l u n a) ies =
  let b = Tuple l u
      xs = map (\(Tuple i e) -> Tuple (index b i) e) ies
  in IxArray l u n $ A.updateAtIndices xs a

infixl 9 replace as //


bounds :: forall i e. IxArray i e -> Tuple i i
bounds (IxArray l u _ _) = Tuple l u



-- | A raw canvas to paint ANSI-styled characters on.
type Lines
  = IxArray (Tuple Int Int) -- (line, column)
            (Tuple (Array GraphicsParam) Char)


-- | Replace a number of `(index, element)` values from an `IxArray`.
safeReplace :: forall i e. Ix i => IxArray i e -> Array (Tuple i e) -> IxArray i e
safeReplace a xs = a // A.filter (inRange (bounds a) <<< fst) xs

infix 9 safeReplace as ///


grow :: Int -> Lines -> Lines
grow y a@(IxArray _ _ _ xs) =
  if inRange (Tuple t b) y
  then a
  else arr new (indexWithDefault <$> range new)
  where
  old@(Tuple (Tuple t l) (Tuple b u)) = bounds a
  new = Tuple (Tuple (min t y) l) (Tuple (max b y) u)
  default = Tuple [] ' '
  indexWithDefault = \i ->
    if inRange old i
    then Tuple i (fromMaybe default (A.index xs (index old i)))
    else Tuple i default


draw
  :: Array GraphicsParam -- ANSI styles to use
  -> Int                 -- Line; 0 is at the top
  -> Int                 -- Column; 0 is on the left
  -> String              -- Text to be written
  -> Lines
  -> Lines
draw e y n xs a0 = case xs of
  "" -> a0
  _  -> gt $ lt (a /// out)
  where
  a = grow y a0
  Tuple (Tuple _ lo) (Tuple _ hi) = bounds a
  out = A.zipWith (\i c -> Tuple (Tuple y i) (Tuple e c))
                  (map (_ + n) (0..(S.length xs)))
                  (SCU.toCharArray xs)
  lt ys | A.any (\e1 -> snd (fst e1) < lo) out = ys // [Tuple (Tuple y lo) (Tuple (outOfRangeEffect e) '<')]
        | otherwise = ys
  gt ys | A.any (\e1 -> snd (fst e1) > hi) out = ys // [Tuple (Tuple y hi) (Tuple (outOfRangeEffect e) '>')]
        | otherwise = ys



-- | A 'Rendering' is a canvas of text that output can be written to.
data Rendering = Rendering
  Delta -- focus, the render will keep this visible
  Int   -- actual line length
  Int   -- line length in bytes
  (Lines -> Lines)
  (Delta -> Lines -> Lines)


instance showRendering :: Show Rendering where
  show (Rendering p ll lb _ _) =
    "(Rendering " <> show p <> " " <> show ll <> " " <> show lb <> " ... ...)"


instance semigroupRendering :: Semigroup Rendering where
  append (Rendering (Columns 0 0) 0 0 _ f) (Rendering del len lb dc g) =
    Rendering del len lb dc $ \d l -> f d (g d l)
  append (Rendering del len lb dc f) (Rendering _ _ _ _ g) =
    Rendering del len lb dc $ \d l -> f d (g d l)


instance monoidRendering :: Monoid Rendering where
  mempty = emptyRendering


instance hasDeltaRendering :: HasDelta Rendering where
  delta (Rendering d _ _ _ _) = d


nullRendering :: Rendering -> Boolean
nullRendering (Rendering (Columns 0 0) 0 0 _ _) = true
nullRendering _ = false


emptyRendering :: Rendering
emptyRendering = Rendering (Columns 0 0) 0 0 identity (const identity)


ifNear
  :: Delta            -- Position 1
  -> (Lines -> Lines) -- Modify the fallback result if the positions are near each other
  -> Delta            -- Position 2
  -> Lines            -- Fallback result if the positions are not near each other
  -> Lines
ifNear d f d' l | near d d' = f l
                | otherwise = l


class Renderable t where
  render :: t -> Rendering


instance renderableRendering :: Renderable Rendering where
  render = identity


data SourceT = SourceT
  Int              -- Number of (padded) columns
  Int              -- Number of bytes
  (Lines -> Lines) -- line

class Source t where
  source :: t -> SourceT


instance sourceString :: Source String where
  source s =
    if S.contains (Pattern "\n") s
    then SourceT ls bs (draw [] 0 0 s')
    else SourceT (ls + S.length end) bs (draw [PForeground BrightBlue, PMode Bold] 0 ls end <<< draw [] 0 0 s')
    where
    go n str = case SCU.uncons str of
      Nothing -> ""
      Just { head: x, tail: xs }
        | x == '\t' ->
          let t = 8 - mod n 8
          in (SCU.fromCharArray $ A.replicate t ' ') <> go (n + t) xs
        | x == '\n' -> ""
        | otherwise -> SCU.singleton x <> go (n + 1) xs

    end = "<EOF>"
    s' = go 0 s
    bs = S.length $ SCU.takeWhile (_ /= '\n') s
    ls = S.length s'


-- | create a drawing surface
rendered :: forall s. Source s => Delta -> s -> Rendering
rendered del s = case source s of
  SourceT len lb dc -> Rendering del len lb dc (\_ l -> l)


drawAt :: (Delta -> Lines -> Lines) -> Rendering -> Rendering
drawAt f (Rendering d ll lb s g) = Rendering d ll lb s $ \e l -> f e $ g e l

infix 5 drawAt as .#


prettyRendering :: Rendering -> Doc AnsiStyle
prettyRendering (Rendering d ll _ l f) = nesting $ \k -> columns $ \mn -> go (fromMaybe 80 mn - k)
  where
  go cols = align (vsep (map ln (t..b)))
    where
    n = show $ case d of
      Lines      n' _ _ _ -> 1 + n'
      Directed _ n' _ _ _ -> 1 + n'
      _                   -> 1

    separator = char '|'
    gutterWidth = S.length n
    gutter = pretty n <+> separator
    margin = fill gutterWidth space <+> separator
    Tuple lo hi = window (column d) ll (min (max (cols - 5 - gutterWidth) 30) 200)
    a = f d <<< l $ arr (Tuple (Tuple 0 lo) (Tuple (-1) hi)) []
    IxArray _ _ _ es = a
    bound@(Tuple (Tuple t _) (Tuple b _)) = bounds a

    ln y = (\x -> gp gutterEffects (if y == 0 then gutter else margin) <+> x)
       <<< hcat
       <<< map (\g -> gp (fst (fromMaybe (Tuple [] ' ') (A.head g))) (pretty (map snd g)))
       <<< map NEA.toArray
       <<< A.groupBy ((==) `on` fst)
       <<< A.catMaybes
       <<< map (\ix -> A.index es (index bound ix))
         $ A.zip (A.replicate (hi-lo) y) (lo..hi)


window :: Int -> Int -> Int -> Tuple Int Int
window c l w
  | w2 <- div w 2, c <= w2     = Tuple 0 (min w l)
  | w2 <- div w 2, c + w2 >= l = if l > w then Tuple (l-w) l
                                   else Tuple 0 w
  | w2 <- div w 2, otherwise   = Tuple (c-w2) (c+w2)


-- | ANSI terminal style for rendering the gutter.
gutterEffects :: Array GraphicsParam
gutterEffects = [PForeground BrightBlue]



data Rendered a = Rendered a Rendering


instance functorRendered :: Functor Rendered where
  map f (Rendered a s) = Rendered (f a) s


instance hasDeltaRendered :: HasDelta (Rendered a) where
  delta = delta <<< render


instance hasBytesRendered :: HasBytes (Rendered a) where
  bytes = bytes <<< delta


instance extendRendered :: Extend Rendered where
  extend f r@(Rendered _ s) = Rendered (f r) s


instance comonadRendered :: Comonad Rendered where
  extract (Rendered a _) = a


instance foldableRendered :: Foldable Rendered where
  foldMap f (Rendered a _) = f a
  foldr f x xs = foldrDefault f x xs
  foldl f x xs = foldlDefault f x xs


instance traversableRendered :: Traversable Rendered where
  traverse f (Rendered a s) = flip Rendered s <$> f a
  sequence xs = sequenceDefault xs


instance renderableRendered :: Renderable (Rendered a) where
  render (Rendered _ s) = s


-- | A `Caret` marks a point in the input with a simple `^` character.
data Caret = Caret Delta String

derive instance eqCaret :: Eq Caret
derive instance ordCaret :: Ord Caret


class HasCaret t where
  caret :: Lens' t Caret


instance hasCaretCaret :: HasCaret Caret where
  caret = identity


-- | ANSI terminal style for rendering the caret.
caretEffects :: Array GraphicsParam
caretEffects = [PForeground BrightGreen]


drawCaret :: Delta -> Delta -> Lines -> Lines
drawCaret p = ifNear p $ draw caretEffects 1 (column p) "^"


-- | Render a caret at a certain position in a 'Rendering'.
addCaret :: Delta -> Rendering -> Rendering
addCaret p r = drawCaret p .# r


instance hasBytesCaret :: HasBytes Caret where
  bytes = bytes <<< delta

instance hasDeltaCaret :: HasDelta Caret where
  delta (Caret d _) = d

instance renderableCaret :: Renderable Caret where
  render (Caret d bs) = addCaret d $ rendered d bs

instance reducerCaret :: Reducer Caret Rendering where
  unit = render
  snoc m c = append m (Reducer.unit c)
  cons c m = append (Reducer.unit c) m

instance semigroupCaret :: Semigroup Caret where
  append a _ = a


renderingCaret :: Delta -> String -> Rendering
renderingCaret d bs = addCaret d $ rendered d bs


data Careted a = Careted a Caret

derive instance eqCareted :: Eq a => Eq (Careted a)
derive instance ordCareted :: Ord a => Ord (Careted a)


instance hasCaretCareted :: HasCaret (Careted a) where
  caret = lens getter setter
    where
    getter (Careted _ c) = c
    setter (Careted a _) c' = Careted a c'


instance functorCareted :: Functor Careted where
  map f (Careted a s) = Careted (f a) s


instance hasDeltaCareted :: HasDelta (Careted a) where
  delta (Careted _ c) = delta c


instance hasBytesCareted :: HasBytes (Careted a) where
  bytes (Careted _ c) = bytes c


instance extendCareted :: Extend Careted where
  extend f c@(Careted _ s) = Careted (f c) s


instance comonadCareted :: Comonad Careted where
  extract (Careted a _) = a


instance foldableCareted :: Foldable Careted where
  foldMap f (Careted a _) = f a
  foldr f x xs = foldrDefault f x xs
  foldl f x xs = foldlDefault f x xs


instance traversableCareted :: Traversable Careted where
  traverse f (Careted a s) = flip Careted s <$> f a
  sequence xs = sequenceDefault xs


instance renderableCareted :: Renderable (Careted a) where
  render (Careted _ s) = render s


instance reducerCareted :: Reducer (Careted a) Rendering where
  unit = render
  snoc m c = append m (Reducer.unit c)
  cons c m = append (Reducer.unit c) m


-- | ANSI terminal style to render spans with.
spanEffects :: Array GraphicsParam
spanEffects  = [PForeground Green]


drawSpan
    :: Delta -- Start of the region of interest
    -> Delta -- End of the region of interest
    -> Delta -- Current location
    -> Lines -- `Lines` to add the rendering to
    -> Lines
drawSpan start end d a =
  if nearLo && nearHi
  then go (column lo) (rep (max (column hi - column lo) 0) '~') a
  else if nearLo
  then go (column lo) (rep (max (snd (snd (bounds a)) - column lo + 1) 0) '~') a
  else if nearHi
  then go (-1) (rep (max (column hi + 1) 0) '~') a
  else a

  where

  go = draw spanEffects 1
  lo = argmin bytes start end
  hi = argmax bytes start end
  nearLo = near lo d
  nearHi = near hi d
  rep n c = SCU.fromCharArray $ A.replicate n c


addSpan :: Delta -> Delta -> Rendering -> Rendering
addSpan s e r = drawSpan s e .# r


-- | A `Span marks a range of input characters. If `Caret` is a point, then
-- | `Span` is a line.
data Span = Span Delta Delta String

derive instance eqSpan :: Eq Span
derive instance ordSpan :: Ord Span


class HasSpan t where
  span :: Lens' t Span


instance hasSpanSpan :: HasSpan Span where
  span = identity


instance renderableSpan :: Renderable Span where
  render (Span s e t) = addSpan s e $ rendered s t


instance semigroupSpan :: Semigroup Span where
  append (Span s _ t) (Span _ e _) = Span s e t


instance reducerSpan :: Reducer Span Rendering where
  unit = render
  snoc m c = append m (Reducer.unit c)
  cons c m = append (Reducer.unit c) m


-- | Annotate an arbitrary piece of data with a `Span`, typically its
-- | corresponding input location.
data Spanned a = Spanned a Span


instance hasSpanSpanned :: HasSpan (Spanned a) where
  span = lens getter setter
    where
    getter (Spanned _ c) = c
    setter (Spanned a _) c' = Spanned a c'


instance functorSpanned :: Functor Spanned where
  map f (Spanned a s) = Spanned (f a) s


instance exendSpanned :: Extend Spanned where
  extend f sp@(Spanned _ s) = Spanned (f sp) s


instance comonadSpanned :: Comonad Spanned where
  extract (Spanned a _) = a


instance foldableSpanned :: Foldable Spanned where
  foldMap f (Spanned a _) = f a
  foldr f x xs = foldrDefault f x xs
  foldl f x xs = foldlDefault f x xs


instance traversableSpanned :: Traversable Spanned where
  traverse f (Spanned a s) = flip Spanned s <$> f a
  sequence xs = sequenceDefault xs


instance reducerSpanned :: Reducer (Spanned a) Rendering where
  unit = render
  snoc m c = append m (Reducer.unit c)
  cons c m = append (Reducer.unit c) m


instance renderableSpanned :: Renderable (Spanned a) where
  render (Spanned _ s) = render s


drawFixit :: Delta -> Delta -> String -> Delta -> Lines -> Lines
drawFixit s e rpl d a =
  ifNear l (draw [PForeground Blue] 2 (column l) rpl) d
  $ drawSpan s e d a
  where
  l = argmin bytes s e

addFixit :: Delta -> Delta -> String -> Rendering -> Rendering
addFixit s e rpl r = drawFixit s e rpl .# r


-- | A `Fixit` is a `Span` with a suggestion.
data Fixit = Fixit Span String

derive instance eqFixit :: Eq Fixit
derive instance ordFixit :: Ord Fixit


instance hasSpanFixit :: HasSpan Fixit where
  span = lens getter setter
    where
    getter (Fixit s _) = s
    setter (Fixit _ t) s' = Fixit s' t


instance reducerFixit :: Reducer Fixit Rendering where
  unit = render
  snoc m c = append m (Reducer.unit c)
  cons c m = append (Reducer.unit c) m


instance renderableFixit :: Renderable Fixit where
  render (Fixit (Span s e t) r) = addFixit s e r $ rendered s t
