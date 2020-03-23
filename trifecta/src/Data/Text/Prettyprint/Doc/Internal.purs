module Data.Text.Prettyprint.Doc.Internal
  ( -- | Documents
    Doc(..)

    -- | Basic functionality
  , class Pretty, pretty, prettyArray
  , viaShow, unsafeViaShow, unsafeTextWithoutNewlines
  , emptyDoc, nest, line, line', softline, softline', hardline

    -- | Primitives for alternative layouts
  , group, flatAlt

    -- | Alignment functions
    -- |
    -- | The functions in this section cannot be described by Wadler's original
    -- | functions. They align their output relative to the current output
    -- | position - in contrast to `nest` which always aligns to the current
    -- | nesting level. This deprives these functions from being *optimal*. In
    -- | practice however they prove to be very useful. The functions in this
    -- | section should be used with care, since they are more expensive than the
    -- | other functions. For example, `align` shouldn't be used to pretty print
    -- | all top-level declarations of a language, but using `hang` for let
    -- | expressions is fine.
  , align, hang, indent, encloseSep, array, tupled

    -- | Binary functions
  , concatDoc, (<+>)

    -- | Array functions
    -- |
    -- | The `sep` and `cat` functions differ in one detail: when `group`ed, the
    -- | `sep`s replace newlines wich `space`s, while the `cat`s simply remove
    -- | them. If you're not sure what you want, start with the `sep`s.

  , concatWith

    -- | `sep` family
    -- |
    -- | When `group`ed, these will replace newlines with spaces.
  , hsep, vsep, fillSep, sep

    -- | `cat` family
    -- |
    -- | When `group`ed, these will remove newlines.
  , hcat, vcat, fillCat, cat
    -- | Others
  , punctuate

    -- | Reactive/conditional layouts
    -- |
    -- | Lay documents out differently based on current position and the page
    -- | layout.
  , column, nesting, width, pageWidth

    -- | Filler functions
    -- |
    -- | Fill up available space
  , fill, fillBreak

    -- | General convenience
    -- |
    -- | Useful helper functions
  , plural, enclose, surround

    -- | Annotations
  , annotate
  , unAnnotate
  , reAnnotate
  , alterAnnotations
  , unAnnotateS
  , reAnnotateS
  , alterAnnotationsS

    -- | Optimization
    -- |
    -- | Render documents faster
  , fuse, FusionDepth(..)

    -- | Layout
    -- |
    -- | Laying a `Doc`ument out produces a straightforward `SimpleDocStream`
    -- | based on parameters such as page width and ribbon size, by evaluating
    -- | how a `Doc` fits these constraints the best. There are various ways to
    -- | render a `SimpleDocStream`. For the common case of rendering a
    -- | `SimpleDocStream` as plain `Text` take a look at
    -- | `Data.Text.Prettyprint.Doc.Render.Text`.
  , SimpleDocStream(..)
  , PageWidth(..), defaultPageWidth
  , LayoutOptions(..), defaultLayoutOptions
  , layoutPretty, layoutCompact, layoutSmart
  , removeTrailingWhitespace

  , renderShowS

  , textSpaces
  ) where

import Prelude
import Data.Array (catMaybes, (:))
import Data.Array as A
import Data.Const (Const(..))
import Data.Foldable (class Foldable, foldlDefault, foldl, foldr, foldrDefault, null)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity(..))
import Data.Int as I
import Data.Lazy (force)
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty)
import Data.String (CodePoint, codePointFromChar, fromCodePointArray, toCodePointArray)
import Data.String as S
import Data.Text.Prettyprint.Doc.Render.Util.Panic (panicPeekedEmpty, panicUncaughtFail)
import Data.Traversable (class Traversable, sequenceDefault)
import Data.Tuple (Tuple(..))
import Effect.Exception.Unsafe (unsafeThrow)



data Doc ann
  -- | Occurs when flattening a line. The layouter will reject this document,
  -- | choosing a more suitable rendering.
  = Fail

  -- | The empty document; conceptually the unit of 'Cat'
  | Empty

  -- | invariant: not '\n'
  | Char CodePoint

  | Text Int String

  | Line

  | FlatAlt (Doc ann) (Doc ann)

  | Cat (Doc ann) (Doc ann)

  | Nest Int (Doc ann)

  | Union (Doc ann) (Doc ann)

  | Column (Int -> Doc ann)

  | WithPageWidth (PageWidth -> Doc ann)

  | Nesting (Int -> Doc ann)

  | Annotated ann (Doc ann)


instance semigroupDoc :: Semigroup (Doc ann) where
  append = Cat


instance monoidDoc :: Monoid (Doc ann) where
  mempty = emptyDoc


instance functorDoc :: Functor Doc where
  map = reAnnotate


-- | Overloaded conversion to 'Doc'.
-- |
-- | Laws:
-- |
-- |  1. output should be pretty. Seriously.
class Pretty a where
  pretty :: forall ann. a -> Doc ann
  prettyArray :: forall ann. Array a -> Doc ann


instance prettyConst :: Pretty a => Pretty (Const a b) where
  pretty (Const a) = pretty a
  prettyArray = align <<< array <<< map pretty


instance prettyIdentity :: Pretty a => Pretty (Identity a) where
  pretty (Identity a) = pretty a
  prettyArray = align <<< array <<< map pretty


instance prettyArr :: Pretty a => Pretty (Array a) where
  pretty = prettyArray
  prettyArray xs = align <<< array $ map pretty xs


instance prettyNonEmpty :: (Foldable f, Pretty a) => Pretty (NonEmpty f a) where
  pretty = pretty <<< A.fromFoldable
  prettyArray = align <<< array <<< map pretty <<< A.fromFoldable


instance prettyUnit :: Pretty Unit where
  pretty _ = pretty "()"
  prettyArray xs = align <<< array $ map pretty xs


instance prettyBoolean :: Pretty Boolean where
  pretty true = pretty "true"
  pretty false = pretty "false"
  prettyArray xs = align <<< array $ map pretty xs


instance prettyChar :: Pretty Char where
  pretty '\n' = line
  pretty c = Char (codePointFromChar c)

  prettyArray = pretty <<< fromCodePointArray <<< map codePointFromChar


-- | Convenience function to convert a `Show`able value to a `Doc`. If the
-- | `String` does not contain newlines, consider using the more performant
-- | `unsafeViaShow`.
viaShow :: forall a ann. Show a => a -> Doc ann
viaShow = pretty <<< show


-- | Convenience function to convert a Show`able value *that must not contain
-- | newlines* to a `Doc`. If there may be newlines, use `viaShow` instead.
unsafeViaShow :: forall a ann. Show a => a -> Doc ann
unsafeViaShow = unsafeTextWithoutNewlines <<< show


instance prettyString :: Pretty String where
  pretty s = vsep <<< map unsafeTextWithoutNewlines $ S.split (S.Pattern "\n") s
  prettyArray ss = align <<< array $ map pretty ss


-- | `unsafeTextWithoutNewlines s` contains the literal string `s`.
-- |
-- | The string must not contain any newline characters, since this is an
-- | invariant of the 'Text' constructor.
unsafeTextWithoutNewlines :: forall ann. String -> Doc ann
unsafeTextWithoutNewlines text = case S.uncons text of
  Nothing -> Empty
  Just { head, tail }
    | S.null tail -> Char head
    | otherwise   -> Text (S.length text) text


instance prettyInt :: Pretty Int where
  pretty = unsafeViaShow
  prettyArray xs = align <<< array $ map pretty xs


instance prettyNumber :: Pretty Number where
  pretty = unsafeViaShow
  prettyArray xs = align <<< array $ map pretty xs


instance prettyTuple :: (Pretty a, Pretty b) => Pretty (Tuple a b) where
  pretty (Tuple x y) = tupled [pretty x, pretty y]
  prettyArray xs = align <<< array $ map pretty xs


instance prettyMaybe :: Pretty a => Pretty (Maybe a) where
  pretty = maybe mempty pretty
  prettyArray = prettyArray <<< catMaybes


emptyDoc :: forall ann. Doc ann
emptyDoc = Empty


nest :: forall ann. Int -> Doc ann -> Doc ann
nest 0 x = x
nest i x = Nest i x


line :: forall ann. Doc ann
line = FlatAlt Line (Char (codePointFromChar ' '))


line' :: forall ann. Doc ann
line' = FlatAlt Line mempty


softline :: forall ann. Doc ann
softline = Union (Char (codePointFromChar ' ')) Line


softline' :: forall ann. Doc ann
softline' = Union mempty Line


hardline :: forall ann. Doc ann
hardline = Line


group :: forall ann. Doc ann -> Doc ann
group x = case x of
  Union _ _ -> x
  FlatAlt a b -> case changesUponFlattening b of
    Flattened b' -> Union b' a
    AlreadyFlat  -> Union b a
    NeverFlat    -> a
  _ -> case changesUponFlattening x of
    Flattened x' -> Union x' x
    AlreadyFlat  -> x
    NeverFlat    -> x


column :: forall ann. (Int -> Doc ann) -> Doc ann
column = Column


nesting :: forall ann. (Int -> Doc ann) -> Doc ann
nesting = Nesting


width :: forall ann. Doc ann -> (Int -> Doc ann) -> Doc ann
width doc f =
  column (\colStart ->
    doc <> column (\colEnd ->
      f (colEnd - colStart)))


pageWidth :: forall ann. (PageWidth -> Doc ann) -> Doc ann
pageWidth = WithPageWidth


fill :: forall ann. Int -> Doc ann -> Doc ann
fill n doc = width doc (\w -> spaces (n - w))


fillBreak :: forall ann. Int -> Doc ann -> Doc ann
fillBreak f x = width x (\w ->
  if w > f
  then nest f line'
  else spaces (f - w))


-- | Insert a number of spaces. Negative values count as 0.
spaces :: forall ann. Int -> Doc ann
spaces n
  | n <= 0    = Empty
  | n == 1    = Char (codePointFromChar ' ')
  | otherwise = Text n (textSpaces n)


plural :: forall doc. doc -> doc -> Int -> doc
plural one multiple n
    | n == 1    = one
    | otherwise = multiple


enclose :: forall ann. Doc ann -> Doc ann -> Doc ann -> Doc ann
enclose l r x = l <> x <> r


surround :: forall ann. Doc ann -> Doc ann -> Doc ann -> Doc ann
surround x l r = l <> x <> r


annotate :: forall ann. ann -> Doc ann -> Doc ann
annotate = Annotated


unAnnotate :: forall ann ann'. Doc ann -> Doc ann'
unAnnotate = alterAnnotations (const [])


reAnnotate :: forall ann ann'. (ann -> ann') -> Doc ann -> Doc ann'
reAnnotate re = alterAnnotations (pure <<< re)


alterAnnotations :: forall ann ann'. (ann -> Array ann') -> Doc ann -> Doc ann'
alterAnnotations re = go
  where
  go = case _ of
    Fail     -> Fail
    Empty    -> Empty
    Char c   -> Char c
    Text l t -> Text l t
    Line     -> Line

    FlatAlt x y     -> FlatAlt (go x) (go y)
    Cat x y         -> Cat (go x) (go y)
    Nest i x        -> Nest i (go x)
    Union x y       -> Union (go x) (go y)
    Column f        -> Column (go <<< f)
    WithPageWidth f -> WithPageWidth (go <<< f)
    Nesting f       -> Nesting (go <<< f)
    Annotated ann x -> foldr Annotated (go x) (re ann)


unAnnotateS :: forall ann. SimpleDocStream ann -> SimpleDocStream ann
unAnnotateS = go
  where
  go = case _ of
    SFail           -> SFail
    SEmpty          -> SEmpty
    SChar c rest    -> SChar c (go rest)
    SText l t rest  -> SText l t (go rest)
    SLine l rest    -> SLine l (go rest)
    SAnnPush _ rest -> go rest
    SAnnPop rest    -> go rest


reAnnotateS :: forall ann ann'. (ann -> ann') -> SimpleDocStream ann -> SimpleDocStream ann'
reAnnotateS re = go
  where
  go doc = case doc of
    SFail             -> SFail
    SEmpty            -> SEmpty
    SChar c rest      -> SChar c (go rest)
    SText l t rest    -> SText l t (go rest)
    SLine l rest      -> SLine l (go rest)
    SAnnPush ann rest -> SAnnPush (re ann) (go rest)
    SAnnPop rest      -> SAnnPop (go rest)


data AnnotationRemoval = Remove | DontRemove

derive instance eqAnnotationRemoval :: Eq AnnotationRemoval


alterAnnotationsS
  :: forall ann ann'
   . (ann -> Maybe ann')
   -> SimpleDocStream ann
   -> SimpleDocStream ann'
alterAnnotationsS re = go []
  where
  go stack sds = case sds of
    SFail             -> SFail
    SEmpty            -> SEmpty
    SChar c rest      -> SChar c (go stack rest)
    SText l t rest    -> SText l t (go stack rest)
    SLine l rest      -> SLine l (go stack rest)
    SAnnPush ann rest -> case re ann of
      Nothing   -> go (Remove : stack) rest
      Just ann' -> SAnnPush ann' (go (DontRemove : stack) rest)
    SAnnPop rest      -> case A.uncons stack of
      Nothing                -> force panicPeekedEmpty
      Just { head, tail }
        | head == DontRemove -> SAnnPop (go tail rest)
        | otherwise          -> go tail rest


data FlattenResult a
  = Flattened a
  | AlreadyFlat
  | NeverFlat


instance functorFlattenResult :: Functor FlattenResult where
  map f (Flattened a) = Flattened (f a)
  map _ AlreadyFlat   = AlreadyFlat
  map _ NeverFlat     = NeverFlat


changesUponFlattening :: forall ann. Doc ann -> FlattenResult (Doc ann)
changesUponFlattening = case _ of
  FlatAlt _ y     -> Flattened (flatten y)
  Line            -> NeverFlat
  Union x _       -> Flattened x
  Nest i x        -> map (Nest i) (changesUponFlattening x)
  Annotated ann x -> map (Annotated ann) (changesUponFlattening x)

  Column f        -> Flattened (Column (flatten <<< f))
  Nesting f       -> Flattened (Nesting (flatten <<< f))
  WithPageWidth f -> Flattened (WithPageWidth (flatten <<< f))

  Cat x y -> case Tuple (changesUponFlattening x) (changesUponFlattening y) of
    Tuple NeverFlat _ -> NeverFlat
    Tuple _ NeverFlat -> NeverFlat
    Tuple (Flattened x') (Flattened y') -> Flattened (Cat x' y')
    Tuple (Flattened x') AlreadyFlat    -> Flattened (Cat x' y)
    Tuple AlreadyFlat (Flattened y')    -> Flattened (Cat x y')
    Tuple AlreadyFlat AlreadyFlat       -> AlreadyFlat

  Empty    -> AlreadyFlat
  Char _   -> AlreadyFlat
  Text _ _ -> AlreadyFlat
  Fail     -> NeverFlat

  where

  flatten :: Doc ann -> Doc ann
  flatten = case _ of
    FlatAlt _ y     -> flatten y
    Cat x y         -> Cat (flatten x) (flatten y)
    Nest i x        -> Nest i (flatten x)
    Line            -> Fail
    Union x _       -> flatten x
    Column f        -> Column (flatten <<< f)
    Nesting f       -> Nesting (flatten <<< f)
    WithPageWidth f -> WithPageWidth (flatten <<< f)
    Annotated ann x -> Annotated ann (flatten x)

    x@Fail       -> x
    x@Empty      -> x
    x@(Char _)   -> x
    x@(Text _ _) -> x


flatAlt :: forall ann. Doc ann -> Doc ann -> Doc ann
flatAlt = FlatAlt


align :: forall ann. Doc ann -> Doc ann
align d = column (\k -> nesting (\i -> nest (k - i) d))


hang :: forall ann. Int -> Doc ann -> Doc ann
hang i d = align (nest i d)


indent :: forall ann. Int -> Doc ann -> Doc ann
indent i d = hang i (spaces i <> d)


encloseSep :: forall ann. Doc ann -> Doc ann -> Doc ann -> Array (Doc ann) -> Doc ann
encloseSep l r s ds = case ds of
  []  -> l <> r
  [d] -> l <> d <> r
  _   -> let n = A.length ds - 1
         in cat (A.zipWith (<>) (l : (A.replicate n s)) ds) <> r


array :: forall ann. Array (Doc ann) -> Doc ann
array ds = group $ encloseSep (flatAlt (pretty "[ ") (pretty "["))
                              (flatAlt (pretty "] ") (pretty "]"))
                              (pretty ", ")
                              ds


tupled :: forall ann. Array (Doc ann) -> Doc ann
tupled = group <<< encloseSep (flatAlt (pretty "( ") (pretty "("))
                              (flatAlt (pretty ") ") (pretty ")"))
                              (pretty ", ")


concatDoc :: forall ann. Doc ann -> Doc ann -> Doc ann
concatDoc x y = x <> Char (codePointFromChar ' ') <> y

infixr 6 concatDoc as <+>


concatWith :: forall ann t. Foldable t => (Doc ann -> Doc ann -> Doc ann) -> t (Doc ann) -> Doc ann
concatWith f ds
  | null ds   = mempty
  | otherwise = case A.uncons (A.fromFoldable ds) of
    Just { head, tail } -> foldl f head tail
    Nothing             -> unsafeThrow "unreachable - concatWith"


hsep :: forall ann. Array (Doc ann) -> Doc ann
hsep = concatWith (<+>)


vsep :: forall ann. Array (Doc ann) -> Doc ann
vsep = concatWith (\x y -> x <> line <> y)


fillSep :: forall ann. Array (Doc ann) -> Doc ann
fillSep = concatWith (\x y -> x <> softline <> y)


sep :: forall ann. Array (Doc ann) -> Doc ann
sep = group <<< vsep


hcat :: forall ann. Array (Doc ann) -> Doc ann
hcat = concatWith (<>)


vcat :: forall ann. Array (Doc ann) -> Doc ann
vcat = concatWith (\x y -> x <> line' <> y)


fillCat :: forall ann. Array (Doc ann) -> Doc ann
fillCat = concatWith (\x y -> x <> softline' <> y)


cat :: forall ann. Array (Doc ann) -> Doc ann
cat = group <<< vcat


punctuate :: forall ann. Doc ann -> Array (Doc ann) -> Array (Doc ann)
punctuate p = go
  where
    go ds = case A.uncons ds of
      Nothing         ->  []
      Just { head, tail }
        | A.null tail -> [head]
        | otherwise   -> (head <> p) : go tail


data FusionDepth
  = Shallow
  | Deep

derive instance eqFusionDepth :: Eq FusionDepth
derive instance ordFusionDepth :: Ord FusionDepth

instance showFusionDepth :: Show FusionDepth where
  show Shallow = "Shallow"
  show Deep = "Deep"


fuse :: forall ann. FusionDepth -> Doc ann -> Doc ann
fuse depth = go
  where
  go = case _ of
    Cat Empty x                   -> go x
    Cat x Empty                   -> go x
    Cat (Char c1) (Char c2)       -> Text 2 (S.singleton c1 <> S.singleton c2)
    Cat (Text lt t) (Char c)      -> Text (lt + 1) (t <> S.singleton c)
    Cat (Char c) (Text lt t)      -> Text (1 + lt) (S.singleton c <> t)
    Cat (Text l1 t1) (Text l2 t2) -> Text (l1 + l2) (t1 <> t2)

    Cat x@(Char _) (Cat y@(Char _) z)     -> go (Cat (go (Cat x y)) z)
    Cat x@(Text _ _) (Cat y@(Char _) z)   -> go (Cat (go (Cat x y)) z)
    Cat x@(Char _) (Cat y@(Text _ _) z)   -> go (Cat (go (Cat x y)) z)
    Cat x@(Text _ _) (Cat y@(Text _ _) z) -> go (Cat (go (Cat x y)) z)

    Cat (Cat x y@(Char _)) z   -> go (Cat x (go (Cat y z)))
    Cat (Cat x y@(Text _ _)) z -> go (Cat x (go (Cat y z)))

    Cat x y -> Cat (go x) (go y)

    Nest i (Nest j x) -> go (Nest (i + j) x)

    Nest _ x@Empty      -> x
    Nest _ x@(Text _ _) -> x
    Nest _ x@(Char _)   -> x
    Nest 0 x            -> go x
    Nest i x            -> Nest i (go x)

    Annotated ann x -> Annotated ann (go x)

    FlatAlt x1 x2 -> FlatAlt (go x1) (go x2)
    Union x1 x2   -> Union (go x1) (go x2)

    other | depth == Shallow -> other

    Column f        -> Column (go <<< f)
    WithPageWidth f -> WithPageWidth (go <<< f)
    Nesting f       -> Nesting (go <<< f)

    other -> other


-- | The data type `SimpleDocStream` represents laid out documents and is used
-- |by the display functions.
-- |
-- | A simplified view is that `Doc = [SimpleDocStream]`, and the layout
-- | functions pick one of the `SimpleDocStream`s based on which one fits the
-- | layout constraints best. This means that `SimpleDocStream` has all complexity
-- | contained in `Doc` resolved, making it very easy to convert it to other
-- | formats, such as plain text or terminal output.
-- |
-- | To write your own `Doc` to X converter, it is therefore sufficient to
-- | convert from `SimpleDocStream`. The *Render* submodules provide some
-- | built-in converters to do so, and helpers to create own ones.
data SimpleDocStream ann
  = SFail
  | SEmpty
  | SChar CodePoint (SimpleDocStream ann)

  | SText Int String (SimpleDocStream ann)

  | SLine Int (SimpleDocStream ann)

  | SAnnPush ann (SimpleDocStream ann)

  | SAnnPop (SimpleDocStream ann)

derive instance eqSimpleDocStream :: Eq ann => Eq (SimpleDocStream ann)
derive instance ordSimpleDocStream :: Ord ann => Ord (SimpleDocStream ann)
derive instance genericSimpleDocStream :: Generic (SimpleDocStream ann) _

instance showSimpleDocStream :: Show ann => Show (SimpleDocStream ann) where
  show x = genericShow x


removeTrailingWhitespace :: forall ann. SimpleDocStream ann -> SimpleDocStream ann
removeTrailingWhitespace = go (RecordedWhitespace [] 0)
  where
  commitWhitespace
    :: Array Int
    -> Int
    -> SimpleDocStream ann
    -> SimpleDocStream ann
  commitWhitespace is n sds = case A.uncons is of
    Nothing -> case n of
                  0 -> sds
                  1 -> SChar (codePointFromChar ' ') sds
                  _ -> SText n (textSpaces n) sds
    Just { head, tail } -> let end = SLine (head + n) sds
                          in prependEmptyLines tail end

  prependEmptyLines :: Array Int -> SimpleDocStream ann -> SimpleDocStream ann
  prependEmptyLines is sds0 = foldr (\_ sds -> SLine 0 sds) sds0 is

  go :: WhitespaceStrippingState -> SimpleDocStream ann -> SimpleDocStream ann
  go annLevel@(AnnotationLevel annLvl) = \sds -> case sds of
    SFail             -> SFail
    SEmpty            -> SEmpty
    SChar c rest      -> SChar c (go annLevel rest)
    SText l text rest -> SText l text (go annLevel rest)
    SLine i rest      -> SLine i (go annLevel rest)
    SAnnPush ann rest -> let annLvl' = annLvl + 1
                         in SAnnPush ann (go (AnnotationLevel annLvl') rest)
    SAnnPop rest
      | annLvl > 1    -> let annLvl' = annLvl - 1
                         in SAnnPop (go (AnnotationLevel annLvl') rest)
      | otherwise     -> SAnnPop (go (RecordedWhitespace [] 0) rest)

  go (RecordedWhitespace withheldLines withheldSpaces) = \sds -> case sds of
    SFail -> SFail
    SEmpty -> prependEmptyLines withheldLines SEmpty
    SChar c rest
      | c == codePointFromChar ' '
                  -> go (RecordedWhitespace withheldLines (withheldSpaces + 1)) rest
      | otherwise -> commitWhitespace
                        withheldLines
                        withheldSpaces
                        (SChar c (go (RecordedWhitespace [] 0) rest))
    SText l text rest ->
      let stripped = toCodePointArray text
                        # A.reverse
                        # A.dropWhile (_ == codePointFromChar ' ')
                        # A.reverse
                        # fromCodePointArray
          strippedLength = S.length stripped
          trailingLength = l - strippedLength
          isOnlySpace = strippedLength == 0
      in if isOnlySpace
         then go (RecordedWhitespace withheldLines (withheldSpaces + l)) rest
         else commitWhitespace
                withheldLines
                withheldSpaces
                (SText strippedLength stripped (go (RecordedWhitespace [] trailingLength) rest))
    SLine i rest -> go (RecordedWhitespace (i : withheldLines) 0) rest
    SAnnPush ann rest -> commitWhitespace
                            withheldLines
                            withheldSpaces
                            (SAnnPush ann (go (AnnotationLevel 1) rest))
    SAnnPop _ -> unsafeThrow "Tried skipping spaces in unannotated data! Please report this as a bug in `prettyprinter`."


data WhitespaceStrippingState
    = AnnotationLevel Int
    | RecordedWhitespace (Array Int) Int


instance functorSimpleDocStream :: Functor SimpleDocStream where
  map = reAnnotateS


instance foldableSimpleDocStream :: Foldable SimpleDocStream where
  foldMap f = go
    where
    go sds = case sds of
      SFail             -> mempty
      SEmpty            -> mempty
      SChar _ rest      -> go rest
      SText _ _ rest    -> go rest
      SLine _ rest      -> go rest
      SAnnPush ann rest -> f ann <> go rest
      SAnnPop rest      -> go rest
  foldr f x xs = foldrDefault f x xs
  foldl f x xs = foldlDefault f x xs


instance traversableSimpleDocStream :: Traversable SimpleDocStream where
  traverse f = go
    where
    go sds = case sds of
      SFail             -> pure SFail
      SEmpty            -> pure SEmpty
      SChar c rest      -> SChar c <$> go rest
      SText l t rest    -> SText l t <$> go rest
      SLine i rest      -> SLine i <$> go rest
      SAnnPush ann rest -> SAnnPush <$> f ann <*> go rest
      SAnnPop rest      -> SAnnPop <$> go rest
  sequence xs = sequenceDefault xs


newtype FittingPredicate ann
  = FittingPredicate (Int -> Int -> Maybe Int -> SimpleDocStream ann -> Boolean)


data LayoutPipeline ann
  = Nil
  | Cons Int (Doc ann) (LayoutPipeline ann)
  | UndoAnn (LayoutPipeline ann)


data PageWidth
  = AvailablePerLine Int Number
  | Unbounded

derive instance eqPageWidth :: Eq PageWidth
derive instance ordPageWidth :: Ord PageWidth

instance showPageWidth :: Show PageWidth where
  show = case _ of
    AvailablePerLine i j -> "AvailablePerLine " <> show i <> " " <> show j
    Unbounded -> "Unbounded"


defaultPageWidth :: PageWidth
defaultPageWidth = AvailablePerLine 80 1.0


remainingWidth :: Int -> Number -> Int -> Int -> Int
remainingWidth lineLength ribbonFraction lineIndent currentColumn =
  min columnsLeftInLine columnsLeftInRibbon
  where
  ribbonWidth = (max 0 <<< min lineLength <<< I.round)
                (I.toNumber lineLength * ribbonFraction)
  columnsLeftInLine = lineLength - currentColumn
  columnsLeftInRibbon = lineIndent + ribbonWidth - currentColumn


newtype LayoutOptions = LayoutOptions PageWidth

derive instance eqLayoutOptions :: Eq LayoutOptions
derive instance ordLayoutOptions :: Ord LayoutOptions

instance showLayoutOptions :: Show LayoutOptions where
  show (LayoutOptions p) = "LayoutOptions " <> show p


defaultLayoutOptions :: LayoutOptions
defaultLayoutOptions = LayoutOptions defaultPageWidth


layoutPretty
  :: forall ann
   . LayoutOptions
  -> Doc ann
  -> SimpleDocStream ann
layoutPretty (LayoutOptions pageWidth_@(AvailablePerLine lineLength ribbonFraction)) =
  layoutWadlerLeijen
    (FittingPredicate (\lineIndent currentColumn initialIndent sdoc ->
      fits
        (remainingWidth lineLength ribbonFraction lineIndent currentColumn)
        sdoc))
    pageWidth_
  where
  fits :: Int -> SimpleDocStream ann -> Boolean
  fits w _ | w < 0      = false
  fits _ SFail          = false
  fits _ SEmpty         = true
  fits w (SChar _ x)    = fits (w - 1) x
  fits w (SText l _t x) = fits (w - l) x
  fits _ (SLine _ _)    = true
  fits w (SAnnPush _ x) = fits w x
  fits w (SAnnPop x)    = fits w x

layoutPretty (LayoutOptions Unbounded) = layoutUnbounded


layoutSmart
  :: forall ann
   . LayoutOptions
  -> Doc ann
  -> SimpleDocStream ann
layoutSmart (LayoutOptions pageWidth_@(AvailablePerLine lineLength ribbonFraction)) =
  layoutWadlerLeijen (FittingPredicate fits) pageWidth_
  where
  fits :: Int -> Int -> Maybe Int -> SimpleDocStream ann -> Boolean
  fits lineIndent currentColumn initialIndentY = go availableWidth
    where
      availableWidth = remainingWidth lineLength ribbonFraction lineIndent currentColumn

      minNestingLevel = case initialIndentY of
          Just i  -> min i currentColumn
          Nothing -> currentColumn

      go w _ | w < 0          = false
      go _ SFail              = false
      go _ SEmpty             = true
      go w (SChar _ x)        = go (w - 1) x
      go w (SText l _t x)     = go (w - l) x
      go _ (SLine i x)
        | minNestingLevel < i = go (lineLength - i) x
        | otherwise           = true
      go w (SAnnPush _ x)     = go w x
      go w (SAnnPop x)        = go w x

layoutSmart (LayoutOptions Unbounded) = layoutUnbounded


layoutUnbounded :: forall ann. Doc ann -> SimpleDocStream ann
layoutUnbounded =
  layoutWadlerLeijen
    (FittingPredicate
      (\_lineIndent _currentColumn _initialIndentY sdoc -> not (failsOnFirstLine sdoc)))
    Unbounded
  where
    failsOnFirstLine :: SimpleDocStream ann -> Boolean
    failsOnFirstLine = go
      where
      go sds = case sds of
        SFail        -> true
        SEmpty       -> false
        SChar _ s    -> go s
        SText _ _ s  -> go s
        SLine _ _    -> false
        SAnnPush _ s -> go s
        SAnnPop s    -> go s


layoutWadlerLeijen
  :: forall ann
   . FittingPredicate ann
  -> PageWidth
  -> Doc ann
  -> SimpleDocStream ann
layoutWadlerLeijen (FittingPredicate fits) pageWidth_ doc =
  best 0 0 (Cons 0 doc Nil)
  where
  initialIndentation :: SimpleDocStream ann -> Maybe Int
  initialIndentation sds = case sds of
    SLine i _    -> Just i
    SAnnPush _ s -> initialIndentation s
    SAnnPop s    -> initialIndentation s
    _            -> Nothing

  selectNicer
    :: Int
    -> Int
    -> SimpleDocStream ann
    -> SimpleDocStream ann
    -> SimpleDocStream ann
  selectNicer lineIndent currentColumn x y
    | fits lineIndent currentColumn (initialIndentation y) x = x
    | otherwise = y

  best
    :: Int
    -> Int
    -> LayoutPipeline ann
    -> SimpleDocStream ann
  best _ _ Nil             = SEmpty
  best nl cc (UndoAnn ds)  = SAnnPop (best nl cc ds)
  best nl cc (Cons i d ds) = case d of
    Fail            -> SFail
    Empty           -> best nl cc ds
    Char c          -> let cc' = cc + 1 in SChar c (best nl cc' ds)
    Text l t        -> let cc' = cc + l in SText l t (best nl cc' ds)
    Line            -> SLine i (best i i ds)
    FlatAlt x _     -> best nl cc (Cons i x ds)
    Cat x y         -> best nl cc (Cons i x (Cons i y ds))
    Nest j x        -> let ij = i + j in best nl cc (Cons ij x ds)
    Union x y       -> let x' = best nl cc (Cons i x ds)
                           y' = best nl cc (Cons i y ds)
                       in selectNicer nl cc x' y'
    Column f        -> best nl cc (Cons i (f cc) ds)
    WithPageWidth f -> best nl cc (Cons i (f pageWidth_) ds)
    Nesting f       -> best nl cc (Cons i (f i) ds)
    Annotated ann x -> SAnnPush ann (best nl cc (Cons i x (UndoAnn ds)))


layoutCompact :: forall ann. Doc ann -> SimpleDocStream ann
layoutCompact doc = scan 0 [doc]
  where
  scan col ds = case A.uncons ds of
    Nothing             -> SEmpty
    Just { head, tail } -> case head of
      Fail            -> SFail
      Empty           -> scan col tail
      Char c          -> SChar c (scan (col + 1) tail)
      Text l t        -> let col' = col + l in SText l t (scan col' tail)
      FlatAlt x _     -> scan col (x : tail)
      Line            -> SLine 0 (scan 0 tail)
      Cat x y         -> scan col (x : y : tail)
      Nest _ x        -> scan col (x : tail)
      Union _ y       -> scan col (y : tail)
      Column f        -> scan col (f col : tail)
      WithPageWidth f -> scan col (f Unbounded : tail)
      Nesting f       -> scan col (f 0 : tail)
      Annotated _ x   -> scan col (x : tail)


instance showDoc :: Show (Doc ann) where
  show doc = renderShowS (layoutPretty defaultLayoutOptions doc)


renderShowS :: forall ann. SimpleDocStream ann -> String
renderShowS = case _ of
  SFail        -> force panicUncaughtFail
  SEmpty       -> ""
  SChar c x    -> S.singleton c <> renderShowS x
  SText _l t x -> t <> renderShowS x
  SLine i x    -> fromCodePointArray ((codePointFromChar '\n') : A.replicate i (codePointFromChar ' '))
                  <> renderShowS x
  SAnnPush _ x -> renderShowS x
  SAnnPop x    -> renderShowS x


textSpaces :: Int -> String
textSpaces n = fromCodePointArray (A.replicate n (codePointFromChar ' '))
