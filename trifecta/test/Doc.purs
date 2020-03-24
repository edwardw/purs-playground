module Test.Doc (testDoc) where

import Prelude
import Control.Lazy (fix)
import Control.Monad.Gen as Gen
import Data.Char.Gen (genAsciiChar)
import Data.Int (quot)
import Data.Lazy (Lazy, defer, force)
import Data.List as L
import Data.List.Lazy as LL
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.String (codePointFromChar)
import Data.Text.Prettyprint.Doc (Doc, FusionDepth(..), LayoutOptions(..), PageWidth(..), SimpleDocStream(..), align, alterAnnotationsS, angles, annotate, array, braces, brackets, cat, defaultLayoutOptions, dquotes, emptyDoc, encloseSep, fillCat, fillSep, flatAlt, fuse, group, hang, hardline, hcat, hsep, indent, layoutCompact, layoutPretty, layoutSmart, line, line', nest, parens, pretty, removeTrailingWhitespace, sep, softline, softline', squotes, tupled, vcat, vsep, (<+>))
import Data.Text.Prettyprint.Doc.Render.Text (render)
import Data.Text.Prettyprint.Doc.Render.Util.StackMachine (renderSimplyDecorated)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.QuickCheck (Result, (<?>))
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, frequency, oneOf, perturbGen)
import Test.Unit (Test, suite, test, timeout)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)


testDoc :: Effect Unit
testDoc = runTest do
  suite "Fusion" do
    test "Shallow fusion does not change rendering" do
      quickCheck $ fusionDoesNotChangeRendering Shallow
    test "Deep fusion does not change rendering" do
      quickCheck $ fusionDoesNotChangeRendering Deep

  suite "Performace tests" do
    test "Grouping performance" groupingPerformance
    test "fillSep performance" fillSepPerformance

  suite "Regression tests" do
    test "layoutSmart: softline behaves like a newline (#49)"
      regressionLayoutSmartSoftline
    test "alterAnnotationsS causes panic when removing annotations (#50)"
      regressionAlterAnnotationsS
    test "Bad fallback handling with align (#83)" badFallbackAlign

    suite "removeTrailingWhitespace removes leading whitespace (#84)" do
      test "Text node" doNotRemoveLeadingWhitespaceText
      test "Char node" doNotRemoveLeadingWhitespaceChar
      test "Text+Char nodes" doNotRemoveLeadingWhitespaceTextChar

    suite "removeTrailingWhitespace removes trailing line breaks (#86)" do
      test "Keep lonely single trailing newline"
        removeTrailingWhitespaceKeepLonelyTrailingNewline
      test "Trailing newline with spaces"
        removeTrailingNewlineWithSpaces
      test "Keep single trailing newline"
        removeTrailingWhitespaceKeepTrailingNewline
      test "Reduce to single trailing newline"
        removeTrailingWhitespaceInTrailingNewlines

    suite "removeTrailingWhitespace restores indentation in the wrong spot (#93)" do
      test "Don't restore indentation in the wrong spot"
        removeTrailingWhitespaceDontRestoreIndentationInTheWrongSpot
      test "Preserve leading indentation"
        removeTrailingWhitespacePreserveIndentation

    suite "Unbounded layout of hard linebreak within `group` fails (#91)" do
      test "Line" regressionUnboundedGroupedLine
      test "Line whithin align" regressionUnboundedGroupedLineWithinAlign



--------------------------------------------------------------------------------
-- Test Facility ---------------------------------------------------------------
--------------------------------------------------------------------------------

newtype D ann = D (Doc ann)

instance arbDocument :: Arbitrary ann => Arbitrary (D ann) where
  arbitrary = map D doc


doc :: forall ann. Arbitrary ann => Gen (Doc ann)
doc = fix \p -> scale <<< frequency $ options p
  where
  scale = Gen.resize (\n -> (n * 2) `quot` 3)

  annotated p = annotate <$> arbitrary <*> p

  content = frequency $ (Tuple 1.0 (pure emptyDoc))
                     :| (L.singleton (Tuple 1.0 (map pretty genAsciiChar)))

  newlines = frequency $ (Tuple 1.0 (pure line))
                      :| L.fromFoldable
                         [ Tuple 1.0 (pure line')
                         , Tuple 1.0 (pure softline)
                         , Tuple 1.0 (pure softline')
                         , Tuple 1.0 (pure hardline)
                         ]

  concatenationOfMany p =
    frequency $ (Tuple 1.0 (hsep     <$> arrayOf p))
             :| L.fromFoldable
                [ Tuple 1.0 (vsep    <$> arrayOf p)
                , Tuple 1.0 (fillSep <$> arrayOf p)
                , Tuple 1.0 (sep     <$> arrayOf p)
                , Tuple 1.0 (hcat    <$> arrayOf p)
                , Tuple 1.0 (vcat    <$> arrayOf p)
                , Tuple 1.0 (fillCat <$> arrayOf p)
                , Tuple 1.0 (cat     <$> arrayOf p)
                ]

  nestingAndAlignment p =
    frequency $ (Tuple 1.0 (nest    <$> arbitrary <*> concatenationOfMany p))
             :| L.fromFoldable
                [ Tuple 1.0 (group  <$> p)
                , Tuple 1.0 (hang   <$> arbitrary <*> concatenationOfMany p)
                , Tuple 1.0 (indent <$> arbitrary <*> concatenationOfMany p)
                ]

  grouping p = frequency $ (Tuple 1.0 (align <$> p))
                        :| L.singleton (Tuple 1.0 (flatAlt <$> p <*> p))


  concatenationOfTwo p =
    frequency $ (Tuple 1.0 ((<>) <$> p <*> p))
            :| L.singleton (Tuple 1.0 ((<+>) <$> p <*> p))

  enclosingOfOne p =
    frequency $ (Tuple 1.0 (squotes   <$> p))
             :| L.fromFoldable
                [ Tuple 1.0 (dquotes  <$> p)
                , Tuple 1.0 (parens   <$> p)
                , Tuple 1.0 (angles   <$> p)
                , Tuple 1.0 (brackets <$> p)
                , Tuple 1.0 (braces   <$> p)
                ]

  enclosingOfMany p =
    frequency $ (Tuple 1.0 (encloseSep <$> p <*> p <*> pure (pretty ", ") <*> arrayOf p))
             :| L.fromFoldable
                [ Tuple 1.0 (array  <$> arrayOf p)
                , Tuple 1.0 (tupled <$> arrayOf p)
                ]

  options p = Tuple 20.0 content
           :| L.fromFoldable
              [ Tuple 1.0  newlines
              , Tuple 1.0  (nestingAndAlignment p)
              , Tuple 1.0  (grouping p)
              , Tuple 20.0 (concatenationOfTwo p)
              , Tuple 5.0  (concatenationOfMany p)
              , Tuple 1.0  (enclosingOfOne p)
              , Tuple 1.0  (enclosingOfMany p)
              , Tuple 1.0  (annotated p)
              ]



data Layouter ann
  = LayoutPretty LO
  | LayoutSmart LO
  | LayoutCompact

instance showLayouter :: Show (Layouter ann) where
  show = case _ of
    LayoutPretty lo -> "LayoutPretty " <> show lo
    LayoutSmart lo  -> "LayoutSmart " <> show lo
    LayoutCompact   -> "LayoutCompact"

instance arbLayouter :: Arbitrary (Layouter ann) where
  arbitrary = oneOf options
    where
    options = (LayoutPretty <$> arbitrary)
           :| [LayoutSmart <$> arbitrary, pure LayoutCompact]


layout :: forall ann. Layouter ann -> Doc ann -> SimpleDocStream ann
layout = case _ of
  LayoutPretty (LO lo) -> layoutPretty lo
  LayoutSmart (LO lo)  -> layoutSmart lo
  LayoutCompact        -> layoutCompact


newtype LO = LO LayoutOptions

instance showLO :: Show LO where
  show (LO lo) = show lo

instance arbLO :: Arbitrary LO where
  arbitrary = (LO <<< LayoutOptions) <$> oneOf options
    where
    options = (AvailablePerLine <$> arbitrary <*> arbitrary)
           :| [pure Unbounded]


newtype SDS ann = SDS (SimpleDocStream ann)

instance coarbSDS :: Coarbitrary (SDS ann) where
  coarbitrary = case _ of
    SDS SFail             -> perturbGen 0.0
    SDS SEmpty            -> perturbGen 1.0
    SDS (SChar _ rest)    -> perturbGen 2.0 <<< coarbitrary (SDS rest)
    SDS (SText l _ rest)  -> perturbGen 3.0 <<< coarbitrary (Tuple l (SDS rest))
    SDS (SLine i rest)    -> perturbGen 4.0 <<< coarbitrary (Tuple i (SDS rest))
    SDS (SAnnPush _ rest) -> perturbGen 5.0 <<< coarbitrary (SDS rest)
    SDS (SAnnPop rest)    -> perturbGen 6.0 <<< coarbitrary (SDS rest)


newtype PW = PW PageWidth

instance coarbPW :: Coarbitrary PW where
  coarbitrary = case _ of
    PW (AvailablePerLine a b) -> perturbGen 0.0 <<< coarbitrary (Tuple a b)
    PW Unbounded              -> perturbGen 1.0


fusionDoesNotChangeRendering :: FusionDepth -> D Int -> Layouter Int -> Result
fusionDoesNotChangeRendering depth (D d) layouter =
  let render' = renderSimplyDecorated identity show show <<< layout layouter
      rendered = render' d
      renderedFused = render' (fuse depth d)
      mkCounterexample = render <<< layoutPretty defaultLayoutOptions $ vsep
        [ pretty "Unfused and fused documents render differently!"
        , pretty "Unfused:"
        , indent 4 (pretty rendered)
        , pretty "Fused:"
        , indent 4 (pretty renderedFused)
        ]
  in (rendered == renderedFused) <?> mkCounterexample


docPerformanceTest :: forall a. Lazy a -> Test
docPerformanceTest x =
  -- tiemout in 10 seconds
  timeout 10000 (pure (force x) $> unit)


groupingPerformance :: Test
groupingPerformance = docPerformanceTest (defer \_ -> pathological 1000)
  where
  pathological n = LL.length $ LL.take n (LL.iterate (\x -> hsep [x, sep []]) (pretty "foobar"))


fillSepPerformance :: Test
fillSepPerformance = docPerformanceTest (defer \_ -> pathological 1000)
  where
  a = pretty "a"
  b = pretty "b"
  pathological n = LL.length $ LL.take n (LL.iterate (\x -> fillSep [a, x <+> b]) (pretty "foobar"))


regressionLayoutSmartSoftline :: Test
regressionLayoutSmartSoftline =
  let d = pretty "a" <> softline <> pretty "b"
      a = codePointFromChar 'a'
      b = codePointFromChar 'b'
      space = codePointFromChar ' '

      layouted :: SimpleDocStream Unit
      layouted = layoutSmart (LayoutOptions Unbounded) d
  in Assert.equal'
        "softline should be rendered as space page width is unbounded"
        (SChar a (SChar space (SChar b SEmpty)))
        layouted


regressionAlterAnnotationsS :: Test
regressionAlterAnnotationsS =
  let sdoc :: SimpleDocStream Int
      sdoc = layoutSmart defaultLayoutOptions (annotate 1 (annotate 2 (annotate 3 (pretty "a"))))
      sdoc' :: SimpleDocStream Int
      sdoc' = alterAnnotationsS
        (\ann -> case ann of
          2 -> Just 2
          _ -> Nothing) sdoc
      a = codePointFromChar 'a'
      expected = SAnnPush 2 (SChar a (SAnnPop SEmpty))
  in Assert.equal'
      (render expected <> " /= " <> render sdoc')
      (SAnnPush 2 (SChar a (SAnnPop SEmpty)))
      sdoc'


badFallbackAlign :: Test
badFallbackAlign =
  let x = group (flatAlt (pretty "Default") (pretty "Fallback"))
      d = pretty "/" <> align (cat [x, x, pretty "Too wide!!!!!"])
      actual = render $ layoutSmart (LayoutOptions (AvailablePerLine 12 1.0)) d
      expected = "/Fallback\n Fallback\n Too wide!!!!!"
  in Assert.equal'
      (expected <> " /= " <> actual)
      expected actual


doNotRemoveLeadingWhitespaceText :: Test
doNotRemoveLeadingWhitespaceText =
  let x = codePointFromChar 'x'
      sdoc :: SimpleDocStream Unit
      sdoc = SLine 0 (SText 2 "  " (SChar x SEmpty))
      actual = removeTrailingWhitespace sdoc
      expected = SLine 2 (SChar x SEmpty)
  in Assert.equal'
      (render expected <> " /= " <> render actual)
      expected actual


doNotRemoveLeadingWhitespaceChar :: Test
doNotRemoveLeadingWhitespaceChar =
  let s = codePointFromChar ' '
      x = codePointFromChar 'x'
      sdoc :: SimpleDocStream Unit
      sdoc = SLine 0 (SChar s (SChar x SEmpty))
      actual = removeTrailingWhitespace sdoc
      expected = SLine 1 (SChar x SEmpty)
  in Assert.equal'
      (render expected <> " /= " <> render actual)
      expected actual


doNotRemoveLeadingWhitespaceTextChar :: Test
doNotRemoveLeadingWhitespaceTextChar =
  let s = codePointFromChar ' '
      x = codePointFromChar 'x'
      sdoc :: SimpleDocStream Unit
      sdoc = SLine 0 (SChar s (SText 2 "  " (SChar x SEmpty)))
      actual = removeTrailingWhitespace sdoc
      expected = SLine 3 (SChar x SEmpty)
  in Assert.equal'
      (render expected <> " /= " <> render actual)
      expected actual


removeTrailingWhitespaceKeepTrailingNewline :: Test
removeTrailingWhitespaceKeepTrailingNewline =
  let sdoc :: SimpleDocStream Unit
      sdoc = SLine 0 SEmpty
      actual = removeTrailingWhitespace sdoc
  in Assert.equal'
      (render sdoc <> " /= " <> render actual)
      sdoc actual


removeTrailingNewlineWithSpaces :: Test
removeTrailingNewlineWithSpaces =
  let x = codePointFromChar 'x'
      sdoc :: SimpleDocStream Unit
      sdoc = SChar x (SLine 2 (SText 2 "  " SEmpty))
      expected = SChar x (SLine 0 SEmpty)
      actual = removeTrailingWhitespace sdoc
  in Assert.equal'
      (render expected <> " /= " <> render actual)
      expected actual


removeTrailingWhitespaceKeepLonelyTrailingNewline :: Test
removeTrailingWhitespaceKeepLonelyTrailingNewline =
  let x = codePointFromChar 'x'
      sdoc :: SimpleDocStream Unit
      sdoc = SChar x (SLine 0 SEmpty)
      actual = removeTrailingWhitespace sdoc
  in Assert.equal'
      (render sdoc <> " /= " <> render actual)
      sdoc actual


removeTrailingWhitespaceInTrailingNewlines :: Test
removeTrailingWhitespaceInTrailingNewlines =
  let x = codePointFromChar 'x'
      sdoc :: SimpleDocStream Unit
      sdoc = SChar x (SLine 2 (SLine 2 SEmpty))
      expected = SChar x (SLine 0 (SLine 0 SEmpty))
      actual = removeTrailingWhitespace sdoc
  in Assert.equal'
      (render expected <> " /= " <> render actual)
      expected actual


removeTrailingWhitespaceDontRestoreIndentationInTheWrongSpot :: Test
removeTrailingWhitespaceDontRestoreIndentationInTheWrongSpot =
  let x = codePointFromChar 'x'
      sdoc :: SimpleDocStream Unit
      sdoc = SLine 2 (SLine 0 (SChar x SEmpty))
      actual = removeTrailingWhitespace sdoc
      expected = SLine 0 (SLine 0 (SChar x SEmpty))
  in Assert.equal'
      (render expected <> " /= " <> render actual)
      expected actual

removeTrailingWhitespacePreserveIndentation :: Test
removeTrailingWhitespacePreserveIndentation =
  let x = codePointFromChar 'x'
      sdoc :: SimpleDocStream Unit
      sdoc = SLine 2 (SChar x SEmpty)
      actual = removeTrailingWhitespace sdoc
  in Assert.equal'
      (render sdoc <> " /= " <> render actual)
      sdoc actual

regressionUnboundedGroupedLine :: Test
regressionUnboundedGroupedLine =
  let sdoc :: SimpleDocStream Unit
      sdoc = layoutPretty (LayoutOptions Unbounded) (group hardline)
      expected = (SLine 0 SEmpty)
  in Assert.equal'
      (render expected <> " /= " <> render sdoc)
      expected sdoc

regressionUnboundedGroupedLineWithinAlign :: Test
regressionUnboundedGroupedLineWithinAlign =
  let x = codePointFromChar 'x'
      y = codePointFromChar 'y'
      d :: Doc Unit
      d = group (align (pretty "x" <> hardline <> pretty "y"))
      sdoc = layoutPretty (LayoutOptions Unbounded) d
      expected = SChar x (SLine 0 (SChar y SEmpty))
  in Assert.equal expected sdoc
