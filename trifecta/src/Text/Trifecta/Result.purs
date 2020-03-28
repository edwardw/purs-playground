module Text.Trifecta.Result where

import Prelude
import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.MonadZero (guard)
import Control.Plus (class Plus)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Lens (Prism, Prism', prism)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String.Utils (words)
import Data.Text.Prettyprint.Doc (Doc, annotate, fillSep, nest, pretty, punctuate, vsep, (<+>))
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Data.Text.Prettyprint.Doc.Render.Terminal (Color(..), color) as Pretty
import Data.Traversable (class Traversable, sequenceDefault)
import Data.Set (Set)
import Data.Set as Set
import Text.Trifecta.Delta (Delta, delta, prettyDelta)
import Text.Trifecta.Rendering (Rendering, nullRendering, prettyRendering)
import Text.Trifecta.Util.Pretty (char) as Pretty



data ErrInfo = ErrInfo (Doc AnsiStyle) (Array Delta)


instance showErrInfo :: Show ErrInfo where
  show (ErrInfo d ds) = "ErrInfo " <> show d <> " " <> show ds


data Err = Err
  (Maybe (Doc AnsiStyle)) -- reason
  (Array (Doc AnsiStyle)) -- foot notes
  (Set String)            -- expected
  (Array Delta)           -- final deltas


instance semigroupErr :: Semigroup Err where
  append (Err md mds mes delta1) (Err nd nds nes delta2) =
    Err (nd <|> md)
        (if isJust nd then nds else if isJust md then mds else nds <> mds)
        (mes <> nes)
        (delta1 <> delta2)


instance monoidErr :: Monoid Err where
  mempty = Err Nothing [] mempty mempty


-- | Generate a simple 'Err' word-wrapping the supplied message.
failed :: String -> Err
failed m = Err (Just (fillSep (pretty <$> words m))) [] mempty mempty


-- | Convert a `Rendering` of auxiliary information and an `Err` into a
-- | `Doc AnsiStyle`, ready to be prettyprinted to the user.
explain :: Rendering -> Err -> Doc AnsiStyle
explain r (Err mm ds es _) =
  if Set.isEmpty es
  then report (withEx mempty)
  else if isJust mm
  then report <<< withEx $ Pretty.char ',' <+> expecting
  else report expecting
  where
  spaceHack [""] = ["space"]
  spaceHack xs   = A.filter (_ /= "") xs

  now = spaceHack $ A.fromFoldable es
  withEx x = fromMaybe (fillSep $ pretty <$> words "unspecified error") mm <> x
  expecting = pretty "expected:" <+> fillSep (punctuate (Pretty.char ',') (pretty <$> now))
  report txt = vsep $ [prettyDelta (delta r) <> Pretty.char ':' <+> annotate (Pretty.color Pretty.BrightRed) (pretty "error") <> Pretty.char ':' <+> nest 4 txt]
           <|> prettyRendering r <$ guard (not (nullRendering r))
           <|> ds


class Errable m where
  raiseErr :: forall a. Err -> m a


instance semigroupErrInfo :: Semigroup ErrInfo where
  append (ErrInfo xs d1) (ErrInfo ys d2) = ErrInfo (vsep [xs, ys]) (max d1 d2)


instance monoidErrInfo :: Monoid ErrInfo where
  mempty = ErrInfo mempty mempty


-- | The result of parsing. Either we succeeded or something went wrong.
data Result a
  = Success a
  | Failure ErrInfo

derive instance functorResult :: Functor Result


instance showResult :: Show a => Show (Result a) where
  show (Success a) = "Success " <> show a
  show (Failure e) = "Failure " <> show e


instance foldableResult :: Foldable Result where
  foldMap f (Success a) = f a
  foldMap _ (Failure _) = mempty
  foldr f x xs = foldrDefault f x xs
  foldl f x xs = foldlDefault f x xs


instance traversableResult :: Traversable Result where
  traverse f (Success a) = Success <$> f a
  traverse _ (Failure e) = Failure <$> pure e
  sequence xs = sequenceDefault xs


-- | Fold over a 'Result'
foldResult :: forall a b. (ErrInfo -> b) -> (a -> b) -> Result a -> b
foldResult f g r = case r of
  Failure e -> f e
  Success a -> g a


-- | The `Prism` for the `Success` constructor of `Result`
_Success :: forall a b. Prism (Result a) (Result b) a b
_Success = prism Success case _ of
  Success a -> Right a
  Failure e -> Left $ Failure e


-- | The `Prism` for the `Failure` constructor of `Result`
_Failure :: forall a. Prism' (Result a) ErrInfo
_Failure = prism Failure case _ of
  Failure e -> Right e
  Success a -> Left $ Success a


instance applyResult :: Apply Result where
  apply (Success f) (Success a) = Success (f a)
  apply (Success _) (Failure y) = Failure y
  apply (Failure x) (Success _) = Failure x
  apply (Failure (ErrInfo dx dsx)) (Failure (ErrInfo dy dsy)) =
    Failure $ ErrInfo (vsep [dx, dy]) (dsx <> dsy)


instance applicativeResult :: Applicative Result where
  pure = Success


instance altResult :: Alt Result where
  alt (Failure (ErrInfo dx dsx)) (Failure (ErrInfo dy dsy)) =
    Failure $ ErrInfo (vsep [dx, dy]) (dsx <> dsy)
  alt (Success a) (Success _) = Success a
  alt (Success a) (Failure _) = Success a
  alt (Failure _) (Success a) = Success a


instance plusResult :: Plus Result where
  empty = Failure mempty


instance alternativeResult :: Alternative Result


instance bindResult :: Bind Result where
  bind (Success a) m = m a
  bind (Failure e) _ = Failure e


instance monadResult :: Monad Result
