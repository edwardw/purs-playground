module Data.Text.Prettyprint.Doc.Internal.Debug where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String (CodePoint)
import Data.Text.Prettyprint.Doc.Internal (PageWidth, Doc)
import Data.Text.Prettyprint.Doc.Internal as Doc
import Data.Tuple (Tuple(..))




data Diag ann
    = Fail
    | Empty
    | Char CodePoint
    | Text Int String
    | Line
    | FlatAlt (Diag ann) (Diag ann)
    | Cat (Diag ann) (Diag ann)
    | Nest Int (Diag ann)
    | Union (Diag ann) (Diag ann)
    | Column (Array (Tuple Int (Diag ann)))
    | WithPageWidth (Array (Tuple PageWidth (Diag ann)))
    | Nesting (Array (Tuple Int (Diag ann)))
    | Annotated ann (Diag ann)


derive instance genericDiag :: Generic (Diag ann) _


instance showDiag :: Show ann => Show (Diag ann) where
    show x = genericShow x


diag :: forall ann. Doc ann -> Diag ann
diag = diag' [10] [Doc.defaultPageWidth] [10]


diag'
  :: forall ann
   . Array Int
  -> Array PageWidth
  -> Array Int
  -> Doc ann
  -> Diag ann
diag' columns pageWidths nestings = go
  where
  app :: forall a. (a -> Doc ann) -> Array a -> Array (Tuple a (Diag ann))
  app f = map (\x -> Tuple x (go (f x)))

  go = case _ of
    Doc.Fail            -> Fail
    Doc.Empty           -> Empty
    Doc.Char c          -> Char c
    Doc.Text l t        -> Text l t
    Doc.Line            -> Line
    Doc.FlatAlt a b     -> FlatAlt (go a) (go b)
    Doc.Cat a b         -> Cat (go a) (go b)
    Doc.Nest i d        -> Nest i (go d)
    Doc.Union a b       -> Union (go a) (go b)
    Doc.Column f        -> Column (app f columns)
    Doc.WithPageWidth f -> WithPageWidth (app f pageWidths)
    Doc.Nesting f       -> Nesting (app f nestings)
    Doc.Annotated ann d -> Annotated ann (go d)
