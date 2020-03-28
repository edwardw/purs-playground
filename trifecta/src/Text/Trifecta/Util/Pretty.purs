module Text.Trifecta.Util.Pretty
  ( module TerminalExport

  , char

  , debold, deunderline

  , renderPretty, columns
  ) where


import Prelude
import Data.Maybe (Maybe(..))
import Data.Text.Prettyprint.Doc (Doc, LayoutOptions(..), PageWidth(..), SimpleDocStream, layoutSmart, pageWidth, pretty)
import Data.Text.Prettyprint.Doc.Render.Terminal (bold, renderIO, underlined) as TerminalExport
import Data.Text.Prettyprint.Doc.Render.Terminal.Internal (AnsiStyle(..))
import Data.Text.Prettyprint.Doc.Render.Terminal.Internal (AnsiStyle) as TerminalExport



char :: forall a. Char -> Doc a
char = pretty


renderPretty :: Number -> Int -> Doc AnsiStyle -> SimpleDocStream AnsiStyle
renderPretty ribbonFraction page =
  layoutSmart (LayoutOptions (AvailablePerLine page ribbonFraction))


debold :: AnsiStyle
debold =
  let AnsiStyle blank = mempty
  in AnsiStyle $ blank { ansiBold = Nothing }


deunderline :: AnsiStyle
deunderline =
  let AnsiStyle blank = mempty
  in AnsiStyle $ blank { ansiUnderlining = Nothing }


columns :: (Maybe Int -> Doc AnsiStyle) -> Doc AnsiStyle
columns f = pageWidth (f <<< toMaybeInt)
  where
  toMaybeInt (AvailablePerLine cpl _) = Just cpl
  toMaybeInt Unbounded = Nothing
