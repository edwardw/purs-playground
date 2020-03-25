module Data.Text.Prettyprint.Doc.Render.Terminal
  (module Data.Text.Prettyprint.Doc.Render.Terminal.Internal) where

import Data.Text.Prettyprint.Doc.Render.Terminal.Internal
  ( -- | Styling
    AnsiStyle
  , Color(..)

    -- | Font color
  , color

    -- | Background color
  , bgColor

    -- | Font style
  , bold, italicized, underlined

    -- | Conversion to ANSI-infused text
  , render

    -- | Render directly to console
  , renderIO
  )
