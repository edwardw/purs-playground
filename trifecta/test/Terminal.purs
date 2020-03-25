module Test.Terminal (testTerminal) where

import Prelude
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Text.Prettyprint.Doc (align, annotate, defaultLayoutOptions, layoutPretty, pretty, unAnnotate, vsep, (<+>))
import Data.Text.Prettyprint.Doc.Render.Terminal (Color(..), bold, color, render, underlined)
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)



testTerminal :: Effect Unit
testTerminal = runTest do
  suite "terminal doctest" do
    test "font color and style" do
      let -- replace `\ESC` with `\e` so that the result can be paste directly
          -- after `echo -e` command to see the style
          render' = replaceAll (Pattern "\x1b") (Replacement "\\e") <<< render <<< layoutPretty defaultLayoutOptions
          red = pretty "red"
          blue = pretty "blue+u"
          doc = annotate (color BrightRed) (red <+> align (vsep [annotate (color BrightBlue <> underlined) (blue <+> annotate bold (pretty "bold") <+> blue), red]))
      equal "red blue+u bold blue+u\n    red" (render' $ unAnnotate doc)
      equal "\\e[0;91mred \\e[0;94;4mblue+u \\e[0;94;1;4mbold\\e[0;94;4m blue+u\\e[0;91m\n    red\\e[0m"
            (render' doc)
