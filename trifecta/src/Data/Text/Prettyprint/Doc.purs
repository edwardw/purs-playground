module Data.Text.Prettyprint.Doc
  ( module Data.Text.Prettyprint.Doc.Internal
  , module Data.Text.Prettyprint.Doc.Symbols.Ascii
  ) where

import Data.Text.Prettyprint.Doc.Internal
  ( -- | Documents
    Doc

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
  , (<+>)

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
  )
import Data.Text.Prettyprint.Doc.Symbols.Ascii (angles, backslash, braces, brackets, colon, comma, dot, dquote, dquotes, equals, langle, lbrace, lbracket, lparen, parens, pipe, rangle, rbrace, rbracket, rparen, semi, slash, space, squote, squotes)
