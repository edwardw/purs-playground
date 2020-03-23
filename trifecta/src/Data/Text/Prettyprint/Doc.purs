module Data.Text.Prettyprint.Doc
  ( module Data.Text.Prettyprint.Doc.Internal
  , module Data.Text.Prettyprint.Doc.Symbols.Ascii
  ) where

import Data.Text.Prettyprint.Doc.Internal (class Pretty, Doc, FusionDepth(..), LayoutOptions(..), PageWidth(..), SimpleDocStream(..), align, alterAnnotations, alterAnnotationsS, annotate, array, cat, column, concatWith, defaultLayoutOptions, defaultPageWidth, emptyDoc, enclose, encloseSep, fill, fillBreak, fillCat, fillSep, flatAlt, fuse, group, hang, hardline, hcat, hsep, indent, layoutCompact, layoutPretty, layoutSmart, line, line', nest, nesting, pageWidth, plural, pretty, prettyArray, punctuate, reAnnotate, reAnnotateS, removeTrailingWhitespace, sep, softline, softline', surround, tupled, unAnnotate, unAnnotateS, unsafeTextWithoutNewlines, unsafeViaShow, vcat, viaShow, vsep, width, (<+>))
import Data.Text.Prettyprint.Doc.Symbols.Ascii (angles, backslash, braces, brackets, colon, comma, dot, dquote, dquotes, equals, langle, lbrace, lbracket, lparen, parens, pipe, rangle, rbrace, rbracket, rparen, semi, slash, space, squote, squotes)
