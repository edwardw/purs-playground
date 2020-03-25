module Data.Text.Prettyprint.Doc.Render.Terminal.Internal
  ( AnsiStyle(..)

  , module ANSIExport

  , color, bgColor

  , bold, italicized, underlined

  , render, renderIO
  ) where

import Prelude
import Ansi.Codes (Color, RenderingMode(..))
import Ansi.Codes (Color(..)) as ANSIExport
import Ansi.Codes as ANSI
import Control.Alt ((<|>))
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Data.Array (catMaybes, length, (:))
import Data.Array as A
import Data.Lazy (Lazy, defer, force)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.NonEmpty ((:|))
import Data.String (codePointFromChar, fromCodePointArray)
import Data.String as S
import Data.Text.Prettyprint.Doc (SimpleDocStream(..))
import Data.Text.Prettyprint.Doc.Render.Util.Panic (panicPeekedEmpty, panicUncaughtFail)
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.Process (stdout)
import Node.Stream (uncork, writeString)



color :: Color -> AnsiStyle
color c =
  let AnsiStyle blank = mempty
  in AnsiStyle $ blank { ansiForeground = Just c }


bgColor :: Color -> AnsiStyle
bgColor c =
  let AnsiStyle blank = mempty
  in AnsiStyle $ blank { ansiBackground = Just c }


bold :: AnsiStyle
bold =
  let AnsiStyle blank = mempty
  in AnsiStyle $ blank { ansiBold = Just Bold }


italicized :: AnsiStyle
italicized =
  let AnsiStyle blank = mempty
  in AnsiStyle $ blank { ansiItalics = Just Italic }


underlined :: AnsiStyle
underlined =
  let AnsiStyle blank = mempty
  in AnsiStyle $ blank { ansiUnderlining = Just Underline }


render :: SimpleDocStream AnsiStyle -> String
render sdoc = ST.run do
  styleStackRef <- STRef.new [mempty]
  outputRef <- STRef.new mempty

  let push x = STRef.modify (x : _) styleStackRef $> unit

      unsafePeek = STRef.read styleStackRef >>= \tok -> case A.uncons tok of
        Nothing -> force panicPeekedEmpty
        Just { head: x, tail: _ } -> pure x

      unsafePop = STRef.read styleStackRef >>= \tok -> case A.uncons tok of
        Nothing -> force panicPeekedEmpty
        Just { head: x, tail: xs } -> STRef.write xs styleStackRef *> pure x

      writeOutput x = STRef.modify (_ <> x) outputRef $> unit

      go = case _ of
            SFail -> force panicUncaughtFail
            SEmpty -> pure unit
            SChar c rest -> do
              writeOutput $ S.singleton c
              go rest
            SText _ t rest -> do
              writeOutput t
              go rest
            SLine i rest -> do
              writeOutput $ "\n" <> fromCodePointArray (A.replicate i (codePointFromChar ' '))
              go rest
            SAnnPush style rest -> do
              currentStyle <- unsafePeek
              let newStyle = style <> currentStyle
              push newStyle
              writeOutput $ styleToRawText newStyle
              go rest
            SAnnPop rest -> do
              _ <- unsafePop
              newStyle <- unsafePeek
              writeOutput $ styleToRawText newStyle
              go rest

  go sdoc
  STRef.read styleStackRef >>= case _ of
    []  -> force panicStyleStackFullyConsumed
    [_] -> STRef.read outputRef
    xs  -> force $ panicStyleStackNotFullyConsumed (length xs)


renderIO :: SimpleDocStream AnsiStyle -> Effect Unit
renderIO sdoc = do
  styleStackRef <- Ref.new [mempty]

  let push x = Ref.modify (x : _) styleStackRef $> unit

      unsafePeek = Ref.read styleStackRef >>= \tok -> case A.uncons tok of
        Nothing -> force panicPeekedEmpty
        Just { head: x, tail: _ } -> pure x

      unsafePop = Ref.read styleStackRef >>= \tok -> case A.uncons tok of
        Nothing -> force panicPeekedEmpty
        Just { head: x, tail: xs } -> Ref.write xs styleStackRef *> pure x

      putStr s = writeString stdout UTF8 s (uncork stdout) $> unit

      go = case _ of
            SFail -> force panicUncaughtFail
            SEmpty -> pure unit
            SChar c rest -> do
              putStr $ S.singleton c
              go rest
            SText _ t rest -> do
              putStr  t
              go rest
            SLine i rest -> do
              putStr $ "\n" <> fromCodePointArray (A.replicate i (codePointFromChar ' '))
              go rest
            SAnnPush style rest -> do
              currentStyle <- unsafePeek
              let newStyle = style <> currentStyle
              push newStyle
              putStr $ styleToRawText newStyle
              go rest
            SAnnPop rest -> do
              _ <- unsafePop
              newStyle <- unsafePeek
              putStr $ styleToRawText newStyle
              go rest

  go sdoc
  Ref.read styleStackRef >>= case _ of
    []  -> force panicStyleStackFullyConsumed
    [_] -> pure unit
    xs  -> force $ panicStyleStackNotFullyConsumed (length xs)



panicStyleStackFullyConsumed :: forall a. Lazy a
panicStyleStackFullyConsumed = defer \_ -> unsafeThrow $
  "There is no empty style left at the end of rendering" <>
  " (but there should be). Please report this as a bug."


panicStyleStackNotFullyConsumed :: forall a. Int -> Lazy a
panicStyleStackNotFullyConsumed len = defer \_ -> unsafeThrow $
  "There are " <> show len <> " styles left at the" <>
  "end of rendering (there should be only 1). Please report" <>
  " this as a bug."


data AnsiStyle = AnsiStyle
  { ansiForeground  :: Maybe ANSI.Color
  , ansiBackground  :: Maybe ANSI.Color
  , ansiBold        :: Maybe ANSI.RenderingMode
  , ansiItalics     :: Maybe ANSI.RenderingMode
  , ansiUnderlining :: Maybe ANSI.RenderingMode
  }


instance semigroupAnsiStyle :: Semigroup AnsiStyle where
  append (AnsiStyle cs1) (AnsiStyle cs2) = AnsiStyle
    { ansiForeground : cs1.ansiForeground  <|> cs2.ansiForeground
    , ansiBackground : cs1.ansiBackground  <|> cs2.ansiBackground
    , ansiBold       : cs1.ansiBold        <|> cs2.ansiBold
    , ansiItalics    : cs1.ansiItalics     <|> cs2.ansiItalics
    , ansiUnderlining: cs1.ansiUnderlining <|> cs2.ansiUnderlining
    }


instance monoidAnsiStyle :: Monoid AnsiStyle where
  mempty = AnsiStyle
    { ansiForeground: Nothing
    , ansiBackground: Nothing
    , ansiBold: Nothing
    , ansiItalics: Nothing
    , ansiUnderlining: Nothing
    }


styleToRawText :: AnsiStyle -> String
styleToRawText (AnsiStyle style) = ANSI.escapeCodeToString $ ANSI.Graphics g
  where
  gp = [ ANSI.PForeground <$> style.ansiForeground
       , ANSI.PBackground <$> style.ansiBackground
       , ANSI.PMode <$> style.ansiBold
       , ANSI.PMode <$> style.ansiItalics
       , ANSI.PMode <$> style.ansiUnderlining
       ]

  g = wrap $ ANSI.Reset :| (L.fromFoldable $ catMaybes gp)
