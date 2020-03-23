module Data.Text.Prettyprint.Doc.Render.Util.StackMachine where

import Prelude
import Control.Apply (lift2)
import Data.Array ((:))
import Data.Array as A
import Data.Lazy (force)
import Data.Maybe
import Data.String as S
import Data.Text.Prettyprint.Doc.Internal
import Data.Text.Prettyprint.Doc.Render.Util.Panic



renderSimplyDecorated
  :: forall ann out
   . Monoid out
  => (String -> out)
  -> (ann -> out)
  -> (ann -> out)
  -> SimpleDocStream ann
  -> out
renderSimplyDecorated text push pop = go []
  where
  go _     SFail               = force panicUncaughtFail
  go []    SEmpty              = mempty
  go _     SEmpty              = force panicInputNotFullyConsumed
  go stack (SChar c rest)      = text (S.singleton c) <> go stack rest
  go stack (SText _ t rest)    = text t <> go stack rest
  go stack (SLine i rest)      = text "\n" <> text (textSpaces i) <> go stack rest
  go stack (SAnnPush ann rest) = push ann <> go (ann : stack) rest
  go stack (SAnnPop rest)      = case A.uncons stack of
    Nothing             -> force panicUnpairedPop
    Just { head, tail } -> pop head <> go tail rest


liftedAppend :: forall f a. Applicative f => Semigroup a => f a -> f a -> f a
liftedAppend = lift2 (<>)

infixr 5 liftedAppend as <++>


renderSimplyDecoratedA
  :: forall ann out f
   . Applicative f
  => Monoid out
  => (String -> f out)
  -> (ann -> f out)
  -> (ann -> f out)
  -> SimpleDocStream ann
  -> f out
renderSimplyDecoratedA text push pop = go []
  where
  go _     SFail               = force panicUncaughtFail
  go []    SEmpty              = pure mempty
  go _     SEmpty              = force panicInputNotFullyConsumed
  go stack (SChar c rest)      = text (S.singleton c) <++> go stack rest
  go stack (SText _ t rest)    = text t <++> go stack rest
  go stack (SLine i rest)      = text "\n" <++> text (textSpaces i) <++> go stack rest
  go stack (SAnnPush ann rest) = push ann <++> go (ann : stack) rest
  go stack (SAnnPop rest)      = case A.uncons stack of
    Nothing             -> force panicUnpairedPop
    Just { head, tail } -> pop head `lift2 append` go tail rest
