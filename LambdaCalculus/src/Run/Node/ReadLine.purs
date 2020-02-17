module Run.Node.ReadLine
  ( ReadLineF(..)
  , READLINE
  , _readline
  , setPrompt
  , prompt
  , runReadLine
  , runReadLineAccum
  , module RLExports
  ) where

import Prelude
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.Tuple (Tuple(..))
import Effect.Aff (makeAff, nonCanceler)
import Effect.Aff.Class (liftAff)
import Node.ReadLine (Interface)
import Node.ReadLine (Interface, close, createConsoleInterface, noCompletion) as RLExports
import Node.ReadLine as RL
import Run (AFF, EFFECT, FProxy, Run, SProxy(..), Step(..), interpretRec, liftEffect, on, runAccumPure, send)
import Run as Run
import Run.Reader (READER, ask)

-- | The possible actions we can perform on command line
data ReadLineF a
  = SetPrompt String a
  | Prompt (String -> a)

-- Much boiler plate
derive instance functorReadLineF :: Functor ReadLineF

type READLINE = FProxy ReadLineF

_readline = SProxy :: SProxy "readline"

setPrompt :: forall r. String -> Run (readline :: READLINE | r) Unit
setPrompt str = Run.lift _readline $ SetPrompt str unit

prompt :: forall r. Run (readline :: READLINE | r) String
prompt = Run.lift _readline $ Prompt identity


-- | Run effectful command line
runReadLine
  :: forall r
   . Run (aff :: AFF, effect :: EFFECT, reader :: READER Interface, readline :: READLINE | r)
  ~> Run (aff :: AFF, effect :: EFFECT, reader :: READER Interface | r)
runReadLine = interpretRec (on _readline handleReadLine send)

handleReadLine
  :: forall r
   . ReadLineF
  ~> Run (aff :: AFF, effect :: EFFECT, reader :: READER Interface | r)
handleReadLine = case _ of
  SetPrompt str cb -> do
    iface <- ask
    liftEffect $ RL.setPrompt str (length str) iface
    pure cb
  Prompt cb -> do
    iface <- ask
    input <- go iface
    pure $ cb input
    where
      go iface =
        liftAff $ makeAff \handler -> do
          RL.prompt iface
          RL.setLineHandler iface (handler <<< Right)
          pure nonCanceler

-- | Run pure "command line", providing the given inputs. It expects the
-- | interpreter to stop when the input is `ctrl-d`.
runReadLineAccum
  :: forall r a
   . Array String
  -> Run (readline :: READLINE | r) a
  -> Run r (Tuple (Array String) a)
runReadLineAccum = runAccumPure
  (\inputs -> on _readline (Loop <<< handleAccum inputs) Done)
  Tuple

handleAccum :: forall a. Array String -> ReadLineF a -> Tuple (Array String) a
handleAccum inputs = case _ of
  SetPrompt _ cb -> Tuple inputs cb
  Prompt cb -> case uncons inputs of
    Just { head, tail } -> Tuple tail (cb head)
    Nothing -> Tuple mempty (cb "\\d")
