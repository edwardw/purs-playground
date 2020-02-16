module ReadLine
  ( ReadLineF(..)
  , READLINE
  , _readline
  , setPrompt
  , prompt
  , close
  , run
  , runPure
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
import Node.ReadLine (Interface, noCompletion, createConsoleInterface) as RLExports
import Node.ReadLine as RL
import Run (AFF, EFFECT, FProxy, Run, SProxy(..), Step(..), interpret, liftEffect, on, runAccumPure, send)
import Run as Run
import Run.Reader (READER, ask)

-- | The possible actions we can perform on command line
data ReadLineF a
  = SetPrompt String a
  | Prompt (String -> a)
  | Close a

-- Much boiler plate
derive instance functorReadLine :: Functor ReadLineF

type READLINE = FProxy ReadLineF

_readline = SProxy :: SProxy "readline"

setPrompt :: forall r. String -> Run (readline :: READLINE | r) Unit
setPrompt str = Run.lift _readline $ SetPrompt str unit

prompt :: forall r. Run (readline :: READLINE | r) String
prompt = Run.lift _readline $ Prompt identity

close :: forall r. Run (readline :: READLINE | r) Unit
close = Run.lift _readline $ Close unit


-- | Run effectful command line
run
  :: forall r
   . Run (aff :: AFF, effect :: EFFECT, reader :: READER Interface, readline :: READLINE | r)
  ~> Run (aff :: AFF, effect :: EFFECT, reader :: READER Interface | r)
run = interpret (on _readline handleReadLine send)

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
  Close cb -> do
    iface <- ask
    liftEffect $ RL.close iface
    pure cb

-- | Run pure "command line", providing the given inputs. It expects the
-- | interpreter to stop when the input is `ctrl-d`.
runPure
  :: forall r a
   . Array String
  -> Run (readline :: READLINE | r) a
  -> Run r (Tuple (Array String) a)
runPure = runAccumPure
  (\inputs -> on _readline (Loop <<< handlePure inputs) Done)
  Tuple

handlePure :: forall a. Array String -> ReadLineF a -> Tuple (Array String) a
handlePure inputs = case _ of
  SetPrompt _ cb -> Tuple inputs cb
  Prompt cb -> case uncons inputs of
    Just { head, tail } -> Tuple tail (cb head)
    Nothing -> Tuple mempty (cb "\\d")
  Close cb -> Tuple inputs cb
