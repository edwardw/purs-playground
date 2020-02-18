module Run.Console
  ( error
  , errorShow
  , info
  , infoShow
  , log
  , logShow
  , warn
  , warnShow
  , runConsole
  , runNoConsole
  , runConsoleAccum
  , ConsoleF(..)
  , CONSOLE
  , _console
  )
  where

import Prelude
import Effect.Console as EC
import Data.Array (snoc)
import Data.Tuple (Tuple(..))
import Run (EFFECT, FProxy, Run, SProxy(..), Step(..), interpretRec, liftEffect, on, runAccumPure, runPure, send)
import Run as Run

-- | The possible messages we can have on the console
-- |
-- | You'll only need to interact with this if you are writing an interpreter.
data ConsoleF a
  = Error String a
  | Info String a
  | Log String a
  | Warn String a

derive instance functorConsoleF :: Functor ConsoleF

type CONSOLE = FProxy ConsoleF

_console = SProxy :: SProxy "console"

error :: forall r. String -> Run (console :: CONSOLE | r) Unit
error str = Run.lift _console $ Error str unit


errorShow :: forall a r. Show a => a -> Run (console :: CONSOLE | r) Unit
errorShow x = Run.lift _console $ Error (show x) unit

info :: forall r. String -> Run (console :: CONSOLE | r) Unit
info str = Run.lift _console $ Info str unit

infoShow :: forall a r. Show a => a -> Run (console :: CONSOLE | r) Unit
infoShow x = Run.lift _console $ Info (show x) unit

log :: forall r. String -> Run (console :: CONSOLE | r) Unit
log str = Run.lift _console $ Log str unit

logShow :: forall a r. Show a => a -> Run (console :: CONSOLE | r) Unit
logShow x = Run.lift _console $ Log (show x) unit

warn :: forall r. String -> Run (console :: CONSOLE | r) Unit
warn str = Run.lift _console $ Warn str unit

warnShow :: forall a r. Show a => a -> Run (console :: CONSOLE | r) Unit
warnShow x = Run.lift _console $ Warn (show x) unit

-- | Accumulates all console messages into a array
-- | but does not print any to the console.
-- |
-- | Useful when you want to see what would be printed to the console.
runConsoleAccum
  :: forall r a
   . Run (console :: CONSOLE | r) a
  -> Run r (Array String)
runConsoleAccum = runAccumPure
  (\acc -> on _console (Loop <<< handlePure acc) Done)
  (\acc _ -> acc)
  []
  where
    handlePure acc = case _ of
      Error str cb -> Tuple (snoc acc str) cb
      Info str cb -> Tuple (snoc acc str) cb
      Log str cb -> Tuple (snoc acc str) cb
      Warn str cb -> Tuple (snoc acc str) cb

-- | Prints all messages to the console.
-- |
-- | Normally this is what you'll use to print to the console.
runConsole
  :: forall r
   . Run (effect :: EFFECT, console :: CONSOLE | r)
  ~> Run (effect :: EFFECT | r)
runConsole = interpretRec (on _console handleConsole send)

handleConsole
  :: forall r
   . ConsoleF
  ~> Run (effect :: EFFECT | r)
handleConsole = case _ of
  Error str cb ->
    liftEffect $ EC.error str $> cb
  Info str cb ->
    liftEffect $ EC.info str $> cb
  Log str cb ->
    liftEffect $ EC.log str $> cb
  Warn str cb ->
    liftEffect $ EC.warn str $> cb


-- | Runs without printing any messages to the console.
-- |
-- | Useful when you want to eliminate the `CONSOLE` type
-- | without printing anything to the console.
runNoConsole :: forall r. Run (console :: CONSOLE | r) ~> Run r
runNoConsole = runPure
  (on _console (Loop <<< handlePure) Done)
  where
    handlePure = case _ of
      Error _ cb -> cb
      Info _ cb -> cb
      Log _ cb -> cb
      Warn _ cb -> cb
