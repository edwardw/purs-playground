module Run.Console
  ( error
  , errorShow
  , info
  , infoShow
  , log
  , logShow
  , warn
  , warnShow
  , run
  , runNoConsole
  , runAccum
  , ConsoleF(..)
  , CONSOLE
  , _console
  )
  where

import Prelude
import Effect.Console as EC
import Data.List (reverse)
import Data.List.Types (List(..))
import Run (EFFECT, FProxy, Run, SProxy(..), Step(..), interpretRec, liftEffect, on, onMatch, send)
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

-- | Accumulates all console messages into a list
-- | but does not print any to the console.
-- |
-- | Useful when you want to see what would be printed to the console.
runAccum
  :: forall a r
   . Run (console :: CONSOLE | r) a
  -> Run r (List String)
runAccum x = reverse <$> runPure cons (Nil <$ x)
  where
  cons = case _ of
    Error str w -> Cons str <$> w
    Info str w -> Cons str <$> w
    Log str w -> Cons str <$> w
    Warn str w -> Cons str <$> w


-- | Prints all messages to the console.
-- |
-- | Normally this is what you'll use to print to the console.
run
  :: forall r
   . Run (effect :: EFFECT, console :: CONSOLE | r)
  ~> Run (effect :: EFFECT | r)
run = interpretRec (on _console handleConsole send)

handleConsole
  :: forall r
   . ConsoleF
  ~> Run (effect :: EFFECT | r)
handleConsole = case _ of
  Error str x -> do
    liftEffect $ EC.error str
    pure x
  Info str x -> do
    liftEffect $ EC.info str
    pure x
  Log str x -> do
    liftEffect $ EC.log str
    pure x
  Warn str x -> do
    liftEffect $ EC.warn str
    pure x


-- | Runs without printing any messages to the console.
-- |
-- | Useful when you want to eliminate the `CONSOLE` type
-- | without printing anything to the console.
runNoConsole :: forall r. Run (console :: CONSOLE | r) ~> Run r
runNoConsole = runPure case _ of
  Error _ w -> w
  Info _ w -> w
  Log _ w -> w
  Warn _ w -> w

-- | Runs the given function in a pure context eliminating the `CONSOLE` type.
-- |
-- | Useful for building up new pure interpreters.
runPure
  :: forall a r
  . (ConsoleF (Run (console :: CONSOLE | r) a) -> Run (console :: CONSOLE | r) a)
  -> Run (console :: CONSOLE | r) a
  -> Run r a
runPure f = Run.runPure $ onMatch { console: Loop <<< f } Done
