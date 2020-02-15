module Main where

import Prelude
import Data.Either (Either(..))
import Data.Map as M
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (log)
import LambdaCalculus (Env, LambdaLine(..), line, norm)
import Node.ReadLine as RL
import Node.ReadLine.Aff as RLAff
import Run (AFF, EFFECT, FProxy, Run, SProxy(..), interpret, liftEffect, on, runBaseAff', send)
import Run as Run
import Run.Reader (READER, ask, runReader)
import Run.State (STATE, get, modify, runState)
import Text.Parsing.Parser (runParser)

main :: Effect Unit
main = do
  interface <- RL.createConsoleInterface RL.noCompletion
  RL.setPrompt "λ> " 3 interface
  repl
    # runReplInReadline
    # runReader interface
    # runState M.empty
    # runBaseAff'
    # launchAff_

---

data ReplF a
  = ReplInput (String -> a)
  | ReplOutput String a

derive instance functorReplF :: Functor ReplF

type REPL = FProxy ReplF

_repl = SProxy :: SProxy "repl"

replInput :: forall r. Run (repl :: REPL | r) String
replInput = Run.lift _repl $ ReplInput identity

replOutput :: forall r. String -> Run(repl :: REPL | r) Unit
replOutput s = Run.lift _repl $ ReplOutput s unit

readlineRepl :: forall r. ReplF ~> Run (aff :: AFF, effect :: EFFECT, reader :: READER RL.Interface | r)
readlineRepl = case _ of
  ReplInput go -> do
    interface <- ask
    s <- RLAff.prompt interface
    pure $ go s
  ReplOutput s go -> do
    liftEffect $ log s
    pure go

runReplInReadline
  :: forall r
   . Run (aff :: AFF, effect :: EFFECT, reader :: READER RL.Interface, repl :: REPL | r)
  ~> Run (aff :: AFF, effect :: EFFECT, reader :: READER RL.Interface | r)
runReplInReadline = interpret (on _repl readlineRepl send)

---

type ReplEffects r =
  ( aff :: AFF
  , effect :: EFFECT
  , reader :: READER RL.Interface
  , state :: STATE Env
  , repl :: REPL | r
  )

repl :: forall r. Run (ReplEffects r) Unit
repl = do
  s <- replInput
  case runParser s line of
    Left err ->
      replOutput $ "parse error: " <> show err
    Right Blank -> pure unit
    Right (Run t) -> do
      env <- get
      replOutput <<< show $ norm env t
    Right (Let v t) ->
      modify $ M.insert v t
  repl
