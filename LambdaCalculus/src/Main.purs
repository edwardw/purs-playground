module Main where

import Prelude
import Data.Either (Either(..))
import Data.Map as M
import Effect (Effect)
import Effect.Console (log, logShow)
import LambdaCalculus (Env, LambdaLine(..), Term, line, norm)
import Node.ReadLine as RL
import Run (FProxy, Run, SProxy(..))
import Run as Run
import Run.Except (EXCEPT, throw)
import Run.State (STATE, get, modify)
import Run.Writer (WRITER, tell)
import Text.Parsing.Parser (runParser)

repl :: RL.Interface -> Env -> RL.LineHandler Unit
repl console env s = do
  case runParser s line of
    Left err -> do
      log $ "parse error: " <> show err
      prompt console $ repl console env
    Right Blank -> do
      prompt console $ repl console env
    Right (Run t) -> do
      logShow $ norm env t
      prompt console $ repl console env
    Right (Let v t) -> do
      prompt console <<< repl console $ M.insert v t env

prompt :: RL.Interface -> RL.LineHandler Unit -> Effect Unit
prompt console handler = do
  RL.prompt console
  RL.setLineHandler console handler

main :: Effect Unit
main = do
  console <- RL.createConsoleInterface RL.noCompletion
  RL.setPrompt "λ> " 3 console
  prompt console $ repl console M.empty

---

data LambdaREPLF a
  = ParseLine String a

derive instance functorLambdaREPLF :: Functor LambdaREPLF

type LAMBDAREPL = FProxy LambdaREPLF

_lambdarepl = SProxy :: SProxy "lambdarepl"

parseLine :: forall r. String -> Run (lambdarepl :: LAMBDAREPL | r) Unit
parseLine s = Run.lift _lambdarepl $ ParseLine s unit

type ReplEffects r =
  ( lambdarepl :: LAMBDAREPL
  , state :: STATE Env
  , writer :: WRITER Term
  , except :: EXCEPT String | r
  )

_repl :: forall r. String -> Run (ReplEffects r) Unit
_repl s = do
  case runParser s line of
    Left err ->
      throw $ "parse error: " <> show err
    Right Blank -> pure unit
    Right (Run t) -> do
      env <- get
      tell $ norm env t
    Right (Let v t) ->
      modify $ M.insert v t
