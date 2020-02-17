module Main where

import Prelude
import Data.Either (Either(..))
import Data.Map as M
import Effect (Effect)
import Effect.Aff (launchAff_)
import LambdaCalculus (Env, LambdaLine(..), line, norm)
import Run (Run, runBaseAff')
import Run.Console (CONSOLE, error, logShow, runConsole)
import Run.Node.ReadLine (READLINE, prompt, runReadLine, setPrompt)
import Run.Node.ReadLine as RL
import Run.Reader (runReader)
import Run.State (STATE, get, modify, runState)
import Text.Parsing.Parser (runParser)

main :: Effect Unit
main = do
  iface <- RL.createConsoleInterface RL.noCompletion
  program
    # runReadLine
    # runConsole
    # runReader iface
    # runState M.empty
    # runBaseAff'
    # launchAff_

program :: forall r. Run ( state :: STATE Env
                         , readline :: READLINE
                         , console :: CONSOLE
                         | r) Unit
program = do
  setPrompt "λ> "
  s <- prompt
  when (s /= "\\d") do
    case runParser s line of
      Left err ->
        error $ "parse error: " <> show err
      Right Blank -> pure unit
      Right (Run t) -> do
        env <- get
        logShow $ norm env t
      Right (Let v t) ->
        modify $ M.insert v t
    program
