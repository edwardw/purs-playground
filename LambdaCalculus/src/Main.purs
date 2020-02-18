module Main where

import Prelude
import Data.Either (Either(..))
import Data.Map as M
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (bracket, launchAff_)
import LambdaCalculus (Env, LambdaLine(..), line, norm)
import Run (Run, runBaseAff')
import Run.Console (CONSOLE, error, logShow, runConsole)
import Run.Node.ReadLine (READLINE, prompt, runReadLine, setPrompt)
import Run.Node.ReadLine as RL
import Run.Reader (runReader)
import Run.State (STATE, get, modify, runState)
import Text.Parsing.Parser (runParser)

main :: Effect Unit
main = launchAff_ $ bracket
  (liftEffect $ RL.createConsoleInterface RL.noCompletion)
  (liftEffect <<< RL.close)
  (\iface ->
    program
      # runReadLine
      # runConsole
      # runReader iface
      # runState M.empty
      # runBaseAff'
  )

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
