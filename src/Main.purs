module Main where

import Prelude
import Data.Either (Either(..))
import Data.Map as M
import Effect (Effect)
import Effect.Console (log, logShow)
import LambdaCalculus (LambdaLine(..), Env, line, norm)
import Node.ReadLine as RL
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
