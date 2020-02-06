-- https://crypto.stanford.edu/~blynn/lambda/

module Main where

import Prelude
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Effect.Console (log, logShow)
import LambdaCalculus (LambdaLine(..), Env, line, norm)
import Node.ReadLine as NR
import Text.Parsing.Parser (runParser)

repl :: NR.Interface -> Env -> NR.LineHandler Unit
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

prompt :: NR.Interface -> NR.LineHandler Unit -> Effect Unit
prompt console handler = do
  NR.prompt console
  NR.setLineHandler console handler

main :: Effect Unit
main = do
  console <- NR.createConsoleInterface NR.noCompletion
  NR.setPrompt "> " 2 console
  prompt console $ repl console M.empty
