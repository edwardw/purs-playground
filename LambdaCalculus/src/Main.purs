module Main where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (bracket, launchAff_)
import PCF as PCF
import Run (runBaseAff')
import Run.Console (runConsole)
import Run.Node.ReadLine (runReadLine)
import Run.Node.ReadLine as RL
import Run.Reader (runReader)

main :: Effect Unit
main = launchAff_ $ bracket
  (liftEffect $ RL.createConsoleInterface RL.noCompletion)
  (liftEffect <<< RL.close)
  (\iface ->
    PCF.repl
      # runReadLine
      # runConsole
      # runReader iface
      # runBaseAff'
  )
