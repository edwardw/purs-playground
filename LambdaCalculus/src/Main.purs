module Main where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Map as M
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (bracket, launchAff_)
import PCF as PCF
import Run (runBaseAff')
import Run.Console (runConsole)
import Run.Node.ReadLine (runReadLine)
import Run.Node.ReadLine as RL
import Run.Reader (runReader)
import Run.State (runState)

main :: Effect Unit
main = launchAff_ $ bracket
  (liftEffect $ RL.createConsoleInterface RL.noCompletion)
  (liftEffect <<< RL.close)
  (\iface ->
    PCF.program
      # runReadLine
      # runConsole
      # runReader iface
      # runState (Tuple [] M.empty)
      # runBaseAff'
  )
