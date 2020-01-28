module Main where

import Prelude (Unit)
import Effect (Effect)
import Effect.Console (logShow)
import WordNumbers (answer)

main :: Effect Unit
main = logShow answer
