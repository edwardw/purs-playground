module Text.Trifecta.Internal.Util where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.Pattern (Pattern(..))



takeLine :: String -> String
takeLine s = case S.indexOf (Pattern "\n") s of
  Just i  -> S.take (i + 1) s
  Nothing -> s
