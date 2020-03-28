module Text.Trifecta.Util.Combinators where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.Pattern (Pattern(..))



takeLine :: String -> String
takeLine s = case S.indexOf (Pattern "\n") s of
  Just i  -> S.take (i + 1) s
  Nothing -> s


argmin :: forall a b. Ord b => (a -> b) -> a -> a -> a
argmin f a b
  | f a <= f b = a
  | otherwise  = b


argmax :: forall a b. Ord b => (a -> b) -> a -> a -> a
argmax f a b
  | f a > f b = a
  | otherwise = b
