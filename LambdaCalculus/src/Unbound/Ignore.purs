module Unbound.Ignore where

import Prelude
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Unbound.Alpha (class Alpha, NamePatFind(..), NthPatFind(..))


data Ignore t = I t

derive instance genericIgnore :: Generic (Ignore t) _

instance showIgnore :: Show t => Show (Ignore t) where
  show (I t) = "<-" <> show t <> "->"


instance alphaIgnore :: Show t => Alpha (Ignore t) where
  aeq' _ _ _      = true
  fvAny' _ _      = pure
  isPat _         = Nothing
  isTerm          = mempty
  isEmbed _       = false
  close _ _       = identity
  open _ _        = identity
  nthPatFind _    = NthPatFind { runNthPatFind: Left }
  namePatFind _   = NamePatFind { runNamePatFind: const $ Left 0 }
  swaps' _ _      = identity
  lfreshen' _ a k = k a mempty
  freshen' _ a    = pure $ Tuple a mempty
  acompare' _ _ _ = EQ
