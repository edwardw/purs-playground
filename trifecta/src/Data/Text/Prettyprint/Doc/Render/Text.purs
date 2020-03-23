-- | Render an unannotated 'SimpleDocStream' as plain 'Text'.
module Data.Text.Prettyprint.Doc.Render.Text
  ( render
  , renderIO
  ) where

import Prelude
import Data.Lazy (force)
import Data.String as S
import Data.Text.Prettyprint.Doc.Internal (SimpleDocStream(..), textSpaces)
import Data.Text.Prettyprint.Doc.Render.Util.Panic (panicUncaughtFail)
import Data.Text.Prettyprint.Doc.Render.Util.StackMachine (renderSimplyDecorated)
import Effect (Effect)
import Effect.Console (log)



render :: forall ann. SimpleDocStream ann -> String
render = renderSimplyDecorated identity (pure mempty) (pure mempty)


-- | This function is more efficient than `log <<< render` since it writes to
-- | the console directly, skipping the intermediate `String` representation.
renderIO :: forall ann. SimpleDocStream ann -> Effect Unit
renderIO = case _ of
  SFail           -> force panicUncaughtFail
  SEmpty          -> pure unit
  SChar c rest    -> do log $ S.singleton c
                        renderIO rest
  SText _ t rest  -> do log t
                        renderIO rest
  SLine n rest    -> do log "\n"
                        log $ textSpaces n
                        renderIO rest
  SAnnPush _ rest -> renderIO rest
  SAnnPop rest    -> renderIO rest
