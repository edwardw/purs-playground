module Data.Text.Prettyprint.Doc.Render.Util.Panic where

import Data.Lazy (Lazy, defer)
import Effect.Exception.Unsafe (unsafeThrow)



panicUncaughtFail :: forall a. Lazy a
panicUncaughtFail = defer \_ -> unsafeThrow "SFail must not appear in a rendered SimpleDocStream. This is a bug in the layout algorithm!"


panicPeekedEmpty :: forall a. Lazy a
panicPeekedEmpty = defer \_ -> unsafeThrow "Peeked an empty style stack! Please report this as a bug."


panicInputNotFullyConsumed :: forall a. Lazy a
panicInputNotFullyConsumed = defer \_ ->  unsafeThrow "Conversion from SimpleDocStream to SimpleDocTree left unconsumed input!"


panicUnpairedPop :: forall a. Lazy a
panicUnpairedPop = defer \_ -> unsafeThrow "An unpaired style terminator was encountered. This is a bug in the layout algorithm!"
