module Data.Text.Prettyprint.Doc.Render.Util.Panic where

import Effect.Exception.Unsafe (unsafeThrow)



panicUncaughtFail :: forall a. a
panicUncaughtFail = unsafeThrow "SFail must not appear in a rendered SimpleDocStream. This is a bug in the layout algorithm!"


panicPeekedEmpty :: forall a. a
panicPeekedEmpty = unsafeThrow "Peeked an empty style stack! Please report this as a bug."
