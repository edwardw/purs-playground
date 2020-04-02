{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "parsers"
, dependencies =
  [ "arrays"
  , "charset"
  , "console"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "nonempty"
  , "psci-support"
  , "transformers"
  , "unicode"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
