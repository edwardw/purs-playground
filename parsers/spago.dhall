{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "parsers"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "nonempty"
  , "psci-support"
  , "transformers"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
