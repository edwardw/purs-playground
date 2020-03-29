{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "benchotron"
  , "console"
  , "effect"
  , "intmaps"
  , "ordered-collections"
  , "psci-support"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "benchmarks/**/*.purs", "test/**/*.purs" ]
}
