{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "LambdaCalculus"
, dependencies =
  [ "Unbound"
  , "console"
  , "effect"
  , "fixed-points"
  , "free"
  , "node-readline"
  , "psci-support"
  , "run"
  , "stringutils"
  , "trifecta"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
