{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
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
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}