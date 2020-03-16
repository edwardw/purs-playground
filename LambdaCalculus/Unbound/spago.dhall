{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "Unbound"
, dependencies =
  [ "assert"
  , "console"
  , "effect"
  , "leibniz"
  , "parsing"
  , "profunctor"
  , "psci-support"
  , "test-unit"
  , "typelevel"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "examples/**/*.purs", "test/**/*.purs" ]
}
