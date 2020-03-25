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
  , "nonempty"
  , "parsing"
  , "profunctor"
  , "psci-support"
  , "quickcheck"
  , "strings"
  , "test-unit"
  , "typeable"
  , "typelevel"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "examples/**/*.purs", "test/**/*.purs" ]
}
