{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "trifecta"
, dependencies =
  [ "ansi"
  , "console"
  , "const"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "generics-rep"
  , "lists"
  , "node-process"
  , "proxy"
  , "psci-support"
  , "refs"
  , "sequences"
  , "st"
  , "test-unit"
  , "typeable"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
