{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "const"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "generics-rep"
  , "lists"
  , "proxy"
  , "psci-support"
  , "sequences"
  , "test-unit"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
