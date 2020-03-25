{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "typeable"
, dependencies =
  [ "console"
  , "effect"
  , "foldable-traversable"
  , "functions"
  , "lists"
  , "proxy"
  , "psci-support"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
