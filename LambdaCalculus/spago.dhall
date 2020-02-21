{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "console"
    , "effect"
    , "node-readline"
    , "ordered-collections"
    , "parsing"
    , "profunctor"
    , "psci-support"
    , "run"
    , "stringutils"
    , "test-unit"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
