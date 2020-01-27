{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "WordNumbers"
, dependencies =
    [ "bigints"
    , "console"
    , "effect"
    , "lists"
    , "newtype"
    , "psci-support"
    , "strings"
    , "stringutils"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
