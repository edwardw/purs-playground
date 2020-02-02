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
    , "ordered-collections"
    , "psci-support"
    , "spec"
    , "strings"
    , "stringutils"
    , "typelevel"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
