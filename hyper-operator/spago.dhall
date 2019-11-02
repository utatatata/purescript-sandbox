{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "hyper-operator"
, dependencies =
    [ "console"
    , "effect"
    , "integers"
    , "lists"
    , "psci-support"
    , "strings"
    , "unfoldable"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
