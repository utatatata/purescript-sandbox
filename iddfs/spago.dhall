{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "iddfs"
, dependencies =
    [ "console", "effect", "lists", "psci-support", "tuples" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
