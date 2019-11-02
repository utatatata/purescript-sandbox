{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "reinventing-the-monads"
, dependencies =
    [ "console", "effect", "psci-support", "spec", "tuples" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
