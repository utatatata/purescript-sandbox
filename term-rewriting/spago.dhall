{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "term-rewriting"
, dependencies =
    [ "console", "effect", "maybe", "psci-support", "transformers" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
