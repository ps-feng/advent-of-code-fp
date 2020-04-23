{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "advent-of-code-fp"
, dependencies =
    [ "assert"
    , "console"
    , "debug"
    , "effect"
    , "formatters"
    , "memoize"
    , "node-fs"
    , "numbers"
    , "pqueue"
    , "prelude"
    , "psci-support"
    , "string-parsers"
    , "stringutils"
    , "unordered-collections"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
