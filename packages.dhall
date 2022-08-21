let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220816/packages.dhall
        sha256:8b4467b4b5041914f9b765779c8936d6d4c230b1f60eb64f6269c71812fd7e98

in  upstream
  with graphql-parser =
      { dependencies =
        [ "aff"
        , "arrays"
        , "console"
        , "control"
        , "effect"
        , "either"
        , "enums"
        , "exceptions"
        , "foldable-traversable"
        , "integers"
        , "lists"
        , "maybe"
        , "newtype"
        , "node-buffer"
        , "node-fs"
        , "numbers"
        , "ordered-collections"
        , "parsing"
        , "prelude"
        , "profunctor"
        , "profunctor-lenses"
        , "psci-support"
        , "spec"
        , "spec-discovery"
        , "strings"
        , "transformers"
        , "tuples"
        ]
      , repo =
          "https://github.com/OxfordAbstracts/purescript-graphql-parser.git"
      , version =
          "13bca01"
      }