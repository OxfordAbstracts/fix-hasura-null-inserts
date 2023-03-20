let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221214/packages.dhall
        sha256:e462fb4d932e4bbc522cb563a71d312d6514f97050125d1a3f95cc3a2df3bffb

in  upstream
  with graphql-parser =
      { dependencies =
          [ "aff"
          , "argonaut-codecs"
          , "argonaut-generic"
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
          , "unordered-collections"
          ]
      , repo =
          "https://github.com/OxfordAbstracts/purescript-graphql-parser.git"
      , version =
          "6c59c45048b43a659a1022d01cf474975ae88d04"
      }