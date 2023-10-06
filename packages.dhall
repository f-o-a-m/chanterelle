let upstream =
      https://raw.githubusercontent.com/f-o-a-m/package-sets/purs-0.15-web3/purs-0.15.7-web3.dhall
        sha256:cb35bdebefab6fd0d9b0a09b1f461cd8e053509b12ee17099d9324287d20f1f5

let additions =
      { solc =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "argonaut-codecs"
          , "arrays"
          , "bifunctors"
          , "control"
          , "effect"
          , "either"
          , "eth-core"
          , "foldable-traversable"
          , "foreign-object"
          , "functions"
          , "integers"
          , "maybe"
          , "newtype"
          , "node-path"
          , "prelude"
          , "strings"
          , "transformers"
          , "tuples"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-solc.git"
        , version = "v4.2.0"
        }
      , web3-generator =
        { dependencies =
          [ "aff"
          , "ansi"
          , "argonaut"
          , "argonaut-codecs"
          , "argonaut-core"
          , "argonaut-traversals"
          , "arrays"
          , "bifunctors"
          , "console"
          , "control"
          , "effect"
          , "either"
          , "errors"
          , "eth-core"
          , "exceptions"
          , "fixed-points"
          , "foldable-traversable"
          , "identity"
          , "integers"
          , "lists"
          , "maybe"
          , "language-cst-parser"
          , "tidy-codegen"
          , "mkdirp"
          , "newtype"
          , "node-buffer"
          , "node-fs"
          , "node-fs-aff"
          , "node-path"
          , "node-process"
          , "ordered-collections"
          , "optparse"
          , "partial"
          , "prelude"
          , "profunctor-lenses"
          , "string-parsers"
          , "strings"
          , "transformers"
          , "tuples"
          , "web3"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-web3-generator.git"
        , version = "v6.0.0"
        }
      }

in  upstream // additions
