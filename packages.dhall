let upstream =
      https://raw.githubusercontent.com/f-o-a-m/package-sets/9e9623711d22de73259e765f5acf8407b7332998/purs-0.15.7-web3.dhall
        sha256:f0ffdb72aef1af63caf537a130b40d8f73779e82496404d9cedb66c6a699b55d

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
        , version = "v7.0.0"
        }
      }

in  upstream // additions
