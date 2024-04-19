let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20240416/packages.dhall
        sha256:ca727657c01cc31d0e79c2113b59126b9826f4b56d20a8193be3c725599fb754

let eth-core-deps =
      https://raw.githubusercontent.com/f-o-a-m/purescript-eth-core/master/packages.dhall
        sha256:af2751772a729d58edf7056805007934e3687b3079f8a02ac514e705aeab8c42

let web3-deps =
      https://raw.githubusercontent.com/f-o-a-m/purescript-web3/master/packages.dhall
        sha256:c1bebe7e4899bd64304a84fea26f9ea635e20897f206ce6fa86bd446715c5ffc

let web3-generator-deps =
      https://raw.githubusercontent.com/f-o-a-m/purescript-web3-generator/master/packages.dhall
        sha256:89a7ee097ae9a095bf3bfddcb354d4c6b747d15b9b47ef0f5dc5f2280d3a8929

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
          , "newtype"
          , "node-buffer"
          , "node-fs"
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
        , version = "v7.1.0"
        }
      }

in  upstream // eth-core-deps // web3-deps // web3-generator-deps // additions
