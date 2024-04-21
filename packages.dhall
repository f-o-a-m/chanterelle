let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20240416/packages.dhall
        sha256:ca727657c01cc31d0e79c2113b59126b9826f4b56d20a8193be3c725599fb754

let eth-core-deps =
      https://raw.githubusercontent.com/f-o-a-m/purescript-eth-core/v10.1.0/packages.dhall
        sha256:ca727657c01cc31d0e79c2113b59126b9826f4b56d20a8193be3c725599fb754

let web3-deps =
      https://raw.githubusercontent.com/f-o-a-m/purescript-web3/v7.2.0/packages.dhall
        sha256:2687f2bfcd60b5260d340407c9851e963b440ad520bbc93595f0452f859c9846

let web3-generator-deps =
      https://raw.githubusercontent.com/f-o-a-m/purescript-web3-generator/v7.1.0/packages.dhall
        sha256:cb46062e88d379ffc9871af092f3b219b6f519b86b31490fe6233747cb130d00

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
