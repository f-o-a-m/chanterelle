let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230306/packages.dhall
        sha256:0757626c7422b8b5b5b1d0df3d3628e5deac755d7f89c433a9bf89009787dcbd

let overrides = {=}

let additions =
      { bytestrings =
        { dependencies =
          [ "arrays"
          , "console"
          , "effect"
          , "exceptions"
          , "foldable-traversable"
          , "integers"
          , "leibniz"
          , "maybe"
          , "newtype"
          , "node-buffer"
          , "partial"
          , "prelude"
          , "quickcheck"
          , "quickcheck-laws"
          , "quotient"
          , "unsafe-coerce"
          ]
        , repo =
            "https://github.com/rightfold/purescript-bytestrings"
        , version = "6733a32fca306015b3428e9985ffac65325a9864"
        }
      , web3 =
        { dependencies =
          [ "aff"
          , "coroutines"
          , "coroutine-transducers"
          , "effect"
          , "errors"
          , "eth-core"
          , "foreign"
          , "fork"
          , "heterogeneous"
          , "parsing"
          , "partial"
          , "profunctor-lenses"
          , "tagged"
          , "transformers"
          , "typelevel-prelude"
          , "variant"
          , "argonaut"
          , "argonaut-generic"
          , "arrays"
          , "bifunctors"
          , "bytestrings"
          , "control"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "integers"
          , "maybe"
          , "newtype"
          , "parallel"
          , "prelude"
          , "record"
          , "ring-modules"
          , "simple-json"
          , "strings"
          , "tailrec"
          , "tuples"
          , "unfoldable"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-web3"
        , version = "dd47c28a3b8adc0167e43615f2ac27f48f23e59b"
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
            , "language-cst-codegen"
            , "mkdirp"
            , "newtype"
            , "node-buffer"
            , "node-fs"
            , "node-fs-aff"
            , "node-path"
            , "node-process"
            , "nonempty"
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
          , repo = "https://github.com/f-o-a-m/purescript-web3-generator"
          , version = "e1f9f18a15a6ecf26212d69adb5ec189739f0db1"
          }
      , eth-core =
        { dependencies =
          [ "argonaut"
          , "arrays"
          , "bytestrings"
          , "effect"
          , "either"
          , "foreign"
          , "functions"
          , "integers"
          , "maybe"
          , "node-buffer"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "quotient"
          , "ring-modules"
          , "simple-json"
          , "strings"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-eth-core.git"
        , version = "b35eec551db445cb6a3577eaea3fe7a3bc052472"
        }
      , coroutine-transducers =
        { dependencies =
            [ "aff"
            , "coroutines"
            , "effect"
            , "maybe"
            , "psci-support"
            ]
        , repo =
            "https://github.com/blinky3713/purescript-coroutine-transducers"
        , version =
            "v1.0.0"
        }
      , solc =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "console"
          , "effect"
          , "node-path"
          , "prelude"
          , "psci-support"
          , "web3"
          ]
        , repo =
            "https://github.com/f-o-a-m/purescript-solc"
        , version =
            "v3.0.0"
        }
      , errors =
        { dependencies =
          [ "control"
          , "effect"
          , "either"
          , "identity"
          , "maybe"
          , "newtype"
          , "prelude"
          , "test-unit"
          , "transformers"
          ]
        , repo = "https://github.com/passy/purescript-errors"
        , version = "670485beb1e026f77d52ca58ce10c145d96c11ba"
        }
      , mkdirp =
        { dependencies =
            [ "console"
            , "effect"
            , "either"
            , "exceptions"
            , "functions"
            , "node-fs"
            , "nullable"
            , "prelude"
            , "psci-support"
            ]
        , repo =
            "https://github.com/f-o-a-m/purescript-mkdirp"
        , version =
            "v2.0.0"
        }
      , quotient =
        { dependencies = [ "prelude", "quickcheck" ]
        , repo = "https://github.com/rightfold/purescript-quotient.git"
        , version = "v3.0.0"
        }
      , tagged =
        { dependencies =
            [ "identity"
            , "profunctor"
            ]
        , repo =
            "https://github.com/kejace/purescript-tagged"
        , version =
            "v0.14"
        }
      , language-cst-parser =
        { dependencies =
          [ "arrays"
          , "const"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "free"
          , "functors"
          , "maybe"
          , "numbers"
          , "ordered-collections"
          , "strings"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          ]
        , repo =
            "https://github.com/natefaubion/purescript-language-cst-parser.git"
        , version = "v0.9.1"
        }
      , language-cst-codegen =
        { dependencies =
          [ "aff"
          , "ansi"
          , "arrays"
          , "avar"
          , "bifunctors"
          , "console"
          , "control"
          , "dodo-printer"
          , "effect"
          , "either"
          , "enums"
          , "exceptions"
          , "filterable"
          , "foldable-traversable"
          , "free"
          , "identity"
          , "integers"
          , "language-cst-parser"
          , "lazy"
          , "lists"
          , "maybe"
          , "newtype"
          , "node-buffer"
          , "node-child-process"
          , "node-fs-aff"
          , "node-path"
          , "node-process"
          , "node-streams"
          , "ordered-collections"
          , "parallel"
          , "partial"
          , "posix-types"
          , "prelude"
          , "record"
          , "safe-coerce"
          , "strings"
          , "tidy"
          , "transformers"
          , "tuples"
          , "type-equality"
          , "unicode"
          ]
        , repo =
            "https://github.com/natefaubion/purescript-tidy-codegen.git"
        , version = "v1.1.1"
        }
      , tidy =
        { dependencies =
          [ "arrays"
          , "dodo-printer"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "language-cst-parser"
          , "strings"
          , "tuples"
          ]
        , repo =
            "https://github.com/natefaubion/purescript-tidy.git"
        , version = "v0.5.3"
        }
      }

in  upstream // overrides // additions
