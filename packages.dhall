let upstream =
      https://raw.githubusercontent.com/f-o-a-m/package-sets/921ac217454768ec3f614a9571c109b2ad542c07/purs-0.15.7-web3.dhall
        sha256:818d12df6f7ce455657ff559798e50ec14e098f8d6acc655f674f26c7a007e3d

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
        , version = "cb1fcd29b2e5ccd35a78bf13165e78fcdfeb349a"
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
        , version = "v5.1.0"
        }
      }

in  upstream // additions
