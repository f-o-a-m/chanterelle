let upstream =
      https://raw.githubusercontent.com/f-o-a-m/package-sets/b3ecf8e8e4e1a35ba97fcb7e9f2858d14ee6a912/purs-0.15.7-web3.dhall
        sha256:ce57fd949b7cd331d7c61ff45283e35983dd5797b3f17616dd69f8bc06f54784

let overrides = {=}

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
    , repo =
        "https://github.com/f-o-a-m/purescript-solc.git"
    , version =
        "d6e4f3be2f249d9a464bf9bbb6ca247bc24a759e"
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
    , repo =
      "https://github.com/f-o-a-m/purescript-web3-generator.git"
    , version =
      "2c96bbe2fd0a3af4236406e3c0b118ec21250027"
    }
  }

in  upstream // overrides // additions
