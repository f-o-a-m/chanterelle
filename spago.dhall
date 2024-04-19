{ name = "chanterelle"
, dependencies =
  [ "aff"
  , "ansi"
  , "argonaut"
  , "argonaut-core"
  , "argonaut-traversals"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "eth-core"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "functors"
  , "identity"
  , "integers"
  , "js-date"
  , "logging"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-path"
  , "node-process"
  , "now"
  , "optparse"
  , "ordered-collections"
  , "parallel"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "record"
  , "refs"
  , "simple-json"
  , "solc"
  , "strings"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "validation"
  , "web3"
  , "web3-generator"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
