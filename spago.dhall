{-
Welcome to a Spago project!
You can edit this file as you like.
-}
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
  , "functions"
  , "functors"
  , "identity"
  , "integers"
  , "language-cst-codegen"
  , "logging"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "optparse"
  , "ordered-collections"
  , "parallel"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "record"
  , "refs"
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
