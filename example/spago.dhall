{ name = "web3-tests"
, dependencies =
  [ "aff"
  , "arrays"
  , "avar"
  , "chanterelle"
  , "effect"
  , "either"
  , "eth-core"
  , "foldable-traversable"
  , "maybe"
  , "newtype"
  , "parallel"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "spec"
  , "tagged"
  , "transformers"
  , "tuples"
  , "web3"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
