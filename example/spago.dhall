{ name = "web3-tests"
, dependencies =
  [ "aff"
  , "arrays"
  , "chanterelle"
  , "effect"
  , "either"
  , "eth-core"
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
  , "identity"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
