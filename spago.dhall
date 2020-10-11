{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "chanterelle"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "foreign-object"
  , "logging"
  , "mkdirp"
  , "node-process"
  , "optparse"
  , "prelude"
  , "psci-support"
  , "solc"
  , "validation"
  , "web3"
  , "web3-generator"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs"]
}
