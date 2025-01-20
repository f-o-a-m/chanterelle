module Test.Main where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Data.Maybe (Maybe(..))
import Deploy (deployContracts)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Test.Spec (beforeAll)
import Test.SimplePaidStorageSpec as SimplePaidStorageSpec
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess')
import Test.Spec.Runner.Node.Config (defaultConfig)

main :: Effect Unit
main =
  let
    cfg =
      { defaultConfig: defaultConfig { timeout = Just (Milliseconds $ 120.0 * 1000.0) }
      , parseCLIOptions: false
      }
  in
    runSpecAndExitProcess' @Aff cfg [ consoleReporter ] do
      beforeAll
        ( do
            testCfg <- buildTestConfig nodeUrl 60 deployContracts
            void $ SimplePaidStorageSpec.distributeTokens testCfg
            pure testCfg
        ) $
        SimplePaidStorageSpec.spec
  where
  nodeUrl = "http://localhost:8545"
