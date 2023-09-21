module Test.Main where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Data.Maybe (Maybe(..))
import Deploy (deployContracts)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Test.SimplePaidStorageSpec as SimplePaidStorageSpec
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)

main :: Effect Unit
main = do
  launchAff_ do
    testCfg <- buildTestConfig nodeUrl 60 deployContracts
    void $ join $ runSpecT specCfg [ consoleReporter ] do
      SimplePaidStorageSpec.spec testCfg
  where
  specCfg = defaultConfig { timeout = Just (Milliseconds $ 120.0 * 1000.0) }
  nodeUrl = "http://localhost:8545"
