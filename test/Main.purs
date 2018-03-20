module Test.Main where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run', defaultConfig)

import Utils (makeDeployConfig)

import SimpleStorageSpec (simpleStorageSpec)

main :: Eff _ Unit
main = void <<< launchAff $ do
  deployConfig <- makeDeployConfig
  liftEff $ run' defaultConfig {timeout = Just (60 * 1000)} [consoleReporter] do
    simpleStorageSpec deployConfig
