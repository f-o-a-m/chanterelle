module Test.Main where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (PROCESS, run', defaultConfig)
import Network.Ethereum.Web3 (ETH)
import Node.FS.Aff (FS)

import Utils (makeDeployConfig)

import SimpleStorageSpec (simpleStorageSpec)
import ParkingAuthoritySpec (parkingAuthoritySpec)

main
  :: forall e.
     Eff ( console :: CONSOLE
         , eth :: ETH
         , avar :: AVAR
         , fs :: FS
         , process :: PROCESS
         | e
         ) Unit
main = void <<< launchAff $ do
  deployConfig <- makeDeployConfig
  liftEff $ run' defaultConfig {timeout = Just (60 * 1000)} [consoleReporter] do
    simpleStorageSpec deployConfig
    parkingAuthoritySpec deployConfig
