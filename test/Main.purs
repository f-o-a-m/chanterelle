module Test.Main where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Deploy.Types (logDeployError)
import Deploy.Utils (makeDeployConfig)
import Network.Ethereum.Web3 (ETH)
import Node.FS.Aff (FS)
import Node.Process as NP
import ParkingAuthoritySpec (parkingAuthoritySpec)
import SimpleStorageSpec (simpleStorageSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (PROCESS, run', defaultConfig)

main
  :: forall e.
     Eff ( console :: CONSOLE
         , eth :: ETH
         , avar :: AVAR
         , fs :: FS
         , process :: PROCESS
         , process :: NP.PROCESS
         | e
         ) Unit
main = void <<< launchAff $ do
  edeployConfig <- unsafeCoerceAff <<< runExceptT $ makeDeployConfig
  case edeployConfig of
    Left err -> logDeployError err *> pure unit
    Right deployConfig ->
      liftEff $ run' defaultConfig {timeout = Just (60 * 1000)} [consoleReporter] do
        simpleStorageSpec deployConfig
        parkingAuthoritySpec deployConfig
