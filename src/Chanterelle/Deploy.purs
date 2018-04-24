module Chanterelle.Deploy
  ( deploy
  , module Exports
  ) where

import Prelude

import Chanterelle.Internal.Deploy (deployContract, readDeployAddress) as Exports
import Chanterelle.Internal.Logging (logDeployError)
import Chanterelle.Internal.Types.Deploy (DeployM, runDeployM)
import Chanterelle.Internal.Types.Deploy ((??)) as Exports
import Chanterelle.Internal.Types (runDeployM) as Exports
import Chanterelle.Internal.Utils (makeDeployConfig)
import Control.Monad.Aff (launchAff, throwError)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Network.Ethereum.Web3 (ETH)
import Node.FS.Aff (FS)

-- | Run an arbitrary deployment script in the DeployM monad
deploy
  :: forall eff a.
     String
  -> Int
  -> DeployM eff a
  -> Eff (console :: CONSOLE, eth :: ETH, fs :: FS | eff) Unit
deploy url tout deployScript = void <<< launchAff $ do
  edeployConfig <- runExceptT $ makeDeployConfig url tout
  case edeployConfig of
    Left err -> logDeployError err *> throwError (error "Error in building DeployConfig!")
    Right deployConfig -> do
      eDeployResult <- runDeployM deployScript deployConfig
      case eDeployResult of
        Left err -> logDeployError err *> throwError (error "Error during deployment!")
        Right a -> pure a
