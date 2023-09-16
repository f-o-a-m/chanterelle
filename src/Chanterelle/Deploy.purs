module Chanterelle.Deploy
  ( deploy
  , deployWithProvider
  , module Exports
  ) where

import Prelude

import Chanterelle.Internal.Deploy (deployContract, deployLibrary, linkLibrary, readDeployAddress) as Exports
import Chanterelle.Internal.Logging (logDeployError)
import Chanterelle.Internal.Types (runDeployM) as Exports
import Chanterelle.Internal.Types.Deploy (DeployM, runDeployM)
import Chanterelle.Internal.Utils (makeDeployConfigWithProvider, makeProvider)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Effect.Aff (Aff, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Network.Ethereum.Web3 (Provider)

-- | Run an arbitrary deployment script in the DeployM monad
deploy
  :: String
  -> Int
  -> DeployM ~> Aff
deploy url tout deployScript =
  runExceptT (makeProvider url) >>= case _ of
    Left err -> do
      logDeployError err
      liftEffect $ throw "DeployM error"
    Right provider -> do
      deployWithProvider provider tout deployScript

-- | Run an arbitrary deployment script in the DeployM monad against a specified Provider
deployWithProvider
  :: Provider
  -> Int
  -> DeployM ~> Aff
deployWithProvider provider tout deployScript = do
  edeployConfig <- runExceptT $ makeDeployConfigWithProvider provider tout
  case edeployConfig of
    Left err -> logDeployError err *> throwError (error "Error in building DeployConfig!")
    Right deployConfig -> do
      eDeployResult <- runDeployM deployScript deployConfig
      case eDeployResult of
        Left err -> logDeployError err *> throwError (error "Error during deployment!")
        Right a -> pure a
