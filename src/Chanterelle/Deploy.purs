module Chanterelle.Deploy
  ( deploy
  , deployWithProvider
  , module Exports
  ) where

import Prelude

import Chanterelle.Internal.Deploy (deployContract, readDeployAddress) as Exports
import Chanterelle.Internal.Logging (logDeployError)
import Chanterelle.Internal.Types (runDeployM) as Exports
import Chanterelle.Internal.Types.Deploy ((??)) as Exports
import Chanterelle.Internal.Types.Deploy (DeployM, runDeployM)
import Chanterelle.Internal.Utils (makeDeployConfigWithProvider, makeProvider)
import Effect.Aff (launchAff, throwError)
import Effect (Effect)
import Effect.Exception (error, throw)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Network.Ethereum.Web3 (Provider)

-- | Run an arbitrary deployment script in the DeployM monad
deploy
  :: forall a.
     String
  -> Int
  -> DeployM a
  -> Effect Unit
deploy url tout deployScript = 
  runExceptT (makeProvider url) >>= case _ of
    Left err -> do
      logDeployError err
      throw "DeployM error"
    Right provider -> do
      deployWithProvider provider tout deployScript

-- | Run an arbitrary deployment script in the DeployM monad against a specified Provider
deployWithProvider
  :: forall a.
     Provider
  -> Int
  -> DeployM a
  -> Effect Unit
deployWithProvider provider tout deployScript = void <<< launchAff $ do
  edeployConfig <- runExceptT $ makeDeployConfigWithProvider provider tout
  case edeployConfig of
    Left err -> do
      logDeployError err
      throwError (error "Error in building DeployConfig!")
    Right deployConfig -> do
      eDeployResult <- runDeployM deployScript deployConfig
      case eDeployResult of
        Left err -> logDeployError err *> throwError (error "Error during deployment!")
        Right a -> pure a
