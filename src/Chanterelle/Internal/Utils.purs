module Chanterelle.Internal.Utils
  ( module Utils.Web3
  , module Utils.Error
  , module Utils.FS
  , module Utils.Aff
  , makeDeployConfig
  , makeDeployConfigWithProvider
  , validateDeployArgs
  ) where

import Prelude

import Chanterelle.Internal.Types (ContractConfig, DeployConfig(..), DeployError(..))
import Chanterelle.Internal.Utils.Error (catchingAff')
import Chanterelle.Internal.Utils.Error (catchingAff, catchingAff', eitherM, eitherM_, except', exceptM', exceptNoteA', exceptNoteM', withExceptM', withExceptT', (!?), (??)) as Utils.Error
import Chanterelle.Internal.Utils.FS (assertDirectory, fileIsDirty, fileModTime, readTextFile, unparsePath, withTextFile, writeTextFile) as Utils.FS
import Chanterelle.Internal.Utils.Web3 (getCodeForContract, getPrimaryAccount, getNetworkID, logAndThrow, logAndThrow', makeProvider, pollTransactionReceipt, providerForNetwork, resolveCodeForContract, resolveProvider, web3WithTimeout) as Utils.Web3
import Chanterelle.Internal.Utils.Aff (attemptWithTimeout, withTimeout) as Utils.Aff
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Validation.Semigroup (validation)
import Effect.Aff (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Effect.Class (liftEffect)
import Network.Ethereum.Web3 (Provider, runWeb3)

makeDeployConfig
  :: forall m
   . MonadAff m
  => MonadThrow DeployError m
  => String
  -> Int
  -> m DeployConfig
makeDeployConfig url timeout = do
  provider <- Utils.Web3.makeProvider url
  makeDeployConfigWithProvider provider timeout

makeDeployConfigWithProvider
  :: forall m
   . MonadAff m
  => MonadThrow DeployError m
  => Provider
  -> Int
  -> m DeployConfig
makeDeployConfigWithProvider provider timeout =
  let timeout' = Milliseconds (toNumber timeout)
      toError = ConfigurationError <<< append "Couldn't create DeployConfig: " <<< show
   in catchingAff' toError $ runWeb3 provider do
        primaryAccount <- Utils.Web3.getPrimaryAccount
        networkID <- Utils.Web3.getNetworkID
        artifactCache <- liftEffect $ Ref.new Map.empty
        pure $ DeployConfig {provider, primaryAccount, networkID, timeout: timeout', ignoreNetworksInArtifact: false, writeArtifacts: true, artifactCache }

validateDeployArgs
  :: forall m args
   . MonadThrow DeployError m
  => ContractConfig args
  -> m (Record args)
validateDeployArgs cfg =
  let onErr msg = throwError $ ConfigurationError ("Couldn't validate args for contract deployment " <> cfg.name <> ": " <> show msg)
      onSucc = pure
  in validation onErr onSucc cfg.unvalidatedArgs
