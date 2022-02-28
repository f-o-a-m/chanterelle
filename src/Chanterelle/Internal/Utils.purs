module Chanterelle.Internal.Utils
  ( module Utils.Web3
  , module Utils.Error
  , module Utils.FS
  , makeDeployConfig
  , makeDeployConfigWithProvider
  , attemptWithTimeout
  , withTimeout
  , validateDeployArgs
  ) where

import Prelude

import Chanterelle.Internal.Types (ContractConfig, DeployConfig(..), DeployError(..))
import Chanterelle.Internal.Utils.Error (catchingAff')
import Chanterelle.Internal.Utils.Error (catchingAff, catchingAff', eitherM, eitherM_, except', exceptM', exceptNoteA', exceptNoteM', withExceptM', withExceptT', (!?), (??)) as Utils.Error
import Chanterelle.Internal.Utils.FS (assertDirectory, fileIsDirty, fileModTime, readTextFile, unparsePath, withTextFile, writeTextFile) as Utils.FS
import Chanterelle.Internal.Utils.Web3 (getCodeForContract, getPrimaryAccount, getNetworkID, logAndThrow, logAndThrow', makeProvider, pollTransactionReceipt, providerForNetwork, resolveCodeForContract, resolveProvider, web3WithTimeout) as Utils.Web3
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Parallel (parOneOf)
import Data.Either (Either)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Validation.Semigroup (validation)
import Effect.Aff (Aff, Milliseconds(..), attempt, delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
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

-- | try an aff action for the specified amount of time before giving up.
withTimeout
  :: forall a
   . Milliseconds
  -> Aff a
  -> Aff a
withTimeout maxTimeout action =
  let timeout = delay maxTimeout *> throwError (error "timed out")
   in parOneOf [action, timeout]

attemptWithTimeout
  :: forall a
   . Milliseconds
  -> Aff a
  -> Aff (Either Error a)
attemptWithTimeout t = attempt <<< withTimeout t

validateDeployArgs
  :: forall m args
   . MonadThrow DeployError m
  => ContractConfig args
  -> m (Record args)
validateDeployArgs cfg =
  let onErr msg = throwError $ ConfigurationError ("Couldn't validate args for contract deployment " <> cfg.name <> ": " <> show msg)
      onSucc = pure
  in validation onErr onSucc cfg.unvalidatedArgs
