module Chanterelle.Internal.Utils
  ( module Json
  , module Web3
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
import Chanterelle.Internal.Utils.Json (jsonStringifyWithSpaces) as Json
import Chanterelle.Internal.Utils.Web3 (getCodeForContract, getPrimaryAccount, getNetworkID, logAndThrow, logAndThrow', makeProvider, pollTransactionReceipt, providerForNetwork, resolveCodeForContract, resolveProvider, web3WithTimeout) as Web3
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Parallel (parOneOf)
import Data.Either (Either)
import Data.Int (toNumber)
import Data.Validation.Semigroup (unV)
import Effect.Aff (Aff, Milliseconds(..), attempt, delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error, error)
import Network.Ethereum.Web3 (Provider, runWeb3)

makeDeployConfig
  :: forall m
   . MonadAff m
  => MonadThrow DeployError m
  => String
  -> Int
  -> m DeployConfig
makeDeployConfig url tout = do
  provider <- Web3.makeProvider url
  makeDeployConfigWithProvider provider tout

makeDeployConfigWithProvider 
  :: forall m
   . MonadAff m
  => MonadThrow DeployError m
  => Provider
  -> Int
  -> m DeployConfig
makeDeployConfigWithProvider provider tout =
  let timeout = Milliseconds (toNumber tout)
      toError = ConfigurationError <<< append "Couldn't create DeployConfig: " <<< show
   in catchingAff' toError $ runWeb3 provider do
        primaryAccount <- Web3.getPrimaryAccount
        networkID <- Web3.getNetworkID
        pure $ DeployConfig {provider, primaryAccount, networkID, timeout, writeArtifacts: true }

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
  in unV onErr onSucc cfg.unvalidatedArgs
