module Chanterelle.Internal.Utils
  ( module Json
  , module Web3
  , module Utils.FS
  , makeDeployConfig
  , withTimeout
  , validateDeployArgs
  ) where

import Chanterelle.Internal.Utils.Json (jsonStringifyWithSpaces) as Json
import Chanterelle.Internal.Utils.Web3 as Web3
import Chanterelle.Internal.Utils.FS as Utils.FS

import Prelude
import Chanterelle.Internal.Types (ContractConfig, DeployConfig(..), DeployError(..))
import Control.Monad.Aff (Aff, Milliseconds(..), delay)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Parallel (parOneOf)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Validation.Semigroup (unV)
import Network.Ethereum.Web3 (ETH, runWeb3)
import Network.Ethereum.Web3.Api (net_version)

makeDeployConfig
  :: forall eff m.
     MonadAff (eth :: ETH, console :: CONSOLE | eff) m
  => MonadThrow DeployError m
  => String
  -> Int
  -> m DeployConfig
makeDeployConfig url tout = do
  provider <- Web3.makeProvider url
  let timeout = Milliseconds (toNumber tout)
  econfig <- liftAff $ runWeb3 provider do
    primaryAccount <- Web3.getPrimaryAccount
    networkId <- net_version
    pure $ DeployConfig {provider, primaryAccount, networkId, timeout, writeArtifacts: true }
  case econfig of
    Left err ->
      let errMsg = "Couldn't create DeployConfig -- " <> show err
      in throwError $ ConfigurationError errMsg
    Right config -> pure config

-- | try an aff action for the specified amount of time before giving up.
withTimeout
  :: forall eff a.
     Milliseconds
  -> Aff eff a
  -> Aff eff a
withTimeout maxTimeout action = do
  let timeout = do
        delay maxTimeout
        throwError $ error "TimeOut"
  parOneOf [action, timeout]

validateDeployArgs
  :: forall m args.
     MonadThrow DeployError m
  => ContractConfig args
  -> m (Record args)
validateDeployArgs cfg =
  let onErr msg = throwError $ ConfigurationError ("Couldn't validate args for contract deployment " <> cfg.name <> ": " <> show msg)
      onSucc = pure
  in unV onErr onSucc cfg.unvalidatedArgs