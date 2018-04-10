module Chanterelle.Internal.Utils
  ( makeProvider
  , makeDeployConfig
  , getPrimaryAccount
  , pollTransactionReceipt
  , withTimeout
  , reportIfErrored
  , validateDeployArgs
  ) where

import Prelude

import Chanterelle.Internal.Types (ContractConfig, DeployConfig(..), DeployError(..))
import Control.Monad.Aff (Aff, Milliseconds(..), delay)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Console as C
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (error, try)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Parallel (parOneOf)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Validation.Semigroup (unV)
import Network.Ethereum.Web3 (ETH, Web3, HexString, Address, runWeb3)
import Network.Ethereum.Web3.Api (eth_getAccounts, eth_getTransactionReceipt, net_version)
import Network.Ethereum.Web3.Types (TransactionReceipt, Web3Error(NullError))
import Network.Ethereum.Web3.Types.Provider (Provider, httpProvider)
import Node.Process (PROCESS, lookupEnv)

-- | Make an http provider with address given by NODE_URL, falling back
-- | to localhost.
makeProvider
  :: forall eff m.
     MonadEff (eth :: ETH, process :: PROCESS | eff) m
  => MonadThrow DeployError m
  => m Provider
makeProvider = do
  eProvider <- liftEff do
    murl <- lookupEnv "NODE_URL"
    url <- maybe (pure "http://localhost:8545") pure murl
    try $ httpProvider url
  case eProvider of
    Left _ -> throwError $ ConfigurationError "Cannot connect to Provider, check NODE_URL"
    Right p -> pure p

makeDeployConfig
  :: forall eff m.
     MonadAff (eth :: ETH, console :: CONSOLE, process :: PROCESS | eff) m
  => MonadThrow DeployError m
  => m DeployConfig
makeDeployConfig = do
  provider <- makeProvider
  econfig <- liftAff $ runWeb3 provider do
    primaryAccount <- getPrimaryAccount
    networkId <- net_version
    pure $ DeployConfig {provider, primaryAccount, networkId}
  case econfig of
    Left err ->
      let errMsg = "Couldn't create DeployConfig -- " <> show err
      in throwError $ ConfigurationError errMsg
    Right config -> pure config

-- | get the primary account for the ethereum client
getPrimaryAccount
  :: forall eff.
     Web3 (console :: CONSOLE | eff) Address
getPrimaryAccount = do
    accounts <- eth_getAccounts
    maybe accountsError pure $ accounts !! 0
  where
    accountsError = do
      liftAff $ C.error "No PrimaryAccount found on ethereum client!"
      throwError NullError

-- | indefinitely poll for a transaction receipt, sleeping for 3
-- | seconds in between every call.
pollTransactionReceipt
  :: forall eff m.
     MonadAff (eth :: ETH | eff) m
  => HexString
  -> Provider
  -> m TransactionReceipt
pollTransactionReceipt txHash provider = do
  etxReceipt <- liftAff <<< runWeb3 provider $ eth_getTransactionReceipt txHash
  case etxReceipt of
    Left _ -> do
      liftAff $ delay (Milliseconds 3000.0)
      pollTransactionReceipt txHash provider
    Right txRec -> pure txRec

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

reportIfErrored
  :: forall err a eff.
     Show err
  => String
  -> Either err a
  -> Aff (console :: CONSOLE | eff) a
reportIfErrored msg eRes =
  case eRes of
    Left err -> do
      C.error msg
      throwError <<< error <<< show $ err
    Right res -> pure res

validateDeployArgs
  :: forall m args r.
     MonadThrow DeployError m
  => ContractConfig args r
  -> m (Record args)
validateDeployArgs cfg =
  let onErr msg = throwError $ ConfigurationError ("Couldn't validate args for contract deployment " <> cfg.name <> ": " <> msg)
      onSucc = pure
  in unV onErr onSucc cfg.unvalidatedArgs
