module Chanterelle.Internal.Utils.Web3 where

import Prelude
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (DeployError(..))
import Control.Monad.Aff (Milliseconds(..), delay)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (error, try)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Parallel (parOneOf)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Network.Ethereum.Web3 (ETH, Web3, HexString, Address, runWeb3)
import Network.Ethereum.Web3.Api (eth_getAccounts, eth_getTransactionReceipt)
import Network.Ethereum.Web3.Types (TransactionReceipt, Web3Error(NullError))
import Network.Ethereum.Web3.Types.Provider (Provider, httpProvider)

-- | Make an http provider with address given by NODE_URL, falling back
-- | to localhost.
makeProvider
  :: forall eff m.
     MonadEff (eth :: ETH | eff) m
  => MonadThrow DeployError m
  => String
  -> m Provider
makeProvider url = do
  eProvider <- liftEff $ try $ httpProvider url
  case eProvider of
    Left _ -> throwError $ ConfigurationError "Cannot connect to Provider, check NODE_URL"
    Right p -> pure p

-- | get the primary account for the ethereum client
getPrimaryAccount
  :: forall eff.
     Web3 (console :: CONSOLE | eff) Address
getPrimaryAccount = do
    accounts <- eth_getAccounts
    maybe accountsError pure $ accounts !! 0
  where
    accountsError = do
      log Error "No PrimaryAccount found on ethereum client!"
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

web3WithTimeout
  :: forall eff a.
     Milliseconds
  -> Web3 eff a
  -> Web3 eff a
web3WithTimeout maxTimeout action = do
  let timeout = liftAff do
        delay maxTimeout
        throwError $ error "TimeOut"
  parOneOf [action, timeout]