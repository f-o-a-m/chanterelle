module Chanterelle.Utils.Web3 where

import Prelude

import Chanterelle.Logging (LogLevel(..), log)
import Chanterelle.Types.Deploy (DeployError(..), NetworkID)
import Chanterelle.Types.Project (Network(..), networkIDFitsChainSpec)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT(..), except, runExceptT, withExceptT)
import Control.Parallel (parOneOf)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe, maybe)
import Data.String (null)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error, try)
import Network.Ethereum.Web3 (Address, ChainCursor(Latest), HexString, Web3, runWeb3, unHex)
import Network.Ethereum.Web3.Api (eth_getAccounts, eth_getCode, eth_getTransactionReceipt, net_version)
import Network.Ethereum.Web3.Types (TransactionReceipt, Web3Error(..))
import Network.Ethereum.Web3.Types.Provider (Provider, httpProvider)

-- | Make an http provider with address given by NODE_URL, falling back
-- | to localhost.
makeProvider
  :: forall m
   . MonadEffect m
  => MonadThrow DeployError m
  => String
  -> m Provider
makeProvider url = do
  eProvider <- liftEffect $ try $ httpProvider url
  case eProvider of
    Left _ -> throwError $ ConfigurationError "Cannot connect to Provider, check NODE_URL"
    Right p -> pure p

providerForNetwork :: forall m. MonadEffect m => Network -> m Provider
providerForNetwork (Network network) = liftEffect $ httpProvider network.providerUrl

resolveProvider
  :: forall m
   . MonadAff m
  => Network
  -> m (Either String Provider)
resolveProvider rn@(Network realNet) = runExceptT do
  provider <- liftAff $ providerForNetwork rn
  validatedProvider <- withExceptT showWeb3Error $ ExceptT <<< liftAff <<< runWeb3 provider $ do
    v <- net_version
    pure $
      if networkIDFitsChainSpec realNet.allowedChains v then Right provider
      else Left $ "Network " <> show realNet.name <> " resolves to a provider which is serving chain ID " <> v <> ", which is not within that network's permitted chains."
  except validatedProvider

  where
  showWeb3Error = case _ of
    Rpc e -> "Web3 Rpc: " <> show e
    RemoteError e -> "Web3 Remote: " <> e
    ParserError e -> "Web3 Parser: " <> e
    NullError -> "Web3 NullError"

getCodeForContract
  :: forall m
   . MonadAff m
  => Address
  -> Provider
  -> m (Either String HexString)
getCodeForContract addr provider = runExceptT do
  code <- liftAff <<< runWeb3 provider $ eth_getCode addr Latest
  case code of
    Left err -> throwError (show err)
    Right hs ->
      if null (unHex hs) then throwError $ "no code at address " <> show addr
      else pure hs

resolveCodeForContract
  :: forall m
   . MonadAff m
  => Network
  -> Address
  -> m (Either String HexString)
resolveCodeForContract network contract = runExceptT do
  provider <- (ExceptT $ resolveProvider network)
  ExceptT $ getCodeForContract contract provider

logAndThrow
  :: forall m a
   . MonadEffect m
  => MonadThrow Error m
  => String
  -> m a
logAndThrow msg = log Error msg *> throwError (error msg)

logAndThrow'
  :: forall m a
   . MonadEffect m
  => MonadThrow Error m
  => String
  -> Maybe a
  -> m a
logAndThrow' msg = maybe (logAndThrow msg) pure

-- | get the primary account for the ethereum client
getPrimaryAccount
  :: Web3 Address
getPrimaryAccount = do
  accounts <- eth_getAccounts
  logAndThrow' "No primary account exists on the ethereum node" $ head accounts

getNetworkID
  :: Web3 NetworkID
getNetworkID = do
  net_version <- net_version
  logAndThrow' "net_version was not an Int!" $ fromString net_version

-- | indefinitely poll for a transaction receipt, sleeping for 3
-- | seconds in between every call.
pollTransactionReceipt
  :: HexString
  -> Provider
  -> Aff TransactionReceipt
pollTransactionReceipt txHash provider = do
  etxReceipt <- liftAff <<< runWeb3 provider $ eth_getTransactionReceipt txHash
  case etxReceipt of
    Left _ -> do
      delay (Milliseconds 1000.0)
      pollTransactionReceipt txHash provider
    Right txRec -> pure txRec

-- | try an aff action for the specified amount of time before giving up.
attemptWithTimeout
  :: forall a
   . Milliseconds
  -> Aff a
  -> Aff a
attemptWithTimeout t action =
  let
    timeout = delay t *> throwError (error "timed out")
  in
    parOneOf [ action, timeout ]
