module Chanterelle.Internal.Utils.Web3 where

import Prelude

import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types.Deploy (DeployError(..))
import Chanterelle.Internal.Types.Project (Network(..), networkIDFitsChainSpec)
import Control.Monad.Aff (Milliseconds(..), delay)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (error, try)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT(..), except, runExceptT, withExceptT)
import Control.Parallel (parOneOf)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.String (null)
import Network.Ethereum.Web3 (Address, ChainCursor(Latest), ETH, HexString, Web3, runWeb3, unHex)
import Network.Ethereum.Web3.Api (eth_getAccounts, eth_getCode, eth_getTransactionReceipt, net_version)
import Network.Ethereum.Web3.Types (TransactionReceipt, Web3Error(..))
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

providerForNetwork :: forall eff m. MonadEff eff m => Network -> m Provider
providerForNetwork (Network network) = liftEff $ httpProvider network.providerUrl

resolveProvider
  :: forall eff m.
     MonadAff (eth :: ETH | eff) m
  => Network
  -> m (Either String Provider)
resolveProvider rn@(Network realNet) = runExceptT do
  provider <- liftAff $ providerForNetwork rn
  validatedProvider <- withExceptT showWeb3Error $ ExceptT <<< liftAff <<< runWeb3 provider $ do
    v <- net_version
    pure $ if networkIDFitsChainSpec realNet.allowedChains v
      then Right provider
      else Left $ "Network " <> show realNet.name <> " resolves to a provider which is serving chain ID " <> v <> ", which is not within that network's permitted chains."
  except validatedProvider

  where showWeb3Error = case _ of
          Rpc         e -> "Web3 Rpc: " <> show e
          RemoteError e -> "Web3 Remote: " <> e
          ParserError e -> "Web3 Parser: " <> e
          NullError     -> "Web3 NullError"

getCodeForContract
  :: forall eff m.
     MonadAff (eth :: ETH | eff) m
  => Address
  -> Provider
  -> m (Either String HexString)
getCodeForContract addr provider = runExceptT do
  code <- liftAff <<< runWeb3 provider $ eth_getCode addr Latest
  case code of
    Left err -> throwError (show err)
    Right hs -> if null (unHex hs)
                  then throwError $ "no code at address " <> show addr
                  else pure hs

resolveCodeForContract
  :: forall eff m
   . MonadAff (eth :: ETH | eff) m
  => Network
  -> Address
  -> m (Either String HexString)
resolveCodeForContract network contract = runExceptT do
  provider <- (ExceptT $ resolveProvider network)
  ExceptT $ getCodeForContract contract provider

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
      throwError $ error "No PrimaryAccount found on ethereum client!"

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
