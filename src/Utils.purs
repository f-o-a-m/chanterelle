module Utils
  ( makeProvider
  , makeDeployConfig
  , getPrimaryAccount
  , pollTransactionReceipt
  , withTimeout
  , reportIfErrored
  , validateDeployArgs
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, Milliseconds(..), delay, liftEff')
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Console as C
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Exception (EXCEPTION, error, throw)
import Control.Monad.Except (throwError)
import Control.Parallel (parOneOf)
import Data.Array ((!!))
import Data.Maybe (Maybe, maybe, fromJust)
import Data.Either (Either(..))
import Network.Ethereum.Web3 (ETH, Web3, HexString, Address, runWeb3)
import Network.Ethereum.Web3.Api (eth_getAccounts, eth_getTransactionReceipt, net_version)
import Network.Ethereum.Web3.Types (TransactionReceipt)
import Network.Ethereum.Web3.Types.Provider (Provider, httpProvider)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafePartial)
import Types (DeployConfig, ContractConfig)

-- | Make an http provider with address given by NODE_URL, falling back
-- | to localhost.
makeProvider
  :: forall eff.
     Eff (eth :: ETH, exception :: EXCEPTION | eff) Provider
makeProvider = unsafeCoerceEff $ do
  murl <- lookupEnv "NODE_URL"
  url <- maybe (pure "http://localhost:8545") pure murl
  httpProvider url

makeDeployConfig
  :: forall eff.
     Aff (console :: CONSOLE, eth :: ETH | eff) DeployConfig
makeDeployConfig = do
  provider <- liftAff <<< liftEff' $ makeProvider
  econfig <- runWeb3 provider $ do
    primaryAccount <- unsafePartial getPrimaryAccount
    networkId <- net_version
    pure {provider, primaryAccount, networkId}
  case econfig of
    Left err -> do
      C.error $ "Couldn't create DeployConfig: " <> show err
      throwError <<< error <<< show $ err
    Right config -> pure config

-- | get the primary account for the ethereum client
getPrimaryAccount
  :: forall eff.
     Partial
  => Web3 eff Address
getPrimaryAccount = do
  accounts <- eth_getAccounts
  pure $ fromJust $ accounts !! 0

-- | indefinitely poll for a transaction receipt, sleeping for 3
-- | seconds in between every call.
pollTransactionReceipt
  :: forall eff.
     HexString
  -> Provider
  -> Aff (eth :: ETH, console :: CONSOLE | eff) TransactionReceipt
pollTransactionReceipt txHash provider = do
  etxReceipt <- runWeb3 provider $ eth_getTransactionReceipt txHash
  case etxReceipt of
    Left _ -> do
      delay (Milliseconds 3000.0)
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
  :: forall args eff.
     ContractConfig (deployArgs :: Maybe args)
  -> Eff (exception :: EXCEPTION | eff) (ContractConfig (deployArgs :: args))
validateDeployArgs cfg = do
  args <- maybe (throw $ "Couldn't validate args for contract deployment: " <> cfg.name) pure cfg.deployArgs
  pure cfg {deployArgs = args}
