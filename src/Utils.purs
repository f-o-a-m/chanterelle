module Utils
  ( makeProvider
  , getPrimaryAccount
  , pollTransactionReceipt
  , withTimeout
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, Milliseconds(..), delay, forkAff)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, tryTakeVar, putVar)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array ((!!))
import Data.Maybe (Maybe, maybe, fromJust)
import Data.Either (Either(..))
import Network.Ethereum.Web3 (ETH, Web3, HexString, Address, runWeb3)
import Network.Ethereum.Web3.Api (eth_getAccounts, eth_getTransactionReceipt)
import Network.Ethereum.Web3.Types (TransactionReceipt)
import Network.Ethereum.Web3.Types.Provider (Provider, httpProvider)
import Node.Process (lookupEnv)


-- | Make an http provider with address given by NODE_URL, falling back
-- | to localhost.
makeProvider
  :: forall eff.
     Eff (eth :: ETH, exception :: EXCEPTION | eff) Provider
makeProvider = unsafeCoerceEff $ do
  murl <- lookupEnv "NODE_URL"
  url <- maybe (pure "http://localhost:8545") pure murl
  httpProvider url

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
  -> Aff (eth :: ETH | eff) TransactionReceipt
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
  -> Aff (avar :: AVAR | eff) (Maybe a)
withTimeout maxTimeout action = do
  var <- makeEmptyVar
  _ <- forkAff $ do
    res <- unsafeCoerceAff action
    putVar res var
  delay maxTimeout
  tryTakeVar var
