module Main where

import Prelude
import Deploy (defaultDeployContract, writeDeployAddress)
import Utils (makeProvider, getPrimaryAccount, pollTransactionReceipt)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (either)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Network.Ethereum.Web3 (ETH, runWeb3)
import Network.Ethereum.Web3.Api (net_version)
import Node.FS.Aff (FS)
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)

main :: forall e. Eff _ Unit
main = do
  provider <- makeProvider
  void $ launchAff $ do
    esimpleStorageDeployHash <- runWeb3 provider $ do
      primaryAccount <- unsafePartial getPrimaryAccount
      defaultDeployContract simpleStorageFP primaryAccount
    simpleStorageDeployHash <- either (const $ liftEff <<< throw $ "Couln't run deployment for contract") pure esimpleStorageDeployHash
    void $ runWeb3 provider $ do
      simpleStorageReceipt <- liftAff $ pollTransactionReceipt simpleStorageDeployHash provider
      let msimpleStorageAddress = unNullOrUndefined (unwrap simpleStorageReceipt).contractAddress
      simpleStorageAddress <- maybe (liftEff <<< throw $ "Transaction did not deploy new contract: " <> show simpleStorageDeployHash) pure msimpleStorageAddress
      nId <- net_version
      liftAff $ writeDeployAddress simpleStorageFP simpleStorageAddress nId


simpleStorageFP :: FilePath
simpleStorageFP = "./contracts/SimpleStorage.json"
