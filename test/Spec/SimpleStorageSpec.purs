module SimpleStorageSpec (simpleStorageSpec) where

import Prelude

import Contracts.SimpleStorage as SimpleStorage
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Lens.Setter ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Network.Ethereum.Web3 (ETH, runWeb3, EventAction(..), event, embed, eventFilter, uIntNFromBigNumber, _from, _to, defaultTransactionOptions, ChainCursor(..))
import Node.FS.Aff (FS)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))
import Deploy (readDeployAddress)
import ContractConfig (simpleStorageConfig)
import Types (DeployConfig)

simpleStorageSpec
  :: forall e.
     DeployConfig
  -> Spec ( fs :: FS
          , eth :: ETH
          , avar :: AVAR
          , console :: CONSOLE
          | e
          ) Unit
simpleStorageSpec deployConfig = do

  describe "Getting the default value" do
    it "can read the value set when the contract was deployed" do
      simpleStorageAddress <- readDeployAddress simpleStorageConfig.filepath deployConfig.networkId
      let txOpts = defaultTransactionOptions # _to ?~ simpleStorageAddress
      ecount <- runWeb3 deployConfig.provider $ SimpleStorage.count txOpts Latest
      let initialValue = unsafePartial fromJust $ simpleStorageConfig.deployArgs
      ecount `shouldEqual` (Right (Right initialValue._count))

  describe "Setting the value of a SimpleStorage Contract" do
    it "can set the value of simple storage" $ do
      simpleStorageAddress <- readDeployAddress simpleStorageConfig.filepath deployConfig.networkId
      var <- makeEmptyVar
      let n = unsafePartial $ fromJust <<< uIntNFromBigNumber <<< embed $ 42
          txOptions = defaultTransactionOptions # _from ?~ deployConfig.primaryAccount
                                                # _to ?~ simpleStorageAddress
      hx <- runWeb3 deployConfig.provider $ SimpleStorage.setCount txOptions {_count: n}
      liftEff <<< log $ "setCount tx hash: " <> show hx
      let filterCountSet = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
      _ <- liftAff $ runWeb3 deployConfig.provider $
        event filterCountSet $ \e@(SimpleStorage.CountSet cs) -> do
          liftEff $ log $ "Received Event: " <> show e
          _ <- liftAff $ putVar cs._count var
          pure TerminateEvent
      val <- takeVar var
      Just val `shouldEqual` Just n
