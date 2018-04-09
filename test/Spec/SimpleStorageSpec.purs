module SimpleStorageSpec (simpleStorageSpec) where

import Prelude

import ContractConfig (simpleStorageConfig)
import Contracts.SimpleStorage as SimpleStorage
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Lens.Setter ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Chanterelle.Internal.Deploy (readDeployAddress)
import Network.Ethereum.Web3 (ETH, EventAction(..), _from, _gas, _to, defaultTransactionOptions, embed, event, eventFilter, runWeb3, uIntNFromBigNumber)
import Node.FS.Aff (FS)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))
import Chanterelle.Internal.Types (DeployConfig(..))

simpleStorageSpec
  :: forall e.
     DeployConfig
  -> Spec ( fs :: FS
          , eth :: ETH
          , avar :: AVAR
          , console :: CONSOLE
          | e
          ) Unit
simpleStorageSpec (DeployConfig deployConfig) = do

  describe "Setting the value of a SimpleStorage Contract" do
    it "can set the value of simple storage" $ do
      esimpleStorageAddress <- runExceptT $ readDeployAddress simpleStorageConfig.filepath deployConfig.networkId
      let simpleStorageAddress = case esimpleStorageAddress of
            Right x -> x
            Left err -> unsafeCrashWith $ "Expected SimpleStorage Address in artifact, got error" <> show err
      var <- makeEmptyVar
      let n = unsafePartial $ fromJust <<< uIntNFromBigNumber <<< embed $ 42
          txOptions = defaultTransactionOptions # _from ?~ deployConfig.primaryAccount
                                                # _to ?~ simpleStorageAddress
                                                # _gas ?~ embed 90000
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
