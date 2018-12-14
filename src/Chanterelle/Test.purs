module Chanterelle.Test
  ( takeEvent
  , assertWeb3
  , TestConfig
  , TestConfigR
  , buildTestConfig
  ) where

import Prelude

import Chanterelle.Internal.Logging (logDeployError)
import Chanterelle.Internal.Types.Deploy (DeployConfig(..), DeployError(..), DeployM, runDeployM)
import Chanterelle.Internal.Utils (makeDeployConfig)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..), either)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Network.Ethereum.Web3 (class EventFilter, Address, EventAction(..), Provider, Web3, event, eventFilter, forkWeb3', runWeb3)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Solidity (class DecodeEvent)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Record.Builder (build, merge)
import Type.Proxy (Proxy)

-- | Run a `Web3` action which will dispatch a single event, wait for the event,
-- | then return the action's result and the event.
takeEvent
  :: forall a ev i ni.
     DecodeEvent i ni ev
  => Show ev
  => EventFilter ev
  => Proxy ev
  -> Address
  -> Web3 a
  -> Web3 (Tuple a ev)
takeEvent prx addrs web3Action = do
  var <- liftAff AVar.empty
  _ <- forkWeb3' do
    event (eventFilter prx addrs) $ \e -> do
      _ <- liftAff $ AVar.put e var
      pure TerminateEvent
  efRes <- web3Action
  event <- liftAff $ AVar.take var
  pure $ Tuple efRes event

-- | Assert the `Web3` action's result, crash the program if it doesn't succeed.
assertWeb3
  :: forall a.
     Provider
  -> Web3 a
  -> Aff a
assertWeb3 provider a = runWeb3 provider a <#> case _ of
  Right x -> x
  Left err -> unsafeCrashWith $ "expected Right in `assertWeb3`, got error" <> show err

type TestConfig r = Record (TestConfigR r)
type TestConfigR r =
  ( accounts :: Array Address
  , provider :: Provider
  | r
  )

-- | This builds a deployment environment, runs a deployment against
-- | that environment, then returns the results together with the
-- | other useful data for testing, e.g. a list of ethereum accounts
-- | on a node. Note, deploy results must lack keys "accounts" and "provider".
buildTestConfig
  :: forall r
  . Row.Union
      r
      (TestConfigR ())
      (TestConfigR r)
  => Row.Nub (TestConfigR r) (TestConfigR r)
  => String
  -> Int
  -> DeployM (Record r)
  -> Aff (TestConfig r)
buildTestConfig url timeout deployScript = do
  edeployConfig <- runExceptT $ makeDeployConfig url timeout
  case edeployConfig of
    Left err -> logDeployError err *> (liftEffect $ throw "Couldn't make deploy config for tests!")
    Right (DeployConfig baseDeployConfig) -> do
      let provider = baseDeployConfig.provider
          deployConfig = DeployConfig $ baseDeployConfig { writeArtifacts = false }
      eDeployResults <- flip runDeployM deployConfig $ do
        eaccounts <- liftAff $ runWeb3 provider eth_getAccounts
        accounts <- either (\err -> throwError <<< ConfigurationError $ "Couldn't find accounts for tests " <> show err) pure eaccounts
        results <- deployScript
        pure $ Tuple accounts results
      case eDeployResults of
        Left err -> logDeployError err *> (liftEffect $ throw "Error during deployment!")
        Right (Tuple accounts results) ->
          let base = {accounts, provider}
          in pure $ build (merge base) results
