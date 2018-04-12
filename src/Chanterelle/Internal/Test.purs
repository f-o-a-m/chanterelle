module Chanterelle.Internal.Test
  ( takeEvent
  , assertWeb3
  , TestConfig
  , buildTestConfig
  ) where

import Prelude

import Chanterelle.Internal.Types (DeployConfig(..), DeployError(..), DeployM, logDeployError, runDeployM)
import Chanterelle.Internal.Utils (makeDeployConfig)
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Exception (throw)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..), either)
import Data.Record.Builder (build, merge)
import Data.Tuple (Tuple(..))
import Network.Ethereum.Web3 (class EventFilter, Address, ETH, EventAction(..), Provider, Web3, event, eventFilter, forkWeb3', runWeb3)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Solidity (class DecodeEvent)
import Node.FS.Aff (FS)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy)

-- | Run a `Web3` action which will dispatch a single event, wait for the event,
-- | then return the action's result and the event.
takeEvent
  :: forall eff a ev i ni.
     DecodeEvent i ni ev
  => Show ev
  => EventFilter ev
  => Proxy ev
  -> Address
  -> Web3 (console :: CONSOLE, avar :: AVAR | eff) a
  -> Web3 (console :: CONSOLE, avar :: AVAR | eff) (Tuple a ev)
takeEvent prx addrs web3Action = do
  var <- liftAff makeEmptyVar
  _ <- forkWeb3' do
    event (eventFilter prx addrs) $ \e -> do
      _ <- liftAff $ putVar e var
      pure TerminateEvent
  efRes <- web3Action
  event <- liftAff $ takeVar var
  pure $ Tuple efRes event

-- | Assert the `Web3` action's result, crash the program if it doesn't succeed.
assertWeb3
  :: forall eff a.
     Provider
  -> Web3 eff a
  -> Aff (eth :: ETH | eff) a
assertWeb3 provider a = runWeb3 provider a <#> case _ of
  Right x -> x
  Left err -> unsafeCrashWith $ "expected Right in `assertWeb3`, got error" <> show err

type TestConfig r =
  { accounts :: Array Address
  , provider :: Provider
  | r
  }

-- | This builds a deployment environment, runs a deployment against
-- | that environment, then returns the results together with the
-- | other useful data for testing, e.g. a list of ethereum accounts
-- | on a node. Note, deploy results must lack keys "accounts" and "provider".
buildTestConfig
  :: forall eff r.
     Union r ( accounts :: Array Address
             , provider :: Provider
             )
             ( accounts :: Array Address
             , provider :: Provider
             | r
             )

  => String
  -> Int
  -> DeployM eff (Record r)
  -> Aff (console :: CONSOLE, eth :: ETH, fs :: FS | eff) (TestConfig r)
buildTestConfig url timeout deployScript = do
  edeployConfig <- runExceptT $ makeDeployConfig url timeout
  case edeployConfig of
    Left err -> logDeployError err *> (liftEff' $ throw "Couldn't make deploy config for tests!")
    Right deployConfig@(DeployConfig {provider}) -> do
      eDeployResults <- flip runDeployM deployConfig $ do
        eaccounts <- liftAff $ runWeb3 provider eth_getAccounts
        accounts <- either (throwError <<< ConfigurationError <<< show) pure eaccounts
        results <- deployScript
        pure $ Tuple accounts results
      case eDeployResults of
        Left err -> logDeployError err *> (liftEff' $ throw "Error during deployment!")
        Right (Tuple accounts results) ->
          let base = {accounts, provider}
          in pure $ build (merge base) results
