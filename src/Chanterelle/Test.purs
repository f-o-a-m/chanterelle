module Chanterelle.Test
  ( takeEvent
  , takeEvents
  , class TakeEvents
  , takeEventsBuilder
  , class JoinEvents
  , joinEventsBuilder
  , assertWeb3
  , TestConfig
  , TestConfigR
  , buildTestConfig
  ) where

import Prelude

import Chanterelle.Deploy (makeDeployConfig)
import Chanterelle.Logging (logDeployError)
import Chanterelle.Types.Deploy (DeployConfig(..), DeployError(..), DeployM, runDeployM)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ask)
import Data.Either (Either(..), either)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Fiber, error, joinFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Network.Ethereum.Web3 (class EventFilter, Address, Change(..), EventAction(..), Filter, HexString, Provider, Web3, Web3Error, event, eventFilter, forkWeb3', runWeb3)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Solidity (class DecodeEvent)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList as RL
import Record as R
import Record.Builder (Builder, build, merge)
import Record.Builder as Builder
import Simple.JSON (writeJSON)
import Type.Proxy (Proxy(..))

-- | Run a transaction which will dispatch a single event, wait for the event,
-- | then return the txHash and the event. NB: It will return the first event
-- | from the given contract caused by the transaction.
takeEvent
  :: forall ev i ni
   . DecodeEvent i ni ev
  => EventFilter ev
  => Proxy ev
  -> Address
  -> Web3 HexString
  -> Web3 (Tuple HexString ev)
takeEvent p addr web3Action = do
  txHashVar <- liftAff AVar.empty
  let filter = eventFilter p addr
  fiber <- hashMonitor txHashVar filter
  txHash <- web3Action
  liftAff $ AVar.put txHash txHashVar
  res <- liftAff $ joinFiber fiber
  case res of
    Left e -> throwError (error $ writeJSON e)
    Right a -> pure (Tuple txHash a)

hashMonitor
  :: forall ev i ni
   . DecodeEvent i ni ev
  => AVar HexString
  -> Filter ev
  -> Web3 (Fiber (Either Web3Error ev))
hashMonitor txHashVar filter = do
  var <- liftAff AVar.empty
  let
    handler = \e -> do
      txHash <- liftAff $ AVar.read txHashVar
      Change { transactionHash } <- ask
      if txHash == transactionHash then do
        liftAff (AVar.put e var)
        pure TerminateEvent
      else pure ContinueEvent
  forkWeb3' (event filter handler *> liftAff (AVar.take var))

takeEvents
  :: forall row xs row' events
   . RL.RowToList row xs
  => TakeEvents xs row () row'
  => JoinEvents xs row' () events
  => Web3 HexString
  -> Record row
  -> Web3 (Record events)
takeEvents tx r = do
  txHashVar <- liftAff $ AVar.empty
  fibersBuilder <- takeEventsBuilder (Proxy :: _ xs) txHashVar r
  let fibers = Builder.build fibersBuilder {}
  txHash <- tx
  liftAff $ AVar.put txHash txHashVar
  eventsBuilder <- liftAff $ joinEventsBuilder (Proxy :: _ xs) fibers
  pure $ Builder.build eventsBuilder {}

class
  TakeEvents (xs :: RL.RowList Type) (row :: Row Type) (from :: Row Type) (to :: Row Type)
  | xs -> row from to where
  takeEventsBuilder :: Proxy xs -> AVar HexString -> Record row -> Web3 (Builder { | from } { | to })

instance TakeEvents RL.Nil row () () where
  takeEventsBuilder _ _ _ = pure (identity)

instance
  ( DecodeEvent i ni ev
  , IsSymbol name
  , Row.Cons name (Filter ev) trash row
  , TakeEvents tail row from from'
  , Row.Lacks name from'
  , Row.Cons name (Fiber (Either Web3Error ev)) from' to
  ) =>
  TakeEvents (RL.Cons name (Filter ev) tail) row from to where
  takeEventsBuilder _ txHash r = do
    let nameP = Proxy :: _ name
    fiber <- hashMonitor txHash (R.get nameP r :: Filter ev)
    let first = Builder.insert nameP fiber
    rest <- takeEventsBuilder (Proxy :: _ tail) txHash r
    pure (first <<< rest)

class
  JoinEvents (xs :: RL.RowList Type) (row :: Row Type) (from :: Row Type) (to :: Row Type)
  | xs -> row from to where
  joinEventsBuilder :: Proxy xs -> Record row -> Aff (Builder { | from } { | to })

instance JoinEvents RL.Nil row () () where
  joinEventsBuilder _ _ = pure identity

instance
  ( DecodeEvent i ni ev
  , IsSymbol name
  , Row.Cons name (Fiber (Either Web3Error ev)) trash row
  , JoinEvents tail row from from'
  , Row.Lacks name from'
  , Row.Cons name ev from' to
  ) =>
  JoinEvents (RL.Cons name (Filter ev) tail) row from to where
  joinEventsBuilder _ r = do
    let
      nameP = Proxy :: _ name
      fiber = R.get nameP r :: Fiber (Either Web3Error ev)
    a <- joinFiber fiber >>= case _ of
      -- This is a hack and relies on the internals of ps-web3.
      Left e -> throwError (error $ writeJSON e)
      Right a -> pure a
    let first = Builder.insert nameP a
    rest <- joinEventsBuilder (Proxy :: _ tail) r
    pure (first <<< rest)

-- | Assert the `Web3` action's result, crash the program if it doesn't succeed.
assertWeb3
  :: forall a
   . Provider
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
      let
        provider = baseDeployConfig.provider
        deployConfig = DeployConfig $ baseDeployConfig { writeArtifacts = false, ignoreNetworksInArtifact = true }
      eDeployResults <- flip runDeployM deployConfig $ do
        eaccounts <- liftAff $ runWeb3 provider eth_getAccounts
        accounts <- either (\err -> throwError <<< ConfigurationError $ "Couldn't find accounts for tests " <> show err) pure eaccounts
        results <- deployScript
        pure $ Tuple accounts results
      case eDeployResults of
        Left err -> logDeployError err *> (liftEffect $ throw "Error during deployment!")
        Right (Tuple accounts results) ->
          let
            base = { accounts, provider }
          in
            pure $ build (merge base) results
