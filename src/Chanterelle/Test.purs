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
import Chanterelle.Utils (pollTransactionReceipt)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ask)
import Data.Array (head)
import Data.Array.Partial as Array
import Data.Either (Either(..), either)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Fiber, error, joinFiber)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Network.Ethereum.Web3 (class EventFilter, Address, BlockNumber, ChainCursor(..), Change(..), EventAction(..), Filter, HexString, Provider, TransactionReceipt(..), TransactionStatus(..), Web3, Web3Error, _fromBlock, _toBlock, event, eventFilter, forkWeb3', runWeb3, throwWeb3, unHex)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Solidity (class DecodeEvent)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
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
  txHash <- web3Action
  provider <- ask
  TransactionReceipt { blockNumber, status } <- liftAff $ pollTransactionReceipt txHash provider
  unless (status == Succeeded)
    $ throwError
    $ error
    $ "transaction failed: " <> unHex txHash
  let
    filter = eventFilter p addr
      # _fromBlock .~ BN blockNumber
      # _toBlock .~ BN blockNumber
  fiber <- hashMonitor txHash filter
  res <- liftAff $ joinFiber fiber
  case head <$> res of
    Left e -> throwError (error $ writeJSON e)
    Right Nothing -> throwError (error "No event found")
    Right (Just a) -> pure (Tuple txHash a)

data MonitorStatus = Waiting | Active

derive instance Eq MonitorStatus

hashMonitor
  :: forall ev i ni f
   . DecodeEvent i ni ev
  => Applicative f
  => Monoid (f ev)
  => HexString
  -> Filter ev
  -> Web3 (Fiber (Either Web3Error (f ev)))
hashMonitor txHash filter = do
  eventsVar <- liftAff $ AVar.new mempty
  statusVar <- liftAff $ AVar.new Waiting
  let
    handler = \e -> do
      Change { transactionHash } <- ask
      if txHash == transactionHash then liftAff do
        evs <- AVar.take eventsVar
        AVar.put (evs <> pure e) eventsVar
        _ <- AVar.take statusVar
        AVar.put Active statusVar
        pure ContinueEvent
      else liftAff do
        status <- AVar.read statusVar
        case status of
          Active -> pure TerminateEvent
          Waiting -> pure ContinueEvent
  forkWeb3' do
    _ <- event filter handler
    liftAff do
      ev <- AVar.read eventsVar
      AVar.kill (error "clean up statusVar") statusVar
      AVar.kill (error "clean up eventsVar") eventsVar
      pure ev

takeEvents
  :: forall row xs row' events
   . RL.RowToList row xs
  => TakeEvents xs row () row'
  => JoinEvents xs row' () events
  => Web3 HexString
  -> Record row
  -> Web3 (Tuple HexString (Record events))
takeEvents tx r = do
  txHash <- tx
  provider <- ask
  TransactionReceipt { blockNumber } <- liftAff $ pollTransactionReceipt txHash provider
  fibersBuilder <- takeEventsBuilder (Proxy :: _ xs) (Tuple txHash blockNumber) r
  let fibers = Builder.build fibersBuilder {}
  eventsBuilder <- joinEventsBuilder (Proxy :: _ xs) fibers
  pure $ Tuple txHash $ Builder.build eventsBuilder {}

class
  TakeEvents (xs :: RL.RowList Type) (row :: Row Type) (from :: Row Type) (to :: Row Type)
  | xs -> row from to where
  takeEventsBuilder :: Proxy xs -> Tuple HexString BlockNumber -> Record row -> Web3 (Builder { | from } { | to })

instance TakeEvents RL.Nil row () () where
  takeEventsBuilder _ _ _ = pure (identity)

instance
  ( DecodeEvent i ni ev
  , Applicative f
  , Monoid (f ev)
  , EventFilter ev
  , IsSymbol name
  , Row.Cons name (Tuple (Proxy ev) Address) trash row
  , TakeEvents tail row from from'
  , Row.Lacks name from'
  , Row.Cons name (Fiber (Either Web3Error (f ev))) from' to
  ) =>
  TakeEvents (RL.Cons name (Tuple (Proxy ev) Address) tail) row from to where
  takeEventsBuilder _ a@(Tuple txHash blockNumber) r = do
    let
      nameP = Proxy :: _ name
      Tuple p addr = R.get nameP r :: Tuple (Proxy ev) Address
      filter = eventFilter p addr
        # _fromBlock .~ BN blockNumber
        # _toBlock .~ BN blockNumber
    fiber <- hashMonitor txHash filter
    let first = Builder.insert nameP fiber
    rest <- takeEventsBuilder (Proxy :: _ tail) a r
    pure (first <<< rest)

class
  JoinEvents (xs :: RL.RowList Type) (row :: Row Type) (from :: Row Type) (to :: Row Type)
  | xs -> row from to where
  joinEventsBuilder :: Proxy xs -> Record row -> Web3 (Builder { | from } { | to })

instance JoinEvents RL.Nil row () () where
  joinEventsBuilder _ _ = pure identity

instance
  ( DecodeEvent i ni ev
  , IsSymbol name
  , Row.Cons name (Fiber (Either Web3Error (f ev))) trash row
  , JoinEvents tail row from from'
  , Row.Lacks name from'
  , Row.Cons name (f ev) from' to
  ) =>
  JoinEvents (RL.Cons name (Tuple (Proxy ev) Address) tail) row from to where
  joinEventsBuilder _ r = do
    let
      nameP = Proxy :: _ name
      fiber = R.get nameP r :: Fiber (Either Web3Error (f ev))
    a <- liftAff (joinFiber fiber) >>= case _ of
      -- This is a hack and relies on the internals of ps-web3.
      Left e -> throwWeb3 e
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
