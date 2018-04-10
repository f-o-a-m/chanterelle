module Chanterelle.Internal.Test
  ( takeEvent
  , assertWeb3
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Network.Ethereum.Web3 (class EventFilter, Address, ETH, EventAction(..), Provider, Web3, event, eventFilter, forkWeb3', runWeb3)
import Network.Ethereum.Web3.Solidity (class DecodeEvent)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy)

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

assertWeb3
  :: forall eff a.
     Provider
  -> Web3 eff a
  -> Aff (eth :: ETH | eff) a
assertWeb3 provider a = runWeb3 provider a <#> case _ of
  Right x -> x
  Left err -> unsafeCrashWith $ "expected Right in `assertWeb3`, got error" <> show err
