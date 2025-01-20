module Test.SimplePaidStorageSpec where

import Prelude

import Chanterelle.Test (assertWeb3, takeEvent, takeEvents)
import Contract.SimplePaidStorage as SPS
import Contract.Token as Token
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Parallel (parTraverse_)
import Data.Array (length, zip, (..))
import Data.Array.Partial as Array
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Maybe.First (First(..))
import Data.Newtype (un)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Network.Ethereum.Web3 (Address, ChainCursor(..), Web3Error, _from, _to, defaultTransactionOptions, fromInt, runWeb3, unUIntN)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Test.Common (DeploySpecConfig, unsafeToUInt)
import Test.Spec (SpecT, beforeWith, before, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

spec
  :: forall r
   . SpecT
       Aff
       ( DeploySpecConfig
           ( token :: Address
           , tokenOwner :: Address
           , simplePaidStorage :: Address
           , simplePaidStorageOwner :: Address
           | r
           )
       )
       Aff
       Unit
spec = do
  it "Each user can approve the token contract for transfering" \testCfg ->
    flip parTraverse_ testCfg.accounts \account -> do
      let
        v = unsafeToUInt (Proxy @256) one
        txOpts = defaultTransactionOptions
          # _from ?~ account
          # _to ?~ testCfg.token
        tx = Token.approve txOpts { spender: testCfg.simplePaidStorage, amount: v }
      Tuple _ (Token.Approval { spender, value }) <- assertWeb3 testCfg.provider $
        takeEvent (Proxy @Token.Approval) testCfg.token tx
      -- check that the event indicates the SimplePaidStorageContract has an allowance
      spender `shouldEqual` testCfg.simplePaidStorage
      -- that allowance should be the value we set
      value `shouldEqual` v

  it "Each user can set the count" \testCfg ->
    flip parTraverse_ (zip testCfg.accounts (1 .. length testCfg.accounts))
      $ \(Tuple account i) -> do
          let
            n = unsafeToUInt (Proxy @256) (fromInt i)
            txOpts = defaultTransactionOptions
              # _from ?~ account
              # _to ?~ testCfg.simplePaidStorage
            tx = SPS.updateCount txOpts { _newCount: n }

          Tuple _ { countUpdated, transfer } <- assertWeb3 testCfg.provider $
            takeEvents tx
              { countUpdated: Tuple (Proxy @SPS.CountUpdated) testCfg.simplePaidStorage
              , transfer: Tuple (Proxy @Token.Transfer) testCfg.token
              }
          let SPS.CountUpdated { newCount } = unsafePartial $ Array.head countUpdated
          -- check that the new count is the one we submitted
          newCount `shouldEqual` n
          let Token.Transfer { to, from, value } = unsafePartial $ fromJust $ un First transfer
          from `shouldEqual` account
          to `shouldEqual` testCfg.simplePaidStorage
          unUIntN value `shouldEqual` one

  it "The owner of the SimplePaidStorage can collect the tokens" \testCfg -> do
    let
      fetchBalances = do
        let tokenTxOpts = defaultTransactionOptions # _to ?~ testCfg.token
        eRes <- assertWeb3 testCfg.provider $ runExceptT do
          ownerBalance <- ExceptT $ Token.balanceOf tokenTxOpts Latest { account: testCfg.simplePaidStorageOwner }
          contractBalance <- ExceptT $ Token.balanceOf tokenTxOpts Latest { account: testCfg.simplePaidStorage }
          pure $ { ownerBalance, contractBalance }
        case eRes of
          Left e -> unsafeCrashWith $ "Failed to query balances " <> show e
          Right res -> pure res
    -- fetch the balances before withdrawing from the contract
    { contractBalance, ownerBalance } <- fetchBalances
    let
      fetchTokens =
        let
          txOpts = defaultTransactionOptions
            # _from ?~ testCfg.simplePaidStorageOwner
            # _to ?~ testCfg.simplePaidStorage
        in
          SPS.withdrawTokens txOpts { amount: contractBalance }
    -- withdraw tokens and wait for event confirmation
    Tuple _ (Token.Transfer { to, from, value }) <- assertWeb3 testCfg.provider $
      takeEvent (Proxy @Token.Transfer) testCfg.token fetchTokens

    from `shouldEqual` testCfg.simplePaidStorage
    to `shouldEqual` testCfg.simplePaidStorageOwner
    let n = unUIntN value
    n `shouldEqual` unUIntN contractBalance

    -- check that tokens are not created or destroyed (accounting for fees)
    { contractBalance: newContractBalance, ownerBalance: newOwnerBalance } <- fetchBalances
    unUIntN newContractBalance `shouldEqual` zero
    unUIntN newOwnerBalance `shouldEqual` (unUIntN ownerBalance + unUIntN contractBalance)

distributeTokens
  :: forall r
   . DeploySpecConfig (token :: Address, tokenOwner :: Address | r)
  -> Aff (Either Web3Error Unit)
distributeTokens { accounts, provider, token, tokenOwner } = runWeb3 provider $
  let
    distribute recipient =
      let
        txOpts = defaultTransactionOptions
          # _to ?~ token
          # _from ?~ tokenOwner
        amount = unsafeToUInt (Proxy @256) one
        tx = Token.transfer txOpts { recipient, amount }
      in
        void $ takeEvent (Proxy @Token.Transfer) token tx
  in
    parTraverse_ distribute accounts
