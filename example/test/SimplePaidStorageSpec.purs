module Test.SimplePaidStorageSpec where

import Prelude

import Chanterelle.Test (assertWeb3, takeEvent, takeEvents)
import Contract.SimplePaidStorage as SPS
import Contract.Token as Token
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Parallel (parTraverse_)
import Data.Array (length, zip, (..))
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Network.Ethereum.Web3 (Address, ChainCursor(..), Web3Error, _from, _to, defaultTransactionOptions, eventFilter, fromInt, runWeb3, unUIntN)
import Partial.Unsafe (unsafeCrashWith)
import Test.Common (DeploySpecConfig, unsafeToUInt)
import Test.Spec (SpecT, beforeAll_, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

spec
  :: forall r
   . DeploySpecConfig
       ( token :: Address
       , tokenOwner :: Address
       , simplePaidStorage :: Address
       , simplePaidStorageOwner :: Address
       | r
       )
  -> SpecT Aff Unit Aff Unit
spec testCfg =
  beforeAll_ (void $ distributeTokens testCfg) $ do
    describe "SimplePaidStorage" do

      it "Each user can approve the token contract for transfering"
        $ flip parTraverse_ testCfg.accounts
        $ \account -> do
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

      it "Each user can set the count"
        $ flip parTraverse_ (zip testCfg.accounts (1 .. length testCfg.accounts))
        $ \(Tuple account i) -> do
            let
              n = unsafeToUInt (Proxy @256) (fromInt i)
              txOpts = defaultTransactionOptions
                # _from ?~ account
                # _to ?~ testCfg.simplePaidStorage
              tx = SPS.updateCount txOpts { _newCount: n }
            Tuple _ (SPS.CountUpdated { newCount }) <- assertWeb3 testCfg.provider $
              takeEvent (Proxy @SPS.CountUpdated) testCfg.simplePaidStorage tx
            -- check that the new count is the one we submitted
            newCount `shouldEqual` n

      it "The owner of the SimplePaidStorage can collect the tokens" $ do
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
        { withdrawn, transfer } <- assertWeb3 testCfg.provider $
          takeEvents fetchTokens
            { withdrawn: eventFilter (Proxy @SPS.TokensWithdrawn) testCfg.simplePaidStorage
            , transfer: eventFilter (Proxy @Token.Transfer) testCfg.token
            }
        let Token.Transfer { from, to, value } = transfer
        let SPS.TokensWithdrawn { amount } = withdrawn
        -- check that the transfer passes the sanity check
        from `shouldEqual` testCfg.simplePaidStorage
        to `shouldEqual` testCfg.simplePaidStorageOwner
        value `shouldEqual` contractBalance
        amount `shouldEqual` value
        -- check that tokens are not created or destroyed
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
