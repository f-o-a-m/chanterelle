module Test.SimplePaidStorageSpec where

import Prelude

import Chanterelle.Test (assertWeb3, takeEvent)
import Contract.SimplePaidStorage as SPS
import Contract.Token as Token
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (ask)
import Control.Parallel (parTraverse_)
import Data.Array (length, zip, (..))
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, joinFiber)
import Effect.Aff.AVar (empty, put, take)
import Effect.Aff.Class (liftAff)
import Network.Ethereum.Web3 (Address, ChainCursor(..), Change(..), EventAction(..), Web3Error, _from, _to, defaultTransactionOptions, event, eventFilter, forkWeb3, fromInt, runWeb3, uIntNFromBigNumber, unUIntN)
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
            let filter = eventFilter (Proxy @Token.Approval) testCfg.token
            var <- empty
            -- When the owner of the tokens is our curent account, return the event
            f <- forkWeb3 testCfg.provider $ event filter $ \e@(Token.Approval { owner }) ->
              if owner == account then do
                liftAff $ put e var
                pure TerminateEvent
              else pure ContinueEvent
            let
              v = unsafeToUInt (Proxy @256) one
              txOpts = defaultTransactionOptions
                # _from ?~ account
                # _to ?~ testCfg.token
            _ <- assertWeb3 testCfg.provider $
              Token.approve txOpts { spender: testCfg.simplePaidStorage, amount: v }
            _ <- joinFiber f
            Token.Approval { spender, value } <- take var
            -- check that the event indicates the SimplePaidStorageContract has an allowance
            spender `shouldEqual` testCfg.simplePaidStorage
            -- that allowance should be the value we set
            value `shouldEqual` v

      it "Each user can set the count"
        $ flip parTraverse_ (zip testCfg.accounts (1 .. length testCfg.accounts))
        $ \(Tuple account i) -> do
            let
              filter = eventFilter (Proxy @SPS.CountUpdated) testCfg.simplePaidStorage
              n = unsafeToUInt (Proxy @256) (fromInt i)
            var <- empty
            -- when the count is set to our unique value, return the tx hash that set it
            f <- forkWeb3 testCfg.provider $ event filter $ \(SPS.CountUpdated { newCount }) -> do
              if newCount == n then do
                change <- ask
                liftAff $ put change var
                pure TerminateEvent
              else pure ContinueEvent
            let
              txOpts = defaultTransactionOptions
                # _from ?~ account
                # _to ?~ testCfg.simplePaidStorage
            txHash <- assertWeb3 testCfg.provider $ SPS.updateCount txOpts { _newCount: n }
            _ <- joinFiber f
            Change { transactionHash } <- take var
            -- check that the transaction hash that set the count is our transaction hash
            transactionHash `shouldEqual` txHash

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
        (Tuple _ e) <- assertWeb3 testCfg.provider $ takeEvent (Proxy @Token.Transfer) testCfg.token fetchTokens
        let Token.Transfer { from, to, value } = e
        -- check that the transfer passes the sanity check
        from `shouldEqual` testCfg.simplePaidStorage
        to `shouldEqual` testCfg.simplePaidStorageOwner
        value `shouldEqual` contractBalance
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
      in
        Token.transfer txOpts { recipient, amount }
  in
    parTraverse_ distribute accounts
