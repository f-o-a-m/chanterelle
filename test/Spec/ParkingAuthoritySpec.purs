module ParkingAuthoritySpec (parkingAuthoritySpec) where


import Prelude
import Contracts.ParkingAuthority as ParkingAuthority
import Contracts.User as User
import Data.ByteString as BS
import ContractConfig (foamCSRConfig, makeParkingAuthorityConfig)
import Contracts.ParkingAuthority (RegisterParkingUser(..))
import Contracts.User (ZoneGranted(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Lens.Setter ((?~))
import Data.Maybe (Maybe(..))
import Data.Record (insert)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Deploy (readDeployAddress)
import Network.Ethereum.Web3 (class EventFilter, Address, BigNumber, ChainCursor(..), D4, ETH, EventAction(..), Web3, _from, _gas, _to, decimal, defaultTransactionOptions, event, eventFilter, forkWeb3', parseBigNumber, runWeb3)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Solidity (class DecodeEvent, BytesN, fromByteString)
import Node.FS.Aff (FS)
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))
import Types (DeployConfig, ContractConfig)


buildParkingAuthorityConfig
  :: forall eff.
     BigNumber
  -> Aff (fs :: FS | eff) (ContractConfig (deployArgs :: {foamCSR :: Address}, parkingAuthority :: Address))
buildParkingAuthorityConfig networkId= do
  foamCSRAddress <- readDeployAddress foamCSRConfig.filepath networkId
  let parkingAuthorityConfig = makeParkingAuthorityConfig {foamCSR : foamCSRAddress}
  parkingAuthorityAddress <- readDeployAddress parkingAuthorityConfig.filepath networkId
  pure $ insert (SProxy :: SProxy "parkingAuthority") parkingAuthorityAddress parkingAuthorityConfig

parkingAuthoritySpec
  :: forall e.
     DeployConfig
  -> Spec ( fs :: FS
          , eth :: ETH
          , avar :: AVAR
          , console :: CONSOLE
          | e
          ) Unit
parkingAuthoritySpec deployConfig = do

  let
    run :: forall e' a. Web3 e' a -> Aff (eth :: ETH | e') a
    run a = runWeb3 deployConfig.provider a <#> case _ of
      Right x -> x
      Left err -> unsafeCrashWith $ "expected Right in `run`, got error" <> show err

  describe "Testing basic functionality of the parking authority" do
    it "has the correct foamCSR contract" do
      parkingAuthorityConfig <- buildParkingAuthorityConfig deployConfig.networkId
      parkingAuthorityAddress <- readDeployAddress parkingAuthorityConfig.filepath deployConfig.networkId
      let txOpts = defaultTransactionOptions # _to ?~ parkingAuthorityAddress
      ecsr <- run $ ParkingAuthority.parkingCSR txOpts Latest
      ecsr `shouldEqual` (Right parkingAuthorityConfig.deployArgs.foamCSR)

  describe "User Registration" do
    it "can register a user and that user is owned by the right account" $ run do
      accounts <- eth_getAccounts
      let
        account1 = case accounts !! 1 of
          Just x -> x
          Nothing -> unsafeCrashWith "accounts is empty"
      {owner, user} <- registerUser account1 deployConfig
      let txOpts = defaultTransactionOptions # _from ?~ account1
                                              # _to ?~ user
      actualOwner <- User.owner txOpts Latest <#> case _ of
        Right x -> x
        Left err -> unsafeCrashWith $ "expected Right in `User.owner`, got error" <> show err

      liftAff $ owner `shouldEqual` actualOwner


    it "can create a user and that user can request more zones from the authority" $ run do
      {parkingAuthority} <- liftAff $ buildParkingAuthorityConfig deployConfig.networkId
      accounts <- eth_getAccounts
      let
        account1 = case accounts !! 1 of
          Just x -> x
          Nothing -> unsafeCrashWith "accounts is empty"
      {owner, user} <- registerUser account1 deployConfig
      let 
        zone :: BytesN D4
        zone = case fromByteString =<< BS.fromString "01234567" BS.Hex of
          Just x -> x
          Nothing -> unsafeCrashWith "01234567 should result in valid BytesN D4"
        txOpts = defaultTransactionOptions # _from ?~ account1
                                            # _gas ?~ bigGasLimit
                                            # _to ?~ user
      Tuple txHash (ZoneGranted {zone: eventZone}) <- takeEvent (Proxy :: Proxy ZoneGranted) user 
        $ User.requestZone txOpts { _zone: zone }
      liftAff $ zone `shouldEqual` eventZone


  pending "create anchor, check that it has the correct owner" 
  pending "call the registerParkingAnchor function on the ParkingAuthority from accounts[2], capture the RegisterParkingAnchor event, check the owner of the new Anchor contract is accounts[2]"
  pending "create user, create anchor, user requests zone relevant for anchor, user pays for parking, CheckIn event fires, pending anchor is reset."

takeEvent
  :: forall eff a ev i ni
  . DecodeEvent i ni ev
  => EventFilter ev
  => Proxy ev
  -> Address
  -> Web3 (console :: CONSOLE, avar :: AVAR | eff) a
  -> Web3 (console :: CONSOLE, avar :: AVAR | eff) (Tuple a ev)
takeEvent prx addrs ef' = do
  var <- liftAff $ makeEmptyVar
  _ <- forkWeb3' do 
    liftAff $ log "event filter forked"
    event (eventFilter prx addrs) $ \e -> do
      liftAff $ log "got event"
      _ <- liftAff $ putVar e var
      pure TerminateEvent
    liftAff $ log "event filter terminated"
  liftAff $ log "invoke event producing effect"
  efRes <- ef'
  liftAff $ log "taking event"
  event <- liftAff $ takeVar var
  liftAff $ log "event has been taken"
  pure $ Tuple efRes event
      


registerUser
  :: forall eff.
     Address
  -- ^ from address
  -> DeployConfig
  -> Web3 (fs :: FS, avar :: AVAR, console :: CONSOLE | eff) {owner :: Address, user :: Address}
registerUser fromAccount {provider, networkId} = do
  {parkingAuthority} <- liftAff $ buildParkingAuthorityConfig networkId
  let txOpts = defaultTransactionOptions # _from ?~ fromAccount
                                         # _gas ?~ bigGasLimit
                                         # _to ?~ parkingAuthority
  Tuple _ (RegisterParkingUser ev) <- takeEvent (Proxy :: Proxy RegisterParkingUser) parkingAuthority do 
      txHash <- ParkingAuthority.registerUser txOpts
      liftAff <<< log $ "Registered User " <> show fromAccount <> ", Transaction Hash:" <> show txHash
  pure ev

bigGasLimit :: BigNumber
bigGasLimit = case parseBigNumber decimal "9000000" of
  Just x -> x
  Nothing -> unsafeCrashWith "expected to get big number from 9000000 not it failed"
