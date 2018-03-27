module ParkingAuthorityMigration where

import Prelude

import ContractConfig (foamCSRConfig, makeParkingAuthorityConfig)
import Contracts.ParkingAnchor as ParkingAnchor
import Contracts.ParkingAuthority as PA
import Contracts.User as User
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExceptT)
import Data.Array ((!!))
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Lens.Setter ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Record (insert)
import Data.String (take)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Deploy (readDeployAddress)
import Network.Ethereum.Web3 (class EventFilter, EventAction(..), event, eventFilter, forkWeb3', runWeb3)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Solidity (class DecodeEvent, BytesN, D2, D3, D4, D8, type (:&), fromByteString)
import Network.Ethereum.Web3.Types (Address, BigNumber, ChainCursor(..), TransactionReceipt(..), ETH, Web3, _from, _gas, _to, decimal, _value, defaultTransactionOptions, mkAddress, mkHexString, parseBigNumber, sha3, unHex, embed, fromWei)
import Node.FS.Aff (FS)
import Node.Process as NP
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (PROCESS, run', defaultConfig)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))
import Types (DeployConfig(..), ContractConfig, logDeployError)
import Utils (makeDeployConfig, pollTransactionReceipt)


main
  :: forall e.
     Eff ( console :: CONSOLE
         , eth :: ETH
         , avar :: AVAR
         , fs :: FS
         , process :: PROCESS
         , process :: NP.PROCESS
         | e
         ) Unit
main = void $ do
  launchAff $ do
    log $ "MAIN"
    edeployConfig <- unsafeCoerceAff <<< runExceptT $ makeDeployConfig
    case edeployConfig of
      Left err -> logDeployError err *> pure unit
      Right deployConfig ->
        liftEff $ run' defaultConfig {timeout = Just (60 * 1000)} [consoleReporter] do
          parkingAuthoritySpec deployConfig

parkingAuthoritySpec deployCfg@(DeployConfig deployConfig) = do

  let
    run :: forall e' a. Web3 e' a -> Aff (eth :: ETH | e') a
    run a = runWeb3 deployConfig.provider a <#> case _ of
      Right x -> x
      Left err -> unsafeCrashWith $ "expected Right in `run`, got error" <> show err

  describe "Testing basic functionality of the parking authority" do
    it "has the correct foamCSR contract" do
      parkingAuthorityConfig <- buildParkingAuthorityConfig deployConfig.networkId
      let txOpts = defaultTransactionOptions # _to ?~ parkingAuthorityConfig.parkingAuthority
      ecsr <- run $ PA.parkingCSR txOpts Latest
      ecsr `shouldEqual` (Right parkingAuthorityConfig.deployArgs.foamCSR)

  describe "App Flow" do
    it "can register a user and that user is owned by the right account" $ run do
      void $ createUser deployCfg 1


    it "can create a user and that user can request more zones from the authority" $ run do
      {user, owner} <- createUser deployCfg 1
      let
        zone :: BytesN D4
        zone = case fromByteString =<< BS.fromString "01234567" BS.Hex of
          Just x -> x
          Nothing -> unsafeCrashWith "zone should result in valid BytesN D4"
        txOpts = defaultTransactionOptions # _from ?~ owner
                                           # _gas ?~ bigGasLimit
                                           # _to ?~ user
      Tuple _ (User.ZoneGranted {zone: eventZone}) <- takeEvent (Proxy :: Proxy User.ZoneGranted) user
        $ User.requestZone txOpts { _zone: zone }
      liftAff $ zone `shouldEqual` eventZone

    it "can create an anchor, and that anchor is owned by the right account" $ run do
      let _anchorId = case fromByteString =<< BS.fromString (unHex $ sha3 "I'm an anchor!") BS.Hex of
            Just x -> x
            Nothing -> unsafeCrashWith "anchorId should result in valid BytesN 32"
          _geohash = case fromByteString =<< BS.fromString ("0123456701234567") BS.Hex of
            Just x -> x
            Nothing -> unsafeCrashWith "geohash should result in valid BytesN 8"
      void $ createParkingAnchor deployCfg 2 {_geohash, _anchorId}


--------------------------------------------------------------------------------
-- | SetupTests
--------------------------------------------------------------------------------
-- Setup tests are like sub-tests, they create and account (either user or parking) and
-- return all the necessary information about that account to use it in another test

createUser
  :: forall eff.
     DeployConfig
  -> Int
  -- ^ the index of the account to use for transactions
  -> Web3 (fs :: FS, avar :: AVAR, console :: CONSOLE | eff) {owner :: Address, user :: Address}
createUser deployCfg@(DeployConfig deployConfig) accountIndex = do
  accounts <- eth_getAccounts
  let
    account = case accounts !! accountIndex of
      Just x -> x
      Nothing -> unsafeCrashWith $ "no index " <> show accountIndex <> " in accounts"
  {owner, user} <- registerUser account deployCfg
  let txOpts = defaultTransactionOptions # _from ?~ account
                                         # _to ?~ user
  actualOwner <- User.owner txOpts Latest <#> case _ of
    Right x -> x
    Left err -> unsafeCrashWith $ "expected Right in `User.owner`, got error" <> show err
  liftAff $ owner `shouldEqual` actualOwner
  liftAff $ owner `shouldEqual` account
  pure {owner, user}

createParkingAnchor
  :: forall eff.
     DeployConfig
  -> Int
  -- ^ the index for the account to use for transactions
  -> {_geohash :: BytesN D8, _anchorId :: BytesN (D3 :& D2)}
  -> Web3 (fs :: FS, console :: CONSOLE, avar :: AVAR | eff) {owner :: Address, anchor :: Address, anchorId :: BytesN (D3 :& D2) , geohash :: BytesN D8}
createParkingAnchor deployConfig accountIndex args = do
  accounts <- eth_getAccounts
  let
    account = case accounts !! accountIndex of
      Just x -> x
      Nothing -> unsafeCrashWith $ "no index " <> show accountIndex <> " in accounts"
  res@{owner, anchor, geohash, anchorId} <- registerAnchor account args deployConfig
  let txOpts = defaultTransactionOptions # _from ?~ account
                                         # _to ?~ anchor
  actualOwner <- ParkingAnchor.owner txOpts Latest <#> case _ of
    Right x -> x
    Left err -> unsafeCrashWith $ "expected Right in `ParkingAnchor.owner`, got error" <> show err

  liftAff $ owner `shouldEqual` actualOwner
  liftAff $ owner `shouldEqual` account
  liftAff $ geohash `shouldEqual` args._geohash
  liftAff $ anchorId `shouldEqual` args._anchorId
  pure res


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
  var <- liftAff $ makeEmptyVar
  _ <- forkWeb3' do
    event (eventFilter prx addrs) $ \e -> do
      liftAff <<< log $ "Received Event: " <> show e
      _ <- liftAff $ putVar e var
      pure TerminateEvent
  efRes <- web3Action
  event <- liftAff $ takeVar var
  pure $ Tuple efRes event


registerUser
  :: forall eff.
     Address
  -- ^ from address
  -> DeployConfig
  -> Web3 (fs :: FS, avar :: AVAR, console :: CONSOLE | eff) {owner :: Address, user :: Address}
registerUser fromAccount (DeployConfig {provider, networkId}) = do
  {parkingAuthority} <- liftAff $ buildParkingAuthorityConfig networkId
  let txOpts = defaultTransactionOptions # _from ?~ fromAccount
                                         # _gas ?~ bigGasLimit
                                         # _to ?~ parkingAuthority
  Tuple _ (PA.RegisterParkingUser ev) <- takeEvent (Proxy :: Proxy PA.RegisterParkingUser) parkingAuthority do
      txHash <- PA.registerUser txOpts
      liftAff <<< log $ "Registered User " <> show fromAccount <> ", Transaction Hash:" <> show txHash
  pure ev

registerAnchor
  :: forall eff.
     Address
  -- ^ from address
  -> {_geohash :: BytesN D8, _anchorId :: BytesN (D3 :& D2)}
  -> DeployConfig
  -> Web3 (fs :: FS, console :: CONSOLE, avar :: AVAR | eff) {owner :: Address, anchor :: Address, anchorId :: BytesN (D3 :& D2) , geohash :: BytesN D8}
registerAnchor fromAccount args (DeployConfig {provider, networkId}) = do
  {parkingAuthority} <- liftAff $ buildParkingAuthorityConfig networkId
  let txOpts = defaultTransactionOptions # _from ?~ fromAccount
                                         # _gas ?~ bigGasLimit
                                         # _to ?~ parkingAuthority
  Tuple _ (PA.RegisteredParkingAnchor ev) <- takeEvent (Proxy :: Proxy PA.RegisteredParkingAnchor) parkingAuthority do
    txHash <- PA.registerParkingAnchor txOpts args
    liftAff <<< log $ "Registered Anchor " <> show fromAccount <> ", Transaction Hash:" <> show txHash
  pure ev

bigGasLimit :: BigNumber
bigGasLimit = case parseBigNumber decimal "4712388" of
  Just x -> x
  Nothing -> unsafeCrashWith "expected to get big number from 4712388 but it failed"

buildParkingAuthorityConfig
  :: forall eff.
     BigNumber
  -> Aff (fs :: FS | eff) (ContractConfig (deployArgs :: {foamCSR :: Address}, parkingAuthority :: Address))
buildParkingAuthorityConfig networkId = do
  let foamCSRAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "0x606c977259D39b51D199Bda969C59A5ceD682531"
  let parkingAuthorityConfig = makeParkingAuthorityConfig {foamCSR : foamCSRAddress}
  eparkingAuthorityAddress <- runExceptT $ readDeployAddress parkingAuthorityConfig.filepath networkId
  let parkingAuthorityAddress = case eparkingAuthorityAddress of
        Right x -> x
        Left err -> unsafeCrashWith $ "Expected ParkingAuthority Address in artifact, got error" <> show err
  pure $ insert (SProxy :: SProxy "parkingAuthority") parkingAuthorityAddress parkingAuthorityConfig
