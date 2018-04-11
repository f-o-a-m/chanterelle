module ParkingAuthoritySpec (parkingAuthoritySpec) where


import Prelude

import Chanterelle.Internal.Deploy (readDeployAddress)
import Chanterelle.Internal.Test (assertWeb3, takeEvent)
import Chanterelle.Internal.Types (DeployConfig(..), ContractConfig)
import Chanterelle.Internal.Utils (pollTransactionReceipt, validateDeployArgs)
import ContractConfig (foamCSRConfig, makeParkingAuthorityConfig)
import Contracts.ParkingAnchor as ParkingAnchor
import Contracts.ParkingAuthority as PA
import Contracts.User as User
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Except (runExceptT)
import Data.Array ((!!))
import Data.ByteString as BS
import Data.Either (Either(..), fromRight)
import Data.Lens.Setter ((?~))
import Data.Maybe (Maybe(..))
import Data.String (take)
import Data.Tuple (Tuple(..))
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D4, D8, type (:&), fromByteString)
import Network.Ethereum.Web3.Types (Address, BigNumber, ChainCursor(..), TransactionReceipt(..), ETH, Web3, _from, _gas, _to, decimal, _value, defaultTransactionOptions, parseBigNumber, sha3, unHex, embed, fromWei)
import Node.FS.Aff (FS)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))


parkingAuthoritySpec
  :: forall e.
     DeployConfig
  -> Spec ( fs :: FS
          , eth :: ETH
          , avar :: AVAR
          , console :: CONSOLE
          | e
          ) Unit
parkingAuthoritySpec deployCfg@(DeployConfig deployConfig) = do

  describe "Testing basic functionality of the parking authority" do
    it "has the correct foamCSR contract" do
      (Tuple parkingCfg addr) <- buildParkingAuthorityConfig deployConfig.networkId
      let txOpts = defaultTransactionOptions # _to ?~ addr
      ecsr <- assertWeb3 deployConfig.provider $ PA.parkingCSR txOpts Latest
      let csr = validateDeployArgs (parkingCfg :: ContractConfig (foamCSR :: Address))
      ecsr `shouldEqual` Right (unsafePartial fromRight $ csr).foamCSR

  describe "App Flow" do
    it "can register a user and that user is owned by the right account" $ assertWeb3 deployConfig.provider do
      void $ createUser deployCfg 1


    it "can create a user and that user can request more zones from the authority" $ assertWeb3 deployConfig.provider do
      {user, owner} <- createUser deployCfg 1
      let
        zone :: BytesN D4
        zone = case fromByteString =<< BS.fromString "01234567" BS.Hex of
          Just x -> x
          Nothing -> unsafeCrashWith "zone should result in valid BytesN D4"
        txOpts = defaultTransactionOptions # _from ?~ owner
                                           # _gas ?~ bigGasLimit
                                           # _to ?~ user
      Tuple _ e@(User.ZoneGranted {zone: eventZone}) <- takeEvent (Proxy :: Proxy User.ZoneGranted) user
        $ User.requestZone txOpts { _zone: zone }
      liftAff <<< log $ "Received Event: " <> show e
      liftAff $ zone `shouldEqual` eventZone

    it "can create an anchor, and that anchor is owned by the right account" $ assertWeb3 deployConfig.provider do
      let _anchorId = case fromByteString =<< BS.fromString (unHex $ sha3 "I'm an anchor!") BS.Hex of
            Just x -> x
            Nothing -> unsafeCrashWith "anchorId should result in valid BytesN 32"
          _geohash = case fromByteString =<< BS.fromString ("0123456701234567") BS.Hex of
            Just x -> x
            Nothing -> unsafeCrashWith "geohash should result in valid BytesN 8"
      void $ createParkingAnchor deployCfg 2 {_geohash, _anchorId}

    it "can create a user and an anchor, the user requests permission at the anchor, then parks there, but not another zone" $ assertWeb3 deployConfig.provider do
      userResult <- createUser deployCfg 1
      let _anchorId = case fromByteString =<< BS.fromString (unHex $ sha3 "I'm an anchor!") BS.Hex of
            Just x -> x
            Nothing -> unsafeCrashWith "anchorId should result in valid BytesN 32"
          geohashString = "0123456701234567"
          _geohash = case fromByteString =<< BS.fromString geohashString BS.Hex of
            Just x -> x
            Nothing -> unsafeCrashWith "geohash should result in valid BytesN 8"
      parkingAnchorResult <- createParkingAnchor deployCfg 2 {_geohash, _anchorId}
      let
        zoneStr = take 8 geohashString
        zone = case fromByteString =<< BS.fromString zoneStr BS.Hex of
          Just x -> x
          Nothing -> unsafeCrashWith "zone should result in valid BytesN D4"
        txOpts = defaultTransactionOptions # _from ?~ userResult.owner
                                           # _gas ?~ bigGasLimit
                                           # _to ?~ userResult.user
      (Tuple _ e) <- takeEvent (Proxy :: Proxy User.ZoneGranted) userResult.user $ User.requestZone txOpts { _zone: zone }
      liftAff <<< log $ "Received Event: " <> show e
      let parkingReqOpts = txOpts # _value ?~ (fromWei $ embed 1)
      Tuple _ (User.CheckIn {user, anchor}) <- takeEvent (Proxy :: Proxy User.CheckIn) userResult.user $
        User.payForParking parkingReqOpts {_anchor: parkingAnchorResult.anchor}
      liftAff $ user `shouldEqual` userResult.user
      liftAff $ anchor `shouldEqual` parkingAnchorResult.anchor

      badUserResult <- createUser deployCfg 3
      let
        badZone :: BytesN D4
        badZone = case fromByteString =<< BS.fromString "00000000" BS.Hex of
          Just x -> x
          Nothing -> unsafeCrashWith "zone should result in valid BytesN D4"

        badTxOpts = defaultTransactionOptions # _from ?~ badUserResult.owner
                                              # _gas ?~ bigGasLimit
                                              # _to ?~ badUserResult.user
                                              # _value ?~ fromWei (embed 1)
      badHash <- User.payForParking badTxOpts {_anchor: parkingAnchorResult.anchor}
      (TransactionReceipt txReceipt) <- liftAff $ pollTransactionReceipt badHash deployConfig.provider
      liftAff $ txReceipt.status `shouldEqual` "0x0"



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
      Nothing -> unsafeCrashWith $ "no index " <> show accountIndex <> "in accounts"
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
      Nothing -> unsafeCrashWith $ "no index " <> show accountIndex <> "in accounts"
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


registerUser
  :: forall eff.
     Address
  -- ^ from address
  -> DeployConfig
  -> Web3 (fs :: FS, avar :: AVAR, console :: CONSOLE | eff) {owner :: Address, user :: Address}
registerUser fromAccount (DeployConfig {provider, networkId}) = do
  (Tuple cfg addr) <- liftAff $ buildParkingAuthorityConfig networkId
  let txOpts = defaultTransactionOptions # _from ?~ fromAccount
                                         # _gas ?~ bigGasLimit
                                         # _to ?~ addr
  Tuple _ (PA.RegisterParkingUser ev) <- takeEvent (Proxy :: Proxy PA.RegisterParkingUser) addr do
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
  (Tuple cfg addr) <- liftAff $ buildParkingAuthorityConfig networkId
  let txOpts = defaultTransactionOptions # _from ?~ fromAccount
                                         # _gas ?~ bigGasLimit
                                         # _to ?~ addr
  Tuple _ (PA.RegisteredParkingAnchor ev) <- takeEvent (Proxy :: Proxy PA.RegisteredParkingAnchor) addr do
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
  -> Aff (fs :: FS | eff) (Tuple (ContractConfig (foamCSR :: Address)) Address)
buildParkingAuthorityConfig networkId= do
  efoamCSRAddress <- runExceptT $ readDeployAddress foamCSRConfig.filepath networkId
  let foamCSRAddress = case efoamCSRAddress of
        Right x -> x
        Left err -> unsafeCrashWith $ "Expected FoamCSR Address in artifact, got error" <> show err
  let parkingAuthorityConfig = makeParkingAuthorityConfig {foamCSR : foamCSRAddress}
  eparkingAuthorityAddress <- runExceptT $ readDeployAddress parkingAuthorityConfig.filepath networkId
  let parkingAuthorityAddress = case eparkingAuthorityAddress of
        Right x -> x
        Left err -> unsafeCrashWith $ "Expected ParkingAuthority Address in artifact, got error" <> show err
  pure $ Tuple parkingAuthorityConfig parkingAuthorityAddress
