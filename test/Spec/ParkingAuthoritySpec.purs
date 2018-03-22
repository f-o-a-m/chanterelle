module ParkingAuthoritySpec (parkingAuthoritySpec) where


import Prelude

import ContractConfig (foamCSRConfig, makeParkingAuthorityConfig)
import Contracts.ParkingAuthority as ParkingAuthority
import Contracts.User as User
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Array ((!!))
import Data.ByteString as BS
import Data.Either (Either(..), fromRight)
import Data.Lens.Setter ((?~))
import Data.Maybe (fromJust)
import Data.Record (insert)
import Data.Symbol (SProxy(..))
import Deploy (readDeployAddress)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Solidity (BytesN, fromByteString)
import Network.Ethereum.Web3 (Address, BigNumber, ChainCursor(..), ETH, EventAction(..), Web3, _from, _gas, _to, decimal, defaultTransactionOptions, event, eventFilter, forkWeb3', parseBigNumber, runWeb3)
import Node.FS.Aff (FS)
import Partial.Unsafe (unsafePartial)
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

  describe "Testing basic functionality of the parking authority" do
    it "has the correct foamCSR contract" do
      parkingAuthorityConfig <- buildParkingAuthorityConfig deployConfig.networkId
      parkingAuthorityAddress <- readDeployAddress parkingAuthorityConfig.filepath deployConfig.networkId
      let txOpts = defaultTransactionOptions # _to ?~ parkingAuthorityAddress
      ecsr <- runWeb3 deployConfig.provider $ ParkingAuthority.parkingCSR txOpts Latest
      ecsr `shouldEqual` (Right (Right parkingAuthorityConfig.deployArgs.foamCSR))



  describe "User Registration" do
    it "can register a user and that user is owned by the right account" do
      accounts <- unsafePartial fromRight <$> runWeb3 deployConfig.provider eth_getAccounts
      let account1 = unsafePartial fromJust $ accounts !! 1
      {eventOwner, actualOwner} <- map (unsafePartial fromRight) <<< runWeb3 deployConfig.provider $ do
                      {owner, user} <- registerUser account1 deployConfig
                      let txOpts = defaultTransactionOptions # _from ?~ account1
                                                             # _to ?~ user
                      owner' <- unsafePartial fromRight <$> User.owner txOpts Latest
                      pure {eventOwner: owner, actualOwner: owner'}
      eventOwner `shouldEqual` actualOwner

-- TODO: make a separate requestZone function, we might need this in more than one place
--  it "can create a user and that user can request more zones from the authority" do
--    {parkingAuthority} <- liftAff $ buildParkingAuthorityConfig deployConfig.networkId
--    accounts <- unsafePartial fromRight <$> runWeb3 deployConfig.provider eth_getAccounts
--    let account1 = unsafePartial fromJust $ accounts !! 1
--    {user} <- map (unsafePartial fromRight) <<< runWeb3 deployConfig.provider $ registerUser account1 deployConfig
--    let mzone = (fromByteString =<<  BS.fromString "01234567" BS.Hex) :: Maybe (BytesN D4)
--        zone = unsafePartial fromJust $ mzone
--        txOpts = defaultTransactionOption  # _from ?~ account1
--                                           # _gas ?~ bigGasLimit
--                                           # _to ? parkingAuthority
--    <- map (unsafePartial fromRight) <<< runWeb3 deployConfig.provider User.requestZone 



  pending "create anchor, check that it has the correct owner"

  pending "call the registerParkingAnchor function on the ParkingAuthority from accounts[2], capture the RegisterParkingAnchor event, check the owner of the new Anchor contract is accounts[2]"



  pending "create user, create anchor, user requests zone relevant for anchor, user pays for parking, CheckIn event fires, pending anchor is reset."


registerUser
  :: forall eff.
     Address
  -- ^ from address
  -> DeployConfig
  -> Web3 (fs :: FS, avar :: AVAR, console :: CONSOLE | eff) {owner :: Address, user :: Address}
registerUser fromAccount {provider, networkId} = do
  {parkingAuthority} <- liftAff $ buildParkingAuthorityConfig networkId
  let filterRegisterUser = eventFilter (Proxy :: Proxy ParkingAuthority.RegisterParkingUser) parkingAuthority
  registerUserVar <- liftAff makeEmptyVar
  _ <- forkWeb3' $ event filterRegisterUser $ \e@(ParkingAuthority.RegisterParkingUser ev) -> liftAff $ do
    log $ "Got RegisterParkingUser: " <> show e
    putVar ev registerUserVar
    pure TerminateEvent
  let txOpts = defaultTransactionOptions # _from ?~ fromAccount
                                         # _gas ?~ bigGasLimit
                                         # _to ?~ parkingAuthority
  txHash <- ParkingAuthority.registerUser txOpts
  liftAff <<< log $ "Registered User " <> show fromAccount <> ", Transaction Hash:" <> show txHash
  liftAff $ takeVar registerUserVar

bigGasLimit :: BigNumber
bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "9000000"
