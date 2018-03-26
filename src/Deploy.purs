module Deploy
  ( deployContractNoArgs
  , deployContractWithArgs
  , readDeployAddress
  , DeployM
  , runDeployM
  ) where

import Prelude
import Control.Error.Util ((??))
import Control.Monad.Aff (Aff, Milliseconds(..), liftEff', attempt)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Console as C
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.Eff.Exception (throw)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Argonaut (stringify, _Object, _String, jsonEmptyObject, (~>), (:=))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Lens ((^?), (?~), (%~))
import Data.Lens.Index (ix)
import Data.Maybe (isNothing, fromJust)
import Data.StrMap as M
import Network.Ethereum.Web3 (ETH, Web3, Address, BigNumber, HexString, TransactionOptions, mkHexString, _data, fromWei, _value, runWeb3, mkAddress)
import Network.Ethereum.Web3.Api (eth_sendTransaction)
import Network.Ethereum.Web3.Types (NoPay)
import Network.Ethereum.Web3.Types.Provider (Provider)
import Data.Newtype (unwrap)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (FS, readTextFile, writeTextFile)
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)
import Utils (withTimeout, pollTransactionReceipt, reportIfErrored)
import Types (DeployConfig(..), ContractConfig)


-- | Fetch the bytecode from a solidity build artifact
getBytecode
  :: forall eff.
     FilePath
  -- ^ filename of contract artifact
  -> Aff (fs :: FS | eff) (Either String HexString)
getBytecode filename = runExceptT $ do
  artifact <- ExceptT $ jsonParser <$> readTextFile UTF8 filename
  bytecode <- (artifact ^? _Object <<< ix "bytecode" <<< _String) ?? "artifact missing 'bytecode' field."
  mkHexString bytecode ?? "bytecode not a valid hex) string"

-- | Publish a contract based on the bytecode. Used for contracts with no constructor.
defaultPublishContract
  :: forall eff.
     TransactionOptions NoPay
  -> HexString
  -- ^ Contract bytecode
  -> Web3 eff HexString
defaultPublishContract txOpts bytecode =
  eth_sendTransaction $ txOpts # _data ?~ bytecode
                               # _value ?~ fromWei zero

-- | Write the "network object" for a given deployment on a network with
-- | the given id.
-- | TODO: this currently overwrites the entire network object
writeDeployAddress
  :: forall eff.
     FilePath
  -- filename of contract artifact
  -> Address
  -- deployed contract address
  -> BigNumber
  -- network id
  -> Aff (fs :: FS | eff) (Either String Unit)
writeDeployAddress filename deployAddress nid = runExceptT $ do
  artifact <- ExceptT $ jsonParser <$> readTextFile UTF8 filename
  let networkIdObj = "address" := show deployAddress ~> jsonEmptyObject
      artifactWithAddress = artifact # _Object <<< ix "networks" <<< _Object %~ M.insert (show nid) networkIdObj
  liftAff $ writeTextFile UTF8 filename $ stringify artifactWithAddress

readDeployAddress
  :: forall eff.
     FilePath
  -- contract filepath
  -> BigNumber
  -- network id
  -> Aff (fs :: FS | eff) Address
readDeployAddress filepath nid = do
  eAddr <- runExceptT $ do
    artifact <- ExceptT $ jsonParser <$> readTextFile UTF8 filepath
    let maddress = do
          addrString <- artifact ^? _Object <<< ix "networks" <<< _Object <<< ix (show nid) <<< _Object <<< ix "address" <<< _String
          mkAddress =<< mkHexString addrString
    maddress ?? ("Couldn't find valid Deploy Address in artifact: " <> filepath)
  either (liftEff' <<< throw) pure eAddr

getPublishedContractAddress
  :: forall eff.
     HexString
   -- ^ publishing transaction hash
  -> Provider
  -- ^ web3 connection
  -> String
  -- ^ contract name
  -> Aff (eth :: ETH, console :: CONSOLE | eff) Address
getPublishedContractAddress txHash provider name = do
  C.log $ "Polling for TransactionReceipt: " <> show txHash
  etxReceipt <- attempt $ withTimeout (Milliseconds $ 90.0 * 1000.0) (pollTransactionReceipt txHash provider)
  case unwrap <$> etxReceipt of
    Left err -> do
      liftAff $ C.error $ "No Transaction Receipt found for deployment : " <> name <> " : " <> show txHash
      liftAff $ throwError err
    Right txReceipt ->
      if txReceipt.status == "0x0" || isNothing (unNullOrUndefined txReceipt.contractAddress)
         then do
            let missingMessage = "Deployment failed to create contract, no address found or status 0x0 in receipt: " <> name
            liftAff $ C.error missingMessage
            liftAff $ liftEff' $ throw missingMessage
         else do
           let contractAddress = unsafePartial fromJust <<< unNullOrUndefined $ txReceipt.contractAddress
           liftAff <<< C.log $ "Contract " <> name <> " deployed to address " <> show contractAddress
           pure contractAddress

-- | `deployContractNoArgs` grabs the bytecode from a build artifact and deploys it
-- | from the primary account, writing the contract address to the artifact.
deployContractNoArgs
  :: forall eff.
     ContractConfig ()
  -> TransactionOptions NoPay
  -> DeployM eff Address
deployContractNoArgs {filepath, name} txOpts = do
  cfg@(DeployConfig {provider}) <- ask
  bytecode <- liftAff do
    ebc <- getBytecode filepath
    reportIfErrored ("Couln't find contract bytecode in artifact " <> filepath) ebc
  let deployAction =  defaultPublishContract txOpts bytecode
  deployContractAndWriteToArtifact filepath name deployAction

-- | `deployContractWithArgs` grabs the bytecode from the artifact and uses the
-- | args defined in the contract config to deploy, then writes the address
-- | to the artifact.
deployContractWithArgs
  :: forall eff args.
     ContractConfig (deployArgs :: args)
  -> (HexString -> args -> Web3 eff HexString)
  -> DeployM eff Address
deployContractWithArgs {filepath, name, deployArgs} deployer = do
  cfg@(DeployConfig {provider, primaryAccount}) <- ask
  bytecode <- liftAff do
    ebc <- getBytecode filepath
    reportIfErrored ("Couln't find contract bytecode in artifact " <> filepath) ebc
  deployContractAndWriteToArtifact filepath name (deployer bytecode deployArgs)

-- | The common deployment function for contracts with or without args.
deployContractAndWriteToArtifact
  :: forall eff.
     FilePath
  -- ^ artifact filepath
  -> String
  -- ^ contract name
  -> Web3 eff HexString
  -- ^ deploy action returning txHash
  -> DeployM eff Address
deployContractAndWriteToArtifact filepath name deployAction = do
    (DeployConfig {provider, networkId, primaryAccount}) <- ask
    liftAff $ do
      C.log $ "Deploying contract " <> name
      etxHash <- unsafeCoerceAff $ runWeb3 provider deployAction
      txHash <- reportIfErrored ("Web3 error during contract deployment for " <> show name) etxHash
      contractAddress <- getPublishedContractAddress txHash provider name
      writeDeployAddress filepath contractAddress networkId >>= reportIfErrored ("Failed to write address for artifact " <> filepath)
      pure contractAddress

--------------------------------------------------------------------------------
-- | DeployM
--------------------------------------------------------------------------------

newtype DeployM eff a = DeployM (ReaderT DeployConfig (Aff (eth :: ETH, fs :: FS, console :: CONSOLE | eff)) a)

runDeployM :: forall eff a. DeployM eff a -> DeployConfig -> Aff (fs :: FS, console :: CONSOLE, eth :: ETH | eff) a
runDeployM (DeployM deploy) = runReaderT deploy

derive newtype instance functorDeployM :: Functor (DeployM eff)
derive newtype instance applyDeployM :: Apply (DeployM eff)
derive newtype instance applicativeDeployM :: Applicative (DeployM eff)
derive newtype instance bindDeployM :: Bind (DeployM eff)
derive newtype instance monadDeployM :: Monad (DeployM eff)
derive newtype instance monadAskDeployM :: MonadAsk DeployConfig (DeployM eff)
derive newtype instance monadEffDeployM :: MonadEff (eth :: ETH, fs :: FS, console :: CONSOLE | eff) (DeployM eff)
derive newtype instance monadAffDeployM :: MonadAff (eth :: ETH, fs :: FS, console :: CONSOLE | eff) (DeployM eff)

