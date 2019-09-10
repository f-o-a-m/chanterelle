module Chanterelle.Internal.Deploy
  ( deployContract
  , deployLibrary
  , linkLibrary
  , readDeployAddress
  , DeployReceipt
  , NetworkBytecode(..)
  ) where

import Prelude

import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types.Bytecode (Bytecode(..))
import Chanterelle.Internal.Types.Bytecode as CBC
import Chanterelle.Internal.Types.Deploy (ContractConfig, DeployConfig(..), DeployError(..), LibraryConfig, NetworkID)
import Chanterelle.Internal.Utils (attemptWithTimeout, catchingAff', except', jsonStringifyWithSpaces, pollTransactionReceipt, readTextFile, validateDeployArgs, withExceptM', withExceptT', withTextFile, (??))
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, _Object, _String, decodeJson, encodeJson, jsonEmptyObject, jsonNull, (.:), (:=), (~>))
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Lens (Optic', (%~), (?~), (.~), (^?))
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Iso (non)
import Data.Maybe (Maybe(..), isNothing, fromJust)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Effect.Aff.Class (class MonadAff, liftAff)
import Network.Ethereum.Core.HexString as HexString
import Network.Ethereum.Web3 (runWeb3)
import Network.Ethereum.Web3.Api (eth_sendTransaction)
import Network.Ethereum.Web3.Types (NoPay, Web3, Address, BlockNumber(..), HexString, TransactionOptions, TransactionReceipt(..), TransactionStatus(..), _data, _value, convert, mkHexString, mkAddress)
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)

newtype DeployInfo = DeployInfo
  { deployAddress    :: Address
  , blockHash        :: HexString
  , blockNumber      :: BlockNumber
  , transactionHash  :: HexString
  , networkBytecode  :: NetworkBytecode
  }
instance decodeJsonDeployInfo :: DecodeJson DeployInfo where
  decodeJson j = do
    o <- decodeJson j
    networkBytecode <- decodeJson j
    deployAddress <- o .: "address"
    blockHash <- o .: "blockHash"
    blockNumber <- o .: "blockNumber"
    transactionHash <- o .: "transactionHash"
    pure $ DeployInfo { deployAddress, blockHash, blockNumber, transactionHash, networkBytecode }

instance encodeJsonDeployInfo :: EncodeJson DeployInfo where
  encodeJson (DeployInfo d) =
    let BlockNumber bn      = d.blockNumber
    in    "address" := show d.deployAddress
       ~> "blockNumber" := show (HexString.toHexString bn)
       ~> "blockHash" := show d.blockHash
       ~> "transactionHash" := show d.transactionHash
       ~> encodeJson d.networkBytecode

newtype NetworkBytecode = NetworkBytecode
  { bytecode         :: Bytecode
  , deployedBytecode :: Bytecode
  }

instance decodeJsonNetworkBytecode :: DecodeJson NetworkBytecode where
  decodeJson j = do
    o <- decodeJson j
    bytecode <- o .: "bytecode"
    deployedBytecode <- o .: "deployedBytecode"
    pure $ NetworkBytecode { bytecode, deployedBytecode }

instance encodeJsonNetworkBytecode :: EncodeJson NetworkBytecode where
  encodeJson (NetworkBytecode nbc) =
       "bytecode"         := encodeJson nbc.bytecode
    ~> "deployedBytecode" := encodeJson nbc.deployedBytecode
    ~> jsonEmptyObject

-- 
setNetworkField
  :: Json -- Value of "networks.<net_version>" field
  -> NetworkID -- Network ID being operated on
  -> Json -- Starting artifact
  -> Json -- Updated artifact
setNetworkField field nid input = input # networkLens nid .~ field

networkLens :: forall p. Strong p => Choice p => NetworkID -> Optic' p Json Json
networkLens nid = _Object <<< at "networks" <<< non jsonEmptyObject <<< _Object <<< at (show nid) <<< non jsonNull

readArtifact
  :: forall m
   . MonadAff m
  => MonadThrow String m
  => FilePath
  -> m Json
readArtifact filepath = except' =<< (jsonParser <$> readTextFile filepath)

updateArtifact
  :: forall m
   . MonadAff m
  => MonadThrow String m
  => FilePath
  -> (Json -> m Json)
  -> m Unit
updateArtifact filepath action = withTextFile filepath $ \contents -> do
  artifact <- except' $ jsonParser contents
  newArtifact <- action artifact
  pure $ jsonStringifyWithSpaces 4 newArtifact

-- | Write updated "networks" object in the solc artifact with a (NetworkId, Address) pair corresponding
-- | to a deployment.
writeDeployInfo
  :: forall m
   . MonadAff m
  => MonadThrow String m
  => FilePath
  -- filename of contract artifact
  -> NetworkID
  -- network id
  -> DeployInfo
  -- deployed contract metadata
  -> m Unit
writeDeployInfo filename nid deployInfo = updateArtifact filename $ \artifact ->
  pure $ setNetworkField artifact nid (encodeJson deployInfo)

writeNewBytecode
  :: forall m
   . MonadAff m
  => MonadError String m
  => FilePath
  -> NetworkID
  -> NetworkBytecode
  -> m Unit
writeNewBytecode filename nid nbc = updateArtifact filename $ \artifact ->
  pure $ setNetworkField artifact nid (encodeJson nbc)

-- | Read the deployment address for a given network id from the solc artifact.
readDeployAddress
  :: forall m
   . MonadThrow DeployError m
  => MonadAff m
  => FilePath
  -- ^ contract filepath
  -> NetworkID
  -- ^ network id
  -> m Address
readDeployAddress filepath nid = withExceptT' ConfigurationError $ do
  artifact <- readArtifact filepath
  let maddress = do
        addrString <- artifact ^? networkLens nid <<< _Object <<< ix "address" <<< _String
        mkAddress =<< mkHexString addrString
  maddress ?? ("Couldn't find valid deploy address in artifact: " <> filepath)

-- | Poll a TransactionHash for the receipt of a deployment transaction, and throw an error in the event that the
-- | transaction failed.
getPublishedContractDeployInfo
  :: forall m
   . MonadThrow DeployError m
  => MonadAff m
  => MonadAsk DeployConfig m
  => HexString
   -- ^ publishing transaction hash
  -> String
  -- ^ contract name
  -> NetworkBytecode
  -- ^ NetworkBytecode used in publishing
  -> m DeployInfo
getPublishedContractDeployInfo txHash name networkBytecode = do
  (DeployConfig {timeout, provider}) <- ask
  log Info $ "Polling for " <> name <> " transaction receipt: " <> show txHash
  let txReceiptError err = OnDeploymentError { name, message: "Failed to get transaction receipt: " <> show err }
  TransactionReceipt txReceipt <- catchingAff' txReceiptError $ attemptWithTimeout timeout (pollTransactionReceipt txHash provider)
  if txReceipt.status == Failed || isNothing (txReceipt.contractAddress)
    then
      let message = "Deployment failed to create contract, no address found or status 0x0 in receipt: " <> name
      in throwError $ OnDeploymentError {name, message}
    else do
      let deployAddress = unsafePartial fromJust $ txReceipt.contractAddress
      log Info $ "Contract " <> name <> " deployed to address " <> show deployAddress
      pure $ DeployInfo
              { deployAddress
              , blockNumber: txReceipt.blockNumber
              , blockHash: txReceipt.blockHash
              , transactionHash: txReceipt.transactionHash
              , networkBytecode
              }

-- | Get the contract bytecode from the solc output corresponding to the contract config.
-- | First it will attempt to check for any network-specific bytecode under the corresponding network
-- | before falling back to the "raw" solc bytecode.
getContractBytecode
  :: forall m args
   . MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff m
  => LibraryConfig args
  -> m NetworkBytecode
getContractBytecode { filepath } = do
  DeployConfig { networkID } <- ask
  let fullError err = ConfigurationError $ "Couln't find contract bytecode in artifact " <> filepath <> ": " <> err
      compiledBytecodeError err = "Couldn't get compiled artifact bytecode: " <> err
      networkBytecodeError  err = " Couldn't get bytecode for network " <> show networkID <> ": " <> err
  withExceptT' fullError $ do
    artifact <- readArtifact filepath
    except' $ case artifact ^? networkLens networkID of
      Nothing -> lmap compiledBytecodeError $ decodeJson artifact
      Just net -> lmap networkBytecodeError $ decodeJson net



type DeployReceipt args =
  { deployAddress :: Address
  , deployArgs :: Record args
  , deployHash :: HexString
  }

type LibraryMeta = (libraryName :: String, libraryAddress :: Address)

-- | Deploy a Library. Naturally, there's no contractConfig here...
deployLibrary
  :: forall m
   . MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff m
  => TransactionOptions NoPay
  -> LibraryConfig ()
  -> m (DeployReceipt LibraryMeta)
deployLibrary txo ccfg@{filepath, name} = do
  DeployConfig { provider } <- ask
  nbc@NetworkBytecode { bytecode: bc } <- getContractBytecode ccfg
  case bc of
    BCUnlinked _ -> throwError $ DeployingUnlinkedBytecodeError { name, libs: CBC.unlinkedLibraryNames bc }
    BCLinked { bytecode } -> do
      let txo' = txo # _data ?~ bytecode
                     # _value %~ map convert
          deploymentAction = eth_sendTransaction txo'
      { deployAddress, deployHash } <- deployContractAndWriteToArtifact filepath name deploymentAction nbc
      pure {deployAddress, deployHash, deployArgs: { libraryName: name, libraryAddress: deployAddress } }

linkLibrary
  :: forall args m
   . MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff m
  => ContractConfig args
  -> Record LibraryMeta
  -> m NetworkBytecode
linkLibrary ccfg@{ filepath, name } { libraryName, libraryAddress } = do
    (DeployConfig { provider, networkID, writeArtifacts }) <- ask
    NetworkBytecode { bytecode: originalBytecode, deployedBytecode: originalDeployedBytecode } <- getContractBytecode ccfg
    bytecode <- link' "construction bytecode" originalBytecode
    deployedBytecode <- link' "on-chain bytecode" originalDeployedBytecode
    let newBytecode = NetworkBytecode { bytecode, deployedBytecode }
    when writeArtifacts <<< withExceptT' writeBytecodeError $ writeNewBytecode filepath networkID newBytecode
    pure newBytecode

  where
    linkError bytecodeKind msg = LinkingError { contractName: name, libraryName, libraryAddress, bytecodeKind, msg }
    writeBytecodeError emsg = LinkingError { contractName: name, libraryName, libraryAddress, bytecodeKind: "bytecode", msg: ("Error while writing new artifact: " <> emsg)}
    link' bytecodeKind bytecode = do
      log Info $ "Linking " <> libraryName <> " at " <> show libraryAddress <> " to the " <> bytecodeKind <> " of " <> name <> " in " <> filepath
      except' <<< (lmap $ linkError bytecodeKind) $ CBC.linkLibrary libraryName libraryAddress bytecode

-- | Deploy a contract using its ContractConfig object.
deployContract
  :: forall args m
   . MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff m
  => TransactionOptions NoPay
  -> ContractConfig args
  -> m (DeployReceipt args)
deployContract txOptions ccfg@{filepath, name, constructor} = do
  DeployConfig { provider } <- ask
  nbc@NetworkBytecode { bytecode: bc } <- getContractBytecode ccfg
  case bc of
    BCUnlinked _ -> throwError $ DeployingUnlinkedBytecodeError { name, libs: CBC.unlinkedLibraryNames bc }
    BCLinked { bytecode } -> do
      validatedArgs <- validateDeployArgs ccfg
      let deploymentAction = constructor txOptions bytecode validatedArgs
      {deployAddress, deployHash} <- deployContractAndWriteToArtifact filepath name deploymentAction nbc
      pure {deployAddress, deployArgs: validatedArgs, deployHash}

-- | Helper function which deploys a contract and writes the new contract address to the solc artifact.
deployContractAndWriteToArtifact
  :: forall m
   . MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff m
  => FilePath
  -- ^ artifact filepath
  -> String
  -- ^ contract name
  -> Web3 HexString
  -- ^ deploy action returning txHash
  -> NetworkBytecode
  -- ^ NetworkBytecode being deployed
  -> m { deployAddress :: Address, deployHash :: HexString }
deployContractAndWriteToArtifact filepath name deployAction nbc = do
    (DeployConfig { provider, networkID, primaryAccount, writeArtifacts }) <- ask
    log Info $ "Deploying contract " <> name
    deployHash <- withExceptM' onDeploymentError <<< liftAff $ runWeb3 provider deployAction
    deployInfo@(DeployInfo { deployAddress }) <- getPublishedContractDeployInfo deployHash name nbc
    when writeArtifacts <<< withExceptT' postDeploymentError $ writeDeployInfo filepath networkID deployInfo
    pure { deployAddress, deployHash }
  where
    onDeploymentError err = OnDeploymentError { name, message: "Web3 error while deploying contract: " <> show err }
    postDeploymentError err = PostDeploymentError { name, message: "Failed to update deployed address in artifact at " <> filepath <> ": " <> show err }