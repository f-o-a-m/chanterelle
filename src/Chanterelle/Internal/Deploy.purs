module Chanterelle.Internal.Deploy
  ( deployContract
  , deployLibrary
  , linkLibrary
  , readDeployAddress
  , DeployReceipt
  ) where

import Prelude

import Chanterelle.Internal.Artifact ( _code, _network, readArtifact, updateArtifact)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types.Artifact (ArtifactBytecode(..), NetworkInfo(..), _Deployed, _NetworkBytecode, _deployAddress)
import Chanterelle.Internal.Types.Bytecode (Bytecode(..))
import Chanterelle.Internal.Types.Bytecode as CBC
import Chanterelle.Internal.Types.Deploy (ContractConfig, DeployConfig(..), DeployError(..), LibraryConfig, NetworkID)
import Chanterelle.Internal.Utils (attemptWithTimeout, catchingAff', except', pollTransactionReceipt, validateDeployArgs, withExceptM', withExceptT', (??))
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Bifunctor (lmap)
import Data.Lens (_Just, (%~), (?~), (^.), (^?))
import Data.Maybe (fromMaybe, isNothing)
import Effect.Aff.Class (class MonadAff, liftAff)
import Network.Ethereum.Web3 (runWeb3)
import Network.Ethereum.Web3.Api (eth_sendTransaction)
import Network.Ethereum.Web3.Types (NoPay, Web3, Address, HexString, TransactionOptions, TransactionReceipt(..), TransactionStatus(..), _data, _value, convert)
import Node.Path (FilePath)

-- | Write updated "networks" object in the solc artifact with a (NetworkId, Address) pair corresponding
-- | to a deployment.
writeNetworkInfo
  :: forall m
   . MonadAff m
  => MonadThrow String m
  => FilePath
  -- filename of contract artifact
  -> NetworkID
  -- network id
  -> NetworkInfo
  -- deployed contract metadata
  -> m Unit
writeNetworkInfo filename nid ni = updateArtifact filename $ 
  pure <<< (_network nid ?~ ni)

writeNewBytecode
  :: forall m
   . MonadAff m
  => MonadError String m
  => FilePath
  -> NetworkID
  -> ArtifactBytecode
  -> m Unit
writeNewBytecode filename nid (ArtifactBytecode u) = 
  writeNetworkInfo filename nid (Undeployed u)

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
  let maddress = artifact ^? _network nid <<< _Just <<< _Deployed <<< _Just <<< _deployAddress
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
  -> ArtifactBytecode
  -- ^ NetworkBytecode used in publishing
  -> m NetworkInfo
getPublishedContractDeployInfo txHash name (ArtifactBytecode { bytecode, deployedBytecode }) = do
  (DeployConfig {timeout, provider}) <- ask
  log Info $ "Polling for " <> name <> " transaction receipt: " <> show txHash
  let txReceiptError err = OnDeploymentError { name, message: "Failed to get transaction receipt: " <> show err }
  TransactionReceipt txReceipt <- catchingAff' txReceiptError $ attemptWithTimeout timeout (pollTransactionReceipt txHash provider)
  if txReceipt.status == Failed || isNothing (txReceipt.contractAddress)
    then
      let message = "Deployment failed to create contract, no address found or status 0x0 in receipt: " <> name
      in throwError $ OnDeploymentError {name, message}
    else do
      deployAddress <- txReceipt.contractAddress ?? Impossibility "A contract which has an address also has no address"
      log Info $ "Contract " <> name <> " deployed to address " <> show deployAddress
      pure $ Deployed
              { deployAddress
              , blockNumber: txReceipt.blockNumber
              , blockHash: txReceipt.blockHash
              , transactionHash: txReceipt.transactionHash
              , bytecode
              , deployedBytecode
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
  -> m ArtifactBytecode
getContractBytecode { filepath } = do
  DeployConfig { networkID } <- ask
  let fullError err = ConfigurationError $ "Couldn't find contract bytecode in artifact " <> filepath <> ": " <> err
      compiledBytecodeError err = "Couldn't get compiled artifact bytecode: " <> err
      networkBytecodeError  err = " Couldn't get bytecode for network " <> show networkID <> ": " <> err
  withExceptT' fullError $ do
    artifact <- readArtifact filepath
    let networkBytecode  = artifact ^? _network networkID <<< _Just <<< _NetworkBytecode
        compiledBytecode = artifact ^. _code
    pure $ fromMaybe compiledBytecode networkBytecode


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
  nbc@ArtifactBytecode { bytecode: bc } <- getContractBytecode ccfg
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
  -> m ArtifactBytecode
linkLibrary ccfg@{ filepath, name } { libraryName, libraryAddress } = do
    (DeployConfig { provider, networkID, writeArtifacts }) <- ask
    ArtifactBytecode { bytecode: originalBytecode, deployedBytecode: originalDeployedBytecode } <- getContractBytecode ccfg
    bytecode <- link' "construction bytecode" originalBytecode
    deployedBytecode <- link' "on-chain bytecode" originalDeployedBytecode
    let newBytecode = ArtifactBytecode { bytecode, deployedBytecode }
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
  nbc@ArtifactBytecode { bytecode: bc } <- getContractBytecode ccfg
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
  -> ArtifactBytecode
  -- ^ ArtifactBytecode being deployed
  -> m { deployAddress :: Address, deployHash :: HexString }
deployContractAndWriteToArtifact filepath name deployAction nbc = do
    (DeployConfig { provider, networkID, primaryAccount, writeArtifacts }) <- ask
    log Info $ "Deploying contract " <> name
    deployHash <- withExceptM' onDeploymentError <<< liftAff $ runWeb3 provider deployAction
    networkInfo <- getPublishedContractDeployInfo deployHash name nbc
    deployAddress <- (networkInfo ^? _Deployed <<< _Just <<< _deployAddress) ?? Impossibility "A published contract did not have a deploy address"
    when writeArtifacts <<< withExceptT' postDeploymentError $ writeNetworkInfo filepath networkID networkInfo
    pure { deployAddress, deployHash }
  where
    onDeploymentError err = OnDeploymentError { name, message: "Web3 error while deploying contract: " <> show err }
    postDeploymentError err = PostDeploymentError { name, message: "Failed to update deployed address in artifact at " <> filepath <> ": " <> show err }
