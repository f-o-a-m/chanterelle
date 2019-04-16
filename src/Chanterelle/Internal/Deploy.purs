module Chanterelle.Internal.Deploy
  ( deployContract
  , deployLibrary
  , linkLibrary
  , readDeployAddress
  , DeployReceipt
  ) where

import Prelude

import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types.Bytecode (Bytecode(..))
import Chanterelle.Internal.Types.Bytecode as CBC
import Chanterelle.Internal.Types.Deploy (ContractConfig, DeployConfig(..), DeployError(..))
import Chanterelle.Internal.Utils (jsonStringifyWithSpaces, pollTransactionReceipt, validateDeployArgs, withTimeout)
import Control.Error.Util ((??))
import Effect.Aff (attempt)
import Effect.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Argonaut (_Object, _String, decodeJson, encodeJson, jsonEmptyObject, (:=), (~>))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.Lens ((^?), (%~), (?~))
import Data.Lens.Index (ix)
import Data.Maybe (isNothing, fromJust)
import Foreign.Object as M
import Network.Ethereum.Core.HexString as HexString
import Network.Ethereum.Web3 (runWeb3)
import Network.Ethereum.Web3.Types (NoPay, Web3, Address, BigNumber, BlockNumber(..), HexString, TransactionOptions, TransactionReceipt(..), TransactionStatus(..), mkHexString, mkAddress)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)


type DeployInfo =
  { deployAddress :: Address
  , blockHash :: HexString
  , blockNumber :: BlockNumber
  , transactionHash :: HexString
  }

-- | Write updated "networks" object in the solc artifact with a (NetworkId, Address) pair corresponding
-- | to a deployment.
writeDeployInfo
  :: forall m.
     MonadAff m
  => FilePath
  -- filename of contract artifact
  -> String
  -- network id
  -> DeployInfo
  -- deployed contract address
  -> m (Either String Unit)
writeDeployInfo filename nid {deployAddress, blockNumber, blockHash, transactionHash} = runExceptT $ do
  artifact <- ExceptT $ jsonParser <$> liftAff (readTextFile UTF8 filename)
  let BlockNumber bn = blockNumber
      networkIdObj =  "address" := show deployAddress
                   ~> "blockNumber" := show (HexString.toHexString bn)
                   ~> "blockHash" := show blockHash
                   ~> "transactionHash" := show transactionHash
                   ~> jsonEmptyObject
      artifactWithAddress = artifact # _Object <<< ix "networks" <<< _Object %~ M.insert nid networkIdObj
  liftAff $ writeTextFile UTF8 filename $ jsonStringifyWithSpaces 4 artifactWithAddress

writeNewBytecode
  :: forall eff m.
      MonadAff (fs :: FS | eff) m
  => FilePath
  -> Bytecode
  -> m (Either String Unit)
writeNewBytecode filename bc = runExceptT do
  artifact <- ExceptT $ jsonParser <$> liftAff (readTextFile UTF8 filename)
  let artifactWithBytecode = artifact # _Object %~ M.insert "bytecode" (encodeJson bc)
  liftAff $ writeTextFile UTF8 filename $ jsonStringifyWithSpaces 4 artifactWithBytecode
  
-- | Read the deployment address for a given network id from the solc artifact.
readDeployAddress
  :: forall m.
     MonadThrow DeployError m
  => MonadAff m
  => FilePath
  -- ^ contract filepath
  -> BigNumber
  -- ^ network id
  -> m Address
readDeployAddress filepath nid = do
  eAddr <- runExceptT $ do
    artifact <- ExceptT $ jsonParser <$> liftAff (readTextFile UTF8 filepath)
    let maddress = do
          addrString <- artifact ^? _Object <<< ix "networks" <<< _Object <<< ix (show nid) <<< _Object <<< ix "address" <<< _String
          mkAddress =<< mkHexString addrString
    maddress ?? ("Couldn't find valid Deploy Address in artifact: " <> filepath)
  either (throwError <<< ConfigurationError) pure eAddr

-- | Poll a TransactionHash for the receipt of a deployment transaction, and throw an error in the event that the
-- | transaction failed.
getPublishedContractDeployInfo
  :: forall m.
     MonadThrow DeployError m
  => MonadAff m
  => MonadAsk DeployConfig m
  => HexString
   -- ^ publishing transaction hash
  -> String
  -- ^ contract name
  -> m DeployInfo
getPublishedContractDeployInfo txHash name = do
  (DeployConfig {timeout, provider}) <- ask
  log Info $ "Polling for " <> name <> " TransactionReceipt: " <> show txHash
  etxReceipt <- liftAff <<< attempt $ withTimeout timeout (pollTransactionReceipt txHash provider)
  case etxReceipt of
    Left err ->
      let message = "No Transaction Receipt found for deployment " <> show txHash
      in throwError $ OnDeploymentError {name, message}
    Right (TransactionReceipt txReceipt) ->
      if txReceipt.status == Failed || isNothing (txReceipt.contractAddress)
         then
            let message = "Deployment failed to create contract, no address found or status 0x0 in receipt: " <> name
            in throwError $ OnDeploymentError {name, message}
         else do
           let deployAddress = unsafePartial fromJust $ txReceipt.contractAddress
           log Info $ "Contract " <> name <> " deployed to address " <> show deployAddress
           pure { deployAddress
                , blockNumber: txReceipt.blockNumber
                , blockHash: txReceipt.blockHash
                , transactionHash: txReceipt.transactionHash
                }

-- | Get the contract bytecode from the solc output corresponding to the contract config.
getContractBytecode
  :: forall m args.
     MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff m
  => ContractConfig args
  -> m HexString
getContractBytecode cconfig@{filepath, name} = do
    cfg@(DeployConfig {provider}) <- ask
    ebc <- getBytecode
    case ebc of
      Left err ->
        let errMsg = "Couln't find contract bytecode in artifact " <> filepath <> " -- " <> show err
        in throwError $ ConfigurationError errMsg
      Right bc -> pure bc
  where
    getBytecode = runExceptT $ do
      artifact <- ExceptT $ jsonParser <$> liftAff (readTextFile UTF8 filepath)
      bytecode <- (artifact ^? _Object <<< ix "bytecode") ?? "artifact missing 'bytecode' field."
      ExceptT $ pure $ decodeJson bytecode

type DeployReceipt args =
  { deployAddress :: Address
  , deployArgs :: Record args
  , deployHash :: HexString
  }

type LibraryMeta = (libraryName :: String, libraryAddress :: Address)

-- | Deploy a Library. Naturally, there's no contractConfig here...
deployLibrary
  :: forall eff args m.
     MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff (console :: CONSOLE, eth :: ETH, fs :: FS | eff) m
  => TransactionOptions NoPay
  -> FilePath
  -> String
  -> m (DeployReceipt LibraryMeta)
deployLibrary txo filepath name = do
  (DeployConfig {provider}) <- ask
  bc <- getContractBytecode filepath name
  case bc of
    BCUnlinked _ -> throwError $ DeployingUnlinkedBytecodeError { name }
    BCLinked { bytecode } -> do
      let txo' = txo # _data ?~ bytecode
                     # _value %~ map convert
          deploymentAction = eth_sendTransaction txo'
      {deployAddress, deployHash} <- deployContractAndWriteToArtifact filepath name deploymentAction
      pure {deployAddress, deployHash, deployArgs: { libraryName: name, libraryAddress: deployAddress } }

linkLibrary
  :: forall eff args m.
     MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff (console :: CONSOLE, eth :: ETH, fs :: FS | eff) m
  => ContractConfig args
  -> Record LibraryMeta
  -> m Bytecode
linkLibrary ccg@{filepath, name} { libraryName, libraryAddress } = do
  (DeployConfig { provider, writeArtifacts }) <- ask
  bc <- getContractBytecode filepath name
  log Info $ "Linking " <> libraryName <> " at " <> show libraryAddress <> " to " <> name <> " in " <> filepath
  let res = CBC.linkLibrary libraryName libraryAddress bc
      errPrefix = "While linking " <> libraryName <> " to " <> name <> ": "
  case res of
    Left err -> throwError $ LinkingError $ errPrefix <> err
    Right bc' -> do
      if writeArtifacts
        then do
          eWriteRes <- writeNewBytecode filepath bc'
          case eWriteRes of
            Left err ->
              let msg = errPrefix <> "Failed to write linked bytecode for artifact " <> filepath <> " -- " <> err
              in throwError $ LinkingError msg
            Right _ -> pure bc'
        else pure bc'

-- | Deploy a contract using its ContractConfig object.
deployContract
  :: forall args m.
     MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff m
  => TransactionOptions NoPay
  -> ContractConfig args
  -> m (DeployReceipt args)
deployContract txOptions ccfg@{filepath, name, constructor} = do
  (DeployConfig { provider }) <- ask
  validatedArgs <- validateDeployArgs ccfg
  bc <- getContractBytecode filepath name
  case bc of
    BCUnlinked _ -> throwError $ DeployingUnlinkedBytecodeError { name }
    BCLinked { bytecode } -> do
      let deploymentAction = constructor txOptions bytecode validatedArgs
      {deployAddress, deployHash} <- deployContractAndWriteToArtifact filepath name deploymentAction
      pure {deployAddress, deployArgs: validatedArgs, deployHash}

-- | Helper function which deploys a contract and writes the new contract address to the solc artifact.
deployContractAndWriteToArtifact
  :: forall m.
     MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff m
  => FilePath
  -- ^ artifact filepath
  -> String
  -- ^ contract name
  -> Web3 HexString
  -- ^ deploy action returning txHash
  -> m {deployAddress :: Address, deployHash :: HexString}
deployContractAndWriteToArtifact filepath name deployAction = do
  (DeployConfig { provider, networkId, primaryAccount, writeArtifacts }) <- ask
  log Info $ "Deploying contract " <> name
  etxHash <- liftAff $ runWeb3 provider deployAction
  case etxHash of
    Left err ->
      let message = "Web3 error " <>  show err
      in throwError $ OnDeploymentError {name, message}
    Right txHash -> do
      deployInfo <- getPublishedContractDeployInfo txHash name
      if writeArtifacts
        then do
          eWriteRes <- writeDeployInfo filepath networkId deployInfo
          case eWriteRes of
            Left err ->
              let message = "Failed to write address for artifact " <> filepath <> " -- " <> err
              in throwError $ PostDeploymentError {name, message}
            Right _ -> pure {deployAddress: deployInfo.deployAddress, deployHash: txHash}
        else pure {deployAddress: deployInfo.deployAddress, deployHash: txHash}
