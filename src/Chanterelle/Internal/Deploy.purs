module Chanterelle.Internal.Deploy
  ( deployContract
  , readDeployAddress
  ) where

import Prelude

import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types.Deploy (ContractConfig, DeployConfig(..), DeployError(..))
import Chanterelle.Internal.Utils (jsonStringifyWithSpaces, pollTransactionReceipt, validateDeployArgs, withTimeout)
import Control.Error.Util ((??))
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Argonaut (_Object, _String, jsonEmptyObject, (~>), (:=))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Lens ((^?), (%~))
import Data.Lens.Index (ix)
import Data.Maybe (isNothing, fromJust)
import Data.StrMap as M
import Network.Ethereum.Core.HexString as HexString
import Network.Ethereum.Web3 (runWeb3)
import Network.Ethereum.Web3.Types (NoPay, ETH, Web3, Address, BigNumber, BlockNumber(..), HexString, TransactionOptions, TransactionReceipt(..), TransactionStatus(..), mkHexString, mkAddress)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (FS, readTextFile, writeTextFile)
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)


type DeployInfo =
  { deployAddress :: Address
  , blockHash :: HexString
  , blockNumber :: BigNumber
  , transactionHash :: HexString
  }

-- | Write update "networks" object in the solc artifact with a (NetworkId, Address) pair corresponding
-- | to a deployment.
writeDeployInfo
  :: forall eff m.
     MonadAff (fs :: FS | eff) m
  => FilePath
  -- filename of contract artifact
  -> BigNumber
  -- network id
  -> DeployInfo
  -- deployed contract address
  -> m (Either String Unit)
writeDeployInfo filename nid {deployAddress, blockNumber, blockHash, transactionHash} = runExceptT $ do
  artifact <- ExceptT $ jsonParser <$> liftAff (readTextFile UTF8 filename)
  let networkIdObj =    "address" := show deployAddress
                     ~> "blockNumber" := show (HexString.toHexString blockNumber)
                     ~> "blockHash" := show blockHash
                     ~> "transactionHash" := show transactionHash
                     ~> jsonEmptyObject
      artifactWithAddress = artifact # _Object <<< ix "networks" <<< _Object %~ M.insert (show nid) networkIdObj
  liftAff $ writeTextFile UTF8 filename $ jsonStringifyWithSpaces 4 artifactWithAddress

-- | Read the deployment address for a given network id from the solc artifact.
readDeployAddress
  :: forall eff m.
     MonadThrow DeployError m
  => MonadAff (fs :: FS | eff) m
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
  :: forall eff m.
     MonadThrow DeployError m
  => MonadAff (console :: CONSOLE, eth :: ETH | eff) m
  => MonadAsk DeployConfig m
  => HexString
   -- ^ publishing transaction hash
  -> String
  -- ^ contract name
  -> m DeployInfo
getPublishedContractDeployInfo txHash name = do
  (DeployConfig {timeout, provider}) <- ask
  log Info $ "Polling for TransactionReceipt: " <> show txHash
  etxReceipt <- liftAff <<< attempt $ withTimeout timeout (pollTransactionReceipt txHash provider)
  case etxReceipt of
    Left err ->
      let message = "No Transaction Receipt found for deployment " <> show txHash
      in throwError $ OnDeploymentError {name, message}
    Right (TransactionReceipt txReceipt) ->
      if txReceipt.status == Failed || isNothing (unNullOrUndefined txReceipt.contractAddress)
         then
            let message = "Deployment failed to create contract, no address found or status 0x0 in receipt: " <> name
            in throwError $ OnDeploymentError {name, message}
         else do
           let deployAddress = unsafePartial fromJust <<< unNullOrUndefined $ txReceipt.contractAddress
               BlockNumber blockNumber = txReceipt.blockNumber
           log Info $ "Contract " <> name <> " deployed to address " <> show deployAddress
           pure { deployAddress
                , blockNumber
                , blockHash: txReceipt.blockHash
                , transactionHash: txReceipt.transactionHash
                }

-- | Get the contract bytecode from the solc output corresponding to the contract config.
getContractBytecode
  :: forall eff m args.
     MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff (fs :: FS | eff) m
  => ContractConfig args
  -> m HexString
getContractBytecode cconfig@{filepath, name} = do
    cfg@(DeployConfig {provider}) <- ask
    ebc <- getBytecode filepath
    case ebc of
      Left err ->
        let errMsg = "Couln't find contract bytecode in artifact " <> filepath <> " -- " <> show err
        in throwError $ ConfigurationError errMsg
      Right bc -> pure bc
  where
    getBytecode filename = runExceptT $ do
      artifact <- ExceptT $ jsonParser <$> liftAff (readTextFile UTF8 filename)
      bytecode <- (artifact ^? _Object <<< ix "bytecode" <<< _String) ?? "artifact missing 'bytecode' field."
      mkHexString bytecode ?? "bytecode not a valid hex string"

-- | Deploy a contract using its ContractConfig object.
deployContract
  :: forall eff args m.
     MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff (console :: CONSOLE, eth :: ETH, fs :: FS | eff) m
  => TransactionOptions NoPay
  -> ContractConfig args
  -> m {deployAddress :: Address, deployArgs :: Record args}
deployContract txOptions ccfg@{filepath, name, constructor} = do
  (DeployConfig {provider, primaryAccount}) <- ask
  validatedArgs <- validateDeployArgs ccfg
  bytecode <- getContractBytecode ccfg
  let deploymentAction = constructor txOptions bytecode validatedArgs
  deployAddress <- deployContractAndWriteToArtifact filepath name deploymentAction
  pure {deployAddress, deployArgs: validatedArgs}

-- | Helper function which deploys a contract and writes the new contract address to the solc artifact.
deployContractAndWriteToArtifact
  :: forall eff m.
     MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff (console :: CONSOLE , eth :: ETH, fs :: FS | eff) m
  => FilePath
  -- ^ artifact filepath
  -> String
  -- ^ contract name
  -> Web3 eff HexString
  -- ^ deploy action returning txHash
  -> m Address
deployContractAndWriteToArtifact filepath name deployAction = do
  (DeployConfig {provider, networkId, primaryAccount}) <- ask
  log Info $ "Deploying contract " <> name
  etxHash <- liftAff <<< unsafeCoerceAff $ runWeb3 provider deployAction
  case etxHash of
    Left err ->
      let message = "Web3 error " <>  show err
      in throwError $ OnDeploymentError {name, message}
    Right txHash -> do
      deployInfo <- getPublishedContractDeployInfo txHash name
      eWriteRes <- writeDeployInfo filepath networkId deployInfo
      case eWriteRes of
        Left err ->
          let message = "Failed to write address for artifact " <> filepath <> " -- " <> err
          in throwError $ PostDeploymentError {name, message}
        Right _ -> pure deployInfo.deployAddress
