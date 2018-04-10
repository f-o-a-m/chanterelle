module Chanterelle.Internal.Deploy
  ( deployContract
  , readDeployAddress
  ) where

import Prelude

import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (DeployConfig(..), DeployError(..), ContractConfig)
import Chanterelle.Internal.Utils (withTimeout, pollTransactionReceipt, validateDeployArgs)
import Control.Error.Util ((??))
import Control.Monad.Aff (Milliseconds(..), attempt)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Argonaut (stringify, _Object, _String, jsonEmptyObject, (~>), (:=))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Lens ((^?), (%~))
import Data.Lens.Index (ix)
import Data.Maybe (isNothing, fromJust)
import Data.StrMap as M
import Network.Ethereum.Web3 (runWeb3)
import Network.Ethereum.Web3.Types (NoPay, ETH, Web3, Address, BigNumber, HexString, TransactionOptions, TransactionReceipt(..), mkHexString, mkAddress)
import Network.Ethereum.Web3.Types.Provider (Provider)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (FS, readTextFile, writeTextFile)
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)

-- | Fetch the bytecode from a solidity build artifact
getBytecode
  :: forall eff m.
     MonadAff (fs :: FS | eff) m
  => FilePath
  -- ^ filename of contract artifact
  -> m (Either String HexString)
getBytecode filename = runExceptT $ do
  artifact <- ExceptT $ jsonParser <$> liftAff (readTextFile UTF8 filename)
  bytecode <- (artifact ^? _Object <<< ix "bytecode" <<< _String) ?? "artifact missing 'bytecode' field."
  mkHexString bytecode ?? "bytecode not a valid hex) string"

-- | Write the "network object" for a given deployment on a network with
-- | the given id.
-- | TODO: this currently overwrites the entire network object
writeDeployAddress
  :: forall eff m.
     MonadAff (fs :: FS | eff) m
  => FilePath
  -- filename of contract artifact
  -> BigNumber
  -- network id
  -> Address
  -- deployed contract address
  -> m (Either String Unit)
writeDeployAddress filename nid deployAddress = runExceptT $ do
  artifact <- ExceptT $ jsonParser <$> liftAff (readTextFile UTF8 filename)
  let networkIdObj = "address" := show deployAddress ~> jsonEmptyObject
      artifactWithAddress = artifact # _Object <<< ix "networks" <<< _Object %~ M.insert (show nid) networkIdObj
  liftAff $ writeTextFile UTF8 filename $ stringify artifactWithAddress

readDeployAddress
  :: forall eff m.
     MonadThrow DeployError m
  => MonadAff (fs :: FS | eff) m
  => FilePath
  -- contract filepath
  -> BigNumber
  -- network id
  -> m Address
readDeployAddress filepath nid = do
  eAddr <- runExceptT $ do
    artifact <- ExceptT $ jsonParser <$> liftAff (readTextFile UTF8 filepath)
    let maddress = do
          addrString <- artifact ^? _Object <<< ix "networks" <<< _Object <<< ix (show nid) <<< _Object <<< ix "address" <<< _String
          mkAddress =<< mkHexString addrString
    maddress ?? ("Couldn't find valid Deploy Address in artifact: " <> filepath)
  either (throwError <<< ConfigurationError) pure eAddr

getPublishedContractAddress
  :: forall eff m.
     MonadThrow DeployError m
  => MonadAff (console :: CONSOLE, eth :: ETH | eff) m
  => HexString
   -- ^ publishing transaction hash
  -> Provider
  -- ^ web3 connection
  -> String
  -- ^ contract name
  -> m Address
getPublishedContractAddress txHash provider name = do
  log Info $ "Polling for TransactionReceipt: " <> show txHash
  etxReceipt <- liftAff <<< attempt $ withTimeout (Milliseconds $ 90.0 * 1000.0) (pollTransactionReceipt txHash provider)
  case etxReceipt of
    Left err ->
      let errMsg = "No Transaction Receipt found for deployment : " <> name <> " : " <> show txHash
      in throwError $ OnDeploymentError errMsg
    Right (TransactionReceipt txReceipt) ->
      if txReceipt.status == "0x0" || isNothing (unNullOrUndefined txReceipt.contractAddress)
         then
            let missingMessage = "Deployment failed to create contract, no address found or status 0x0 in receipt: " <> name
            in throwError $ OnDeploymentError missingMessage
         else do
           let contractAddress = unsafePartial fromJust <<< unNullOrUndefined $ txReceipt.contractAddress
           log Info $ "Contract " <> name <> " deployed to address " <> show contractAddress
           pure contractAddress

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

-- | `deployContractWithArgs` grabs the bytecode from the artifact and uses the
-- | args defined in the contract config to deploy, then writes the address
-- | to the artifact.
deployContract
  :: forall eff args m.
     MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff (console :: CONSOLE, eth :: ETH, fs :: FS | eff) m
  => TransactionOptions NoPay
  -> ContractConfig args
  -> m Address
deployContract txOptions ccfg@{filepath, name, constructor} = do
  (DeployConfig {provider, primaryAccount}) <- ask
  validatedArgs <- validateDeployArgs ccfg
  bytecode <- getContractBytecode ccfg
  let deploymentAction = constructor txOptions bytecode validatedArgs
  deployContractAndWriteToArtifact filepath name deploymentAction

-- | The common deployment function for contracts with or without args.
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
      let errMsg = "Web3 error during contract deployment for " <> show name <> " -- " <> show err
      in throwError $ OnDeploymentError errMsg
    Right txHash -> do
      contractAddress <- getPublishedContractAddress txHash provider name
      eWriteRes <- writeDeployAddress filepath networkId contractAddress
      case eWriteRes of
        Left err ->
          let errMsg = "Failed to write address for artifact " <> filepath <> " -- " <> err
          in throwError $ PostDeploymentError errMsg
        Right _ -> pure contractAddress
