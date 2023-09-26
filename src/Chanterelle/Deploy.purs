module Chanterelle.Deploy
  ( deploy
  , deployWithProvider
  , makeDeployConfig
  , makeDeployConfigWithProvider
  , deployContract
  , deployLibrary
  , linkLibrary
  , readDeployAddress
  ) where

import Prelude

import Chanterelle.Artifact (readArtifact, writeArtifact) as Artifact
import Chanterelle.Logging (LogLevel(..), log, logDeployError)
import Chanterelle.Types.Artifact (Artifact(..), ArtifactBytecode(..), NetworkInfo(..), _Deployed, _NetworkBytecode, _address, _code, _network)
import Chanterelle.Types.Bytecode (Bytecode(..))
import Chanterelle.Types.Bytecode as CBC
import Chanterelle.Types.Deploy (DeployM, runDeployM, ContractConfig, DeployConfig(..), DeployError(..), LibraryConfig, NetworkID)
import Chanterelle.Utils (getNetworkID, getPrimaryAccount, pollTransactionReceipt, withExceptT', makeProvider)
import Chanterelle.Utils.Web3 (attemptWithTimeout)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Int (toNumber)
import Data.Lens (_Just, (%~), (?~), (^.), (^?))
import Data.Map as Map
import Data.Maybe (fromMaybe, isNothing, maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Validation.Semigroup (validation)
import Effect.Aff (Aff, attempt, throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Effect.Ref as Ref
import Foreign.Object as FO
import Network.Ethereum.Web3 (Provider, runWeb3)
import Network.Ethereum.Web3.Api (eth_sendTransaction)
import Network.Ethereum.Web3.Types (NoPay, Web3, Address, HexString, TransactionOptions, TransactionReceipt(..), TransactionStatus(..), _data, _value, convert)

-- | Run an arbitrary deployment script in the DeployM monad
deploy
  :: String
  -> Int
  -> DeployM ~> Aff
deploy url tout deployScript =
  runExceptT (makeProvider url) >>= case _ of
    Left err -> do
      logDeployError err
      liftEffect $ throw "DeployM error"
    Right provider -> do
      deployWithProvider provider tout deployScript

-- | Run an arbitrary deployment script in the DeployM monad against a specified Provider
deployWithProvider
  :: Provider
  -> Int
  -> DeployM ~> Aff
deployWithProvider provider tout deployScript = do
  edeployConfig <- runExceptT $ makeDeployConfigWithProvider provider tout
  case edeployConfig of
    Left err -> logDeployError err *> throwError (error "Error in building DeployConfig!")
    Right deployConfig -> do
      eDeployResult <- runDeployM deployScript deployConfig
      case eDeployResult of
        Left err -> logDeployError err *> throwError (error "Error during deployment!")
        Right a -> pure a

-- | Write updated "networks" object in the solc artifact with a (NetworkId, Address) pair corresponding
-- | to a deployment.
writeNetworkInfo
  :: forall m a
   . MonadAff m
  => MonadThrow String m
  => MonadAsk DeployConfig m
  => LibraryConfig a
  -- contract name/filepath of artifact
  -> NetworkID
  -- network id
  -> NetworkInfo
  -- deployed contract metadata
  -> m Unit
writeNetworkInfo lc nid ni = updateArtifact' lc $
  pure <<< (_network nid ?~ ni)

writeNewBytecode
  :: forall m a
   . MonadAff m
  => MonadThrow String m
  => MonadAsk DeployConfig m
  => LibraryConfig a
  -> NetworkID
  -> ArtifactBytecode
  -> m Unit
writeNewBytecode lc nid (ArtifactBytecode u) =
  writeNetworkInfo lc nid (Undeployed u)

-- | Read the deployment address for a given network id from the solc artifact.
readDeployAddress
  :: forall m a
   . MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff m
  => LibraryConfig a
  -- ^ contract filepath/name
  -> NetworkID
  -- ^ network id
  -> m Address
readDeployAddress lc@{ filepath } nid = withExceptT' ConfigurationError $ do
  artifact <- readArtifact' lc
  let maddress = artifact ^? _network nid <<< _Just <<< _Deployed <<< _Just <<< _address
  maybe (throwError $ "Couldn't find valid deploy address in artifact: " <> filepath) pure maddress

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
  (DeployConfig { timeout, provider }) <- ask
  log Info $ "Polling for " <> name <> " transaction receipt: " <> show txHash
  let txReceiptError err = OnDeploymentError { name, message: "Failed to get transaction receipt: " <> show err }
  TransactionReceipt txReceipt <- do
    eRes <- liftAff $ attempt $ attemptWithTimeout timeout (pollTransactionReceipt txHash provider)
    either (throwError <<< txReceiptError) pure eRes
  if txReceipt.status == Failed || isNothing (txReceipt.contractAddress) then
    let
      message = "Deployment failed to create contract, no address found or status 0x0 in receipt: " <> name
    in
      throwError $ OnDeploymentError { name, message }
  else do
    address <- maybe (throwError $ Impossibility "A contract which has an address also has no address") pure txReceipt.contractAddress
    log Info $ "Contract " <> name <> " deployed to address " <> show address
    pure $ Deployed
      { address
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
getContractBytecode lc@{ filepath } = do
  DeployConfig { networkID } <- ask
  let fullError err = ConfigurationError $ "Couldn't find contract bytecode in artifact " <> filepath <> ": " <> err
  withExceptT' fullError $ do
    artifact <- readArtifact' lc
    let
      networkBytecode = artifact ^? _network networkID <<< _Just <<< _NetworkBytecode
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
deployLibrary txo ccfg@{ name } = do
  nbc@(ArtifactBytecode { bytecode: bc }) <- getContractBytecode ccfg
  case bc of
    BCUnlinked _ -> throwError $ DeployingUnlinkedBytecodeError { name, libs: CBC.unlinkedLibraryNames bc }
    BCLinked { bytecode } -> do
      let
        txo' = txo # _data ?~ bytecode
          # _value %~ map convert
        deploymentAction = eth_sendTransaction txo'
      { deployAddress, deployHash } <- deployContractAndWriteToArtifact ccfg deploymentAction nbc
      pure { deployAddress, deployHash, deployArgs: { libraryName: name, libraryAddress: deployAddress } }

linkLibrary
  :: forall args m
   . MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff m
  => ContractConfig args
  -> Record LibraryMeta
  -> m ArtifactBytecode
linkLibrary ccfg@{ filepath, name } { libraryName, libraryAddress } = do
  (DeployConfig { networkID }) <- ask
  ArtifactBytecode { bytecode: originalBytecode, deployedBytecode: originalDeployedBytecode } <- getContractBytecode ccfg
  bytecode <- link' "construction bytecode" originalBytecode
  deployedBytecode <- link' "on-chain bytecode" originalDeployedBytecode
  let newBytecode = ArtifactBytecode { bytecode, deployedBytecode }
  withExceptT' writeBytecodeError $ writeNewBytecode ccfg networkID newBytecode
  pure newBytecode

  where
  linkError bytecodeKind msg = LinkingError { contractName: name, libraryName, libraryAddress, bytecodeKind, msg }
  writeBytecodeError emsg = LinkingError { contractName: name, libraryName, libraryAddress, bytecodeKind: "bytecode", msg: ("Error while writing new artifact: " <> emsg) }
  link' bytecodeKind bytecode = do
    log Info $ "Linking " <> libraryName <> " at " <> show libraryAddress <> " to the " <> bytecodeKind <> " of " <> name <> " in " <> filepath
    either throwError pure $ (lmap $ linkError bytecodeKind) $ CBC.linkLibrary libraryName libraryAddress bytecode

-- | Deploy a contract using its ContractConfig object.
deployContract
  :: forall args m
   . MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff m
  => TransactionOptions NoPay
  -> ContractConfig args
  -> m (DeployReceipt args)
deployContract txOptions ccfg@{ name, constructor } = do
  nbc@(ArtifactBytecode { bytecode: bc }) <- getContractBytecode ccfg
  case bc of
    BCUnlinked _ -> throwError $ DeployingUnlinkedBytecodeError { name, libs: CBC.unlinkedLibraryNames bc }
    BCLinked { bytecode } -> do
      validatedArgs <- validateDeployArgs ccfg
      let deploymentAction = constructor txOptions bytecode validatedArgs
      { deployAddress, deployHash } <- deployContractAndWriteToArtifact ccfg deploymentAction nbc
      pure { deployAddress, deployArgs: validatedArgs, deployHash }

-- | Helper function which deploys a contract and writes the new contract address to the solc artifact.
deployContractAndWriteToArtifact
  :: forall m a
   . MonadThrow DeployError m
  => MonadAsk DeployConfig m
  => MonadAff m
  => LibraryConfig a
  -> Web3 HexString
  -- ^ deploy action returning txHash
  -> ArtifactBytecode
  -- ^ ArtifactBytecode being deployed
  -> m { deployAddress :: Address, deployHash :: HexString }
deployContractAndWriteToArtifact lc@{ filepath, name } deployAction nbc = do
  (DeployConfig { provider, networkID }) <- ask
  log Info $ "Deploying contract " <> name
  deployHash <- do
    eRes <- liftAff $ runWeb3 provider deployAction
    either (throwError <<< onDeploymentError) pure eRes
  networkInfo <- getPublishedContractDeployInfo deployHash name nbc
  deployAddress <- maybe (throwError $ Impossibility "A published contract did not have a deploy address") pure
    (networkInfo ^? _Deployed <<< _Just <<< _address)
  withExceptT' postDeploymentError $ writeNetworkInfo lc networkID networkInfo
  pure { deployAddress, deployHash }
  where
  onDeploymentError err = OnDeploymentError { name, message: "Web3 error while deploying contract: " <> show err }
  postDeploymentError err = PostDeploymentError { name, message: "Failed to update deployed address in artifact at " <> filepath <> ": " <> show err }

readArtifact'
  :: forall m a
   . MonadAsk DeployConfig m
  => MonadThrow String m
  => MonadAff m
  => LibraryConfig a
  -> m Artifact
readArtifact' lc@{ name, filepath } = do
  DeployConfig { artifactCache } <- ask
  cacheVar <- liftEffect $ Ref.read artifactCache
  maybe (loadArtifact' lc) pure $ Map.lookup { name, filepath } cacheVar

loadArtifact'
  :: forall m a
   . MonadAsk DeployConfig m
  => MonadThrow String m
  => MonadAff m
  => LibraryConfig a
  -> m Artifact
loadArtifact' { name, filepath } = do
  DeployConfig { artifactCache, ignoreNetworksInArtifact } <- ask
  cacheVar <- liftEffect $ Ref.read artifactCache
  loadedArtifact@(Artifact la) <- Artifact.readArtifact filepath
  let
    usedArtifact =
      if ignoreNetworksInArtifact then Artifact $ la { networks = FO.empty }
      else loadedArtifact
  liftEffect $ flip Ref.write artifactCache $ Map.insert { name, filepath } usedArtifact cacheVar
  pure usedArtifact

writeArtifact'
  :: forall m a
   . MonadAsk DeployConfig m
  => MonadThrow String m
  => MonadAff m
  => LibraryConfig a
  -> Artifact
  -> m Unit
writeArtifact' { name, filepath } artifact = do
  DeployConfig { artifactCache, writeArtifacts } <- ask
  cacheVar <- liftEffect $ Ref.read artifactCache
  let newCache = Map.insert { name, filepath } artifact cacheVar
  liftEffect $ Ref.write newCache artifactCache
  when writeArtifacts $ Artifact.writeArtifact filepath artifact

updateArtifact'
  :: forall m a
   . MonadAsk DeployConfig m
  => MonadThrow String m
  => MonadAff m
  => LibraryConfig a
  -> (Artifact -> m Artifact)
  -> m Unit
updateArtifact' lc action = readArtifact' lc >>= action >>= writeArtifact' lc

makeDeployConfig
  :: forall m
   . MonadAff m
  => MonadThrow DeployError m
  => String
  -> Int
  -> m DeployConfig
makeDeployConfig url tout = do
  provider <- makeProvider url
  makeDeployConfigWithProvider provider tout

makeDeployConfigWithProvider
  :: forall m
   . MonadAff m
  => MonadThrow DeployError m
  => Provider
  -> Int
  -> m DeployConfig
makeDeployConfigWithProvider provider tout =
  let
    timeout = Milliseconds (toNumber tout)
    toError = ConfigurationError <<< append "Couldn't create DeployConfig: " <<< show
  in
    do
      eRes <- liftAff $ runWeb3 provider do
        primaryAccount <- getPrimaryAccount
        networkID <- getNetworkID
        artifactCache <- liftEffect $ Ref.new Map.empty
        pure $ DeployConfig { provider, primaryAccount, networkID, timeout, ignoreNetworksInArtifact: false, writeArtifacts: true, artifactCache }
      either (throwError <<< toError) pure eRes

validateDeployArgs
  :: forall m args
   . MonadThrow DeployError m
  => ContractConfig args
  -> m (Record args)
validateDeployArgs cfg =
  let
    onErr msg = throwError $ ConfigurationError ("Couldn't validate args for contract deployment " <> cfg.name <> ": " <> show msg)
    onSucc = pure
  in
    validation onErr onSucc cfg.unvalidatedArgs
