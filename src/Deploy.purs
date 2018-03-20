module Deploy
  ( defaultDeployContract
  , writeDeployAddress
  ) where

import Prelude
import Control.Error.Util ((??))
import Control.Monad.Aff (Aff, Milliseconds(..), liftEff', attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Console as C
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.Eff.Exception (throw, error)
import Data.Argonaut (stringify, _Object, _String, jsonEmptyObject, (~>), (:=))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Lens ((^?), (?~), (.~))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import Network.Ethereum.Web3 (ETH, Web3, Address, BigNumber, HexString, mkHexString, defaultTransactionOptions, _from, _data, fromWei, _value, runWeb3)
import Network.Ethereum.Web3.Api (eth_sendTransaction)
import Data.Newtype (unwrap)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (FS, readTextFile, writeTextFile)
import Node.Path (FilePath)
import Utils (DeployConfig, withTimeout, pollTransactionReceipt)

-- | Fetch the bytecode from a solidity build artifact
getBytecode
  :: forall eff.
     FilePath
  -- ^ filename of contract artifact
  -> Aff (fs :: FS | eff) (Either String HexString)
getBytecode filename = runExceptT $ do
  artifact <- ExceptT $ jsonParser <$> readTextFile UTF8 filename
  bytecode <- (artifact ^? _Object <<< ix "bytecode" <<< _String) ?? "artifact missing 'bytecode' field."
  mkHexString bytecode ?? "bytecode not a valid hex string"

-- | Publish a contract based on the bytecode. Used for contracts with no constructor.
defaultPublishContract
  :: forall eff.
     FilePath
  -- filename of contract artifact
  -> Address
  -- deploy from address
  -> Web3 (fs :: FS, console :: CONSOLE | eff) HexString
defaultPublishContract filename primaryAccount = do
  ebytecode <- liftAff $ getBytecode filename
  case ebytecode of
    Left err -> do
      liftAff $ C.error err
      liftAff <<< liftEff' $ throw err
    Right bytecode -> do
      let txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                             # _data ?~ bytecode
                                             # _value ?~ fromWei zero
      liftAff $ C.log $ "Deploying contract for " <> filename
      eth_sendTransaction txOpts

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
      networkObj = show nid := networkIdObj ~> jsonEmptyObject
      artifactWithAddress = artifact # _Object <<< ix "networks" .~ networkObj
  liftAff $ writeTextFile UTF8 filename $ stringify artifactWithAddress

defaultDeployContract
  :: forall eff.
     DeployConfig
  -> FilePath
  -> Aff (eth :: ETH, console :: CONSOLE, fs :: FS | eff) Unit
defaultDeployContract {provider, networkId, primaryAccount} filename = do
  econtractAddr <- runWeb3 provider $ do
    txHash <- defaultPublishContract filename primaryAccount
    liftAff $ C.log $ "Polling for TransactionReceipt: " <> show txHash
    etxReceipt <- liftAff <<< attempt $ withTimeout (Milliseconds $ 90.0 * 1000.0) (pollTransactionReceipt txHash provider)
    case unNullOrUndefined <<< _.contractAddress <<< unwrap <$> etxReceipt of
      Left err -> do
        liftAff $ C.error $ "No Transaction Receipt found for deployemt : " <> filename <> " : " <> show txHash
        liftAff $ throwError err
      Right Nothing -> do
        let timeoutErrMsg = "Didn't find contract address in TransactionReceipt for deployment: " <> filename
        liftAff $ C.error timeoutErrMsg
        liftAff $ liftEff' $ throw timeoutErrMsg
      Right (Just contractAddress) -> do
        liftAff <<< C.log $ "Contract " <> filename <> " deployed to address " <> show contractAddress
        pure contractAddress
  case econtractAddr of
    Left err -> do
      C.error $ "Web3 error during contract deployment for " <> show filename <> " : " <> show err
      liftEff' <<< throw $ show err
    Right contractAddr -> do
      eRes <- writeDeployAddress filename contractAddr networkId
      case eRes of
        Left err -> do
          C.error $ "Failed to write deploy address to artifact " <> show filename <> " : " <> err
          throwError $ error err
        Right _ -> pure unit
