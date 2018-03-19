module Deploy
  ( defaultDeployContract
  , writeDeployAddress
  ) where

import Prelude
import Control.Error.Util ((??))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Data.Argonaut (stringify, _Object, _String, jsonEmptyObject, (~>), (:=))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Lens ((^?), (?~), (.~))
import Data.Lens.Index (ix)
import Network.Ethereum.Web3 (Web3, Address, BigNumber, HexString, mkHexString, defaultTransactionOptions, _from, _data, fromWei, _value)
import Network.Ethereum.Web3.Api (eth_sendTransaction)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (FS, readTextFile, writeTextFile)
import Node.Path (FilePath)

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

-- | Deploy a contract based on the bytecode. Used for contracts with no constructor.
defaultDeployContract
  :: forall eff.
     FilePath
  -- filename of contract artifact
  -> Address
  -- deploy from address
  -> Web3 (fs :: FS, exception :: EXCEPTION | eff) HexString
defaultDeployContract filename primaryAccount = do
  ebytecode <- liftAff $ getBytecode filename
  case ebytecode of
    Left err -> liftEff $ throw err
    Right bytecode ->
      let txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                             # _data ?~ bytecode
                                             # _value ?~ fromWei zero
      in eth_sendTransaction txOpts

-- | Write the "network object" for a given deployment on a network with
-- | the given id.
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
