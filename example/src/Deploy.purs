module Deploy
  ( DeployResults
  , deployContracts
  ) where

import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Types.Deploy (ContractConfig, DeployConfig(..), DeployM, validateWithError)
import Contract.SimplePaidStorage as SimplePaidStorage
import Contract.Token as Token
import Control.Monad.Reader (ask)
import Data.Lens ((?~))
import Network.Ethereum.Core.BigNumber (fromInt)
import Network.Ethereum.Web3 (Address, UIntN, _from, defaultTransactionOptions, uIntNFromBigNumber)
import Type.Proxy (Proxy(..))

-- Deployment Results captures the values which were created/used at deployment. 
type DeployResults =
  { token :: Address
  , tokenOwner :: Address
  , simplePaidStorage :: Address
  , simplePaidStorageOwner :: Address
  }

-- This is our Deploy Script. We return a value of type `DeployResults` to facilitate with testing,
-- or potentially write the deployment data to a file for future use.
deployContracts :: DeployM DeployResults
deployContracts = do
  (DeployConfig { primaryAccount }) <- ask
  let txOpts = defaultTransactionOptions # _from ?~ primaryAccount
  { deployAddress: tokenAddress } <- deployContract txOpts tokenCfg
  { deployAddress: simplePaidStorage } <- deployContract txOpts $ simplePaidStorageCfg { tokenAddress }
  pure
    { token: tokenAddress
    , tokenOwner: primaryAccount
    , simplePaidStorage
    , simplePaidStorageOwner: primaryAccount
    }

-- This is the minimal configuration needed to deploy the Token contract. Notice
-- that since the contract requires arguments to deploy, we need to submit a parser
-- to the unvalidatedArgs field.
tokenCfg :: ContractConfig (initialSupply :: UIntN 256)
tokenCfg =
  { filepath: "build/contracts/Token.json"
  , name: "Token"
  , constructor: Token.constructor
  , unvalidatedArgs: flip validateWithError "Failed to parse as uint" $ do
      n <- uIntNFromBigNumber (Proxy @256) $ fromInt 1_000_000
      pure { initialSupply: n }
  }

-- This is the minimal configuration needed to deploy the SimplePaidStorage contract. 
-- The contract needs the Token address for its constructor, which we will pass after
-- deploying the Token contract.
simplePaidStorageCfg
  :: { tokenAddress :: Address }
  -> ContractConfig (tokenAddress :: Address)
simplePaidStorageCfg args =
  { filepath: "build/contracts/SimplePaidStorage.json"
  , name: "SimplePaidStorage"
  , constructor: SimplePaidStorage.constructor
  , unvalidatedArgs: pure args
  }
