module Types
  ( DeployConfig
  , ContractConfig
  ) where

import Network.Ethereum.Web3.Types (Address, BigNumber)
import Network.Ethereum.Web3.Types.Provider (Provider)
import Node.Path (FilePath)


-- | primary deployment configuration
type DeployConfig =
  { networkId :: BigNumber
  , primaryAccount :: Address
  , provider :: Provider
  }

-- | configuration for deployment of a single contract
type ContractConfig args =
  { filepath :: FilePath
  , name :: String
  | args
  }
