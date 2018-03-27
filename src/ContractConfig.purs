module ContractConfig
  ( simpleStorageConfig
  , foamCSRConfig
  , makeParkingAuthorityConfig
  ) where

import Prelude
import Data.Maybe (Maybe)
import Network.Ethereum.Web3.Types (Address, embed)
import Network.Ethereum.Web3.Solidity (type (:&), UIntN, D2, D5, D6, uIntNFromBigNumber)

import Types (ContractConfig)

--------------------------------------------------------------------------------
-- | SimpleStorage
--------------------------------------------------------------------------------

simpleStorageConfig
  :: ContractConfig (deployArgs :: Maybe {_count :: UIntN (D2 :& D5 :& D6)})
simpleStorageConfig =
    { filepath : "./build/contracts/SimpleStorage.json"
    , deployArgs : simpleStorageArgs
    , name : "SimpleStorage"
    }
  where
    simpleStorageArgs = do
      _count <- uIntNFromBigNumber $ embed 1234
      pure {_count}

--------------------------------------------------------------------------------
-- | FoamCSR
--------------------------------------------------------------------------------

foamCSRConfig
  :: ContractConfig ()
foamCSRConfig =
  { filepath : "./build/contracts/FoamCSR.json"
  , name : "FoamCSR"
  }

--------------------------------------------------------------------------------
-- | ParkingAuthority
--------------------------------------------------------------------------------

makeParkingAuthorityConfig
  :: {foamCSR :: Address}
  -> ContractConfig (deployArgs :: {foamCSR :: Address})
makeParkingAuthorityConfig addressR =
  { filepath : "./build/contracts/ParkingAuthority.json"
  , deployArgs : addressR
  , name : "ParkingAuthority"
  }
