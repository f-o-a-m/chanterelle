module ContractConfig
  ( simpleStorageConfig
  ) where

import Prelude
import Data.Maybe (Maybe)
import Network.Ethereum.Web3.Types (embed)
import Network.Ethereum.Web3.Solidity (type (:&), UIntN, D2, D5, D6, uIntNFromBigNumber)

import Types (ContractConfig)

--------------------------------------------------------------------------------
-- | SimpleStorage
--------------------------------------------------------------------------------

simpleStorageConfig
  :: (ContractConfig (deployArgs :: Maybe {_count :: UIntN (D2 :& D5 :& D6)}))
simpleStorageConfig =
    { filepath : "./build/contracts/SimpleStorage.json"
    , deployArgs : simpleStorageArgs
    , name : "SimpleStorage"
    }
  where
    simpleStorageArgs = do
      _count <- uIntNFromBigNumber $ embed 12345
      pure {_count}
