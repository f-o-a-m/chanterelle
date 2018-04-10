module ContractConfig
  ( simpleStorageConfig
  , foamCSRConfig
  , makeParkingAuthorityConfig
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V, invalid)
import Network.Ethereum.Web3.Types (Address, embed)
import Network.Ethereum.Web3.Solidity (type (:&), UIntN, D2, D5, D6, uIntNFromBigNumber)

import Contracts.SimpleStorage as SimpleStorage
import Contracts.ParkingAuthority as ParkingAuthority
import Chanterelle.Internal.Types (ContractConfig, NoArgs, noArgs, constructorNoArgs)

--------------------------------------------------------------------------------
-- | SimpleStorage
--------------------------------------------------------------------------------

simpleStorageConfig
  :: ContractConfig (_count :: UIntN (D2 :& D5 :& D6))
simpleStorageConfig =
    { filepath : "./build/contracts/SimpleStorage.json"
    , name : "SimpleStorage"
    , constructor : SimpleStorage.constructor
    , unvalidatedArgs : {_count: _} <$> validCount
    }
  where
    validCount = uIntNFromBigNumber (embed 1234) ?? "SimpleStorage: _count must be valid uint"

--------------------------------------------------------------------------------
-- | FoamCSR
--------------------------------------------------------------------------------

foamCSRConfig
  :: ContractConfig NoArgs
foamCSRConfig =
  { filepath : "./build/contracts/FoamCSR.json"
  , name : "FoamCSR"
  , constructor : constructorNoArgs
  , unvalidatedArgs : noArgs
  }

--------------------------------------------------------------------------------
-- | ParkingAuthority
--------------------------------------------------------------------------------

makeParkingAuthorityConfig
  :: {foamCSR :: Address}
  -> ContractConfig (foamCSR :: Address)
makeParkingAuthorityConfig addressR =
  { filepath : "./build/contracts/ParkingAuthority.json"
  , name : "ParkingAuthority"
  , constructor : ParkingAuthority.constructor
  , unvalidatedArgs : pure addressR
  }


validateWithError :: forall a. Maybe a -> String -> V (Array String) a
validateWithError mres msg = case mres of
  Nothing -> invalid [msg]
  Just res -> pure res

infixl 9 validateWithError as ??
