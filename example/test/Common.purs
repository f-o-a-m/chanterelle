module Test.Common where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Reflectable (class Reflectable)
import Network.Ethereum.Core.BigNumber (BigNumber)
import Network.Ethereum.Web3 (Address, Provider, UIntN, uIntNFromBigNumber)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy)

type DeploySpecConfig r =
  { provider :: Provider
  , accounts :: Array Address
  | r
  }

unsafeToUInt
  :: forall n
   . Reflectable n Int
  => Proxy n
  -> BigNumber
  -> UIntN n
unsafeToUInt p n =
  case uIntNFromBigNumber p n of
    Nothing -> unsafeCrashWith $ "Failed to parse as uint: " <> show n
    Just uint -> uint
