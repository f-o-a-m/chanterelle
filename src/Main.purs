module Main where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Web3 (ETH, defaultTransactionOptions, _from, _gas)
import Network.Ethereum.Web3.Types.BigNumber (parseBigNumber, decimal)
import Node.FS.Aff (FS)
import Partial.Unsafe (unsafePartial)

import Contracts.SimpleStorage as SimpleStorage

import Deploy (deployContractWithArgs)
import Utils (makeDeployConfig)
import ContractConfig (simpleStorageConfig)

main :: forall e. Eff (eth :: ETH, console :: CONSOLE, fs :: FS | e) Unit
main = void <<< launchAff $ do
  cfg <- makeDeployConfig
  let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "900000"
      txOpts = defaultTransactionOptions # _from ?~ cfg.primaryAccount
                                         # _gas ?~ bigGasLimit
  _ <- deployContractWithArgs cfg simpleStorageConfig $ SimpleStorage.constructor txOpts
  pure unit
