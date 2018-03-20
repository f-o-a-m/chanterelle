module Main where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Lens ((?~))
import Network.Ethereum.Web3 (ETH, defaultTransactionOptions, _from)
import Node.FS.Aff (FS)

import Contracts.SimpleStorage as SimpleStorage

import Deploy (deployContractWithArgs)
import Utils (makeDeployConfig)
import ContractConfig (simpleStorageConfig)

main :: forall e. Eff (eth :: ETH, console :: CONSOLE, fs :: FS | e) Unit
main = void <<< launchAff $ do
  cfg <- makeDeployConfig
  let txOpts = defaultTransactionOptions # _from ?~ cfg.primaryAccount
  _ <- deployContractWithArgs cfg simpleStorageConfig $ SimpleStorage.constructor txOpts
  pure unit
