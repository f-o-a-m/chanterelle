module Main where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Network.Ethereum.Web3 (ETH)
import Node.FS.Aff (FS)
import Node.Path (FilePath)

import Deploy (defaultDeployContract)
import Utils (makeDeployConfig)

main :: forall e. Eff (eth :: ETH, console :: CONSOLE, fs :: FS | e) Unit
main = void <<< launchAff $ do
  cfg <- makeDeployConfig
  defaultDeployContract cfg simpleStorageFP


simpleStorageFP :: FilePath
simpleStorageFP = "./build/contracts/SimpleStorage.json"
