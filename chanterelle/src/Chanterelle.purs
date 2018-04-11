module Chanterelle where

import Prelude

import Chanterelle.Compile (compileProject)
import Chanterelle.Internal.Deploy (deploy)
import Chanterelle.Internal.Logging (setLogLevel, readLogLevel)
import Chanterelle.Internal.Types (DeployM)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Network.Ethereum.Web3 (ETH)
import Node.FS.Aff (FS)
import Node.Process (PROCESS)
import Node.Yargs.Applicative (yarg, runY)
import Node.Yargs.Setup (usage, defaultVersion, defaultHelp, example)

main
  :: forall eff.
     Eff (console :: CONSOLE, fs :: FS, process :: PROCESS, exception :: EXCEPTION | eff) Unit
main =
    let setup = usage "$0 --log-level <level>"
             <> example "$0 --log-level debug" "Run the compile phase with the given log level."
             <> defaultVersion
             <> defaultHelp
    in runY setup $ go <$> yarg "log-level" [] Nothing (Left "info") false
  where
    go level = do
      setLogLevel $ readLogLevel level
      compileProject

deployMain
  :: forall eff a.
     DeployM eff a
  -> Eff (console :: CONSOLE, fs :: FS, eth :: ETH, exception :: EXCEPTION | eff) Unit
deployMain deployScript =
    let setup = usage "$0 --log-level <level> --node-url <url> --timeout <seconds>"
             <> example "$0 --log-level debug" "Run the deployment script with the given log level, node url, and timeout"
             <> defaultVersion
             <> defaultHelp
    in runY setup $ go <$> yarg "log-level" [] Nothing (Left "info") false
                       <*> yarg "node-url" [] Nothing (Left "http://localhost:8545") false
                       <*> yarg "timeout" [] Nothing (Left 60) false
  where
    go level nodeUrl timeout = unsafeCoerceEff $ do
      setLogLevel $ readLogLevel level
      deploy nodeUrl timeout deployScript
