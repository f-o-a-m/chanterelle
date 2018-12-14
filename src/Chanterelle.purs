module Chanterelle
  ( compileMain
  , deployMain
  ) where

import Prelude

import Chanterelle.Compile (compileProject)
import Chanterelle.Deploy (deploy)
import Chanterelle.Internal.Logging (setLogLevel, readLogLevel)
import Chanterelle.Internal.Types.Deploy (DeployM)
import Effect (Effect)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Node.Yargs.Applicative (yarg, runY)
import Node.Yargs.Setup (usage, defaultVersion, defaultHelp, example)

compileMain
  :: Effect Unit
compileMain =
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
  :: forall a.
     DeployM a
  -> Effect Unit
deployMain deployScript =
    let setup = usage "$0 --log-level <level> --node-url <url> --timeout <seconds>"
             <> example "$0 --log-level debug" "Run the deployment script with the given log level, node url, and timeout"
             <> defaultVersion
             <> defaultHelp
    in runY setup $ go <$> yarg "log-level" [] Nothing (Left "info") false
                       <*> yarg "node-url" [] Nothing (Left "http://localhost:8545") false
                       <*> yarg "timeout" [] Nothing (Left 60) false
  where
    go level nodeUrl timeout = do
      setLogLevel $ readLogLevel level
      deploy nodeUrl timeout deployScript
