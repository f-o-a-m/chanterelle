module Chanterelle where

import Prelude
import Chanterelle.Compile (compileProject)
import Chanterelle.Internal.Logging (setLogLevel, readLogLevel)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Node.Yargs.Applicative (yarg, runY)
import Node.FS.Aff (FS)
import Node.Process (PROCESS)
import Control.Monad.Aff.Console (CONSOLE)
import Node.Yargs.Setup (usage, defaultVersion, defaultHelp, example)

main
  :: forall eff.
     Eff (console :: CONSOLE, fs :: FS, process :: PROCESS, exception :: EXCEPTION | eff) Unit
main =
    let setup = usage "$0 --log-level <level>"
             <> example "$0 --log-level debug" "Set the log level, e.g. debug, info, warn, error."
             <> defaultVersion
             <> defaultHelp
    in runY setup $ go <$> yarg "log-level" [] Nothing (Left "info") false
  where
    go level = do
      setLogLevel $ readLogLevel level
      compileProject
