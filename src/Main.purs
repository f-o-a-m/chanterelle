module Main where

import Prelude

import Chanterelle.Internal.Logging (setLogLevel, readLogLevel)
import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Node.FS.Aff (FS)
import Node.Process (PROCESS, cwd)
import Node.Yargs.Applicative (rest, runY, yarg)
import Node.Yargs.Setup (defaultVersion, defaultHelp, example, usage)
import Unsafe.Coerce (unsafeCoerce)

main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION, fs :: FS, process :: PROCESS | e) Unit
main = do
  ourCwd <- cwd
  let setup =  usage "chanterelle [-v <level>] ACTION"
            <> example "chanterelle -v debug compile" "Run the compile phase with debug logging enabled."
            <> example "chanterelle -r .. deploy" "Run the deploy phase against the chanterelle project in the parent directory."
            <> defaultVersion
            <> defaultHelp
      verbosityArg = yarg "verbosity" ["v"] (Just "The level of logging") (Left "info") false
      rootArg      = yarg "project-root" ["r"] (Just "Override the default project root") (Left ourCwd) false
      go level root actions = do
        setLogLevel $ readLogLevel level
        launchAff_ $ log (unsafeCoerce actions)
        pure unit

  runY setup $ go <$> verbosityArg <*> rootArg <*> rest