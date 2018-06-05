module Main where

import Prelude

import Chanterelle.Internal.Codegen (generatePS) as Chanterelle
import Chanterelle.Internal.Compile (compile) as Chanterelle
import Chanterelle.Internal.Logging (LogLevel(..), log, logCompileError, readLogLevel, setLogLevel)
import Chanterelle.Internal.Types (runCompileM)
import Chanterelle.Internal.Types.Project (ChanterelleProject)
import Chanterelle.Project (loadProject)
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Error.Class (try)
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (toLower)
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
      go level root actions = launchAff_ do
        liftEff $ setLogLevel (readLogLevel level)
        projE <- try $ loadProject root
        case projE of
          Left err -> log Error ("Couldn't parse chanterelle.json: " <> show err)
          Right project -> runAction project (unsafeCoerce actions)

  runY setup $ go <$> verbosityArg <*> rootArg <*> rest

data RunnableAction = ClassicBuild | Compile | Codegen | Genesis | UnknownAction String

runAction :: forall e. ChanterelleProject -> Array String -> Aff (console :: CONSOLE, fs :: FS, process :: PROCESS | e) Unit
runAction project actions = do
  log Info "Loaded chanterelle.json successfully!"
  case uncons actions of
    Nothing -> log Warn "Nothing to do!"
    Just {head, tail} -> case normalizeAction (toLower head) of
        ClassicBuild -> doClassicBuild
        Compile -> doCompile
        Codegen -> doCodegen
        Genesis -> case uncons tail of
            Nothing -> log Error "Usage: chanterelle genesis INPUT_GENESIS.json"
            Just u -> doGenesis u.head
        UnknownAction s -> log Error $ "Don't know how to do " <> s

  where doClassicBuild = doCompile *> doCodegen
        doCompile = do
          eres <- runCompileM Chanterelle.compile project
          case eres of
            Left err -> logCompileError err
            Right _ -> pure unit
        doCodegen = do
          eres <- runCompileM Chanterelle.generatePS project
          case eres of
            Left err -> logCompileError err
            Right _ -> pure unit
        doGenesis inputFile = log Info $ "genesising " <> inputFile

normalizeAction :: String -> RunnableAction
normalizeAction "b" = ClassicBuild
normalizeAction "build" = ClassicBuild
normalizeAction "c" = Compile
normalizeAction "compile" = Compile
normalizeAction "ps" = Codegen
normalizeAction "purs" = Codegen
normalizeAction "purescript" = Codegen
normalizeAction "g" = Genesis
normalizeAction "genesis" = Genesis
-- normalizeAction "i" = Install
-- normalizeAction "install" = Install
normalizeAction a = UnknownAction a