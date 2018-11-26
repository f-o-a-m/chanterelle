module ChanterelleMain where

import Prelude

import Chanterelle.Internal.Codegen (generatePS) as Chanterelle
import Chanterelle.Internal.Compile (compile) as Chanterelle
import Chanterelle.Internal.Genesis (generateGenesis)
import Chanterelle.Internal.Logging (LogLevel(..), log, logCompileError, logGenesisGenerationError, readLogLevel, setLogLevel)
import Chanterelle.Internal.Types (runCompileM)
import Chanterelle.Internal.Types.Project (ChanterelleProject)
import Chanterelle.Internal.Utils (jsonStringifyWithSpaces)
import Chanterelle.Project (loadProject)
import Effect.Aff (Aff, launchAff_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Monad.Error.Class (try)
import Data.Argonaut as A
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.Path (resolve)
import Node.Process (cwd)
import Node.Yargs.Applicative (rest, runY, yarg)
import Node.Yargs.Setup (defaultVersion, defaultHelp, example, usage)
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
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
        liftEffect $ setLogLevel (readLogLevel level)
        resolvedRoot <- liftEffect $ resolve [ourCwd] root
        projE <- try $ loadProject resolvedRoot
        case projE of
          Left err -> log Error ("Couldn't parse chanterelle.json: " <> show err)
          Right project -> runAction project (unsafeCoerce actions)

  runY setup $ go <$> verbosityArg <*> rootArg <*> rest

data RunnableAction = ClassicBuild | Compile | Codegen | Genesis | UnknownAction String

runAction :: ChanterelleProject -> Array String -> Aff Unit
runAction project actions = do
  log Info "Loaded chanterelle.json successfully!"
  case uncons actions of
    Nothing -> log Warn "Nothing to do!"
    Just {head, tail} -> case normalizeAction (toLower head) of
        ClassicBuild -> doClassicBuild
        Compile -> doCompile
        Codegen -> doCodegen
        Genesis -> case uncons tail of -- todo: this is beyond fucking foul
            Nothing -> log Error "Usage: chanterelle genesis INPUT_GENESIS.json OUTPUT_GENESIS.json"
            Just arg1 -> case uncons arg1.tail of
              Nothing -> log Error "Usage: chanterelle genesis INPUT_GENESIS.json OUTPUT_GENESIS.json"
              Just arg2 -> doGenesis arg1.head arg2.head
        UnknownAction s -> log Error $ "Don't know how to do " <> s

  where doClassicBuild = doCompile *> doCodegen
        doCompile = runCompileM Chanterelle.compile project >>= case _ of
            Left err -> logCompileError err
            Right _ -> pure unit
        doCodegen = runCompileM Chanterelle.generatePS project >>= case _ of
            Left err -> logCompileError err
            Right _ -> pure unit
        doGenesis inputFile outputFile = generateGenesis project inputFile >>= case _ of
            Left err -> logGenesisGenerationError err
            Right gb -> do
              let strungGb = jsonStringifyWithSpaces 4 (A.encodeJson gb)
              try (writeTextFile UTF8 outputFile strungGb) >>= case _ of
                Left err -> log Error $ "Couldn't write genesis block to " <> show outputFile <> ": " <> show err
                Right _  -> log Info $ "Successfully wrote generated genesis block to " <> show outputFile

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
