module ChanterelleMain where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Ref (REF)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Node.Process (PROCESS)
import Parser.Opts (Error(Error), OptInfo, Option, mkOpt, runOpts)
import Data.Choice (Choice(..), (<<|>>))
import Parser.Combinators.Array as OA

data Input
  = Compile Boolean
  | New String Boolean
  | Init Boolean
  | Deploy String Boolean
  | Test Boolean
  | Install String Boolean
  | Help

instance showInput :: Show Input where
  show (Compile help) = "Compile {help = " <> show help <> "}"
  show (New src help) = "New {src = " <> src <> ", help = " <> show help <> "}"
  show (Init help) = "Init {help = " <> show help <> "}"
  show (Deploy src help) = "Deploy {src = " <> src <> ", help = " <> show help <> "}"
  show (Test help) = "Test {help = " <> show help <> "}"
  show (Install src help) = "Install {src = " <> src <> ", help = " <> show help <> "}"
  show Help = "Help"

noArg :: ∀ eff. (Boolean -> Input) -> OptInfo -> Option eff Input
noArg ctor info = mkOpt info parser
  where
  parser =
    (\_ help -> ctor help) <$>
    OA.element info.name <*>
    (true <$ OA.element "--help" <|> pure false)

oneArg :: ∀ eff. (String -> Boolean -> Input) -> OptInfo -> Option eff Input
oneArg ctor info = mkOpt info parser
  where
  parser =
    (\_ arg help ->
      if arg == "--help"
        then ctor "" true
        else ctor arg help
    ) <$>
    OA.element info.name <*>
    OA.uncons <*>
    (true <$ OA.element "--help" <|> pure false)

compile :: ∀ eff. Option eff Input
compile = noArg Compile {name, description}
  where
  name = "compile"
  description = "Run the compiler."

new :: ∀ eff. Option eff Input
new = oneArg New {name, description}
  where
  name = "new"
  description = "Make a new thing"

init :: ∀ eff. Option eff Input
init = noArg Init {name, description} where
  name = "init"
  description = "Init the thing"

deploy :: ∀ eff. Option eff Input
deploy = oneArg Deploy {name, description} where
  name = "deploy"
  description = "Deploy the thing"

test :: ∀ eff. Option eff Input
test = noArg Test {name, description} where
  name = "test"
  description = "Test the thing"

install :: ∀ eff. Option eff Input
install = oneArg Install {name, description} where
  name = "install"
  description = "Install the thing"

help :: ∀ eff. Option eff Input
help = mkOpt {name, description} parser
  where
  name = "--help"
  description = "get help"
  parser = Help <$ OA.element name

main :: ∀ eff. Eff (ref :: REF, process :: PROCESS, console :: CONSOLE | eff) Unit
main =
  runOpts opts case _ of
    Left errors -> traverse_ (\(Error err) -> Console.log err) errors
    Right a -> Console.logShow a
  where
    opts =   compile
       <<|>> new
       <<|>> init
       <<|>> deploy
       <<|>> test
       <<|>> install
       <<|>> Finally help


-- import Chanterelle.Internal.Codegen (generatePS) as Chanterelle
-- import Chanterelle.Internal.Compile (compile) as Chanterelle
-- import Chanterelle.Internal.Genesis (generateGenesis)
-- import Chanterelle.Internal.Logging (LogLevel(..), log, logCompileError, logGenesisGenerationError, readLogLevel, setLogLevel)
-- import Chanterelle.Internal.Types (runCompileM)
-- import Chanterelle.Internal.Types.Project (ChanterelleProject)
-- import Chanterelle.Internal.Utils (jsonStringifyWithSpaces)
-- import Chanterelle.Project (loadProject)
-- import Control.Monad.Aff (Aff, launchAff_)
-- import Control.Monad.Aff.Console (CONSOLE)
-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Class (liftEff)
-- import Control.Monad.Eff.Console as Console
-- import Control.Monad.Eff.Exception (EXCEPTION)
-- import Control.Monad.Error.Class (try)
-- import Data.Argonaut as A
-- import Data.Array (fold, uncons)
-- import Data.Either (Either(..))
-- import Data.Maybe (Maybe(..))
-- import Data.String (toLower)
-- import Network.Ethereum.Web3 (ETH)
-- import Node.Encoding (Encoding(..))
-- import Node.FS.Aff (FS, writeTextFile)
-- import Node.Path (resolve)
-- import Node.Process (PROCESS, cwd)
-- import Node.Yargs.Applicative (rest, runY, yarg)
-- import Node.Yargs.Setup (defaultVersion, defaultHelp, example, usage)

-- main :: forall e. Eff (console :: CONSOLE, eth :: ETH, exception :: EXCEPTION, fs :: FS, process :: PROCESS | e) Unit
-- main = do
--   ourCwd <- cwd
--   let setup =  usage "chanterelle [-v <level>] ACTION"
--             <> example "chanterelle -v debug compile" "Run the compile phase with debug logging enabled."
--             <> example "chanterelle -r .. deploy" "Run the deploy phase against the chanterelle project in the parent directory."
--             <> defaultVersion
--             <> defaultHelp
--       verbosityArg = yarg "verbosity" ["v"] (Just "The level of logging") (Left "info") false
--       rootArg      = yarg "project-root" ["r"] (Just "Override the default project root") (Left ourCwd) false
--       go level root actions = launchAff_ do
--         liftEff $ setLogLevel (readLogLevel level)
--         let resolvedRoot = resolve [ourCwd] root
--         projE <- try $ loadProject resolvedRoot
--         case projE of
--           Left err -> log Error ("Couldn't parse chanterelle.json: " <> show err)
--           Right project -> runAction project (unsafeCoerce actions)

--   runY setup $ go <$> verbosityArg <*> rootArg <*> rest

-- data RunnableAction = ClassicBuild | Compile | Codegen | Genesis | UnknownAction String

-- runAction :: forall e. ChanterelleProject -> Array String -> Aff (console :: CONSOLE, eth :: ETH, fs :: FS, process :: PROCESS | e) Unit
-- runAction project actions = do
--   log Info "Loaded chanterelle.json successfully!"
--   case uncons actions of
--     Nothing -> log Warn "Nothing to do!"
--     Just {head, tail} -> case normalizeAction (toLower head) of
--         ClassicBuild -> doClassicBuild
--         Compile -> doCompile
--         Codegen -> doCodegen
--         Genesis -> case uncons tail of -- todo: this is beyond fucking foul
--             Nothing -> log Error "Usage: chanterelle genesis INPUT_GENESIS.json OUTPUT_GENESIS.json"
--             Just arg1 -> case uncons arg1.tail of
--               Nothing -> log Error "Usage: chanterelle genesis INPUT_GENESIS.json OUTPUT_GENESIS.json"
--               Just arg2 -> doGenesis arg1.head arg2.head
--         UnknownAction s -> log Error $ "Don't know how to do " <> s

--   where doClassicBuild = doCompile *> doCodegen
--         doCompile = runCompileM Chanterelle.compile project >>= case _ of
--             Left err -> logCompileError err
--             Right _ -> pure unit
--         doCodegen = runCompileM Chanterelle.generatePS project >>= case _ of
--             Left err -> logCompileError err
--             Right _ -> pure unit
--         doGenesis inputFile outputFile = generateGenesis project inputFile >>= case _ of
--             Left err -> logGenesisGenerationError err
--             Right gb -> do
--               let strungGb = jsonStringifyWithSpaces 4 (A.encodeJson gb)
--               try (writeTextFile UTF8 outputFile strungGb) >>= case _ of
--                 Left err -> log Error $ "Couldn't write genesis block to " <> show outputFile <> ": " <> show err
--                 Right _  -> log Info $ "Successfully wrote generated genesis block to " <> show outputFile

-- normalizeAction :: String -> RunnableAction
-- normalizeAction "b" = ClassicBuild
-- normalizeAction "build" = ClassicBuild
-- normalizeAction "c" = Compile
-- normalizeAction "compile" = Compile
-- normalizeAction "ps" = Codegen
-- normalizeAction "purs" = Codegen
-- normalizeAction "purescript" = Codegen
-- normalizeAction "g" = Genesis
-- normalizeAction "genesis" = Genesis
-- -- normalizeAction "i" = Install
-- -- normalizeAction "install" = Install
-- normalizeAction a = UnknownAction a
