module Chanterelle where

import Prelude

import Chanterelle.Deploy (deploy)
import Chanterelle.Internal.Codegen (generatePS) as Chanterelle
import Chanterelle.Internal.Compile (compile) as Chanterelle
import Chanterelle.Internal.Genesis (generateGenesis)
import Chanterelle.Internal.Logging (LogLevel(..), log, logCompileError, logGenesisGenerationError, readLogLevel, setLogLevel)
import Chanterelle.Internal.Types (DeployM, runCompileM)
import Chanterelle.Internal.Types.Project (ChanterelleProject)
import Chanterelle.Internal.Utils (jsonStringifyWithSpaces)
import Chanterelle.Project (loadProject)
import Control.Monad.Error.Class (try)
import Data.Argonaut as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.Path (resolve)
import Node.Process (cwd)


data SelectCLI a b = SelectCLI a 
data SelectPS a b = SelectPS b
instance showSelectDeployM :: Show (SelectPS a (DeployM Unit)) where show (SelectPS _ ) = "<DeployM Unit>"
instance showSelectDeployPath :: Show a => Show (SelectCLI a b) where show (SelectCLI a ) = show a

type ArgsCLI = Args' SelectCLI
type Args = Args' SelectPS
data Args' s = Args' CommonOpts (Command s)
derive instance genericArgs :: Generic (Args' s) _
instance showArgs :: Show (DeployOptions s) => Show (Args' s) where show = genericShow

type DirPath = String
data CommonOpts = CommonOpts
  { optVerbosity :: String
  , rootPath :: DirPath
  }
derive instance genericCommonOpts :: Generic CommonOpts _
instance showCommonOpts :: Show CommonOpts where show = genericShow


data Command s
  = Build
  | Compile
  | Codegen
  | Genesis GenesisOptions
  | Deploy (DeployOptions s)
  | GlobalDeploy (DeployOptions s)
derive instance genericCommand :: Generic (Command s) _
instance showCommand :: Show (DeployOptions s) => Show (Command s) where show = genericShow

traverseDeployOptions :: forall a b f. Applicative f => (DeployOptions a -> f (DeployOptions b)) -> Args' a -> f (Args' b)
traverseDeployOptions f (Args' o cmd) = Args' o <$> case cmd of
  Build -> pure Build
  Compile -> pure Compile
  Codegen -> pure Codegen
  Genesis opts -> pure $ Genesis opts
  Deploy dopts -> Deploy <$> f dopts
  GlobalDeploy dopts -> GlobalDeploy <$> f dopts

data GenesisOptions = GenesisOptions
  { input :: String
  , output :: String
  }
derive instance genericGenesisOptions :: Generic GenesisOptions _
instance showGenesisOptions :: Show GenesisOptions where show = genericShow

type DeployOptionsCLI = DeployOptions SelectCLI

data DeployOptions s = DeployOptions
  { nodeURL :: String
  , timeout :: Int
  , script :: s String (DeployM Unit)
  }
derive instance genericDeployOptions :: Generic (DeployOptions s) _
instance showDeployOptions :: Show (DeployOptions SelectPS)  where show = genericShow

chanterelle :: Args -> Aff Unit
chanterelle (Args' (CommonOpts{ optVerbosity, rootPath }) cmd) = do
  ourCwd <- liftEffect cwd
  liftEffect $ setLogLevel (readLogLevel optVerbosity)
  resolvedRoot <- liftEffect $ resolve [ourCwd] rootPath
  projE <- try $ loadProject resolvedRoot
  case projE of
    Left err -> log Error ("Couldn't parse chanterelle.json: " <> show err)
    Right project -> do
      log Info "Loaded chanterelle.json successfully!"
      runCommand project cmd

runCommand :: ChanterelleProject -> Command SelectPS -> Aff Unit
runCommand project = case _ of
    Build -> doCompile *> doCodegen
    Compile -> doCompile
    Codegen -> doCodegen
    Genesis opts -> doGenesis opts
    Deploy opts -> doDeploy opts
    GlobalDeploy _ -> doGlobalDeploy
  where
    doDeploy (DeployOptions {nodeURL, timeout, script: SelectPS s}) = do
      deploy nodeURL timeout s
    doGlobalDeploy = do
      log Error $ "deploy is unavailable as Chanterelle is running from a global installation"
      log Error $ "Please ensure your project's Chanterelle instance has compiled"
    doClassicBuild = doCompile *> doCodegen
    doCompile = runCompileM Chanterelle.compile project >>= case _ of
      Left err -> logCompileError err
      Right _ -> pure unit
    doCodegen = runCompileM Chanterelle.generatePS project >>= case _ of
      Left err -> logCompileError err
      Right _ -> pure unit
    doGenesis (GenesisOptions {input,output}) = generateGenesis project input >>= case _ of
      Left err -> logGenesisGenerationError err
      Right gb -> do
        let strungGb = jsonStringifyWithSpaces 4 (A.encodeJson gb)
        try (writeTextFile UTF8 output strungGb) >>= case _ of
          Left err -> log Error $ "Couldn't write genesis block to " <> show output <> ": " <> show err
          Right _  -> log Info $ "Successfully wrote generated genesis block to " <> show output