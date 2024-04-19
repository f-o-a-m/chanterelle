module Chanterelle where

import Prelude

import Chanterelle.Compile (compile) as Chanterelle
import Chanterelle.Deploy (deploy)
import Chanterelle.Internal.Codegen (generatePS) as Chanterelle
import Chanterelle.Logging (LogLevel(..), log, logCompileError, readLogLevel, setLogLevel)
import Chanterelle.Project (loadProject)
import Chanterelle.Types.Compile (runCompileM)
import Chanterelle.Types.Deploy (DeployM)
import Chanterelle.Types.Project (ChanterelleProject)
import Control.Monad.Error.Class (try)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Path (resolve)
import Node.Process (cwd, exit')

data SelectCLI (a :: Type) (b :: Type) = SelectCLI a

data SelectPS (a :: Type) (b :: Type) = SelectPS b

instance Show (SelectPS a (DeployM Unit)) where
  show (SelectPS _) = "<DeployM Unit>"

instance Show a => Show (SelectCLI a b) where
  show (SelectCLI a) = show a

type ArgsCLI = Args' SelectCLI
type Args = Args' SelectPS
data Args' s = Args' CommonOpts (Command s)

derive instance Generic (Args' s) _
instance Show (DeployOptions s) => Show (Args' s) where
  show = genericShow

type DirPath = String
data CommonOpts = CommonOpts
  { optVerbosity :: String
  , rootPath :: DirPath
  }

derive instance Generic CommonOpts _
instance Show CommonOpts where
  show = genericShow

data Command s
  = Build
  | Compile
  | Codegen
  | Deploy (DeployOptions s)
  | GlobalDeploy (DeployOptions s)

derive instance Generic (Command s) _
instance Show (DeployOptions s) => Show (Command s) where
  show = genericShow

traverseDeployOptions :: forall a b f. Applicative f => (DeployOptions a -> f (DeployOptions b)) -> Args' a -> f (Args' b)
traverseDeployOptions f (Args' o cmd) = Args' o <$> case cmd of
  Build -> pure Build
  Compile -> pure Compile
  Codegen -> pure Codegen
  Deploy dopts -> Deploy <$> f dopts
  GlobalDeploy dopts -> GlobalDeploy <$> f dopts

type DeployOptionsCLI = DeployOptions SelectCLI

data DeployOptions s = DeployOptions
  { nodeURL :: String
  , timeout :: Int
  , script :: s String (DeployM Unit)
  }

derive instance Generic (DeployOptions s) _
instance Show (DeployOptions SelectPS) where
  show = genericShow

chanterelle :: Args -> Aff Unit
chanterelle (Args' (CommonOpts { optVerbosity, rootPath }) cmd) = do
  ourCwd <- liftEffect cwd
  liftEffect $ setLogLevel (readLogLevel optVerbosity)
  resolvedRoot <- liftEffect $ resolve [ ourCwd ] rootPath
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
  Deploy opts -> doDeploy opts
  GlobalDeploy _ -> doGlobalDeploy
  where
  doDeploy (DeployOptions { nodeURL, timeout, script: SelectPS s }) = deploy nodeURL timeout s
  doGlobalDeploy = do
    log Error $ "deploy is unavailable as Chanterelle is running from a global installation"
    log Error $ "Please ensure your project's Chanterelle instance has compiled"
  -- doClassicBuild = doCompile *> doCodegen
  doCompile = do
    eRes <- runCompileM Chanterelle.compile project
    either terminateOnCompileError mempty eRes
  doCodegen = do
    eRes <- runCompileM Chanterelle.generatePS project
    either terminateOnCompileError mempty eRes

  terminateOnCompileError e = logCompileError e *> liftEffect (exit' 1)
