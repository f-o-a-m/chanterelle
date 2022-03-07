module Chanterelle where

import Prelude

import Chanterelle.Deploy (deploy)
import Chanterelle.Internal.Codegen (generatePS) as Chanterelle
import Chanterelle.Internal.Compile (compile) as Chanterelle
import Chanterelle.Internal.Logging (LogLevel(..), log, logCompileError, readLogLevel, setLogLevel)
import Chanterelle.Internal.Types (DeployM, runCompileMExceptT)
import Chanterelle.Internal.Types.Project (ChanterelleProject)
import Chanterelle.Internal.Utils (eitherM_)
import Chanterelle.Project (loadProject)
import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Path (resolve)
import Node.Process (cwd, exit)

data SelectCLI (a :: Type) (b :: Type) = SelectCLI a -- for CLI - String

data SelectPS (a :: Type) (b :: Type) = SelectPS b -- for PS - DeployM Unit

instance showSelectDeployM :: Show (SelectPS a (DeployM Unit)) where show (SelectPS _) = "<DeployM Unit>"
instance showSelectDeployPath :: Show a => Show (SelectCLI a b) where show (SelectCLI a) = show a

type ArgsCLI = Args' SelectCLI
type Args = Args' SelectPS
data Args' s = Args' CommonOpts (Command s)
derive instance genericArgs :: Generic (Args' s) _
instance showArgs :: Show (DeployOptions s) => Show (Args' s) where show = genericShow

type DirPath = String
newtype CommonOpts = CommonOpts
  { optVerbosity :: String
  , rootPath :: DirPath
  }
derive instance genericCommonOpts :: Generic CommonOpts _
instance showCommonOpts :: Show CommonOpts where show = genericShow


data Command s
  = Build
  | Compile
  | Codegen
  | Deploy (DeployOptions s)
  | GlobalDeploy (DeployOptions s)
derive instance genericCommand :: Generic (Command s) _
instance showCommand :: Show (DeployOptions s) => Show (Command s) where show = genericShow

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
    Deploy opts -> doDeploy opts
    GlobalDeploy _ -> doGlobalDeploy
  where
    doDeploy (DeployOptions {nodeURL, timeout, script: SelectPS s}) = deploy nodeURL timeout s
    doGlobalDeploy = do
      log Error $ "deploy is unavailable as Chanterelle is running from a global installation"
      log Error $ "Please ensure your project's Chanterelle instance has compiled"
    -- | doClassicBuild = doCompile *> doCodegen
    doCompile = eitherM_ terminateOnCompileError $ runCompileMExceptT Chanterelle.compile project
    doCodegen = eitherM_ terminateOnCompileError $ runCompileMExceptT Chanterelle.generatePS project

    terminateOnCompileError e = logCompileError e *> liftEffect (exit 1)
