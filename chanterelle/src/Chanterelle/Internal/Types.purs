module Chanterelle.Internal.Types where

import Prelude

import Chanterelle.Internal.Logging (LogLevel(..), log)
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (Error, throwException)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Argonaut ((:=), (~>), (.?), (.??))
import Data.Argonaut as A
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((?~))
import Data.Maybe (fromMaybe)
import Data.Traversable (for_)
import Data.Validation.Semigroup (V)
import Network.Ethereum.Web3 (Address, BigNumber, ETH, HexString, TransactionOptions, Web3, _value, _data, fromWei)
import Network.Ethereum.Web3.Api (eth_sendTransaction)
import Network.Ethereum.Web3.Types (NoPay)
import Network.Ethereum.Web3.Types.Provider (Provider)
import Node.FS.Aff (FS)
import Node.Path (FilePath)
import Node.Process (PROCESS)

--------------------------------------------------------------------------------
-- | Chanterelle Project Types
--------------------------------------------------------------------------------

newtype Dependency = Dependency String

derive instance eqDependency  :: Eq Dependency

instance encodeJsonDependency :: A.EncodeJson Dependency where
  encodeJson (Dependency d) = A.encodeJson d

instance decodeJsonDependency :: A.DecodeJson Dependency where
  decodeJson d = Dependency <$> A.decodeJson d

data ChanterelleModule =
  ChanterelleModule { moduleName      :: String
                    , solContractName :: String
                    , solPath         :: FilePath
                    , jsonPath        :: FilePath
                    , pursPath        :: FilePath
                    }

newtype ChanterelleProjectSpec =
  ChanterelleProjectSpec { name                :: String
                         , version             :: String
                         , sourceDir           :: FilePath
                         , modules             :: Array String
                         , dependencies        :: Array Dependency
                         , solcOutputSelection :: Array String
                         , psGen               :: { exprPrefix   :: String
                                                  , modulePrefix :: String
                                                  , outputPath   :: String
                                                  }
                         }

derive instance eqChanterelleProjectSpec  :: Eq ChanterelleProjectSpec

instance encodeJsonChanterelleProjectSpec :: A.EncodeJson ChanterelleProjectSpec where
  encodeJson (ChanterelleProjectSpec project) =
         "name"                  := A.encodeJson project.name
      ~> "version"               := A.encodeJson project.version
      ~> "source-dir"            := A.encodeJson project.sourceDir
      ~> "modules"               := A.encodeJson project.modules
      ~> "dependencies"          := A.encodeJson project.dependencies
      ~> "solc-output-selection" := A.encodeJson project.solcOutputSelection
      ~> "purescript-generator"  := psGenEncode
      ~> A.jsonEmptyObject

      where psGenEncode =  "output-path"       := A.encodeJson project.psGen.outputPath
                        ~> "expression-prefix" := A.encodeJson project.psGen.exprPrefix
                        ~> "module-prefix"     := A.encodeJson project.psGen.modulePrefix
                        ~> A.jsonEmptyObject

instance decodeJsonChanterelleProjectSpec :: A.DecodeJson ChanterelleProjectSpec where
  decodeJson j = do
    obj                 <- A.decodeJson j
    name                <- obj .? "name"
    version             <- obj .? "version"
    sourceDir           <- obj .? "source-dir"
    modules             <- obj .? "modules"
    dependencies        <- obj .? "dependencies"
    solcOutputSelection <- obj .? "solc-output-selection"
    psGenObj            <- obj .? "purescript-generator"
    psGenOutputPath     <- psGenObj .? "output-path"
    psGenExprPrefix     <- fromMaybe "" <$> psGenObj .?? "expression-prefix"
    psGenModulePrefix   <- fromMaybe "" <$> psGenObj .?? "module-prefix"
    let psGen = { exprPrefix: psGenExprPrefix, modulePrefix: psGenModulePrefix, outputPath: psGenOutputPath }
    pure $ ChanterelleProjectSpec { name, version, sourceDir, modules, dependencies, solcOutputSelection, psGen }

data ChanterelleProject =
     ChanterelleProject { root     :: FilePath -- ^ parent directory containing chanterelle.json
                        , srcIn    :: FilePath -- ^ hydrated/absolute path of src dir (root + spec.sourceDir)
                        , jsonOut  :: FilePath -- ^ hydrated/absolute path of jsons dir
                        , psOut    :: FilePath -- ^ hydrated/absolute path of psGen (root + spec.psGen.outputPath)
                        , spec     :: ChanterelleProjectSpec -- ^ the contents of the chanterelle.json
                        , modules  :: Array ChanterelleModule
                        }

--------------------------------------------------------------------------------
-- | CompileM Compilement monad
--------------------------------------------------------------------------------

-- | Monad Stack for contract deployment.
newtype CompileM eff a =
  CompileM (ExceptT CompileError (Aff (fs :: FS, console :: CONSOLE, process :: PROCESS | eff)) a)

runCompileM
  :: forall eff a.
     CompileM eff a
  -> Aff (fs :: FS, console :: CONSOLE, process :: PROCESS | eff) (Either CompileError a)
runCompileM (CompileM deploy) = runExceptT deploy

derive newtype instance functorCompileM :: Functor (CompileM eff)
derive newtype instance applyCompileM :: Apply (CompileM eff)
derive newtype instance applicativeCompileM :: Applicative (CompileM eff)
derive newtype instance bindCompileM :: Bind (CompileM eff)
derive newtype instance monadCompileM :: Monad (CompileM eff)
derive newtype instance monadThrowCompileM :: MonadThrow CompileError (CompileM eff)
derive newtype instance monadEffCompileM :: MonadEff (fs :: FS, console :: CONSOLE, process :: PROCESS | eff) (CompileM eff)
derive newtype instance monadAffCompileM :: MonadAff (fs :: FS, console :: CONSOLE, process :: PROCESS | eff) (CompileM eff)

--------------------------------------------------------------------------------
-- | DeployM Deployment monad
--------------------------------------------------------------------------------

-- | Monad Stack for contract deployment.
newtype DeployM eff a =
  DeployM (ReaderT DeployConfig (ExceptT DeployError (Aff (eth :: ETH, fs :: FS, console :: CONSOLE | eff))) a)

runDeployM
  :: forall eff a.
     DeployM eff a
  -> DeployConfig
  -> Aff (fs :: FS, console :: CONSOLE, eth :: ETH | eff) (Either DeployError a)
runDeployM (DeployM deploy) = runExceptT <<< runReaderT deploy

derive newtype instance functorDeployM :: Functor (DeployM eff)
derive newtype instance applyDeployM :: Apply (DeployM eff)
derive newtype instance applicativeDeployM :: Applicative (DeployM eff)
derive newtype instance bindDeployM :: Bind (DeployM eff)
derive newtype instance monadDeployM :: Monad (DeployM eff)
derive newtype instance monadAskDeployM :: MonadAsk DeployConfig (DeployM eff)
derive newtype instance monadThrowDeployM :: MonadThrow DeployError (DeployM eff)
derive newtype instance monadEffDeployM :: MonadEff (eth :: ETH, fs :: FS, console :: CONSOLE | eff) (DeployM eff)
derive newtype instance monadAffDeployM :: MonadAff (eth :: ETH, fs :: FS, console :: CONSOLE | eff) (DeployM eff)

--------------------------------------------------------------------------------
-- | Error Types
--------------------------------------------------------------------------------

data DeployError =
    ConfigurationError String
  | OnDeploymentError String
  | PostDeploymentError String

derive instance genericError :: Generic DeployError _

instance showDeployError :: Show DeployError where
  show = genericShow

logDeployError
  :: forall eff m.
     MonadAff (console :: CONSOLE | eff) m
  => DeployError
  -> m Unit
logDeployError err = liftAff $ case err of
    ConfigurationError errMsg -> log Error errMsg
    OnDeploymentError errMsg -> log Error errMsg
    PostDeploymentError errMsg -> log Error errMsg

-- | Throw an `Error` Exception inside DeployM.
throwDeploy
  :: forall eff a.
     Error
  -> DeployM eff a
throwDeploy = liftAff <<< liftEff' <<< throwException

data CompileError =
    CompileParseError String
  | MissingArtifactError String
  | FSError String
  | CompilationError (Array String)

derive instance genericCompileError :: Generic CompileError _

instance showCompileError :: Show CompileError where
  show = genericShow

logCompileError
  :: forall eff m.
     MonadAff (console :: CONSOLE | eff) m
  => CompileError
  -> m Unit
logCompileError err = liftAff $ case err of
    CompileParseError errMsg -> log Error errMsg
    MissingArtifactError errMsg -> log Error errMsg
    FSError errMsg -> log Error errMsg
    CompilationError errs -> for_ errs (log Error)

--------------------------------------------------------------------------------
-- | Config Types
--------------------------------------------------------------------------------

-- | primary deployment configuration
newtype DeployConfig =
  DeployConfig { networkId :: BigNumber
               , primaryAccount :: Address
               , provider :: Provider
               }

-- | Contract Config

-- | Represents a contract constructor with input type `args`.
type Constructor args =
  forall eff. TransactionOptions NoPay -> HexString -> Record args -> Web3 eff HexString

-- | Type alias for the empty args
type NoArgs = ()

-- | Value representing empty args
noArgs :: V (Array String) {}
noArgs = pure {}

-- | A constructor that deploys a contract with no constructor args.
constructorNoArgs :: Constructor NoArgs
constructorNoArgs txOpts bytecode _ =
  eth_sendTransaction $ txOpts # _data ?~ bytecode
                               # _value ?~ fromWei zero

type ConfigR args =
  ( filepath :: FilePath
  , name :: String
  , constructor :: Constructor args
  , unvalidatedArgs :: V (Array String) (Record args)
  )

-- | Configuration for deployment of a single contract
type ContractConfig args = Record (ConfigR args)
