module Chanterelle.Internal.Types where

import Prelude
import Ansi.Codes (Color(Red))
import Ansi.Output (withGraphics, foreground)
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Console as C
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, throwException)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Argonaut as A
import Data.Argonaut ((:=), (~>), (.?), (.??))
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromMaybe)
import Network.Ethereum.Web3 (ETH, Address, BigNumber)
import Network.Ethereum.Web3.Types.Provider (Provider)
import Node.FS.Aff (FS)
import Node.Process (PROCESS)
import Node.Path (FilePath)

newtype Dependency = Dependency String
derive instance eqDependency  :: Eq Dependency
instance encodeJsonDependency :: A.EncodeJson Dependency where
  encodeJson (Dependency d) = A.encodeJson d
instance decodeJsonDependency :: A.DecodeJson Dependency where
  decodeJson d = Dependency <$> A.decodeJson d

newtype ChanterelleProjectSpec =
  ChanterelleProjectSpec { name                :: String
                         , version             :: String
                         , sourceDir           :: FilePath
                         , sources             :: Array String
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
      ~> "sources"               := A.encodeJson project.sources
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
    sources             <- obj .? "sources"
    dependencies        <- obj .? "dependencies"
    solcOutputSelection <- obj .? "solc-output-selection"
    psGenObj            <- obj .? "purescript-generator"
    psGenOutputPath     <- psGenObj .? "output-path"
    psGenExprPrefix     <- fromMaybe "" <$> psGenObj .?? "expression-prefix"
    psGenModulePrefix   <- fromMaybe "" <$> psGenObj .?? "module-prefix"
    let psGen = { exprPrefix: psGenExprPrefix, modulePrefix: psGenModulePrefix, outputPath: psGenOutputPath } 
    pure $ ChanterelleProjectSpec { name, version, sourceDir, sources, dependencies, solcOutputSelection, psGen }

data ChanterelleProject =
     ChanterelleProject { root     :: FilePath -- ^ parent directory containing chanterelle.json
                        , srcIn    :: FilePath -- ^ hydrated/absolute path of src dir (root + spec.sourceDir)
                        , jsonOut  :: FilePath -- ^ hydrated/absolute path of jsons dir 
                        , psOut    :: FilePath -- ^ hydrated/absolute path of psGen (root + spec.psGen.outputPath)
                        , spec     :: ChanterelleProjectSpec -- ^ the contents of the chanterelle.json
                        }

--------------------------------------------------------------------------------
-- | DeployM
--------------------------------------------------------------------------------

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
    ConfigurationError errMsg -> C.error $ makeRed "Configuration Error: " <> errMsg
    OnDeploymentError errMsg -> C.error $ makeRed "On Deployment Error: " <> errMsg
    PostDeploymentError errMsg -> C.error $ makeRed "Post Deployment Error: " <> errMsg
  where
    makeRed :: String -> String
    makeRed = withGraphics (foreground Red)

throwDeploy
  :: forall eff a.
     Error
  -> DeployM eff a
throwDeploy = liftAff <<< liftEff' <<< throwException

--------------------------------------------------------------------------------
-- | Config Types
--------------------------------------------------------------------------------

-- | primary deployment configuration
newtype DeployConfig =
  DeployConfig { networkId :: BigNumber
               , primaryAccount :: Address
               , provider :: Provider
               }

type ConfigR r = (filepath :: FilePath, name :: String | r)

-- | configuration for deployment of a single contract
type ContractConfig r = Record (ConfigR r)

