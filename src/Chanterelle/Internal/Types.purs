module Chanterelle.Internal.Types where

import Prelude

import Chanterelle.Internal.Logging (LogLevel(..), log)
import Control.Alt ((<|>))
import Control.Error.Util (note)
import Control.Monad.Aff (Aff, Milliseconds, liftEff')
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (Error, throwException)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Argonaut ((:=), (~>), (.?), (.??))
import Data.Argonaut as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((?~))
import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.StrMap as M
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V)
import Network.Ethereum.Web3 (Address, BigNumber, ETH, HexString, TransactionOptions, Web3, _value, _data, fromWei, unAddress, mkAddress, mkHexString, unHex)
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

---------------------------------------------------------------------

data InjectableLibraryCode = InjectableWithSourceCode (Maybe FilePath) FilePath
                           | InjectableWithBytecode HexString

derive instance eqInjectableLibraryCode :: Eq InjectableLibraryCode

instance encodeJsonInjectableLibraryCode :: A.EncodeJson InjectableLibraryCode where
  encodeJson (InjectableWithSourceCode r f) = A.encodeJson $ M.fromFoldable elems
    where elems = file <> root
          file  = [Tuple "file" $ A.encodeJson f]
          root  = fromMaybe [] (pure <<< Tuple "root" <<< A.encodeJson <$> r)
  encodeJson (InjectableWithBytecode c)   = A.encodeJson $ M.singleton "bytecode" (unHex c)

instance decodeJsonInjectableLibraryCode :: A.DecodeJson InjectableLibraryCode where
  decodeJson d = decodeSourceCode <|> decodeBytecode <|> Left "not a valid InjectableLibrarySource"
      where decodeSourceCode = A.decodeJson d >>= (\o -> InjectableWithSourceCode <$> o .?? "root" <*> o .? "file")
            decodeBytecode   = do 
              o <- A.decodeJson d
              bc <- note "Malformed hexString" <<< mkHexString =<< o .? "bytecode"
              pure $ InjectableWithBytecode bc

data Library = FixedLibrary      { name :: String, address :: Address  }
             | InjectableLibrary { name :: String, address :: Address , code :: InjectableLibraryCode }

isFixedLibrary :: Library -> Boolean
isFixedLibrary (FixedLibrary _) = true
isFixedLibrary _                = false

newtype Libraries = Libraries (Array Library)

derive instance eqLibrary                   :: Eq Library
derive instance eqLibraries                 :: Eq Libraries
derive newtype instance monoidLibraries     :: Monoid Libraries
derive newtype instance semigroiupLibraries :: Semigroup Libraries

instance encodeJsonLibraries :: A.EncodeJson Libraries where
  encodeJson (Libraries libs) =
    let asAssocs = mkTuple <$> libs
        encodeAddress = A.encodeJson <<< show <<< unAddress
        mkTuple (FixedLibrary l)      = Tuple l.name (encodeAddress l.address)
        mkTuple (InjectableLibrary l) = let dl =  "address" := encodeAddress l.address
                                               ~> "code"  := A.encodeJson  l.code
                                               ~> A.jsonEmptyObject
                                         in Tuple l.name (A.encodeJson dl)
        asMap    = M.fromFoldable asAssocs
     in A.encodeJson asMap

instance decodeJsonLibraries :: A.DecodeJson Libraries where
  decodeJson j = do
    obj <- A.decodeJson j
    libs <- for (M.toUnfoldable obj) $ \t -> (decodeFixedLibrary t) <|> (decodeInjectableLibrary t) <|> (failDramatically t)
    pure (Libraries libs)

    where decodeFixedLibrary (Tuple name l) = do
            address' <- A.decodeJson l
            address <- note ("Invalid address " <> address') (mkHexString address' >>= mkAddress)
            pure $ FixedLibrary { name, address }

          decodeInjectableLibrary (Tuple name l) = do
            ilo <- A.decodeJson l
            address' <- ilo .? "address"
            address  <- note ("Invalid address " <> address') (mkHexString address' >>= mkAddress)
            code     <- ilo .? "code"
            pure $ InjectableLibrary { name, address, code }

          failDramatically (Tuple name _) = Left ("Malformed library descriptor for " <> name)

---------------------------------------------------------------------

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
                         , libraries           :: Libraries
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
      ~> "libraries"             := A.encodeJson project.libraries
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
    dependencies        <- fromMaybe mempty <$> obj .?? "dependencies"
    libraries           <- fromMaybe mempty <$> obj .?? "libraries"
    solcOutputSelection <- fromMaybe mempty <$> obj .?? "solc-output-selection"
    psGenObj            <- obj .? "purescript-generator"
    psGenOutputPath     <- psGenObj .? "output-path"
    psGenExprPrefix     <- fromMaybe "" <$> psGenObj .?? "expression-prefix"
    psGenModulePrefix   <- fromMaybe "" <$> psGenObj .?? "module-prefix"
    let psGen = { exprPrefix: psGenExprPrefix, modulePrefix: psGenModulePrefix, outputPath: psGenOutputPath }
    pure $ ChanterelleProjectSpec { name, version, sourceDir, modules, dependencies, libraries, solcOutputSelection, psGen }

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
  CompileM (ReaderT ChanterelleProject (ExceptT CompileError (Aff (fs :: FS, console :: CONSOLE, process :: PROCESS, now :: NOW | eff))) a)

runCompileM
  :: forall eff a.
     CompileM eff a
  -> ChanterelleProject
  -> Aff (fs :: FS, console :: CONSOLE, process :: PROCESS, now :: NOW | eff) (Either CompileError a)
runCompileM (CompileM m) = runExceptT <<< runReaderT m

runCompileMExceptT
  :: forall eff a.
     CompileM eff a
  -> ChanterelleProject
  -> ExceptT CompileError (Aff (fs :: FS, console :: CONSOLE, process :: PROCESS, now :: NOW | eff)) a
runCompileMExceptT (CompileM m) = runReaderT m

derive newtype instance functorCompileM :: Functor (CompileM eff)
derive newtype instance applyCompileM :: Apply (CompileM eff)
derive newtype instance applicativeCompileM :: Applicative (CompileM eff)
derive newtype instance bindCompileM :: Bind (CompileM eff)
derive newtype instance monadCompileM :: Monad (CompileM eff)
derive newtype instance monadThrowCompileM :: MonadThrow CompileError (CompileM eff)
derive newtype instance monadAskCompileM :: MonadAsk ChanterelleProject (CompileM eff)
derive newtype instance monadEffCompileM :: MonadEff (fs :: FS, console :: CONSOLE, process :: PROCESS, now :: NOW | eff) (CompileM eff)
derive newtype instance monadAffCompileM :: MonadAff (fs :: FS, console :: CONSOLE, process :: PROCESS, now :: NOW | eff) (CompileM eff)

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
  | OnDeploymentError {name :: String, message :: String}
  | PostDeploymentError {name :: String, message :: String}

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
    OnDeploymentError msg -> log Error (onDeployMessage msg)
    PostDeploymentError msg -> log Error (postDeployMessage msg)
  where
    onDeployMessage msg = "Error During Deployment -- Name: " <> msg.name <> ", Message: " <> msg.message
    postDeployMessage msg = "Error After Deployment -- Name: " <> msg.name <> ", Message: " <> msg.message

-- | Throw an `Error` Exception inside DeployM.
throwDeploy
  :: forall eff a.
     Error
  -> DeployM eff a
throwDeploy = liftAff <<< liftEff' <<< throwException

data CompileError =
    CompileParseError {objectName :: String, parseError :: String}
  | MissingArtifactError {fileName :: String, objectName :: String}
  | MalformedProjectError String
  | FSError String
  | CompilationError (Array String)
  | UnexpectedSolcOutput String

derive instance genericCompileError :: Generic CompileError _

instance showCompileError :: Show CompileError where
  show = genericShow

logCompileError
  :: forall eff m.
     MonadEff (console :: CONSOLE | eff) m
  => CompileError
  -> m Unit
logCompileError = case _ of
    CompileParseError msg     -> log Error (parseErrorMessage msg)
    MissingArtifactError msg  -> log Error (artifactErrorMessage msg)
    FSError errMsg            -> log Error ("File System Error -- " <> errMsg)
    CompilationError errs     -> for_ errs (log Error)
    MalformedProjectError mpe -> log Error ("Couldn't parse chanterelle.json: " <> mpe)
    UnexpectedSolcOutput e    -> log Error ("Unexpected output from solc: " <> e)
  where
    parseErrorMessage msg = "Parse Error -- " <> "Object: " <> msg.objectName <>  ", Message: " <> msg.parseError
    artifactErrorMessage msg = "Missing Artifact -- " <> "FileName: " <> msg.fileName <> ", Object Name: " <> msg.objectName

--------------------------------------------------------------------------------
-- | Config Types
--------------------------------------------------------------------------------

-- | primary deployment configuration
newtype DeployConfig =
  DeployConfig { networkId :: BigNumber
               , primaryAccount :: Address
               , provider :: Provider
               , timeout :: Milliseconds
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
