module Chanterelle.Internal.Types.Compile where

import Prelude
import Chanterelle.Internal.Types.Project (ChanterelleProject, Library(..), Libraries(..), SolcOptimizerSettings)
import Chanterelle.Internal.Utils.Json (encodeJsonAddress)
import Data.Argonaut (class DecodeJson, class EncodeJson, (:=), (~>), (.?), (.??), decodeJson, encodeJson, jsonEmptyObject)
import Data.Argonaut as A
import Control.Monad.Aff (Aff, Milliseconds(..))
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either)
import Data.Maybe (fromMaybe)
import Data.StrMap (StrMap)
import Data.StrMap as M
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Node.FS (FS)
import Node.Process (PROCESS)
import Network.Ethereum.Web3 (HexString, unHex)

data CompileError = CompileParseError    { objectName :: String, parseError :: String }
                  | MissingArtifactError { fileName :: String,   objectName :: String }
                  | MalformedProjectError String
                  | FSError String
                  | CompilationError (Array String)
                  | UnexpectedSolcOutput String

--------------------------------------------------------------------------------
-- | CompileM Compilation monad
--------------------------------------------------------------------------------

-- | Monad Stack for contract deployment.
newtype CompileM eff a =
  CompileM (ReaderT ChanterelleProject (ExceptT CompileError (Aff (fs :: FS, console :: CONSOLE, process :: PROCESS | eff))) a)

runCompileM
  :: forall eff a.
     CompileM eff a
  -> ChanterelleProject
  -> Aff (fs :: FS, console :: CONSOLE, process :: PROCESS | eff) (Either CompileError a)
runCompileM (CompileM m) = runExceptT <<< runReaderT m

runCompileMExceptT
  :: forall eff a.
     CompileM eff a
  -> ChanterelleProject
  -> ExceptT CompileError (Aff (fs :: FS, console :: CONSOLE, process :: PROCESS | eff)) a
runCompileMExceptT (CompileM m) = runReaderT m

derive newtype instance functorCompileM     :: Functor (CompileM eff)
derive newtype instance applyCompileM       :: Apply (CompileM eff)
derive newtype instance applicativeCompileM :: Applicative (CompileM eff)
derive newtype instance bindCompileM        :: Bind (CompileM eff)
derive newtype instance monadCompileM       :: Monad (CompileM eff)
derive newtype instance monadThrowCompileM  :: MonadThrow CompileError (CompileM eff)
derive newtype instance monadAskCompileM    :: MonadAsk ChanterelleProject (CompileM eff)
derive newtype instance monadEffCompileM    :: MonadEff (fs :: FS, console :: CONSOLE, process :: PROCESS | eff) (CompileM eff)
derive newtype instance monadAffCompileM    :: MonadAff (fs :: FS, console :: CONSOLE, process :: PROCESS | eff) (CompileM eff)

--------------------------------------------------------------------------------
-- | Solc Types and Codecs
--------------------------------------------------------------------------------

-- NOTE: We don't use classes here because parse and encode aren't mutual inverses,
-- we sometimes add (e.g. networks object) or remove (most things) data from solc
-- input/output.

newtype SolcInput =
  SolcInput { language :: String
            , sources  :: StrMap SolcContract
            , settings :: SolcSettings
            }
instance encodeSolcInput :: EncodeJson SolcInput where
  encodeJson (SolcInput {language, sources, settings}) =
       "language" := A.fromString "Solidity"
    ~> "sources"  := encodeJson sources
    ~> "settings" := encodeJson settings
    ~> jsonEmptyObject

--------------------------------------------------------------------------------

type ContractName = String

newtype SolcSettings =
  SolcSettings { outputSelection :: StrMap (StrMap (Array String))
               , remappings      :: Array String
               , libraries       :: StrMap Libraries
               , optimizer       :: SolcOptimizerSettings
               }

instance encodeSolcSettings :: EncodeJson SolcSettings where
  encodeJson (SolcSettings s) =
       "outputSelection" := encodeJson s.outputSelection
    ~> "remappings"      := encodeJson s.remappings
    ~> "libraries"       := encodeJson (solcifyAllLibs s.libraries)
    ~> "optimizer"       := encodeJson s.optimizer
    ~> jsonEmptyObject

    where solcifyAllLibs libs = solcifyLibs <$> libs
          solcifyLibs (Libraries l) = M.fromFoldable (solcifyLib <$> l)
          solcifyLib (FixedLibrary { name, address } )            = Tuple name (encodeJsonAddress address)
          solcifyLib (FixedLibraryWithNetwork { name, address } ) = Tuple name (encodeJsonAddress address)
          solcifyLib (InjectableLibrary { name, address } )       = Tuple name (encodeJsonAddress address)

--------------------------------------------------------------------------------

-- as per http://solidity.readthedocs.io/en/v0.4.21/using-the-compiler.html,
-- "content" is the source code.
newtype SolcContract =
  SolcContract { content :: String
               , hash :: HexString
               }
instance encodeSolcContract :: EncodeJson SolcContract where
  encodeJson (SolcContract {content, hash}) =  "content"   := A.fromString content
                                            ~> "keccak256" := A.fromString (unHex hash)
                                            ~> jsonEmptyObject

--------------------------------------------------------------------------------

-- Solc Errors
-- TODO: pretty print these later
newtype SolcError =
  SolcError { type :: String
            , severity :: String
            , message :: String
            , formattedMessage :: String
            }

instance decodeSolcError :: DecodeJson SolcError where
  decodeJson json = do
    obj <- decodeJson json
    _type <- obj .? "type"
    severity <- obj .? "severity"
    message <- obj .? "message"
    formattedMessage <-obj .? "formattedMessage"
    pure $
      SolcError { type: _type
                , severity
                , message
                , formattedMessage
                }

--------------------------------------------------------------------------------

-- This is the artifact we want, compatible with truffle (subset)
newtype OutputContract =
  OutputContract { abi :: A.JArray
                 , bytecode :: String
                 , deployedBytecode :: String
                 }

parseOutputContract
  :: A.Json
  -> Either String OutputContract
parseOutputContract json = do
  obj <- decodeJson json
  abi <- obj .? "abi"
  evm <- obj .? "evm"
  evmObj <- decodeJson evm
  bytecodeO <- evmObj .? "bytecode"
  bytecode <- bytecodeO .? "object"
  deployedBytecodeO <- evmObj .? "deployedBytecode"
  deployedBytecode <- deployedBytecodeO .? "object"
  pure $ OutputContract { abi, bytecode, deployedBytecode }

encodeOutputContract
  :: OutputContract
  -> Milliseconds
  -> A.Json
encodeOutputContract (OutputContract {abi, bytecode}) (Milliseconds ts) =
       "abi"        := A.fromArray abi
    ~> "bytecode"   := bytecode
    ~> "networks"   := jsonEmptyObject
    ~> "compiledAt" := ts
    ~> jsonEmptyObject

newtype SolcOutput =
  SolcOutput { errors    :: Array SolcError
             , contracts :: StrMap (StrMap OutputContract)
             }

parseSolcOutput
  :: A.Json
  -> Either String SolcOutput
parseSolcOutput json = do
  o <- decodeJson json
  errors <- fromMaybe [] <$> o .?? "errors"
  contractsMap <- o .? "contracts"
  contracts <- for contractsMap (traverse parseOutputContract)
  pure $ SolcOutput {errors, contracts}
