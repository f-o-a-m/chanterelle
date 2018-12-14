module Chanterelle.Internal.Types.Compile where

import Prelude
import Chanterelle.Internal.Types.Project (ChanterelleProject, Library(..), Libraries(..), SolcOptimizerSettings)
import Chanterelle.Internal.Utils.Json (encodeJsonAddress)
import Data.Argonaut (class DecodeJson, class EncodeJson, (:=), (~>), (.?), (.??), decodeJson, encodeJson, jsonEmptyObject)
import Data.Argonaut as A
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either)
import Data.Maybe (fromMaybe)
import Foreign.Object (Object)
import Foreign.Object as M
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
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
newtype CompileM a =
  CompileM (ReaderT ChanterelleProject (ExceptT CompileError Aff) a)

runCompileM
  :: forall a.
     CompileM a
  -> ChanterelleProject
  -> Aff (Either CompileError a)
runCompileM (CompileM m) = runExceptT <<< runReaderT m

runCompileMExceptT
  :: forall a.
     CompileM a
  -> ChanterelleProject
  -> ExceptT CompileError Aff a
runCompileMExceptT (CompileM m) = runReaderT m

derive newtype instance functorCompileM     :: Functor CompileM
derive newtype instance applyCompileM       :: Apply CompileM
derive newtype instance applicativeCompileM :: Applicative CompileM
derive newtype instance bindCompileM        :: Bind CompileM
derive newtype instance monadCompileM       :: Monad CompileM
derive newtype instance monadThrowCompileM  :: MonadThrow CompileError CompileM
derive newtype instance monadAskCompileM    :: MonadAsk ChanterelleProject CompileM
derive newtype instance monadEffCompileM    :: MonadEffect CompileM
derive newtype instance monadAffCompileM    :: MonadAff CompileM

--------------------------------------------------------------------------------
-- | Solc Types and Codecs
--------------------------------------------------------------------------------

-- NOTE: We don't use classes here because parse and encode aren't mutual inverses,
-- we sometimes add (e.g. networks object) or remove (most things) data from solc
-- input/output.

newtype SolcInput =
  SolcInput { language :: String
            , sources  :: Object SolcContract
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
  SolcSettings { outputSelection :: Object (Object (Array String))
               , remappings      :: Array String
               , libraries       :: Object Libraries
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
  OutputContract { abi :: Array A.Json
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
             , contracts :: Object (Object OutputContract)
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
