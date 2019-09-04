module Chanterelle.Internal.Types.Compile where

import Prelude

import Chanterelle.Internal.Types.Bytecode (Bytecode, fromSolidityBytecodeOutput)
import Chanterelle.Internal.Types.Project (ChanterelleProject)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Argonaut (decodeJson, jsonEmptyObject, (:=), (~>))
import Data.Argonaut as A
import Data.Either (Either(..), note)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Language.Solidity.Compiler.Types as ST

data CompileError = CompileParseError    { objectName :: String, parseError :: String }
                  | MissingArtifactError { fileName :: String,   objectName :: String }
                  | MalformedProjectError String
                  | FSError String
                  | CompilationError  { moduleName :: String, errors :: Array ST.CompilationError }
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

-- This is the artifact we want, compatible with truffle (subset)
newtype OutputContract =
  OutputContract { abi :: Array A.Json
                 , bytecode :: Bytecode
                 , deployedBytecode :: Bytecode
                 }

fromSolidityContractLevelOutput :: ST.ContractLevelOutput -> Either String OutputContract
fromSolidityContractLevelOutput (ST.ContractLevelOutput clo) = do
  abi <- decodeJson clo.abi
  (ST.EvmOutput evm) <- note "Solidity contract output did not have an \"evm\" field" clo.evm
  bytecode' <- note "Solidity contract output did not have an \"evm.bytecode\" field" evm.bytecode
  bytecode <- fromSolidityBytecodeOutput bytecode'
  deployedBytecode' <- note "Solidity contract output did not have an \"evm.deployedBytecode\" field" evm.deployedBytecode
  deployedBytecode <- fromSolidityBytecodeOutput deployedBytecode'
  pure $ OutputContract { abi, bytecode, deployedBytecode }

resolveSolidityContractLevelOutput :: forall m
                                    . MonadThrow CompileError m
                                   => ST.ContractLevelOutput
                                   -> m OutputContract
resolveSolidityContractLevelOutput clo =
  case fromSolidityContractLevelOutput clo of
    Left e -> throwError (UnexpectedSolcOutput e)
    Right o -> pure o

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