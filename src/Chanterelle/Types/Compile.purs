module Chanterelle.Types.Compile
  ( CompileError(..)
  , CompileM(..)
  , runCompileM
  , runCompileMExceptT
  ) where

import Prelude

import Chanterelle.Types.Project (ChanterelleProject)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Language.Solidity.Compiler.Types as ST

data CompileError
  = CompileParseError { objectName :: String, parseError :: String }
  | MissingArtifactError { fileName :: String, objectName :: String }
  | MalformedProjectError String
  | FSError String
  | CompilationError { moduleName :: String, errors :: Array ST.CompilationError }
  | UnexpectedSolcOutput String
  | CompilerUnavailable String

--------------------------------------------------------------------------------
-- | CompileM Compilation monad
--------------------------------------------------------------------------------

-- | Monad Stack for contract deployment.
newtype CompileM a =
  CompileM (ReaderT ChanterelleProject (ExceptT CompileError Aff) a)

runCompileM
  :: forall a
   . CompileM a
  -> ChanterelleProject
  -> Aff (Either CompileError a)
runCompileM (CompileM m) = runExceptT <<< runReaderT m

runCompileMExceptT
  :: forall a
   . CompileM a
  -> ChanterelleProject
  -> ExceptT CompileError Aff a
runCompileMExceptT (CompileM m) = runReaderT m

derive newtype instance Functor CompileM
derive newtype instance Apply CompileM
derive newtype instance Applicative CompileM
derive newtype instance Bind CompileM
derive newtype instance Monad CompileM
derive newtype instance MonadThrow CompileError CompileM
derive newtype instance MonadAsk ChanterelleProject CompileM
derive newtype instance MonadEffect CompileM
derive newtype instance MonadAff CompileM
