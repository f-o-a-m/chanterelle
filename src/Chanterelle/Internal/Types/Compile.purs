module Chanterelle.Internal.Types.Compile where

import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, (<<<))
import Chanterelle.Internal.Types.Project (ChanterelleProject)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either)
import Node.FS (FS)
import Node.Process (PROCESS)

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

derive newtype instance functorCompileM     :: Functor (CompileM eff)
derive newtype instance applyCompileM       :: Apply (CompileM eff)
derive newtype instance applicativeCompileM :: Applicative (CompileM eff)
derive newtype instance bindCompileM        :: Bind (CompileM eff)
derive newtype instance monadCompileM       :: Monad (CompileM eff)
derive newtype instance monadThrowCompileM  :: MonadThrow CompileError (CompileM eff)
derive newtype instance monadAskCompileM    :: MonadAsk ChanterelleProject (CompileM eff)
derive newtype instance monadEffCompileM    :: MonadEff (fs :: FS, console :: CONSOLE, process :: PROCESS, now :: NOW | eff) (CompileM eff)
derive newtype instance monadAffCompileM    :: MonadAff (fs :: FS, console :: CONSOLE, process :: PROCESS, now :: NOW | eff) (CompileM eff)