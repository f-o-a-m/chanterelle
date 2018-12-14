module Chanterelle.Compile (compileProject) where

import Prelude

import Chanterelle.Internal.Codegen (generatePS) as Chanterelle
import Chanterelle.Internal.Compile (compile) as Chanterelle
import Chanterelle.Internal.Logging (logCompileError)
import Chanterelle.Internal.Types.Compile (CompileError(..), runCompileM)
import Chanterelle.Project (loadProject)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, message, throw)
import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Node.FS (FS)
import Node.Process (PROCESS)
import Node.Process as P

compileProject
  :: forall e.
     Eff (console :: CONSOLE, fs :: FS, process :: PROCESS, exception :: EXCEPTION | e) Unit
compileProject = do
    root <- liftEff P.cwd
    void $ launchAff $ do
      proj <- try $ loadProject root
      case proj of
        Left err -> do
          logCompileError $ MalformedProjectError (message err)
          liftEff $ throw "LoadProject Error"
        Right project -> do
          eres <- flip runCompileM project $ do
            _ <- Chanterelle.compile
            Chanterelle.generatePS
          case eres of
            Right _ -> pure unit
            Left err -> do
              logCompileError err
              liftEff $ throw "Compile Error"
