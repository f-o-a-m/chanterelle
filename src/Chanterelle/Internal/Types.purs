module Chanterelle.Internal.Types 
  ( module Compile
  , module Deploy
  , module Genesis
  , module Project
  , logCompileError
  , logDeployError
  , logGenesisGenerationError
  ) where

import Prelude

import Chanterelle.Internal.Types.Compile as Compile
import Chanterelle.Internal.Types.Deploy  as Deploy
import Chanterelle.Internal.Types.Genesis as Genesis
import Chanterelle.Internal.Types.Project as Project
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (class MonadEff)
import Data.Traversable (for_)

logCompileError :: forall eff m
                 . MonadEff (console :: CONSOLE | eff) m
                => Compile.CompileError
                -> m Unit
logCompileError = case _ of
    Compile.CompileParseError msg     -> log Error (parseErrorMessage msg)
    Compile.MissingArtifactError msg  -> log Error (artifactErrorMessage msg)
    Compile.FSError errMsg            -> log Error ("File System Error -- " <> errMsg)
    Compile.CompilationError errs     -> for_ errs (log Error)
    Compile.MalformedProjectError mpe -> log Error ("Couldn't parse chanterelle.json: " <> mpe)
    Compile.UnexpectedSolcOutput e    -> log Error ("Unexpected output from solc: " <> e)
  where
    parseErrorMessage msg = "Parse Error -- " <> "Object: " <> msg.objectName <>  ", Message: " <> msg.parseError
    artifactErrorMessage msg = "Missing Artifact -- " <> "FileName: " <> msg.fileName <> ", Object Name: " <> msg.objectName

logDeployError :: forall eff m
                . MonadAff (console :: CONSOLE | eff) m
               => Deploy.DeployError
               -> m Unit
logDeployError = liftAff <<< case _ of
    Deploy.ConfigurationError errMsg -> log Error errMsg
    Deploy.OnDeploymentError msg     -> log Error (onDeployMessage msg)
    Deploy.PostDeploymentError msg   -> log Error (postDeployMessage msg)
  where
    onDeployMessage   msg = "Error During Deployment -- Name: " <> msg.name <> ", Message: " <> msg.message
    postDeployMessage msg = "Error After Deployment -- Name: " <> msg.name <> ", Message: " <> msg.message

logGenesisGenerationError :: forall eff m
                           . MonadEff (console :: CONSOLE | eff) m
                          => Genesis.GenesisGenerationError
                          -> m Unit
logGenesisGenerationError = case _ of
    Genesis.CouldntLoadGenesisBlock path msg    -> log Error $ "Couldn't load the genesis block at " <> show path <> ": " <> msg
    Genesis.CouldntInjectLibraryAddress lib msg -> log Error $ "Couldn't inject the address for " <> show lib <> ": " <> msg
    Genesis.CouldntInjectLibrary lib msg        -> log Error $ "Couldn't inject " <> show lib <> ": " <> msg
    Genesis.CouldntCompileLibrary lib ce        -> log Error ("Couldn't compile " <> show lib) *> logCompileError ce
    Genesis.MalformedProjectErrorG msg          -> log Error $ "Couldn't load chanterelle.json: " <> msg
    Genesis.NothingToDo reason                  -> log Warn  $ "Nothing to do! " <> reason