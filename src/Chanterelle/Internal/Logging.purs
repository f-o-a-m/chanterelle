module Chanterelle.Internal.Logging
    ( LogLevel(..)
    , log
    , setLogLevel
    , readLogLevel
    , logCompileError
    , logDeployError
    , logGenesisGenerationError
    ) where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Chanterelle.Internal.Types.Compile as Compile
import Chanterelle.Internal.Types.Deploy as Deploy
import Chanterelle.Internal.Types.Genesis as Genesis
import Chanterelle.Internal.Types.Project (Network(..))
import Chanterelle.Internal.Utils.Time (now, toISOString)
import Control.Logger as Logger
import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Console as Console
import Data.String (toUpper)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))

data LogLevel = Debug | Info | Warn | Error

derive instance eqLogLevel   :: Eq LogLevel

instance ordLogLevel :: Ord LogLevel where
  compare = comparing levelOrder
    where
      levelOrder level = case level of
        Debug -> 1
        Info -> 2
        Warn -> 3
        Error -> 4

instance showLogLevel :: Show LogLevel where
    show Debug = "DEBUG"
    show Info  = "INFO"
    show Warn  = "WARN"
    show Error = "ERROR"

foreign import getLogLevelWithDefault :: LogLevel -> Effect LogLevel
foreign import setLogLevel :: LogLevel -> Effect Unit

readLogLevel
  :: String
  -> LogLevel
readLogLevel level =
  let normalized = toUpper level
  in case normalized of
       "DEBUG" -> Debug
       "INFO" -> Info
       "WARN" -> Warn
       "ERROR" -> Error
       otherwise -> Info

class Loggable a where
  logify :: a -> String

instance loggableString :: Loggable String where
  logify = identity

fancyColorLogger :: forall m a
                  . MonadEffect m
                 => Loggable a
                 => Logger.Logger m { level :: LogLevel, msg :: a }
fancyColorLogger = Logger.Logger $ \{ level, msg } -> liftEffect do
    iso <- toISOString <$> now
    Console.log $ colorize level (iso <> " [" <> show level <> "] " <> logify msg)
  where
    colorize level = withGraphics (foreground $ logLevelColor level)
    logLevelColor = case _ of
      Debug -> White
      Info  -> Green
      Warn  -> Yellow
      Error -> Red

log :: forall m
     . MonadEffect m
    => LogLevel
    -> String
    -> m Unit
log level msg = do
      currentLevel <- liftEffect $ getLogLevelWithDefault Info
      when (level >= currentLevel) $
        Logger.log fancyColorLogger { level, msg }

logCompileError :: forall m
                 . MonadEffect m
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

logDeployError :: forall m
                . MonadEffect m
               => Deploy.DeployError
               -> m Unit
logDeployError = liftEffect <<< case _ of
    Deploy.ConfigurationError errMsg -> log Error errMsg
    Deploy.OnDeploymentError msg     -> log Error (onDeployMessage msg)
    Deploy.PostDeploymentError msg   -> log Error (postDeployMessage msg)
  where
    onDeployMessage   msg = "Error During Deployment -- Name: " <> msg.name <> ", Message: " <> msg.message
    postDeployMessage msg = "Error After Deployment -- Name: " <> msg.name <> ", Message: " <> msg.message

logGenesisGenerationError :: forall m
                           . MonadEffect m
                          => Genesis.GenesisGenerationError
                          -> m Unit
logGenesisGenerationError = case _ of
    Genesis.CouldntLoadGenesisBlock path msg     -> log Error $ "Couldn't load the genesis block at " <> show path <> ": " <> msg
    Genesis.CouldntInjectLibraryAddress lib msg  -> log Error $ "Couldn't inject the address for " <> show lib <> ": " <> msg
    Genesis.CouldntInjectLibrary lib msg         -> log Error $ "Couldn't inject " <> show lib <> ": " <> msg
    Genesis.CouldntCompileLibrary lib ce         -> log Error ("Couldn't compile " <> show lib) *> logCompileError ce
    Genesis.MalformedProjectErrorG msg           -> log Error $ "Couldn't load chanterelle.json: " <> msg
    Genesis.NothingToDo reason                   -> log Warn  $ "Nothing to do! " <> reason
    Genesis.CouldntResolveLibraryNoNetworks name -> log Error $ "Couldn't resolve the library " <> show name <> " as no networks are available that satisfy its lookup constraints!"
    Genesis.CouldntResolveLibrary name errs      -> do
      log Error $ "Couldn't resolve the library " <> show name <> " on any specified networks:"
      for_ errs $ \(Tuple (Network net) err) -> log Error $ "    via " <> show net.name <> ": " <> err
