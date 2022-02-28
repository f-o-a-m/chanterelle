module Chanterelle.Internal.Logging
    ( LogLevel(..)
    , log
    , setLogLevel
    , readLogLevel
    , logCompileError
    , logDeployError
    , logSolcError
    ) where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Chanterelle.Internal.Types.Compile as Compile
import Chanterelle.Internal.Types.Deploy as Deploy
import Chanterelle.Internal.Utils.Time (now, toISOString)
import Control.Logger as Logger
import Data.Array (intercalate)
import Data.Maybe (fromMaybe)
import Data.String (joinWith, toUpper)
import Data.Traversable (for_)
import Data.Unfoldable as Unfoldable
import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Console as Console
import Language.Solidity.Compiler.Types as ST

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
       "INFO"  -> Info
       "WARN"  -> Warn
       "ERROR" -> Error
       _       -> Info

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
  -- TODO(srghma):
  -- duplication, the information that "Info" is default `currentLevel` is duplicated here AND in `commonOpts`
  -- Expected: make an optparse parser for LogLevel, this info should be only in `commonOpts`
      currentLevel <- liftEffect $ getLogLevelWithDefault Info
      when (level >= currentLevel) $
        Logger.log fancyColorLogger { level, msg }

logSolcError :: forall m
              . MonadEffect m
             => String
             -> ST.CompilationError
             -> m Unit
logSolcError moduleName (ST.SimpleCompilationError msg) = log Error $ "Solidity compiler error in module " <> moduleName <> ":\n" <> msg
logSolcError moduleName (ST.FullCompilationError err) = log severity $ "Solidity compiler " <> severityStr <> " in module " <> moduleName <> ":\n" <> msg
  where { severity, severityStr } =
          case err.severity of
            ST.SeverityError   -> { severity: Error, severityStr: "error" }
            ST.SeverityWarning -> { severity: Warn,  severityStr: "warning" }
        msg = fromMaybe builtMsg err.formattedMessage
        builtMsg = show err.type <> ", in " <> err.component <> ": " <> err.message <> locations
        rawLocations = map (append "at: " <<< show) ((Unfoldable.fromMaybe err.sourceLocation) <> err.secondarySourceLocations)
        locations = joinWith "\n" rawLocations

logCompileError :: forall m
                 . MonadEffect m
                => Compile.CompileError
                -> m Unit
logCompileError = case _ of
    Compile.CompileParseError msg     -> log Error (parseErrorMessage msg)
    Compile.MissingArtifactError msg  -> log Error (artifactErrorMessage msg)
    Compile.FSError errMsg            -> log Error ("File System Error -- " <> errMsg)
    Compile.CompilationError e        -> for_ e.errors (logSolcError e.moduleName)
    Compile.MalformedProjectError mpe -> log Error ("Couldn't parse chanterelle.json: " <> mpe)
    Compile.UnexpectedSolcOutput e    -> log Error ("Unexpected output from solc: " <> e)
    Compile.CompilerUnavailable e     -> log Error ("Solidity compiler unavailable: " <> e)
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
    Deploy.DeployingUnlinkedBytecodeError msg -> log Error (msg.name <> " has unlinked references to libraries: " <> intercalate ", " msg.libs)
    Deploy.LinkingLinkedBytecodeError msg -> log Error ("Attempted to link library " <> msg.libraryName <> " to the " <> msg.bytecodeKind <> " of " <> msg.name <> ", which is already fully linked...")
    Deploy.LinkingError msg -> log Error ("Error while linking " <> msg.libraryName <> " at " <> show msg.libraryAddress <> " to the " <> msg.bytecodeKind <> " of " <> msg.contractName <> ": " <> msg.msg)
    Deploy.Impossibility msg -> log Error ("The impossible happened! " <> msg)

  where
    onDeployMessage   msg = "Error During Deployment -- Name: " <> msg.name <> ", Message: " <> msg.message
    postDeployMessage msg = "Error After Deployment -- Name: " <> msg.name <> ", Message: " <> msg.message
