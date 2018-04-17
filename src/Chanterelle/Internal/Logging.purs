module Chanterelle.Internal.Logging
    ( LogLevel(..)
    , log
    , setLogLevel
    , readLogLevel
    ) where

import Prelude
import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Control.Logger as Logger
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff, class MonadEff)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.JSDate (now, toISOString)
import Data.String (toUpper)

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

foreign import getLogLevelWithDefault :: forall eff. LogLevel -> Eff eff LogLevel
foreign import setLogLevel :: forall eff. LogLevel -> Eff eff Unit

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
  logify = id

fancyColorLogger :: forall eff m a
                  . MonadEff eff m
                 => Loggable a
                 => Logger.Logger m { level :: LogLevel, msg :: a }
fancyColorLogger = Logger.Logger $ \{ level, msg } -> liftEff <<< unsafeCoerceEff $ do
    dt <- now
    iso <- toISOString dt
    Console.log $ colorize level (iso <> " [" <> show level <> "] " <> logify msg)
  where
    colorize level = withGraphics (foreground $ logLevelColor level)
    logLevelColor = case _ of
      Debug -> White
      Info  -> Green
      Warn  -> Yellow
      Error -> Red

log :: forall eff m
     . MonadEff eff m
    => LogLevel
    -> String
    -> m Unit
log level msg = do
      currentLevel <- liftEff $ getLogLevelWithDefault Info
      when (level >= currentLevel) $
        Logger.log fancyColorLogger { level, msg }