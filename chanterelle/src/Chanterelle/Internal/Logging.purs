module Chanterelle.Internal.Logging
    ( LogLevel(..)
    , log
    , setLogLevel
    ) where

import Prelude
import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Control.Logger as Logger
import Control.Monad.Eff.Class (liftEff, class MonadEff)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Data.JSDate (now, toISOString)

data LogLevel = Debug | Info | Warn | Error
derive instance eqLogLevel   :: Eq LogLevel
derive instance ordLogLevel  :: Ord LogLevel
instance showLogLevel :: Show LogLevel where
    show Debug = "DEBUG"
    show Info  = " INFO"
    show Warn  = " WARN"
    show Error = "ERROR"

foreign import getLogLevel :: forall eff m. MonadEff eff m => LogLevel -> m LogLevel
foreign import setLogLevel :: forall eff m. MonadEff eff m => LogLevel -> m Unit

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

  where colorize level = withGraphics (foreground $ logLevelColor level)
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
log level msg = Logger.log filteredLogger { level, msg }
    where filteredLogger = fancyColorLogger # Logger.cfilter levelFilter
          levelFilter m = m.level >= (unsafePerformEff $ getLogLevel Info)