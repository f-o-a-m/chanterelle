module Chanterelle.Internal.Logging
    ( LogLevel(..)
    , log
    , setLogLevel
    ) where

import Prelude
import Control.Logger as Logger
import Control.Monad.Eff.Class (liftEff, class MonadEff)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.JSDate (now, toISOString)

data LogLevel = Debug | Info | Warn | Error

derive instance eqLogLevel   :: Eq LogLevel
derive instance ordLogLevel  :: Ord LogLevel

instance showLogLevel :: Show LogLevel where
    show Debug = "DEBUG"
    show Info  = "INFO"
    show Warn  = "WARN"
    show Error = "ERROR"

foreign import getLogLevel :: forall eff m. MonadEff eff m => LogLevel -> m LogLevel
foreign import setLogLevel :: forall eff m. MonadEff eff m => LogLevel -> m Unit

log :: forall eff m.
       MonadEff eff m
    => LogLevel
    -> String
    -> m Unit
log level msg = Logger.log toConsole unit
    where toConsole = Logger.Logger $ \_ -> liftEff <<< unsafeCoerceEff $ do
              glog <- getLogLevel Info
              if level >= glog
                then do
                    dt <- now
                    iso <- toISOString dt
                    liftEff $ Console.log (iso <> " [" <> show level <> "] " <> msg)
                else pure unit
