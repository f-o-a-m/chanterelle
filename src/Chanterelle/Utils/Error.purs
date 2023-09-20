module Chanterelle.Utils.Error where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExceptT, withExceptT)
import Control.Monad.Except.Trans (ExceptT)
import Data.Either (either)

withExceptT'
  :: forall m e e' a
   . MonadThrow e' m
  => (e -> e')
  -> ExceptT e m a
  -> m a
withExceptT' e m = runExceptT (withExceptT e m) >>= either throwError pure
