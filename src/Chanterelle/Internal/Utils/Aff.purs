module Chanterelle.Internal.Utils.Aff
  ( attemptWithTimeout
  , withTimeout
  ) where

import Prelude
import Effect.Aff (Aff, Milliseconds, attempt, delay)
import Control.Monad.Error.Class (throwError)
import Effect.Exception (Error, error)
import Control.Parallel (parOneOf)
import Data.Either (Either)

-- | try an aff action for the specified amount of time before giving up.
withTimeout
  :: forall a
   . Milliseconds
  -> Aff a
  -> Aff a
withTimeout maxTimeout action =
  let timeout = delay maxTimeout *> throwError (error "timed out")
   in parOneOf [action, timeout]

attemptWithTimeout
  :: forall a
   . Milliseconds
  -> Aff a
  -> Aff (Either Error a)
attemptWithTimeout t = attempt <<< withTimeout t
