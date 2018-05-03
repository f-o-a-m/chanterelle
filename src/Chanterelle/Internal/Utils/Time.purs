module Chanterelle.Internal.Utils.Time where

import Control.Monad.Eff (Eff)
import Data.Time.Duration (Milliseconds)

foreign import data Time :: Type

foreign import now :: forall eff. Eff eff Time

foreign import toEpoch :: Time -> Milliseconds

foreign import toISOString :: Time -> String


