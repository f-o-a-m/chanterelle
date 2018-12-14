module Chanterelle.Internal.Utils.Time where

import Effect (Effect)
import Data.Time.Duration (Milliseconds)

foreign import data Time :: Type

foreign import now :: Effect Time

foreign import toEpoch :: Time -> Milliseconds

foreign import toISOString :: Time -> String


