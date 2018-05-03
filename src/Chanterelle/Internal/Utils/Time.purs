module Chanterelle.Internal.Utils.Time where

import Control.Monad.Eff (Eff)
import Data.Time.Duration (Milliseconds)

foreign import now :: forall eff. Eff eff Milliseconds
