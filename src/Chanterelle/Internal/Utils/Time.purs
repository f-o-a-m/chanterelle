module Chanterelle.Internal.Utils.Time where

import Effect (Effect)
import Data.Time.Duration (Milliseconds)

foreign import data Time :: Type

-- https://pursuit.purescript.org/packages/purescript-js-date/7.0.0/docs/Data.JSDate#v:now
foreign import now :: Effect Time

-- https://pursuit.purescript.org/packages/purescript-js-date/7.0.0/docs/Data.JSDate#v:getMilliseconds
foreign import toEpoch :: Time -> Milliseconds

-- https://pursuit.purescript.org/packages/purescript-js-date/7.0.0/docs/Data.JSDate#v:toISOString
foreign import toISOString :: Time -> String


