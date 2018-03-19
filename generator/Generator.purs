module Generator where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.GeneratorMain (generatorMain)
import Node.FS (FS)

main :: forall e. Eff (fs :: FS, console :: CONSOLE, exception :: EXCEPTION | e) Unit
main = generatorMain
