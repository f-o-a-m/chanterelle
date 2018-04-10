module Compile where

import Prelude

import Compile.Internal (compile)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Argonaut as A
import Data.Argonaut.Parser as AP
import Data.Either (fromRight)
import Data.GeneratorMain (generatorMain)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS
import Node.Process as P
import Partial.Unsafe (unsafePartial)

main :: forall e. Eff (console :: CONSOLE, fs :: FS.FS, exception :: EXCEPTION, process :: P.PROCESS | e) Unit
main = void <<< launchAff $ do
  root <- liftEff P.cwd
  projectJson <- FS.readTextFile UTF8 "chanterelle.json"
  let project = unsafePartial fromRight (AP.jsonParser projectJson >>= A.decodeJson)
  _ <- compile root project
  liftEff $ generatorMain
