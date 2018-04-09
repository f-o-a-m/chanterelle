module Chanterelle.Main where

import Prelude
import Data.Argonaut as A
import Data.Argonaut.Parser as AP
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (fromRight)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process (PROCESS)
import Node.Process as P
import Partial.Unsafe (unsafePartial)

import Chanterelle.Internal.Compile as Chanterelle
import Chanterelle.Internal.Types (ChanterelleProject(..), ChanterelleProjectSpec(..))

import Debug.Trace (traceA)

loadProject :: forall m eff.
               MonadAff (fs :: FS | eff) m 
            => FilePath
            -> m ChanterelleProject
loadProject root = liftAff $ do
  specJson <- FS.readTextFile UTF8 "chanterelle.json"
  let spec@(ChanterelleProjectSpec project) = unsafePartial fromRight (AP.jsonParser specJson >>= A.decodeJson)
      jsonOut = Path.concat [root, "build", project.sourceDir]
      psOut   = Path.concat [root, project.psGen.outputPath]
      srcIn   = Path.concat [root, project.sourceDir]
  pure $ ChanterelleProject { root, srcIn, jsonOut, psOut, spec }

main :: forall e. Eff (console :: CONSOLE, fs :: FS.FS, exception :: EXCEPTION, process :: PROCESS | e) Unit
main = void <<< launchAff $ do
  root <- liftEff P.cwd
  project <- loadProject root
  void $ Chanterelle.compile project
  Chanterelle.generate project