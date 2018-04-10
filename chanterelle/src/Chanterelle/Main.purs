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
import Data.Array (last)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.String (Pattern(..), Replacement(..), replaceAll, split)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process (PROCESS)
import Node.Process as P

import Partial.Unsafe (unsafePartial, unsafePartialBecause)

import Chanterelle.Internal.Compile (compile) as Chanterelle
import Chanterelle.Internal.Codegen (generatePS) as Chanterelle
import Chanterelle.Internal.Types (ChanterelleProject(..), ChanterelleProjectSpec(..), ChanterelleModule(..))

loadProject :: forall m eff.
               MonadAff (fs :: FS | eff) m 
            => FilePath
            -> m ChanterelleProject
loadProject root = liftAff $ do
  specJson <- FS.readTextFile UTF8 "chanterelle.json"
  let spec@(ChanterelleProjectSpec project) = unsafePartial fromRight (AP.jsonParser specJson >>= A.decodeJson)
      jsonOut  = Path.concat [root, "build", project.sourceDir]
      psOut    = Path.concat [root, project.psGen.outputPath]
      srcIn    = Path.concat [root, project.sourceDir]
      modules  = mkModule <$> project.modules
      mkModule moduleName = 
        let solPath      = Path.concat [srcIn, pathModName <> ".sol"]
            jsonPath     = Path.concat [jsonOut, pathModName <> ".json"]
            pursPath     = Path.concat [psOut, psModBase, pathModName <> ".purs"]
            solContractName = unsafePartialBecause "String.split always returns a non-empty Array" $ fromJust $ last $ split (Pattern ".") moduleName
            pathModName = replaceAll (Pattern ".") (Replacement Path.sep) moduleName
            psModBase = replaceAll (Pattern ".") (Replacement Path.sep) project.psGen.modulePrefix
         in ChanterelleModule { moduleName, solContractName, solPath, jsonPath, pursPath }
  pure $ ChanterelleProject { root, srcIn, jsonOut, psOut, spec, modules }

main :: forall e. Eff (console :: CONSOLE, fs :: FS.FS, exception :: EXCEPTION, process :: PROCESS | e) Unit
main = void <<< launchAff $ do
  root <- liftEff P.cwd
  project <- loadProject root
  void $ Chanterelle.compile project
  Chanterelle.generatePS project