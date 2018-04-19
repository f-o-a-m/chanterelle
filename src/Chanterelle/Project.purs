module Chanterelle.Project (loadProject) where

import Prelude
import Chanterelle.Internal.Types.Project (ChanterelleProject(..), ChanterelleProjectSpec(..), ChanterelleModule(..))
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut as A
import Data.Argonaut.Parser as AP
import Data.Array (last)
import Data.Either (either)
import Data.Maybe (fromJust)
import Data.String (Pattern(..), Replacement(..), replaceAll, split)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path
import Partial.Unsafe (unsafePartialBecause)

loadProject
  :: forall eff m
   . MonadAff (fs :: FS | eff) m
  => MonadThrow Error m
  => FilePath
  -> m ChanterelleProject
loadProject root = do
  specJson <- liftAff $ FS.readTextFile UTF8 "chanterelle.json"
  spec@(ChanterelleProjectSpec project) <- either (throwError <<< error)
                                             pure (AP.jsonParser specJson >>= A.decodeJson)
  let jsonOut  = Path.concat [root, project.artifactsDir]
      psOut    = Path.concat [root, project.psGen.outputPath]
      srcIn    = Path.concat [root, project.sourceDir]
      modules  = mkModule <$> project.modules
      mkModule moduleName =
        let solPath      = Path.concat [srcIn, pathModName <> ".sol"]
            jsonPath     = Path.concat [jsonOut, pathModName <> ".json"]
            pursPath     = Path.concat [psOut, psModBase, pathModName <> ".purs"]
            solContractName = unsafePartialBecause "String.split always returns a non-empty Array" $
                                fromJust $ last $ split (Pattern ".") moduleName
            pathModName = replaceAll (Pattern ".") (Replacement Path.sep) moduleName
            psModBase = replaceAll (Pattern ".") (Replacement Path.sep) project.psGen.modulePrefix
         in ChanterelleModule { moduleName, solContractName, solPath, jsonPath, pursPath }
  pure $ ChanterelleProject { root, srcIn, jsonOut, psOut, spec, modules }
