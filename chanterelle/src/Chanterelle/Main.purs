module Chanterelle.Main where

import Prelude

import Chanterelle.Internal.Codegen (generatePS) as Chanterelle
import Chanterelle.Internal.Compile (compile) as Chanterelle
import Chanterelle.Internal.Types (ChanterelleProject(..), ChanterelleProjectSpec(..), ChanterelleModule(..), CompileError(..), runCompileM, logCompileError)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut as A
import Data.Argonaut.Parser as AP
import Data.Array (last)
import Data.Either (Either(..), either)
import Data.Maybe (fromJust)
import Data.String (Pattern(..), Replacement(..), replaceAll, split)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process (PROCESS)
import Node.Process as P
import Partial.Unsafe (unsafePartialBecause)

loadProject
  :: forall m eff.
     MonadAff (fs :: FS | eff) m
  => MonadThrow CompileError m
  => FilePath
  -> m ChanterelleProject
loadProject root = do
  specJson <- liftAff $ FS.readTextFile UTF8 "chanterelle.json"
  spec@(ChanterelleProjectSpec project) <- either (throwError <<< CompileParseError) pure (AP.jsonParser specJson >>= A.decodeJson)
  let jsonOut  = Path.concat [root, "build", project.sourceDir]
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
main = do
    root <- liftEff P.cwd
    void $ launchAff $ do
      eres <- runCompileM $ do
        project <- loadProject root
        _ <- Chanterelle.compile project
        Chanterelle.generatePS project
      case eres of
        Right _ -> pure unit
        Left err -> logCompileError err
