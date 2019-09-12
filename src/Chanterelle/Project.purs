module Chanterelle.Project (loadProject) where

import Prelude

import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types.Project (ChanterelleModule(..), ChanterelleModuleType(..), ChanterelleProject(..), ChanterelleProjectSpec(..), InjectableLibraryCode(..), Libraries(..), Library(..), mkChanterelleSolc)
import Chanterelle.Internal.Utils.FS (assertDirectory, fileModTime, readTextFile, writeTextFile)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Argonaut as A
import Data.Argonaut.Parser as AP
import Data.Array (catMaybes, last)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.String (Pattern(..), Replacement(..), replaceAll, split)
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error, error)
import Language.Solidity.Compiler (SolidityCompiler)
import Language.Solidity.Compiler as Solc
import Language.Solidity.Compiler.Releases as SolcReleases
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path
import Partial.Unsafe (unsafePartialBecause)

mkProjectSolc
  :: Maybe String
  -> FilePath
  -> Aff (Either String SolidityCompiler)
mkProjectSolc Nothing _ = pure (Right Solc.defaultCompiler)
mkProjectSolc (Just v) artifactPath = map Solc.useCompiler <$> fetchOrCacheCompiler
  where fetchOrCacheCompiler = do
          let compilerCacheDirectory = Path.concat [ artifactPath, "__compiler"]
              compilerCacheFile = Path.concat [ compilerCacheDirectory, v]
          cacheAttempt <- runExceptT (readTextFile compilerCacheFile)
          case cacheAttempt of
            Right src -> do
              log Info $ "Using cached solc " <> " at " <> compilerCacheFile
              pure (Right src)
            Left err -> runExceptT do
              log Info $ "Downloading solc " <> v <> " to " <> compilerCacheFile
              assertDirectory compilerCacheDirectory
              source <- ExceptT $ SolcReleases.getReleaseSource SolcReleases.defaultReleaseRepo v
              writeTextFile compilerCacheFile source
              log Info $ "solc " <> v <> " download completed..."
              pure source

loadProject
  :: forall m
   . MonadAff m
  => MonadThrow Error m
  => FilePath
  -> m ChanterelleProject
loadProject root = do
  let fullChanterelleJsonPath = (Path.concat [root, "chanterelle.json"])
  specModTime <- do
    especModTime <- liftAff <<< attempt $ fileModTime fullChanterelleJsonPath
    either (const $ throwError $ error "Error reading chanterelle.json, make sure this file exists.") pure especModTime
  spec@(ChanterelleProjectSpec project) <- do
    -- previous line would have errored if the file doesn't exist, so just need to check parsing.
    specJson <- liftAff $ FS.readTextFile UTF8 fullChanterelleJsonPath
    case AP.jsonParser specJson >>= A.decodeJson of
      Left err -> throwError $ error $ "Error parsing chanterelle.json: " <> err
      Right a -> pure a
  let (Libraries libs) = project.libraries
      jsonOut    = Path.concat [root, project.artifactsDir]
      libJsonOut = Path.concat [root, project.libArtifactsDir]
      psOut      = Path.concat [root, project.psGen.outputPath]
      srcIn      = Path.concat [root, project.sourceDir]
      modules    = mkModule <$> project.modules
      libModules = catMaybes $ mkLibModule <$> libs
      mkModule moduleName =
        let solPath      = Path.concat [srcIn, pathModName <> ".sol"]
            jsonPath     = Path.concat [jsonOut, pathModName <> ".json"]
            pursPath     = Path.concat [psOut, psModBase, pathModName <> ".purs"]
            solContractName = unsafePartialBecause "String.split always returns a non-empty Array" $
                                fromJust $ last $ split (Pattern ".") moduleName
            pathModName = replaceAll (Pattern ".") (Replacement Path.sep) moduleName
            psModBase = replaceAll (Pattern ".") (Replacement Path.sep) project.psGen.modulePrefix
            moduleType = ContractModule
         in ChanterelleModule { moduleName, solContractName, moduleType, solPath, jsonPath, pursPath }
      mkLibModule l = case l of
        InjectableLibrary lib -> case lib.code of
          InjectableWithSourceCode libRoot libPath ->
            let solPath  = Path.concat [maybe "" identity libRoot, libPath]
                jsonPath = Path.concat [libJsonOut, lib.name <> ".json"]
                pursPath = ""
            in Just $ ChanterelleModule { moduleName: lib.name, solContractName: lib.name, moduleType: LibraryModule, solPath, jsonPath, pursPath }
          _ -> Nothing
        DeployableLibrary lib ->
          let solPath  = Path.concat [maybe "" identity lib.sourceRoot, lib.sourceCode]
              jsonPath = Path.concat [libJsonOut, lib.name <> ".json"]
              pursPath = ""
           in Just $ ChanterelleModule { moduleName: lib.name, solContractName: lib.name, moduleType: LibraryModule, solPath, jsonPath, pursPath }
        _ -> Nothing
  solc <- mkChanterelleSolc $ mkProjectSolc project.solcVersion project.artifactsDir
  pure $ ChanterelleProject { root, srcIn, jsonOut, psOut, spec, modules, libModules, specModTime, solc }
