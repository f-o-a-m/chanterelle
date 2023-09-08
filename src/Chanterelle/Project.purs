module Chanterelle.Project (loadProject) where

import Prelude

import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types.Project (ChanterelleModule(..), ChanterelleModuleType(..), ChanterelleProject(..), ChanterelleProjectSpec(..), Libraries(..), Library(..), mkChanterelleSolc)
import Chanterelle.Internal.Utils.FS (assertDirectory, fileModTime, readTextFile, writeTextFile)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Argonaut (printJsonDecodeError)
import Data.Argonaut as A
import Data.Argonaut.Parser as AP
import Data.Array (last)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
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

mkProjectSolc
  :: Maybe String
  -> FilePath
  -> Aff (Either String SolidityCompiler)
mkProjectSolc version artifactPath = runExceptT $
  case version of
    Nothing -> useCompiler { compilerOrigin: "Default", compiler: Solc.defaultCompiler }
    Just v -> fetchOrCacheCompiler v >>= useCompiler
  where
  useCompiler { compilerOrigin, compiler } = do
    log Info $ compilerOrigin <> " compiler's reported version is " <> Solc.version compiler
    pure compiler
  fetchOrCacheCompiler v = do
    let
      compilerCacheDirectory = Path.concat [ artifactPath, "__compiler" ]
      compilerCacheFile = Path.concat [ compilerCacheDirectory, v ]
    cacheAttempt <- runExceptT (readTextFile compilerCacheFile)
    case cacheAttempt of
      Right src -> do
        log Info $ "Using cached solc " <> v <> " at " <> compilerCacheFile
        compiler <- Solc.useCompiler src
        pure { compilerOrigin: "Cached", compiler }
      Left _ -> do
        log Info $ "Downloading solc " <> v <> " to " <> compilerCacheFile
        assertDirectory compilerCacheDirectory
        source <- ExceptT $ SolcReleases.getReleaseSource SolcReleases.defaultReleaseRepo v
        writeTextFile compilerCacheFile source
        log Info $ "solc " <> v <> " download completed..."
        compiler <- Solc.useCompiler source
        pure { compilerOrigin: "Downloaded", compiler }

loadProject
  :: forall m
   . MonadAff m
  => MonadThrow Error m
  => FilePath
  -> m ChanterelleProject
loadProject root = do
  let fullChanterelleJsonPath = (Path.concat [ root, "chanterelle.json" ])
  specModTime <- do
    especModTime <- liftAff <<< attempt $ fileModTime fullChanterelleJsonPath
    either (const $ throwError $ error "Error reading chanterelle.json, make sure this file exists.") pure especModTime
  spec@(ChanterelleProjectSpec project) <- do
    -- previous line would have errored if the file doesn't exist, so just need to check parsing.
    specJson <- liftAff $ FS.readTextFile UTF8 fullChanterelleJsonPath
    case AP.jsonParser specJson >>= lmap printJsonDecodeError <<< A.decodeJson of
      Left err -> throwError $ error $ "Error parsing chanterelle.json: " <> err
      Right a -> pure a
  let
    (Libraries libs) = project.libraries
    jsonOut = Path.concat [ root, project.artifactsDir ]
    libJsonOut = Path.concat [ root, project.libArtifactsDir ]
    psOut = Path.concat [ root, project.psGen.outputPath ]
    srcIn = Path.concat [ root, project.sourceDir ]
    modules = mkModule <$> project.modules
    libModules = mkLibModule <$> libs
    mkModule moduleName =
      let
        solPath = Path.concat [ srcIn, pathModName <> ".sol" ]
        jsonPath = Path.concat [ jsonOut, pathModName <> ".json" ]
        pursPath = Path.concat [ psOut, psModBase, pathModName <> ".purs" ]
        solContractName = fromMaybe moduleName <<< last $ split (Pattern ".") moduleName
        pathModName = replaceAll (Pattern ".") (Replacement Path.sep) moduleName
        psModBase = replaceAll (Pattern ".") (Replacement Path.sep) project.psGen.modulePrefix
        moduleType = ContractModule
      in
        ChanterelleModule { moduleName, solContractName, moduleType, solPath, jsonPath, pursPath }
    mkLibModule (Library lib) =
      let
        solPath = Path.concat [ fromMaybe "" lib.sourceRoot, lib.sourceFile ]
        jsonPath = Path.concat [ libJsonOut, lib.name <> ".json" ]
        pursPath = ""
        moduleName = lib.name
        solContractName = moduleName
        moduleType = LibraryModule
      in
        ChanterelleModule { moduleName, solContractName, moduleType, solPath, jsonPath, pursPath }
  solc <- mkChanterelleSolc $ mkProjectSolc project.solcVersion project.artifactsDir
  pure $ ChanterelleProject { root, srcIn, jsonOut, psOut, spec, modules, libModules, specModTime, solc }
