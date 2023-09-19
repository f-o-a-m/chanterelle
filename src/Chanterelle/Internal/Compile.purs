module Chanterelle.Internal.Compile
  ( compile
  , makeSolcInput
  , compileModuleWithoutWriting
  , decodeModuleOutput
  , resolveModuleContract
  ) where

import Prelude

import Chanterelle.Internal.Artifact (writeArtifact)
import Chanterelle.Internal.Utils.Error (withExcept', withExceptM', withExceptT')
import Chanterelle.Internal.Utils.FS (assertDirectory', fileIsDirty)
import Chanterelle.Logging (LogLevel(..), log, logSolcError)
import Chanterelle.Types.Artifact (Artifact(..))
import Chanterelle.Types.Bytecode (Bytecode(..), flattenLinkReferences)
import Chanterelle.Types.Compile (CompileError(..))
import Chanterelle.Types.Project (ChanterelleModule(..), ChanterelleProject(..), ChanterelleProjectSpec(..), Dependency(..), getSolc, partitionSelectionSpecs)
import Control.Error.Util (note)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Argonaut (decodeJson, printJsonDecodeError)
import Data.Argonaut as A
import Data.Argonaut.Parser as AP
import Data.Array (catMaybes, partition)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (un)
import Data.String (Pattern(..), stripPrefix)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (attempt)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (catchException)
import Foreign.Object as FO
import Foreign.Object as M
import Language.Solidity.Compiler (compile) as Solc
import Language.Solidity.Compiler.Types as ST
import Network.Ethereum.Core.HexString (fromByteString)
import Network.Ethereum.Core.Keccak256 (keccak256)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS
import Node.FS.Sync as FSS
import Node.Path (FilePath)
import Node.Path as Path

-- | compile and write the artifact
compile
  :: forall m
   . MonadAff m
  => MonadThrow CompileError m
  => MonadAsk ChanterelleProject m
  => m (M.Object (Tuple ChanterelleModule ST.CompilerOutput))
compile = do
  dirtyModules <- modulesToCompile
  solcInputs <- for dirtyModules $ \m@(ChanterelleModule mod) -> do
    input <- makeSolcInput mod.solContractName mod.solPath
    pure $ Tuple m input
  solcOutputs <- for solcInputs compileModule
  pure $ M.fromFoldable solcOutputs

-- | Get all of the dirty modules that need to be recompiled
-- NOTE: This is pretty ugly because there are many things that could go wrong,
-- should probably fix.
modulesToCompile
  :: forall m
   . MonadAff m
  => MonadThrow CompileError m
  => MonadAsk ChanterelleProject m
  => m (Array ChanterelleModule)
modulesToCompile = do
  (ChanterelleProject project) <- ask
  mModules <- for (project.modules <> project.libModules) $ \m@(ChanterelleModule mod) -> do
    ejson <- liftAff $ attempt $ FS.readTextFile UTF8 mod.jsonPath
    case ejson of
      Left _ -> pure $ Just m
      Right json -> case hush (AP.jsonParser json) >>= \json' -> json' ^? A._Object <<< ix "compiledAt" <<< A._Number of
        Nothing -> log Debug ("Couldn't find 'compiledAt' timestamp for dirty file checking: " <> mod.jsonPath) *> pure (Just m)
        Just compiledAt -> do
          eIsDirty <- liftAff <<< attempt $ fileIsDirty mod.solPath (Milliseconds compiledAt) project.specModTime
          case eIsDirty of
            Left _ -> throwError $ MissingArtifactError { fileName: mod.solPath, objectName: mod.solContractName }
            Right isDirty ->
              if not isDirty then log Debug ("File is clean: " <> mod.solPath) *> pure Nothing
              else pure (Just m)
  pure $ catMaybes mModules

compileModule
  :: forall m
   . MonadAff m
  => MonadThrow CompileError m
  => MonadAsk ChanterelleProject m
  => Tuple ChanterelleModule ST.CompilerInput
  -> m (Tuple String (Tuple ChanterelleModule ST.CompilerOutput))
compileModule (Tuple m@(ChanterelleModule mod) solcInput) = do
  output <- compileModuleWithoutWriting m solcInput
  writeBuildArtifact mod.solContractName mod.jsonPath output mod.solContractName
  pure $ Tuple mod.moduleName (Tuple m output)

compileModuleWithoutWriting
  :: forall m
   . MonadAff m
  => MonadThrow CompileError m
  => MonadAsk ChanterelleProject m
  => ChanterelleModule
  -> ST.CompilerInput
  -> m ST.CompilerOutput
compileModuleWithoutWriting m@(ChanterelleModule mod) solcInput = do
  (ChanterelleProject project) <- ask
  solc <- withExceptM' CompilerUnavailable $ getSolc project.solc
  log Info ("compiling " <> show mod.moduleType <> " " <> mod.moduleName)
  output <- Solc.compile solc solcInput (loadSolcCallback m project.root project.spec) --liftEffect $ runFn2 _compile (A.stringify $ encodeJson solcInput) (loadSolcCallback m project.root project.spec)
  case output of
    Left err -> throwError $ CompileParseError { objectName: "Solidity Compiler", parseError: err }
    Right output' -> pure output'

-- | load a file when solc requests it
-- | TODO: secure it so that it doesnt try loading crap like /etc/passwd, etc. :P
-- | TODO: be more clever about dependency resolution, that way we don't even have to do
-- |       any remappings!
loadSolcCallback
  :: ChanterelleModule
  -> FilePath
  -> ChanterelleProjectSpec
  -> String
  -> Effect (Either String String)
loadSolcCallback (ChanterelleModule mod) root (ChanterelleProjectSpec _) filePath = do
  let
    modRoot = Path.dirname mod.solPath
    isAbs = Path.isAbsolute filePath
    modRootWithoutRoot = fromMaybe modRoot $ stripPrefix (Pattern root) modRoot
    fullPath =
      if isAbs then filePath
      else Path.normalize (Path.concat [ root, modRootWithoutRoot, filePath ])
  log Debug ("Root: " <> root <> " :: modRoot: " <> modRoot <> " :: Solc load: " <> filePath <> " -> " <> fullPath)
  catchException (pure <<< Left <<< show) (Right <$> (FSS.readTextFile UTF8 fullPath))

makeSolcSource
  :: String
  -> ST.Source
makeSolcSource sourceCode =
  ST.FromContent
    { content: sourceCode
    , keccak256: Just $ fromByteString $ keccak256 sourceCode
    }

--------------------------------------------------------------------------------
-- | SolcInput
--------------------------------------------------------------------------------

makeSolcInput
  :: forall m
   . MonadAff m
  => MonadAsk ChanterelleProject m
  => String
  -> FilePath
  -> m ST.CompilerInput
makeSolcInput moduleName sourcePath = do
  (ChanterelleProject project) <- ask
  let (ChanterelleProjectSpec spec) = project.spec
  code <- liftAff $ FS.readTextFile UTF8 sourcePath
  let
    language = ST.Solidity
    sources = ST.Sources (M.singleton (moduleName <> ".sol") (makeSolcSource code))
    { cls: requestedContractSelections, fls: requestedFileSelections } = partitionSelectionSpecs spec.solcOutputSelection
    contractLevelSelections = [ ST.ABI, ST.EvmOutputSelection (Just $ ST.BytecodeSelection Nothing), ST.EvmOutputSelection (Just $ ST.DeployedBytecodeSelection Nothing) ] <> requestedContractSelections
    outputSelection = Just $ ST.OutputSelections (M.singleton "*" (ST.OutputSelection { file: requestedFileSelections, contract: M.singleton "*" contractLevelSelections }))
    depMappings = (\(Dependency dep) -> ST.Remapping { from: dep, to: (project.root <> "/node_modules/" <> dep) }) <$> spec.dependencies
    sourceDirMapping = [ ST.GlobalRemapping { to: (Path.concat [ project.root, spec.sourceDir ]) } ]
    remappings = sourceDirMapping <> depMappings
    optimizer = spec.solcOptimizerSettings
    evmVersion = spec.solcEvmVersion
    metadata = Nothing
    libraries = Nothing
    settings = Just (ST.CompilerSettings { remappings, optimizer, evmVersion, metadata, libraries, outputSelection })
  pure $ ST.CompilerInput { language, sources, settings }

--------------------------------------------------------------------------------
-- | Solc Output
--------------------------------------------------------------------------------

decodeModuleOutput
  :: forall m
   . MonadEffect m
  => MonadThrow CompileError m
  => String
  -> ST.CompilerOutput
  -> m (ST.ContractMapped ST.ContractLevelOutput)
decodeModuleOutput moduleName (ST.CompilerOutput output) = do
  let
    moduleFilename = moduleName <> ".sol"
    isSolcWarning (ST.FullCompilationError se) = se.severity == ST.SeverityWarning
    isSolcWarning _ = false
    ({ yes: warnings, no: errors }) = partition isSolcWarning output.errors
    isModuleError (ST.FullCompilationError se) = fromMaybe false ado
      ST.SourceLocation fesl <- se.sourceLocation
      in fesl.file == moduleFilename
    isModuleError _ = false
    ({ yes: moduleErrors, no: otherErrors }) = partition isModuleError errors
  for_ (warnings <> otherErrors) (logSolcError moduleName)
  unless (moduleErrors == mempty) $ throwError $ CompilationError { moduleName, errors: moduleErrors }
  case M.lookup moduleFilename output.contracts of
    Nothing -> throwError $ CompilationError { moduleName, errors }
    Just contractMap' -> pure contractMap'

resolveModuleContract
  :: forall m
   . MonadAff m
  => MonadThrow CompileError m
  => FilePath
  -> String
  -> ST.ContractMapped ST.ContractLevelOutput
  -> m ST.ContractLevelOutput
resolveModuleContract fileName objectName decodedOutputs =
  case M.lookup objectName decodedOutputs of
    Nothing -> throwError $ MissingArtifactError { fileName, objectName }
    Just co' -> pure co'

writeBuildArtifact
  :: forall m
   . MonadAff m
  => MonadThrow CompileError m
  => String
  -> FilePath
  -> ST.CompilerOutput
  -> String
  -> m Unit
writeBuildArtifact srcName filepath output solContractName = do
  co <- decodeModuleOutput srcName output
  co' <- resolveModuleContract filepath solContractName co
  outputArtifact <- resolveSolidityContractLevelOutput co'
  assertDirectory' (Path.dirname filepath)
  withExceptT' FSError $ writeArtifact filepath outputArtifact

resolveSolidityContractLevelOutput
  :: forall m
   . MonadThrow CompileError m
  => ST.ContractLevelOutput
  -> m Artifact
resolveSolidityContractLevelOutput = withExcept' UnexpectedSolcOutput <<< fromSolidityContractLevelOutput
  where

  fromSolidityBytecodeOutput :: ST.BytecodeOutput -> Either String Bytecode
  fromSolidityBytecodeOutput (ST.BytecodeOutput o) = do
    rawBytecode <- note "Solidity bytecode output lacked an \"object\" field" o.object
    let linkReferences = maybe FO.empty (flattenLinkReferences <<< un ST.LinkReferences) o.linkReferences
    pure $ case rawBytecode of
      ST.BytecodeHexString bytecode -> BCLinked { bytecode, linkReferences }
      _ -> BCUnlinked { rawBytecode, linkReferences, remainingLinkReferences: linkReferences }

  fromSolidityContractLevelOutput :: ST.ContractLevelOutput -> Either String Artifact
  fromSolidityContractLevelOutput (ST.ContractLevelOutput clo) = do
    abi <- lmap printJsonDecodeError <<< decodeJson =<< note "Solidity contract output did not have an \"abi\" field" clo.abi
    (ST.EvmOutput evm) <- note "Solidity contract output did not have an \"evm\" field" clo.evm
    bytecode' <- note "Solidity contract output did not have an \"evm.bytecode\" field" evm.bytecode
    bytecode <- fromSolidityBytecodeOutput bytecode'
    deployedBytecode' <- note "Solidity contract output did not have an \"evm.deployedBytecode\" field" evm.deployedBytecode
    deployedBytecode <- fromSolidityBytecodeOutput deployedBytecode'
    let lastModified = top
    pure $ Artifact { abi, code: { bytecode, deployedBytecode }, lastModified, networks: FO.empty }
