module Chanterelle.Internal.Compile (compile) where

import Prelude (Unit, bind, discard, pure, show, ($), (<$>), (<<<), (<>))
import Control.Monad.Eff.Exception (catchException, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut as A
import Data.Argonaut.Parser as AP
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Node.Path (FilePath)
import Node.Path as Path
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FSS
import Node.FS.Aff as FS
import Node.Process as P
import Data.StrMap as M
import Network.Ethereum.Web3 (HexString, unHex, sha3)
import Chanterelle.Internal.Types (ChanterelleProject(..), ChanterelleProjectSpec(..), ChanterelleModule(..), Dependency(..))
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Utils (assertDirectory)

--------------------------------------------------------------------------------

foreign import data SolcInputCallbackResult :: Type
foreign import solcInputCallbackSuccess :: String -> SolcInputCallbackResult
foreign import solcInputCallbackFailure :: String -> SolcInputCallbackResult
foreign import _compile :: forall eff cbEff. Fn2 String (String -> Eff cbEff SolcInputCallbackResult) (Eff eff String)

-- | compile and write the artifact
compile :: forall eff m.
           MonadAff (fs :: FS.FS, process :: P.PROCESS | eff) m
        => ChanterelleProject
        -> m (M.StrMap (Tuple ChanterelleModule A.Json))
compile (ChanterelleProject project) = do
  let (ChanterelleProjectSpec spec) = project.spec
  solcInputs <- for project.modules $ \(ChanterelleModule mod) -> do
      input <- makeSolcInput project.spec project.root mod.solContractName mod.solPath
      pure $ Tuple mod input
  solcOutputs <-  for solcInputs $ \(Tuple mod solcInput) -> do
      liftEff $ log Info ("compiling " <> mod.moduleName)
      output <- liftEff $ runFn2 _compile (A.stringify $ A.encodeJson solcInput) (loadSolcCallback project.root project.spec)
      case AP.jsonParser output of
        Left err -> liftAff <<< throwError <<< error $ "Malformed solc output: " <> err
        Right output' -> do
          writeBuildArtifact mod.solContractName mod.jsonPath output' mod.solContractName
          pure $ Tuple mod.moduleName (Tuple (ChanterelleModule mod) output')
  pure $ M.fromFoldable solcOutputs

-- | load a file when solc requests it
-- | TODO: secure it so that it doesnt try loading crap like /etc/passwd, etc. :P
-- | TODO: be more clever about dependency resolution, that way we don't even have to do
-- |       any remappings!
loadSolcCallback :: forall eff. FilePath -> ChanterelleProjectSpec -> String -> Eff (fs :: FS.FS | eff) SolcInputCallbackResult
loadSolcCallback root (ChanterelleProjectSpec project) filePath = do
  let isAbs = Path.isAbsolute filePath
      fullPath = if isAbs then filePath else Path.normalize (Path.concat [root, project.sourceDir, filePath])
  liftEff $ log Debug ("solc load: " <> filePath <> " -> " <> fullPath)
  catchException (pure <<< solcInputCallbackFailure <<< show) (solcInputCallbackSuccess <$> (FSS.readTextFile UTF8 fullPath))

--------------------------------------------------------------------------------
-- | SolcInput
--------------------------------------------------------------------------------

newtype SolcInput =
  SolcInput { language :: String
            , sources :: M.StrMap SolcContract
            , settings :: SolcSettings
            }
instance encodeSolcInput :: A.EncodeJson SolcInput where
  encodeJson (SolcInput {language, sources, settings}) =
    "language" A.:= A.fromString "Solidity" A.~>
    "sources" A.:= A.encodeJson sources A.~>
    "settings" A.:= A.encodeJson settings A.~>
    A.jsonEmptyObject

makeSolcInput
  :: forall eff m.
     MonadAff (fs :: FS.FS | eff) m
  => ChanterelleProjectSpec
  -> FilePath
  -> String
  -> FilePath
  -> m SolcInput
makeSolcInput (ChanterelleProjectSpec project) root moduleName sourcePath = do
  code <- liftAff $ FS.readTextFile UTF8 sourcePath
  let language = "Solidity"
      sources = M.singleton (moduleName <> ".sol") (makeSolcContract code)
      outputSelection = M.singleton "*" (M.singleton "*" (["abi", "evm.bytecode.object"] <> project.solcOutputSelection))
      depMappings = (\(Dependency dep) -> dep <> "=" <> (root <> "/node_modules/" <> dep)) <$> project.dependencies
      sourceDirMapping = [":g" <> (Path.concat [root, project.sourceDir])]
      remappings = sourceDirMapping <> depMappings
      settings = SolcSettings { outputSelection, remappings }
  pure $ SolcInput { language, sources, settings }

--------------------------------------------------------------------------------

type ContractName = String

newtype SolcSettings =
  SolcSettings { outputSelection :: (M.StrMap (M.StrMap (Array String)))
               , remappings      :: Array String
               }

instance encodeSolcSettings :: A.EncodeJson SolcSettings where
  encodeJson (SolcSettings {outputSelection, remappings}) =
         "outputSelection" A.:= A.encodeJson outputSelection
    A.~> "remappings"      A.:= A.encodeJson remappings
    A.~> A.jsonEmptyObject

--------------------------------------------------------------------------------

-- | as per http://solidity.readthedocs.io/en/v0.4.21/using-the-compiler.html,
-- | "content" is the source code.
newtype SolcContract =
  SolcContract { content :: String
               , hash :: HexString
               }
instance encodeSolcContract :: A.EncodeJson SolcContract where
  encodeJson (SolcContract {content, hash}) =
    "content" A.:= A.fromString content A.~>
    "keccak256" A.:= A.fromString (unHex hash) A.~>
    A.jsonEmptyObject

makeSolcContract
  :: String
  -> SolcContract
makeSolcContract  sourceCode =
  SolcContract { content: sourceCode
               , hash: sha3 sourceCode
               }

-- TODO write the relevant contract outputs from our project to the build directory

--------------------------------------------------------------------------------

-- | we pretty print these later
newtype SolcError =
  SolcError { sourceLocation :: { file :: String
                                , start :: Int
                                , end :: Int
                                }
            , type :: String
            , severity :: String
            , message :: String
            , formattedMessage :: String
            }

instance decodeSolcError :: A.DecodeJson SolcError where
  decodeJson json = do
    obj <- A.decodeJson json
    loc <- obj A..? "sourceLocation"
    sourceLocation <- do
      file <- loc A..? "file"
      start <- loc A..? "start"
      end <- loc A..? "end"
      pure {file, start, end}
    _type <- obj A..? "type"
    severity <- obj A..? "severity"
    message <- obj A..? "message"
    formattedMessage <-obj A..? "formattedMessage"
    pure $
      SolcError { sourceLocation
                , type: _type
                , severity
                , message
                , formattedMessage
                }

--------------------------------------------------------------------------------
-- | Solc Output
--------------------------------------------------------------------------------

-- | This is the artifact we want, compatible with truffle (subset)
newtype OutputContract =
  OutputContract { abi :: A.JArray
                 , bytecode :: String
                 }

-- NOTE: We don't use the codecs here because they aren't mutual inverses of eachother,
-- for example we serialize an empty networks object for truffle compatibility,
-- but this is not output from the compiler.
parseOutputContract
  :: A.Json
  -> Either String OutputContract
parseOutputContract json = do
  obj <- A.decodeJson json
  abi <- obj A..? "abi"
  evm <- obj A..? "evm"
  evmObj <- A.decodeJson evm
  bytecodeO <- evmObj A..? "bytecode"
  bytecode <- bytecodeO A..? "object"
  pure $ OutputContract {abi, bytecode}

encodeOutputContract
  :: OutputContract
  -> A.Json
encodeOutputContract (OutputContract {abi, bytecode}) =
    "abi" A.:= A.fromArray abi A.~>
    "bytecode" A.:= bytecode A.~>
    "networks" A.:= A.jsonEmptyObject A.~>
    A.jsonEmptyObject

decodeContract
  :: String
  -> A.Json
  -> Either String (M.StrMap OutputContract)
decodeContract srcName json = do
  let srcNameWithSol = srcName <> ".sol"
  obj <- A.decodeJson json
  contracts <- obj A..? "contracts"
  json' <- maybe (Left $ "couldn't find " <> show srcNameWithSol <> " in object") Right (M.lookup srcNameWithSol contracts)
  contractMap <- A.decodeJson json'
  for contractMap $ parseOutputContract

--------------------------------------------------------------------------------

foreign import jsonStringifyWithSpaces :: Int -> A.Json -> String

writeBuildArtifact
  :: forall eff m
   . MonadAff (fs :: FS.FS | eff) m
  => String
  -> FilePath
  -> A.Json
  -> String
  -> m Unit
writeBuildArtifact srcName filepath output solContractName = liftAff $
  case decodeContract srcName output of
    Left err -> throwError <<< error $ "Malformed solc output: " <> err
    Right co -> do
      let dn = Path.dirname filepath
          contractsMainModule = M.lookup solContractName co -- | TODO: clean up
      case contractsMainModule of -- | TODO: Clean up
        Nothing -> (throwError <<< error $ ("Couldn't find an object named " <> show solContractName <> " in " <> show filepath <> "!"))
        Just co' -> do
            assertDirectory dn
            liftEff $ log Debug ("writing " <> filepath)
            FS.writeTextFile UTF8 filepath <<< jsonStringifyWithSpaces 4 $ encodeOutputContract co'

--------------------------------------------------------------------------------

newtype SolcOutput =
  SolcOutput { errors :: Maybe (Array SolcError)
             , contracts :: M.StrMap OutputContract -- mapping of Filepath
             }
