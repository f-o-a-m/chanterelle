module Compile where

import Prelude
import Control.Error.Util (hush)
import Control.Monad.Eff.Exception (catchException, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Aff (Aff)
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
import Node.FS.Sync.Mkdirp
import Node.FS.Stats as Stats
import Node.Process as P
import Data.StrMap as M
import Data.Tuple (snd)
import Network.Ethereum.Web3 (HexString, unHex, sha3)
import Types (ChanterelleProject(..), Dependency(..))

import Debug.Trace (traceA)

--------------------------------------------------------------------------------

foreign import data SolcInputCallbackResult :: Type
foreign import solcInputCallbackSuccess :: String -> SolcInputCallbackResult
foreign import solcInputCallbackFailure :: String -> SolcInputCallbackResult
foreign import _compile :: forall eff cbEff. Fn2 String (String -> Eff cbEff SolcInputCallbackResult) (Eff eff String)

-- | compile and print the output
compile
  :: forall eff m.
     MonadAff (fs :: FS.FS, process :: P.PROCESS | eff) m
  => FilePath
  -> ChanterelleProject
  -> m (M.StrMap A.Json)
compile root p@(ChanterelleProject project) = do
  solcInputs <- for project.sources $ \srcName -> do
      input <- makeSolcInput p root srcName (Path.concat [root, project.sourceDir, srcName])
      pure $ Tuple srcName input
  solcOutputs <-for solcInputs $ \(Tuple srcName solcInput) -> do
      output <- liftEff $ runFn2 _compile (A.stringify $ A.encodeJson solcInput) (loadSolcCallback root p)
      case AP.jsonParser output of
        Left err -> liftAff <<< throwError <<< error $ "Malformed solc output: " <> err
        Right output' -> do
          let outputPath = Path.concat [root, "build", project.sourceDir, srcName]
          writeBuildArtifact srcName outputPath output'
          pure $ Tuple srcName output'
  pure $ M.fromFoldable solcOutputs

-- | load a file when solc requests it
-- | TODO: secure it so that it doesnt try loading crap like /etc/passwd, etc. :P
-- | TODO: be more clever about dependency resolution, that way we don't even have to do
-- |       any remappings!
loadSolcCallback :: forall eff. FilePath -> ChanterelleProject -> String -> Eff (fs :: FS.FS | eff) SolcInputCallbackResult
loadSolcCallback root (ChanterelleProject project) filePath = do
  let isAbs = Path.isAbsolute filePath
      fullPath = if isAbs then filePath else Path.normalize (Path.concat [root, project.sourceDir, filePath])
  traceA ("solc is requesting that we load " <> show filePath <> ", so we'll give it " <> show fullPath)
  catchException (pure <<< solcInputCallbackFailure <<< show) (solcInputCallbackSuccess <$> (FSS.readTextFile UTF8 fullPath))

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
  => ChanterelleProject
  -> FilePath
  -> String
  -> FilePath
  -> m SolcInput
makeSolcInput (ChanterelleProject project) root moduleName sourcePath = do
  code <- liftAff $ FS.readTextFile UTF8 sourcePath
  let language = "Solidity"
      sources = M.singleton moduleName (makeSolcContract code)
      outputSelection = M.singleton "*" (M.singleton "*" (["abi", "evm.bytecode.object"] <> project.solcOutputSelection))
      depMappings = (\(Dependency dep) -> dep <> "=" <> (root <> "/node_modules/" <> dep)) <$> project.dependencies
      sourceDirMapping = [":g" <> (Path.concat [root, project.sourceDir])]
      remappings = sourceDirMapping <> depMappings
      settings = SolcSettings { outputSelection, remappings }
  pure $ SolcInput { language, sources, settings }

--------------------------------------------------------------------------------

type ContractName = String

newtype SolcSettings = SolcSettings { outputSelection :: (M.StrMap (M.StrMap (Array String)))
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
  obj <- A.decodeJson json
  contracts <- obj A..? "contracts"
  json' <- maybe (Left $ "couldn't find " <> show srcName <> " in object") Right (M.lookup srcName contracts)
  contractMap <- A.decodeJson json'
  for contractMap $ parseOutputContract

foreign import jsonStringifyWithSpaces :: Int -> A.Json -> String

writeBuildArtifact
  :: forall eff m.
     MonadAff (fs :: FS.FS | eff) m
  => String
  -> FilePath
  -> A.Json
  -> m Unit
writeBuildArtifact srcName filepath output = liftAff $
  case decodeContract srcName output of
    Left err -> throwError <<< error $ "Malformed solc output: " <> err
    Right co -> do
      let dn = Path.dirname filepath
      dnExists <- FS.exists dn
      if not dnExists
        then liftEff (mkdirp dn)
        else do
          isDir <- Stats.isDirectory <$> FS.stat dn
          if not isDir
            then (throwError <<< error $ ("Path " <> show dn <> " exists but is not a directory!"))
            else pure unit
      let parsedPath = Path.parse filepath
          withNewExtension = Path.concat [parsedPath.dir, parsedPath.name <> ".json"]
          contractsMainModule = M.lookup parsedPath.name co -- | HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK
      case contractsMainModule of -- | HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK
        Nothing -> (throwError <<< error $ ("Couldn't find an object named " <> show parsedPath.name <> " in " <> show filepath <> "!"))
        Just co' -> FS.writeTextFile UTF8 withNewExtension <<< jsonStringifyWithSpaces 4 $ encodeOutputContract co'


--------------------------------------------------------------------------------

newtype SolcOutput =
  SolcOutput { errors :: Maybe (Array SolcError)
             , contracts :: M.StrMap OutputContract -- mapping of Filepath
             }

{-


{ "contractName": "SimpleStorage"
, "abi" : [..]
, "bytecode" : String
, "source" : String
}

{
  // Optional: not present if no errors/warnings were encountered
  // This contains the file-level outputs. In can be limited/filtered by the outputSelection settings.
  sources: {
    "sourceFile.sol": {
      // Identifier (used in source maps)
      id: 1,
      // The AST object
      ast: {},
      // The legacy AST object
      legacyAST: {}
    }
  },
  // This contains the contract-level outputs. It can be limited/filtered by the outputSelection settings.
  contracts: {
    "sourceFile.sol": {
      // If the language used has no contract names, this field should equal to an empty string.
      "ContractName": {
        // The Ethereum Contract ABI. If empty, it is represented as an empty array.
        // See https://github.com/ethereum/wiki/wiki/Ethereum-Contract-ABI
        abi: [],
        // See the Metadata Output documentation (serialised JSON string)
        metadata: "{...}",
        // User documentation (natspec)
        userdoc: {},
        // Developer documentation (natspec)
        devdoc: {},
        // Intermediate representation (string)
        ir: "",
        // EVM-related outputs
        evm: {
          // Assembly (string)
          assembly: "",
          // Old-style assembly (object)
          legacyAssembly: {},
          // Bytecode and related details.
          bytecode: {
            // The bytecode as a hex string.
            object: "00fe",
            // Opcodes list (string)
            opcodes: "",
            // The source mapping as a string. See the source mapping definition.
            sourceMap: "",
            // If given, this is an unlinked object.
            linkReferences: {
              "libraryFile.sol": {
                // Byte offsets into the bytecode. Linking replaces the 20 bytes located there.
                "Library1": [
                  { start: 0, length: 20 },
                  { start: 200, length: 20 }
                ]
              }
            }
          },
          // The same layout as above.
          deployedBytecode: { },
          // The list of function hashes
          methodIdentifiers: {
            "delegate(address)": "5c19a95c"
          },
          // Function gas estimates
          gasEstimates: {
            creation: {
              codeDepositCost: "420000",
              executionCost: "infinite",
              totalCost: "infinite"
            },
            external: {
              "delegate(address)": "25000"
            },
            internal: {
              "heavyLifting()": "infinite"
            }
          }
        },
        // eWASM related outputs
        ewasm: {
          // S-expressions format
          wast: "",
          // Binary format (hex string)
          wasm: ""
        }
      }
    }
  }
}
-}
