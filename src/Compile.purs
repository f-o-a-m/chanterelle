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
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FSS
import Node.FS.Aff as FS
import Node.Process as P
import Data.StrMap as M
import Network.Ethereum.Web3 (HexString, unHex, sha3)

import Types (ChanterelleProject(..), Dependency(..))

import Debug.Trace (traceA)

foreign import data SolcInputCallbackResult :: Type
foreign import solcInputCallbackSuccess :: String -> SolcInputCallbackResult
foreign import solcInputCallbackFailure :: String -> SolcInputCallbackResult
foreign import _compile :: forall eff cbEff. Fn2 String (String -> Eff cbEff SolcInputCallbackResult) (Eff eff A.Json)

-- | compile and print the output
compile
  :: forall eff m.
     MonadAff (fs :: FS.FS, process :: P.PROCESS | eff) m
  => ChanterelleProject
  -> m (Array A.Json)
compile p@(ChanterelleProject project) = do
  cwd <- liftAff <<< liftEff $ P.cwd
  solcInputs <- for project.sources $ (\src -> makeSolcInput p cwd src (Path.concat [cwd, project.sourceDir, src]))
  solcOutputs <- liftEff $ for solcInputs $ \solcInput -> 
    runFn2 _compile (A.stringify $ A.encodeJson solcInput) (loadSolcCallback p)
  pure solcOutputs

makeSolcInput
  :: forall eff m.
     MonadAff (fs :: FS.FS | eff) m
  => ChanterelleProject
  -> FilePath
  -> String
  -> FilePath
  -> m SolcInput
makeSolcInput (ChanterelleProject project) cwd moduleName sourcePath = do
  code <- liftAff $ FS.readTextFile UTF8 sourcePath
  let language = "Solidity"
      sources = M.singleton moduleName (makeSolcContract code)
      outputSelection = M.singleton "*" (M.singleton "*" (["abi", "evm.bytecode.object"] <> project.solcOutputSelection))
      depMappings = (\(Dependency dep) -> dep <> "=" <> (cwd <> "/node_modules/" <> dep)) <$> project.dependencies
      sourceDirMapping = [":g" <> (Path.concat [cwd, project.sourceDir])]
      remappings = sourceDirMapping <> depMappings
      settings = SolcSettings { outputSelection, remappings }
  pure $ SolcInput { language, sources, settings }

-- | load a file when solc requests it
-- | TODO: secure it so that it doesnt try loading crap like /etc/passwd, etc. :P
loadSolcCallback :: forall eff. ChanterelleProject -> String -> Eff (fs :: FS.FS | eff) SolcInputCallbackResult
loadSolcCallback (ChanterelleProject project) filePath = do
  let isAbs = Path.isAbsolute filePath
      fullPath = if isAbs then filePath else Path.normalize (Path.concat [project.sourceDir, filePath])
  traceA ("solc is requesting that we load " <> show filePath <> ", so we'll give it " <> show fullPath)
  catchException (pure <<< solcInputCallbackFailure <<< show) (solcInputCallbackSuccess <$> (FSS.readTextFile UTF8 fullPath))

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

--------------------------------------------------------------------------------

type SolcCompilationTarget = { dependencies :: Array String
                             , sources      :: Maybe (Array String)
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

parseOutputContract
  :: A.Json
  -> Either String OutputContract
parseOutputContract json = do
  obj <- A.decodeJson json
  abi <- obj A..? "abi"
  evm <- obj A..? "evm"
  evmObj <- A.decodeJson evm
  bytecode <- evmObj A..? "bytecode"
  pure $ OutputContract {abi, bytecode}

instance encodeOutputContact :: A.EncodeJson OutputContract where
  encodeJson (OutputContract {abi, bytecode}) =
    "abi" A.:= A.fromArray abi A.~>
    "bytecode" A.:= bytecode A.~>
    A.jsonEmptyObject

decodeContract
  :: FilePath
  -> A.Json
  -> Maybe (M.StrMap OutputContract)
decodeContract filepath json = do
  obj <- hush $ A.decodeJson json
  json' <- M.lookup filepath obj
  contractMap <- hush $ A.decodeJson json'
  for contractMap $ hush <<< parseOutputContract

writeBuildArtifact
  :: forall eff m.
     MonadAff (fs :: FS.FS | eff) m
  => FilePath
  -> A.Json
  -> m Unit
writeBuildArtifact filepath output = liftAff $ do
  let contractOutput = decodeContract filepath output
  case contractOutput of
    Nothing -> throwError <<< error $ "FilePath not found in solc output: " <> filepath
    Just co -> FS.writeTextFile UTF8 filepath <<< A.stringify <<< A.encodeJson $ co


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
