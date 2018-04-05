module Finder where

import Prelude
import Control.Error.Util (hush)
import Control.Monad.Aff (try)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (class MonadState, StateT, get, evalStateT, put)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut as A
import Data.Array (catMaybes, null, concat)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing, fromJust)
import Data.String as S
import Data.Foldable (foldr)
import Data.Traversable (for)
import Node.Path (FilePath, extname)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS
import Node.FS.Stats as Stats
import Partial.Unsafe (unsafePartialBecause)
import Data.StrMap as M
import Network.Ethereum.Web3 (HexString, unHex, sha3)

import Debug.Trace (traceA)

getAllDirectories
  :: forall eff m.
     MonadAff (fs :: FS.FS | eff) m
  => MonadState FilePath m
  => m (Array FilePath)
getAllDirectories = do
  currentDirectory <- get
  allFiles <- liftAff $ FS.readdir currentDirectory
  mdirs <- for allFiles (validateRootedDir currentDirectory)
  pure $ catMaybes mdirs

validateRootedDir
  :: forall eff m.
  MonadAff (fs :: FS.FS | eff) m
  => FilePath -- prefix
  -> FilePath -- dirname
  -> m (Maybe FilePath)
validateRootedDir prefix dir = liftAff $ do
  let fullPath = prefix <> "/" <> dir
  estat <- try $ FS.stat fullPath
  pure case estat of
    Left _ -> Nothing
    Right s ->
      let isValid = (not $ Stats.isSymbolicLink s)
                      && Stats.isDirectory s
                      && (isNothing $ S.stripPrefix (S.Pattern ".") dir)
      in if isValid
        then Just fullPath
        else Nothing

getSolcFilesInDirectory
  :: forall eff m.
     MonadAff (fs :: FS.FS | eff) m
  => MonadState FilePath m
  => m (Array FilePath)
getSolcFilesInDirectory = do
  currentDirectory <- get
  allFiles <- liftAff $ FS.readdir currentDirectory
  msolcs <- for allFiles (validateFile currentDirectory)
  pure $ catMaybes msolcs

validateFile
  :: forall eff m.
     MonadAff (fs :: FS.FS | eff) m
  => FilePath -- dir
  -> FilePath -- filepath
  -> m (Maybe FilePath)
validateFile dir f = liftAff $ do
  let fullPath = dir <> "/" <> f
  estat <- try $ FS.stat fullPath
  pure case estat of
    Left _ -> Nothing
    Right s ->
      let isValid = Stats.isFile s && extname f == ".sol"
      in if isValid
            then Just fullPath
            else Nothing

type SolcSourceFile =
  { filePath :: FilePath
  , moduleName :: String
  , sourceCode :: String
  }

getAllSolcFilesForProject
  :: forall eff m.
     MonadAff (fs :: FS.FS | eff) m
  => {rootPrefix :: FilePath, rootPath :: FilePath}
  -> m (Array SolcSourceFile)
getAllSolcFilesForProject {rootPrefix, rootPath} = do
    rootedSolcFiles <- evalStateT getAllSolcFiles' (rootPath <> "/contracts")
    for rootedSolcFiles $ \filePath -> do
      sourceCode <- liftAff $ FS.readTextFile UTF8 filePath
      let moduleName = unprepend rootPrefix filePath
      pure {filePath, sourceCode, moduleName}
  where
    unprepend rt f =
      let mstripped = S.stripPrefix (S.Pattern $ rt <> "/") f
      in unsafePartialBecause ("The root \"" <> rt <> "\" was just prepended.") (fromJust mstripped)
    getAllSolcFiles' :: StateT FilePath m (Array FilePath)
    getAllSolcFiles' = do
      cd <- get
      hereFiles <- getSolcFilesInDirectory
      hereDirectories <- getAllDirectories
      if null hereDirectories
         then pure hereFiles
         else do
              thereFiles <- for hereDirectories $ \d -> do
                              put d
                              getAllSolcFiles'
              pure $ hereFiles <> concat thereFiles

getAllSolcFiles
  :: forall eff m.
     MonadAff (fs :: FS.FS | eff) m
  => {dependencies :: Array String}
  -> m (Array SolcSourceFile)
getAllSolcFiles {dependencies} = do
  us <- getAllSolcFilesForProject {rootPrefix: "./", rootPath: "./"}
  them <- for dependencies $ \dep ->
    getAllSolcFilesForProject {rootPrefix: "node_modules", rootPath: "node_modules/" <> dep}
  pure $ us <> concat them

--------------------------------------------------------------------------------

type ContractName = String

newtype SolcSettings = SolcSettings (M.StrMap (M.StrMap (Array String)))

instance encodeSolcSettings :: A.EncodeJson SolcSettings where
  encodeJson (SolcSettings settings) = A.encodeJson settings

defaultSolcSettings :: SolcSettings
defaultSolcSettings = SolcSettings $
  M.insert "*" (M.insert "*" ["abi", "evm.bytecode.object"] M.empty) M.empty

updateSolcSettings
 :: FilePath
 -> ContractName
 -> Array String
 -> SolcSettings
 -> SolcSettings
updateSolcSettings fp contract settings (SolcSettings current) =
  SolcSettings $ M.insert fp (M.insert contract settings M.empty) current

--------------------------------------------------------------------------------

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
  :: SolcSourceFile
  -> SolcContract
makeSolcContract {moduleName, sourceCode} =
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

-- | TODO this json creation is terrible, find something nicer.
makeSolcInput
  :: forall eff m.
     MonadAff (fs :: FS.FS | eff) m
  => {dependencies :: Array String}
  -> m SolcInput
makeSolcInput project = do
  fs <- getAllSolcFiles project
  let sources = foldr (\f m-> M.insert f.moduleName (makeSolcContract f) m) M.empty fs
  pure $ SolcInput { language: "Solidity"
                   , sources
                   , settings: defaultSolcSettings
                   }


foreign import _compile :: forall eff. String -> Eff eff A.Json

compile
  :: forall eff m.
     MonadAff (fs :: FS.FS | eff) m
  => {dependencies :: Array String}
  -> m A.Json
compile project = do
  solcInput <- makeSolcInput project
  solcOutput <- liftEff $ _compile $ A.stringify $ A.encodeJson solcInput
  traceA $ show solcOutput
  pure solcOutput

-- TODO write the relevant contract outputs from our project to the build directory

--------------------------------------------------------------------------------
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
             , contracts :: M.StrMap OutputContract
             }

{-
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
