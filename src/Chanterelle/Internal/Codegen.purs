module Chanterelle.Internal.Codegen
  ( generatePS
  ) where

import Prelude

import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types.Compile (CompileError(..))
import Chanterelle.Internal.Types.Project (ChanterelleProject(..), ChanterelleProjectSpec(..), ChanterelleModule(..))
import Chanterelle.Internal.Utils.FS (assertDirectory')
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Argonaut (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Prisms (_Object)
import Data.Array (catMaybes)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, note)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import Data.Traversable (for, for_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path
import Web3Generator.AbiParser (AbiDecodeError(..), AbiType, decodeArrayOfAbiTipes) as PSWeb3Gen
import Web3Generator.CodeGen (GeneratorOptions, generateCodeFromAbi, generatePS, ABIError(..)) as PSWeb3Gen

generatePS :: forall m
            . MonadAff m
           => MonadThrow CompileError m
           => MonadAsk ChanterelleProject m
           => m Unit
generatePS = do
  p@(ChanterelleProject project) <- ask
  let psArgs = projectPSArgs p
  void <<< for project.modules $ \(ChanterelleModule mod) -> do
      abiWithErrors <- loadAbi mod.jsonPath
      log Debug $ "generating purescript for " <> mod.moduleName
      abi <- for abiWithErrors case _ of
        Left (PSWeb3Gen.AbiDecodeError err) -> do
          log Error $ "while parsing abi type of object at index: " <> show err.idx <> " from: " <> mod.jsonPath <> " got error: " <> printJsonDecodeError err.error
          pure Nothing
        Right x -> pure $ Just x
      let psModule = PSWeb3Gen.generateCodeFromAbi psArgs (catMaybes abi) mod.moduleName
      assertDirectory' (Path.dirname mod.pursPath)
      log Info $ "writing PureScript bindings for " <> mod.moduleName
      liftAff $ FS.writeTextFile UTF8 mod.pursPath psModule
  -- generate all the modules from the extra-abis path
  let mextraJsonDir = case project.spec of
        ChanterelleProjectSpec s -> s.extraAbis
  liftAff $ case mextraJsonDir of
    Nothing -> pure unit
    Just extraJsonDir -> do
      log Info $ "Writing additional PureScript bindings using abis from directory " <> extraJsonDir
      errs <- PSWeb3Gen.generatePS (psArgs { jsonDir = extraJsonDir
                                           , truffle = false
                                           }
                                   )
      for_ errs \(PSWeb3Gen.ABIError err) ->
        log Error $ "while parsing abi type of object at index: "
          <> show err.idx <> " from: " <> err.abiPath <> " got error: " <> printJsonDecodeError err.error

projectPSArgs
  :: ChanterelleProject
  -> PSWeb3Gen.GeneratorOptions
projectPSArgs (ChanterelleProject project) =
  let (ChanterelleProjectSpec spec) = project.spec
  in  { jsonDir: project.jsonOut
      , pursDir: project.psOut
      , truffle: true
      , exprPrefix: spec.psGen.exprPrefix
      , modulePrefix: spec.psGen.modulePrefix
      }

loadAbi :: forall m
         . MonadAff m
        => MonadThrow CompileError m
        => FilePath
        -> m (Array (Either PSWeb3Gen.AbiDecodeError PSWeb3Gen.AbiType))
loadAbi abiFile = do
    ejson <- liftAff (jsonParser <$> FS.readTextFile UTF8 abiFile)
    json <- either (throwError <<< CompileParseError <<< {objectName: "Json File " <> abiFile, parseError:_}) pure ejson
    either (throwError <<< CompileParseError <<< {objectName: "ABI " <> abiFile, parseError:_}) pure $ parseAbi json
  where
    parseAbi json = let mabi = json ^? _Object <<< ix "abi"
                    in note ("ABI field missing in " <> abiFile) mabi >>= lmap printJsonDecodeError <<< PSWeb3Gen.decodeArrayOfAbiTipes
