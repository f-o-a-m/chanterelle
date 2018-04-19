module Chanterelle.Internal.Codegen
  ( generatePS
  ) where

import Prelude

import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types.Compile (CompileError(..))
import Chanterelle.Internal.Types.Project (ChanterelleProject(..), ChanterelleProjectSpec(..), ChanterelleModule(..))
import Chanterelle.Internal.Utils (assertDirectory)
import Control.Error.Util (note)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.AbiParser (Abi(Abi), AbiDecodeError(..), AbiWithErrors) as PSWeb3Gen
import Data.Argonaut (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Prisms (_Object)
import Data.Array (mapMaybe)
import Data.CodeGen (GeneratorOptions, runImported) as PSWeb3Gen
import Data.Either (Either(..), either)
import Data.Generator (genCode) as PSWeb3Gen
import Data.Identity (Identity(..))
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path

generatePS :: forall eff m
            . MonadAff (console :: CONSOLE, fs :: FS.FS | eff) m
           => MonadThrow CompileError m
           => ChanterelleProject
           -> m Unit
generatePS p@(ChanterelleProject project) = do
  let psArgs = projectPSArgs p
  void <<< for project.modules $ \(ChanterelleModule mod) -> do
      (PSWeb3Gen.Abi abiWithErrors) <- loadAbi p mod.jsonPath
      log Debug $ "generating purescript for " <> mod.moduleName
      abi <- for abiWithErrors case _ of
        Left (PSWeb3Gen.AbiDecodeError err) -> do
          log Error $ "while parsing abi type of object at index: " <> show err.idx <> " from: " <> mod.jsonPath <> " got error: " <> err.error
          pure Nothing
        Right x -> pure $ Just x
      let psModule = generatePSModule p (PSWeb3Gen.Abi $ mapMaybe (map Identity) abi) mod.moduleName
      assertDirectory (Path.dirname mod.pursPath)
      log Info $ "writing PureScript bindings for " <> mod.moduleName
      liftAff $ FS.writeTextFile UTF8 mod.pursPath psModule

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

generatePSModule
  :: ChanterelleProject
  -> PSWeb3Gen.Abi Identity
  -> String
  -> String
generatePSModule p@(ChanterelleProject project) abi moduleName =
  let (ChanterelleProjectSpec spec) = project.spec
  in PSWeb3Gen.genCode abi {exprPrefix: spec.psGen.exprPrefix, indentationLevel: 0}
    # PSWeb3Gen.runImported (projectPSArgs p) moduleName

loadAbi :: forall eff m
         . MonadAff (fs :: FS.FS | eff) m
        => MonadThrow CompileError m
        => ChanterelleProject
        -> FilePath
        -> m PSWeb3Gen.AbiWithErrors
loadAbi (ChanterelleProject project) abiFile = do
    let (ChanterelleProjectSpec spec) = project.spec
    ejson <- liftAff (jsonParser <$> FS.readTextFile UTF8 abiFile)
    json <- either (throwError <<< CompileParseError <<< {objectName: "Json File " <> abiFile, parseError:_}) pure ejson
    either (throwError <<< CompileParseError <<< {objectName: "ABI " <> abiFile, parseError:_}) pure $ parseAbi json
  where
    parseAbi json = let mabi = json ^? _Object <<< ix "abi"
                    in note ("ABI field missing in " <> abiFile) mabi >>= decodeJson
