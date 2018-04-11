module Chanterelle.Internal.Codegen
       ( generatePS
       ) where

import Prelude
import Data.Argonaut (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Prisms (_Object)
import Control.Error.Util (note)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Writer (runWriter)
import Data.Either (either)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path
import Node.Encoding (Encoding(UTF8))
import Chanterelle.Internal.Types (ChanterelleProject(..), ChanterelleProjectSpec(..), ChanterelleModule(..), CompileError(..))
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Utils (assertDirectory)

-- ps-web3-gen imports
import Data.AbiParser (Abi) as PSWeb3Gen
import Data.Generator (genCode) as PSWeb3Gen
import Data.CodeGen (GeneratorOptions, genPSModuleStatement, runImports) as PSWeb3Gen

generatePS :: forall eff m
            . MonadAff (console :: CONSOLE, fs :: FS.FS | eff) m
           => MonadThrow CompileError m
           => ChanterelleProject
           -> m Unit
generatePS p@(ChanterelleProject project) = do
  let psArgs = projectPSArgs p
  void <<< for project.modules $ \(ChanterelleModule mod) -> do
      abi <- loadAbi p mod.jsonPath
      let psModule = generatePSModule p abi mod.moduleName
      log Debug $ "generating purescript for " <> mod.moduleName
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
  -> PSWeb3Gen.Abi
  -> String
  -> String
generatePSModule p@(ChanterelleProject project) abi moduleName =
  let (ChanterelleProjectSpec spec) = project.spec
      psArgs = projectPSArgs p
      (Tuple code accImports) = runWriter $ PSWeb3Gen.genCode abi {exprPrefix: spec.psGen.exprPrefix, indentationLevel: 0}
      modStatement = PSWeb3Gen.genPSModuleStatement psArgs moduleName
      imports = if code == "" then "" else PSWeb3Gen.runImports accImports <> "\n"
   in modStatement <> "\n" <> imports <> code

loadAbi :: forall eff m
         . MonadAff (fs :: FS.FS | eff) m
        => MonadThrow CompileError m
        => ChanterelleProject
        -> FilePath
        -> m PSWeb3Gen.Abi
loadAbi (ChanterelleProject project) abiFile = do
    let (ChanterelleProjectSpec spec) = project.spec
    ejson <- liftAff (jsonParser <$> FS.readTextFile UTF8 abiFile)
    json <- either (throwError <<< CompileParseError) pure ejson
    either (throwError <<< CompileParseError) pure $ parseAbi json
  where
    parseAbi json = let mabi = json ^? _Object <<< ix "abi"
                    in note ("abi field missing in " <> abiFile) mabi >>= decodeJson
