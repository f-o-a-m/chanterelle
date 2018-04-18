module Chanterelle.Internal.Types.Project where

import Prelude
import Chanterelle.Internal.Utils.Json (decodeJsonAddress, decodeJsonHexString, encodeJsonAddress, encodeJsonHexString, gfWithDecoder)
import Control.Alt ((<|>))
import Data.Argonaut as A
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (:=), (~>), (.?), (.??))
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.StrMap as M
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Node.Path (FilePath)
import Network.Ethereum.Web3 (Address, HexString)

--------------------------------------------------------------------------------
-- | Chanterelle Project Types
--------------------------------------------------------------------------------

newtype Dependency = Dependency String

derive instance eqDependency  :: Eq Dependency

instance encodeJsonDependency :: EncodeJson Dependency where
  encodeJson (Dependency d) = encodeJson d

instance decodeJsonDependency :: DecodeJson Dependency where
  decodeJson d = Dependency <$> decodeJson d

---------------------------------------------------------------------

data InjectableLibraryCode = InjectableWithSourceCode (Maybe FilePath) FilePath
                           | InjectableWithBytecode HexString

derive instance eqInjectableLibraryCode :: Eq InjectableLibraryCode

instance encodeJsonInjectableLibraryCode :: EncodeJson InjectableLibraryCode where
  encodeJson (InjectableWithSourceCode r f) = encodeJson $ M.fromFoldable elems
    where elems = file <> root
          file  = [Tuple "file" $ encodeJson f]
          root  = fromMaybe [] (pure <<< Tuple "root" <<< encodeJson <$> r)
  encodeJson (InjectableWithBytecode c)   = encodeJson $ M.singleton "bytecode" (encodeJsonHexString c)

instance decodeJsonInjectableLibraryCode :: DecodeJson InjectableLibraryCode where
  decodeJson d = decodeSourceCode <|> decodeBytecode <|> Left "not a valid InjectableLibrarySource"
      where decodeSourceCode = decodeJson d >>= (\o -> InjectableWithSourceCode <$> o .?? "root" <*> o .? "file")
            decodeBytecode   = do 
              o <- decodeJson d
              bc <- gfWithDecoder decodeJsonHexString o "bytecode"
              pure $ InjectableWithBytecode bc

data Library = FixedLibrary      { name :: String, address :: Address  }
             | InjectableLibrary { name :: String, address :: Address , code :: InjectableLibraryCode }

isFixedLibrary :: Library -> Boolean
isFixedLibrary (FixedLibrary _) = true
isFixedLibrary _                = false

newtype Libraries = Libraries (Array Library)

derive instance eqLibrary                   :: Eq Library
derive instance eqLibraries                 :: Eq Libraries
derive newtype instance monoidLibraries     :: Monoid Libraries
derive newtype instance semigroiupLibraries :: Semigroup Libraries

instance encodeJsonLibraries :: EncodeJson Libraries where
  encodeJson (Libraries libs) =
    let asAssocs = mkTuple <$> libs
        mkTuple (FixedLibrary l)      = Tuple l.name (encodeJsonAddress l.address)
        mkTuple (InjectableLibrary l) = let dl =  "address" := encodeJsonAddress l.address
                                               ~> "code"  := encodeJson l.code
                                               ~> A.jsonEmptyObject
                                         in Tuple l.name (encodeJson dl)
        asMap    = M.fromFoldable asAssocs
     in encodeJson asMap

instance decodeJsonLibraries :: DecodeJson Libraries where
  decodeJson j = do
    obj <- decodeJson j
    libs <- for (M.toUnfoldable obj) $ \t -> (decodeFixedLibrary t) <|> (decodeInjectableLibrary t) <|> (failDramatically t)
    pure (Libraries libs)

    where decodeFixedLibrary (Tuple name l) = do
            address <- decodeJsonAddress l
            pure $ FixedLibrary { name, address }

          decodeInjectableLibrary (Tuple name l) = do
            ilo <- decodeJson l
            address <- gfWithDecoder decodeJsonAddress ilo "address"
            code    <- ilo .? "code"
            pure $ InjectableLibrary { name, address, code }

          failDramatically (Tuple name _) = Left ("Malformed library descriptor for " <> name)

---------------------------------------------------------------------

data ChanterelleModule =
  ChanterelleModule { moduleName      :: String
                    , solContractName :: String
                    , solPath         :: FilePath
                    , jsonPath        :: FilePath
                    , pursPath        :: FilePath
                    }

newtype ChanterelleProjectSpec =
  ChanterelleProjectSpec { name                :: String
                         , version             :: String
                         , sourceDir           :: FilePath
                         , artifactsDir        :: FilePath
                         , modules             :: Array String
                         , dependencies        :: Array Dependency
                         , libraries           :: Libraries
                         , solcOutputSelection :: Array String
                         , psGen               :: { exprPrefix   :: String
                                                  , modulePrefix :: String
                                                  , outputPath   :: String
                                                  }
                         }

derive instance eqChanterelleProjectSpec  :: Eq ChanterelleProjectSpec

instance encodeJsonChanterelleProjectSpec :: EncodeJson ChanterelleProjectSpec where
  encodeJson (ChanterelleProjectSpec project) =
         "name"                  := encodeJson project.name
      ~> "version"               := encodeJson project.version
      ~> "source-dir"            := encodeJson project.sourceDir
      ~> "artifacts-dir"         := encodeJson project.artifactsDir
      ~> "modules"               := encodeJson project.modules
      ~> "dependencies"          := encodeJson project.dependencies
      ~> "libraries"             := encodeJson project.libraries
      ~> "solc-output-selection" := encodeJson project.solcOutputSelection
      ~> "purescript-generator"  := psGenEncode
      ~> A.jsonEmptyObject

      where psGenEncode =  "output-path"       := encodeJson project.psGen.outputPath
                        ~> "expression-prefix" := encodeJson project.psGen.exprPrefix
                        ~> "module-prefix"     := encodeJson project.psGen.modulePrefix
                        ~> A.jsonEmptyObject

instance decodeJsonChanterelleProjectSpec :: DecodeJson ChanterelleProjectSpec where
  decodeJson j = do
    obj                 <- decodeJson j
    name                <- obj .? "name"
    version             <- obj .? "version"
    sourceDir           <- obj .? "source-dir"
    artifactsDir        <- fromMaybe "build" <$> obj .?? "artifacts-dir"
    modules             <- obj .? "modules"
    dependencies        <- fromMaybe mempty <$> obj .?? "dependencies"
    libraries           <- fromMaybe mempty <$> obj .?? "libraries"
    solcOutputSelection <- fromMaybe mempty <$> obj .?? "solc-output-selection"
    psGenObj            <- obj .? "purescript-generator"
    psGenOutputPath     <- psGenObj .? "output-path"
    psGenExprPrefix     <- fromMaybe "" <$> psGenObj .?? "expression-prefix"
    psGenModulePrefix   <- fromMaybe "" <$> psGenObj .?? "module-prefix"
    let psGen = { exprPrefix: psGenExprPrefix, modulePrefix: psGenModulePrefix, outputPath: psGenOutputPath }
    pure $ ChanterelleProjectSpec { name, version, sourceDir, artifactsDir, modules, dependencies, libraries, solcOutputSelection, psGen }

data ChanterelleProject =
     ChanterelleProject { root     :: FilePath -- ^ parent directory containing chanterelle.json
                        , srcIn    :: FilePath -- ^ hydrated/absolute path of src dir (root + spec.sourceDir)
                        , jsonOut  :: FilePath -- ^ hydrated/absolute path of jsons dir
                        , psOut    :: FilePath -- ^ hydrated/absolute path of psGen (root + spec.psGen.outputPath)
                        , spec     :: ChanterelleProjectSpec -- ^ the contents of the chanterelle.json
                        , modules  :: Array ChanterelleModule
                        }
