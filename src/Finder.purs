module Finder where

import Prelude
import Control.Monad.Aff (try)
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

-- | TODO this json creation is terrible, find something nicer.
makeSolcJsonInput
  :: forall eff m.
     MonadAff (fs :: FS.FS | eff) m
  => {dependencies :: Array String}
  -> m A.Json
makeSolcJsonInput project = do
  fs <- getAllSolcFiles project
  let sourceObject f = A.fromObject $ M.insert "content" (A.fromString f.sourceCode) M.empty
      srcMapping = foldr (\f sourceMapping -> M.insert f.moduleName (sourceObject f) sourceMapping) M.empty fs
      selectionOptsObj = A.fromObject $ M.insert "*" (A.fromArray (map A.fromString ["abi","evm.bytecode.object"])) M.empty
      outputSelection = "outputSelection" A.:= A.fromObject (M.insert "*" selectionOptsObj M.empty) A.~>
                        A.jsonEmptyObject
      solcInput = "language" A.:= A.fromString "Solidity" A.~>
                  "sources" A.:= A.fromObject srcMapping A.~>
                  "settings" A.:= outputSelection A.~>
                  A.jsonEmptyObject
  pure solcInput


foreign import _compile :: forall eff. String -> Eff eff A.Json

compile
  :: forall eff m.
     MonadAff (fs :: FS.FS | eff) m
  => {dependencies :: Array String}
  -> m A.Json
compile project = do
  solcInput <- makeSolcJsonInput project
  solcOutput <- liftEff $ _compile $ A.stringify solcInput
  traceA $ show solcOutput
  pure solcOutput

-- TODO write the relevant contract outputs from our project to the build directory
