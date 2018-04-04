module Finder where

import Prelude
import Control.Monad.Aff (try)
import Control.Monad.State (class MonadState, StateT, get, evalStateT, put)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Data.Array (catMaybes, null, concat)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.String as S
import Data.Traversable (for)
import Node.Path (FilePath, extname)
import Node.FS.Aff as FS
import Node.FS.Stats as Stats

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


getAllSolcFiles
  :: forall eff m.
     MonadAff (fs :: FS.FS | eff) m
  => Array FilePath
  -> m (Array FilePath)
getAllSolcFiles rootDirs = map concat <<< for rootDirs $ \root -> evalStateT getAllSolcFiles' root
  where
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
