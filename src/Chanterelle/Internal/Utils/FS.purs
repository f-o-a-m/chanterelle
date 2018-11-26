module Chanterelle.Internal.Utils.FS where

import Prelude

import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types.Compile (CompileError(..))
import Effect.Aff (Milliseconds)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.DateTime.Instant (fromDateTime, unInstant)
import Node.FS.Aff as FS
import Node.FS.Stats as Stats
import Node.FS.Sync.Mkdirp (mkdirp)
import Node.Path (FilePath)
import Node.Path as Path

unparsePath :: forall p. { dir :: String, name :: String, ext :: String | p} -> Path.FilePath
unparsePath p = Path.concat [p.dir, p.name <> p.ext]

assertDirectory
  :: forall m.
     MonadAff m
  => MonadThrow CompileError m
  => FilePath
  -> m Unit
assertDirectory dn = do
  dnExists <- liftAff $ FS.exists dn
  if not dnExists
    then log Debug ("creating directory " <> dn) *> (liftEffect $ mkdirp dn)
    else do
      isDir <- liftAff (Stats.isDirectory <$> FS.stat dn)
      if not isDir
        then throwError $ FSError ("Path " <> dn <> " exists but is not a directory!")
        else log Debug ("path " <>  dn <> " exists and is a directory")

fileModTime
  :: forall m.
     MonadAff m
  => FilePath
  -> m Milliseconds
fileModTime filepath = do
  unInstant <<< fromDateTime <<< Stats.modifiedTime <$> liftAff (FS.stat filepath)

fileIsDirty
  :: forall m.
     MonadAff m
  => FilePath
  -> Milliseconds
  -> Milliseconds
  -> m Boolean
fileIsDirty filepath compiledAt chanterelleJsonModTime = do
  modifiedAt <- fileModTime filepath
  pure $ compiledAt < modifiedAt || compiledAt < chanterelleJsonModTime
