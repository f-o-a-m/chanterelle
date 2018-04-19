module Chanterelle.Internal.Utils.FS where

import Prelude
import Chanterelle.Internal.Types.Compile (CompileError(..))
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Control.Monad.Aff (Milliseconds)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (liftEff)
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
  :: forall eff m.
     MonadAff (fs :: FS.FS | eff) m
  => MonadThrow CompileError m
  => FilePath
  -> m Unit
assertDirectory dn = do
  dnExists <- liftAff $ FS.exists dn
  if not dnExists
    then log Debug ("creating directory " <> dn) *> (liftEff $ mkdirp dn)
    else do
      isDir <- liftAff (Stats.isDirectory <$> FS.stat dn)
      if not isDir
        then throwError $ FSError ("Path " <> dn <> " exists but is not a directory!")
        else log Debug ("path " <>  dn <> " exists and is a directory")

fileIsDirty
  :: forall eff m.
     MonadAff (fs :: FS.FS | eff) m
  => FilePath
  -> Milliseconds
  -> m Boolean
fileIsDirty filepath compiledAt = do
  modifiedAt <- Stats.modifiedTime <$> liftAff (FS.stat filepath)
  pure $ compiledAt < unInstant (fromDateTime modifiedAt)
