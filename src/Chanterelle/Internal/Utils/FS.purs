module Chanterelle.Internal.Utils.FS where

import Prelude

import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types.Compile (CompileError(..))
import Chanterelle.Internal.Utils.Error (catchingAff, withExceptT')
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Function.Uncurried (runFn2, runFn3)
import Effect (Effect)
import Effect.Aff (Milliseconds)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Internal as FSInternal
import Node.FS.Stats as Stats
import Node.Path (FilePath)
import Node.Path as Path

unparsePath
  :: forall p
   . { dir  :: String
     , name :: String
     , ext  :: String
     | p
     }
  -> Path.FilePath
unparsePath p = Path.concat [p.dir, p.name <> p.ext]

assertDirectory
  :: forall m
   . MonadAff m
  => MonadThrow String m
  => FilePath
  -> m Unit
assertDirectory dn = do
  dnExists <- liftAff $ FS.exists dn
  if not dnExists
    then log Debug ("creating directory " <> dn) *> (liftEffect $ mkdirp dn)
    else do
      isDir <- liftAff (Stats.isDirectory <$> FS.stat dn)
      if not isDir
        then throwError ("Path " <> dn <> " exists but is not a directory!")
        else log Debug ("path " <>  dn <> " exists and is a directory")

foreign import mkdirp ::String -> Effect Unit

assertDirectory'
  :: forall m
   . MonadAff m
  => MonadThrow CompileError m
  => FilePath
  -> m Unit
assertDirectory' = withExceptT' FSError <<< assertDirectory

fileModTime
  :: forall m
   . MonadAff m
  => FilePath
  -> m Milliseconds
fileModTime filepath = do
  unInstant <<< fromDateTime <<< Stats.modifiedTime <$> liftAff (FS.stat filepath)

fileIsDirty
  :: forall m
   . MonadAff m
  => FilePath
  -> Milliseconds
  -> Milliseconds
  -> m Boolean
fileIsDirty filepath compiledAt chanterelleJsonModTime = do
  modifiedAt <- fileModTime filepath
  log Debug ("fileIsDirty => modifiedAt: " <> show modifiedAt <> ", compiledAt: " <> show compiledAt <> ", chanterelleJsonModTime: " <> show chanterelleJsonModTime)
  pure $ compiledAt < modifiedAt || compiledAt < chanterelleJsonModTime

readTextFile
  :: forall m
   . MonadAff m
  => MonadThrow String m
  => FilePath
  -> m String
readTextFile filename = catchingAff wrapInternalRead
  where wrapInternalRead = liftEffect <<< FSInternal.mkEffect $ \_ -> runFn2
          FSInternal.unsafeRequireFS.readFileSync filename { encoding: show UTF8, flag: "rs+" }

writeTextFile
  :: forall m
   . MonadAff m
  => MonadThrow String m
  => FilePath
  -> String
  -> m Unit
writeTextFile filename contents = catchingAff wrapInternalWrite
  where wrapInternalWrite = liftEffect <<< FSInternal.mkEffect $ \_ -> runFn3
          FSInternal.unsafeRequireFS.writeFileSync filename contents { encoding: show UTF8, flag: "rs+" }

withTextFile
  :: forall m
   . MonadAff m
  => MonadThrow String m
  => FilePath
  -> (String -> m String)
  -> m Unit
withTextFile filename action = withTextFile' filename wrapper
  where wrapper = map toResult <<< action
        toResult contents = { contents, result: unit }

withTextFile'
  :: forall m a
   . MonadAff m
  => MonadThrow String m
  => FilePath
  -> (String -> m { contents :: String, result :: a })
  -> m a
withTextFile' filename action = do
  oldContents <-  readTextFile filename
  {contents, result} <- action oldContents
  writeTextFile filename contents *> pure result