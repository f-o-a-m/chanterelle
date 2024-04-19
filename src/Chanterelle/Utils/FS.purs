module Chanterelle.Utils.FS where

import Prelude

import Chanterelle.Logging (LogLevel(..), log)
import Chanterelle.Types.Compile (CompileError(..))
import Chanterelle.Utils.Error (withExceptT')
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Either (Either(..))
import Data.Int.Bits ((.|.))
import Effect.Aff (Milliseconds, attempt)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Perms (permsReadWrite)
import Node.FS.Stats as Stats
import Node.Path (FilePath)
import Node.Path as Path

foreign import readFileSync :: forall a opts. EffectFn2 FilePath { | opts } a
foreign import writeFileSync :: forall a opts. EffectFn3 FilePath a { | opts } Unit
foreign import _O_TRUNC :: Int
foreign import _O_CREAT :: Int
foreign import _O_RDWR :: Int
foreign import _O_SYNC :: Int

unparsePath
  :: forall p
   . { dir :: String
     , name :: String
     , ext :: String
     | p
     }
  -> Path.FilePath
unparsePath p = Path.concat [ p.dir, p.name <> p.ext ]

assertDirectory
  :: forall m
   . MonadAff m
  => MonadThrow String m
  => FilePath
  -> m Unit
assertDirectory dn = do
  stat <- liftAff $ attempt $ FS.stat dn
  case stat of
    Left _ -> do
      -- assume an error means the file doesn't exist
      log Debug ("creating directory " <> dn)
      liftAff $ FS.mkdir' dn {recursive: true, mode: permsReadWrite}
    Right stats ->
      if not (Stats.isDirectory stats) then throwError ("Path " <> dn <> " exists but is not a directory!")
      else log Debug ("path " <> dn <> " exists and is a directory")

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
readTextFile filename = wrapInternalRead
  where
  wrapInternalRead = liftEffect $ runEffectFn2 readFileSync filename opts
  opts = { encoding: show UTF8, flag: "rs+" }

writeTextFile
  :: forall m
   . MonadAff m
  => MonadThrow String m
  => FilePath
  -> String
  -> m Unit
writeTextFile filename contents = wrapInternalWrite
  where
  wrapInternalWrite = liftEffect $ runEffectFn3 writeFileSync filename contents opts
  opts = { encoding: show UTF8, flag: writeSyncFlag }
  writeSyncFlag = _O_TRUNC .|. _O_CREAT .|. _O_RDWR .|. _O_SYNC

withTextFile
  :: forall m
   . MonadAff m
  => MonadThrow String m
  => FilePath
  -> (String -> m String)
  -> m Unit
withTextFile filename action = withTextFile' filename wrapper
  where
  wrapper = map toResult <<< action
  toResult contents = { contents, result: unit }

withTextFile'
  :: forall m a
   . MonadAff m
  => MonadThrow String m
  => FilePath
  -> (String -> m { contents :: String, result :: a })
  -> m a
withTextFile' filename action = do
  oldContents <- readTextFile filename
  { contents, result } <- action oldContents
  writeTextFile filename contents *> pure result
