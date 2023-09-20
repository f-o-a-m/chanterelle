module Chanterelle.Artifact
  ( readArtifact
  , updateArtifact
  , writeArtifact
  ) where

import Prelude

import Chanterelle.Types.Artifact (Artifact(..))
import Chanterelle.Utils (jsonStringifyWithSpaces, parseDecodeM, readTextFile, withTextFile, writeTextFile)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut (encodeJson)
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (now)
import Node.Path (FilePath)

setModTimeAndStringify
  :: forall m
   . MonadEffect m
  => Artifact
  -> m String
setModTimeAndStringify (Artifact a) = do
  Milliseconds newLastModified <- unInstant <$> liftEffect now
  let newArtifact = Artifact (a { lastModified = newLastModified })
  pure $ jsonStringifyWithSpaces 4 $ encodeJson newArtifact

readArtifact
  :: forall m
   . MonadAff m
  => MonadThrow String m
  => FilePath
  -> m Artifact
readArtifact filepath = parseDecodeM =<< readTextFile filepath

updateArtifact
  :: forall m
   . MonadAff m
  => MonadThrow String m
  => FilePath
  -> (Artifact -> m Artifact)
  -> m Unit
updateArtifact filepath action = withTextFile filepath $ \a ->
  setModTimeAndStringify =<< action =<< parseDecodeM a

writeArtifact
  :: forall m
   . MonadAff m
  => MonadThrow String m
  => FilePath
  -> Artifact
  -> m Unit
writeArtifact filepath a =
  liftEffect (setModTimeAndStringify a) >>= writeTextFile filepath
