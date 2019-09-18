module Chanterelle.Internal.Artifact
  ( module ArtifactExports
  , readArtifact
  , updateArtifact
  , writeArtifact
  ) where

import Prelude

import Chanterelle.Internal.Types.Artifact (Artifact(..))
import Chanterelle.Internal.Types.Artifact (Artifact(..), ArtifactBytecode(..), _Deployed, _NetworkBytecode, _abi, _address, _blockHash, _blockNumber, _bytecode, _code, _deployedBytecode, _lastModified, _network, _networks, _transactionHash, emptyArtifactBytecode, fromSolidityContractLevelOutput) as ArtifactExports
import Chanterelle.Internal.Utils.FS (readTextFile, withTextFile, writeTextFile)
import Chanterelle.Internal.Utils.Json (jsonStringifyWithSpaces, parseDecodeM)
import Chanterelle.Internal.Utils.Time (now, toEpoch)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut (encodeJson)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Path (FilePath)

setModTimeAndStringify
  :: forall m
   . MonadEffect m
  => Artifact
  -> m String
setModTimeAndStringify (Artifact a) = do
  Milliseconds newLastModified <- toEpoch <$> liftEffect now
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