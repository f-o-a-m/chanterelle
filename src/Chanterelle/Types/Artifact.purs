module Chanterelle.Types.Artifact
  ( ArtifactBytecode(..)
  , ArtifactBytecodeR
  , DeployedArtifact
  , NetworkInfo(..)
  , UndeployedArtifact
  , Artifact(..)
  , _abi
  , _code
  , _bytecode
  , _deployedBytecode
  , _network
  , _networks
  , _NetworkBytecode
  , _Deployed
  , _address
  , _blockHash
  , _blockNumber
  , _transactionHash
  , _lastModified
  ) where

import Prelude

import Chanterelle.Types.Bytecode (Bytecode)
import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Lens (Lens', Getter', lens', to)
import Data.Lens.At (at)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Foreign.Object as M
import Network.Ethereum.Web3 (Address, HexString, BlockNumber)

type ArtifactBytecodeR a = Record
  ( bytecode :: Bytecode
  , deployedBytecode :: Bytecode
  | a
  )

type UndeployedArtifact = ArtifactBytecodeR ()
type DeployedArtifact = ArtifactBytecodeR
  ( address :: Address
  , blockHash :: HexString
  , blockNumber :: BlockNumber
  , transactionHash :: HexString
  )

data NetworkInfo
  = Undeployed UndeployedArtifact
  | Deployed DeployedArtifact

derive instance Eq NetworkInfo
derive instance Ord NetworkInfo

instance DecodeJson NetworkInfo where
  decodeJson j = (Deployed <$> decodeJson j) <|> (Undeployed <$> decodeJson j)

instance EncodeJson NetworkInfo where
  encodeJson (Undeployed u) = encodeJson u
  encodeJson (Deployed d) = encodeJson d

newtype Artifact = Artifact
  { abi :: Array Json
  , code :: ArtifactBytecodeR ()
  , lastModified :: Number
  , networks :: M.Object NetworkInfo
  }

derive instance Eq Artifact
derive instance Ord Artifact
derive instance Newtype Artifact _
derive newtype instance DecodeJson Artifact
derive newtype instance EncodeJson Artifact

newtype ArtifactBytecode = ArtifactBytecode (ArtifactBytecodeR ())

derive instance Newtype ArtifactBytecode _
derive newtype instance Eq ArtifactBytecode
derive newtype instance Ord ArtifactBytecode
derive newtype instance DecodeJson ArtifactBytecode
derive newtype instance EncodeJson ArtifactBytecode

_abi :: Lens' Artifact (Array Json)
_abi = lens' $ \(Artifact a) -> Tuple a.abi (\abi' -> Artifact (a { abi = abi' }))

_code :: Lens' Artifact ArtifactBytecode
_code = lens' $ \(Artifact a) -> Tuple (ArtifactBytecode a.code) (\(ArtifactBytecode code') -> Artifact (a { code = code' }))

_bytecode :: Lens' ArtifactBytecode Bytecode
_bytecode = lens' $ \(ArtifactBytecode abc) -> Tuple abc.bytecode (\bytecode' -> ArtifactBytecode (abc { bytecode = bytecode' }))

_deployedBytecode :: Lens' ArtifactBytecode Bytecode
_deployedBytecode = lens' $ \(ArtifactBytecode abc) -> Tuple abc.deployedBytecode (\deployedBytecode' -> ArtifactBytecode (abc { deployedBytecode = deployedBytecode' }))

_networks :: Lens' Artifact (M.Object NetworkInfo)
_networks = lens' $ \(Artifact a) -> Tuple a.networks (\networks' -> Artifact (a { networks = networks' }))

_network :: Int -> Lens' Artifact (Maybe NetworkInfo)
_network nid = _networks <<< at (show nid)

-- Note that modifying bytecode with this Lens automatically makes your NetworkInfo undeployed...
_NetworkBytecode :: Lens' NetworkInfo ArtifactBytecode
_NetworkBytecode = lens' $ \ni -> Tuple (fromNI ni) (toNI ni)
  where
  fromNI (Deployed { bytecode, deployedBytecode }) = ArtifactBytecode { bytecode, deployedBytecode }
  fromNI (Undeployed u) = ArtifactBytecode u
  toNI d@(Deployed o) (ArtifactBytecode n) =
    if o.bytecode == n.bytecode && o.deployedBytecode == n.deployedBytecode then d
    else Undeployed n
  toNI _ (ArtifactBytecode u) = Undeployed u

_Deployed :: Getter' NetworkInfo (Maybe DeployedArtifact)
_Deployed = to $
  case _ of
    Undeployed _ -> Nothing
    Deployed d -> Just d

_address :: Getter' DeployedArtifact Address
_address = to _.address

_blockHash :: Getter' DeployedArtifact HexString
_blockHash = to _.blockHash

_blockNumber :: Getter' DeployedArtifact BlockNumber
_blockNumber = to _.blockNumber

_transactionHash :: Getter' DeployedArtifact HexString
_transactionHash = to _.transactionHash

_lastModified :: Lens' Artifact Number
_lastModified = lens' $ \(Artifact a) -> Tuple a.lastModified (\lastModified' -> Artifact (a { lastModified = lastModified' }))
