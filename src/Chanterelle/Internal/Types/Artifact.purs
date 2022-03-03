module Chanterelle.Internal.Types.Artifact where

import Prelude

import Chanterelle.Internal.Types.Bytecode (Bytecode, emptyBytecode, fromSolidityBytecodeOutput)
import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.Lens (Lens', Getter', lens', lens, to)
import Data.Lens.At (at)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Foreign.Object as M
import Language.Solidity.Compiler.Types as ST
import Network.Ethereum.Web3 (Address, HexString, BlockNumber)

type ArtifactBytecodeR a = Record
  ( bytecode :: Bytecode -- type(ContractName).creationCode
  , deployedBytecode :: Bytecode -- type(ContractName).runtimeCode
  | a
  )

type UndeployedArtifact = ArtifactBytecodeR ()
type DeployedArtifact = ArtifactBytecodeR
  ( address         :: Address
  , blockHash       :: HexString
  , blockNumber     :: BlockNumber
  , transactionHash :: HexString
  )

data NetworkInfo = Undeployed UndeployedArtifact
                 | Deployed DeployedArtifact
derive instance eqNetworkInfo  :: Eq NetworkInfo
derive instance ordNetworkInfo :: Ord NetworkInfo

instance decodeJsonNetworkInfo :: DecodeJson NetworkInfo where
  decodeJson j = (Deployed <$> decodeJson j) <|> (Undeployed <$> decodeJson j)

instance encodeJsonNetworkInfo :: EncodeJson NetworkInfo where
  encodeJson (Undeployed u) = encodeJson u
  encodeJson (Deployed d) = encodeJson d

newtype Artifact = Artifact
  { abi :: Array Json
  , code :: ArtifactBytecodeR ()
  , lastModified :: Number
  , networks :: M.Object NetworkInfo
  }
derive instance eqArtifact :: Eq Artifact
derive instance ordArtifact :: Ord Artifact
derive instance newtypeArtifact :: Newtype Artifact _
derive newtype instance decodeJsonArtifact :: DecodeJson Artifact
derive newtype instance encodeJsonArtifact :: EncodeJson Artifact

newtype ArtifactBytecode = ArtifactBytecode (ArtifactBytecodeR ())
derive instance newtypeArtifactBytecode :: Newtype ArtifactBytecode _
derive newtype instance eqArtifactBytecode  :: Eq ArtifactBytecode
derive newtype instance ordArtifactBytecode :: Ord ArtifactBytecode
derive newtype instance decodeJsonArtifactBytecode :: DecodeJson ArtifactBytecode
derive newtype instance encodeJsonArtifactBytecode :: EncodeJson ArtifactBytecode

emptyArtifactBytecode :: ArtifactBytecode
emptyArtifactBytecode = ArtifactBytecode { bytecode: emptyBytecode, deployedBytecode: emptyBytecode }

fromSolidityContractLevelOutput :: ST.ContractLevelOutput -> Either String Artifact
fromSolidityContractLevelOutput (ST.ContractLevelOutput clo) = do
  abi <- lmap printJsonDecodeError <<< decodeJson =<< note "Solidity contract output did not have an \"abi\" field" clo.abi
  (ST.EvmOutput evm) <- note "Solidity contract output did not have an \"evm\" field" clo.evm
  bytecode' <- note "Solidity contract output did not have an \"evm.bytecode\" field" evm.bytecode
  bytecode <- fromSolidityBytecodeOutput bytecode'
  deployedBytecode' <- note "Solidity contract output did not have an \"evm.deployedBytecode\" field" evm.deployedBytecode
  deployedBytecode <- fromSolidityBytecodeOutput deployedBytecode'
  let lastModified = top
  pure $ Artifact { abi, code: { bytecode, deployedBytecode }, lastModified, networks: M.empty }

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
-- TODO: the toNI is not used, so Lens' can be replaced with Getter'
_NetworkBytecode :: Lens' NetworkInfo ArtifactBytecode
_NetworkBytecode = lens fromNI toNI
  where
    fromNI (Deployed { bytecode, deployedBytecode }) = ArtifactBytecode { bytecode, deployedBytecode }
    fromNI (Undeployed u) = ArtifactBytecode u

    toNI d@(Deployed o) (ArtifactBytecode n)  =
      if o.bytecode == n.bytecode && o.deployedBytecode == n.deployedBytecode
      then d
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
