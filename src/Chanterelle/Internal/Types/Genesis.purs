module Chanterelle.Internal.Types.Genesis where

import Prelude (bind, pure, show, ($), (<$>), (<<<), (=<<), (>>=))
import Control.Alt ((<|>))
import Chanterelle.Internal.Types.Compile (CompileError)
import Chanterelle.Internal.Types.Project (Network)
import Chanterelle.Internal.Utils.Json
import Data.Argonaut (class DecodeJson, class EncodeJson, (:=), (~>), (.?), (.??), decodeJson, encodeJson, jsonEmptyObject)
import Data.Array (catMaybes)
import Data.Maybe (Maybe, maybe)
import Foreign.Object (Object)
import Foreign.Object as M
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Network.Ethereum.Web3 (Address, BigNumber, BlockNumber, HexString, mkHexString, unAddress, unHex)
import Node.Path (FilePath)

data TemplatableHexString = UntemplatedHexString HexString
                          | TemplatedHexString String

instance decodeJsonTemplatableHexString :: DecodeJson TemplatableHexString where
    decodeJson j = do
        str <- decodeJson j
        pure $ maybe (TemplatedHexString str) UntemplatedHexString (mkHexString str)

instance encodeJsonTemplatableHexString :: EncodeJson TemplatableHexString where
    encodeJson (TemplatedHexString str)   = encodeJson str
    encodeJson (UntemplatedHexString str) = encodeJsonHexString str

newtype GenesisConfig
    = GenesisConfig { chainId        :: Maybe BigNumber
                    , byzantiumBlock :: Maybe BlockNumber
                    , eip150Block    :: Maybe BlockNumber
                    , eip150Hash     :: Maybe HexString
                    , eip155Block    :: Maybe BlockNumber
                    , eip158Block    :: Maybe BlockNumber
                    , homesteadBlock :: Maybe BlockNumber
                    , clique         :: Maybe CliqueSettings
                    }

newtype CliqueSettings 
    = CliqueSettings { period :: BigNumber
                     , epoch  :: BigNumber
                     }

instance decodeJsonGenesisConfig :: DecodeJson GenesisConfig where
    decodeJson j = do
        obj <- decodeJson j
        chainId        <- gfoWithDecoder decodeJsonBigNumber   obj "chainId"
        byzantiumBlock <- gfoWithDecoder decodeJsonBlockNumber obj "byzantiumBlock"
        eip150Block    <- gfoWithDecoder decodeJsonBlockNumber obj "eip150Block"
        eip150Hash     <- gfoWithDecoder decodeJsonHexString   obj "eip150Hash"
        eip155Block    <- gfoWithDecoder decodeJsonBlockNumber obj "eip155Block"
        eip158Block    <- gfoWithDecoder decodeJsonBlockNumber obj "eip158Block"
        homesteadBlock <- gfoWithDecoder decodeJsonBlockNumber obj "homesteadBlock"
        clique         <- obj .?? "clique"
        pure $ GenesisConfig { chainId, byzantiumBlock, eip150Block, eip150Hash, eip155Block, eip158Block, homesteadBlock, clique }

instance encodeJsonGenesisConfig :: EncodeJson GenesisConfig where
    encodeJson (GenesisConfig gc) = encodeJson $ M.fromFoldable elems
        where elems = catMaybes [ci, bB, e150B, e150H, e155B, e158B, hB, cq]
              ci    = (Tuple "chainId"        <<< encodeJsonConfigBigNumber  ) <$> gc.chainId
              bB    = (Tuple "byzantiumBlock" <<< encodeJsonConfigBlockNumber) <$> gc.byzantiumBlock
              e150B = (Tuple "eip150Block"    <<< encodeJsonConfigBlockNumber) <$> gc.eip150Block
              e150H = (Tuple "eip150Hash"     <<< encodeJsonHexString        ) <$> gc.eip150Hash
              e155B = (Tuple "eip155Block"    <<< encodeJsonConfigBlockNumber) <$> gc.eip155Block
              e158B = (Tuple "eip158Block"    <<< encodeJsonConfigBlockNumber) <$> gc.eip158Block
              hB    = (Tuple "homesteadBlock" <<< encodeJsonConfigBlockNumber) <$> gc.homesteadBlock
              cq    = (Tuple "clique"         <<< encodeJson                 ) <$> gc.clique

instance decodeJsonCliqueSettings :: DecodeJson CliqueSettings where
    decodeJson j = do
        obj <- decodeJson j
        period <- decodeJsonBigNumber =<< obj .? "period"
        epoch <- decodeJsonBigNumber =<< obj .? "epoch"
        pure $ CliqueSettings { period, epoch }

instance encodeJsonCliqueSettings :: EncodeJson CliqueSettings where
    encodeJson (CliqueSettings clique) =
         "period" := encodeJsonConfigBigNumber clique.period
      ~> "epoch"  := encodeJsonConfigBigNumber clique.epoch
      ~> jsonEmptyObject

newtype GenesisAlloc = GenesisAlloc { code    :: Maybe HexString
                                    , storage :: Maybe (Object String) -- this is a Object String to simplify parsing
                                    , balance :: BigNumber
                                    }
newtype GenesisAllocs = GenesisAllocs (Object GenesisAlloc)

lookupGenesisAllocs :: Address -> GenesisAllocs -> Maybe GenesisAlloc
lookupGenesisAllocs addr (GenesisAllocs allocs) =  M.lookup (show addr) allocs 
                                               <|> M.lookup (unHex $ unAddress addr) allocs

insertGenesisAllocs :: Address -> GenesisAlloc -> GenesisAllocs -> GenesisAllocs
insertGenesisAllocs addr alloc (GenesisAllocs allocs) = GenesisAllocs $ M.insert (unHex $ unAddress addr) alloc allocs

instance decodeJsonGenesisAlloc :: DecodeJson GenesisAlloc where
    decodeJson j = do
        obj <- decodeJson j
        code <- gfoWithDecoder decodeJsonHexString obj "code"
        storage <- obj .?? "storage"
        balance <- gfWithDecoder decodeJsonBigNumber obj "balance"
        pure $ GenesisAlloc { code, storage, balance }

instance encodeJsonGenesisAlloc :: EncodeJson GenesisAlloc where
    encodeJson (GenesisAlloc alloc) = encodeJson $ M.fromFoldable elems
        where elems   = catMaybes [balance, code, storage]
              code    = (Tuple "code" <<< encodeJsonHexString) <$> alloc.code
              storage = (Tuple "storage" <<< encodeJson) <$> alloc.storage
              balance = pure $ Tuple "balance" (encodeJsonBigNumber alloc.balance)

instance decodeJsonGenesisAllocs :: DecodeJson GenesisAllocs where
    decodeJson j = decodeJson j >>= (\obj -> GenesisAllocs <$> for obj decodeJson)

instance encodeJsonGenesisAllocs :: EncodeJson GenesisAllocs where
    encodeJson (GenesisAllocs allocs) = encodeJson $ encodeJson <$> allocs

newtype GenesisBlock
    = GenesisBlock { config     :: GenesisConfig
                   , allocs     :: GenesisAllocs
                   , nonce      :: BigNumber
                   , coinbase   :: Address
                   , extraData  :: TemplatableHexString
                   , gasLimit   :: BigNumber
                   , mixHash    :: HexString
                   , parentHash :: HexString
                   , timestamp  :: BigNumber
                   , difficulty :: BigNumber
                   }

instance decodeJsonGenesisBlock :: DecodeJson GenesisBlock where
    decodeJson j = do
        obj        <- decodeJson j
        config     <- obj .? "config"
        allocs     <- obj .? "alloc"
        nonce      <- obj .? "nonce"      >>= decodeJsonBigNumber
        coinbase   <- gfWithDecoder decodeJsonAddress obj "coinbase"
        extraData  <- obj .? "extraData"
        gasLimit   <- obj .? "gasLimit"   >>= decodeJsonBigNumber
        mixHash    <- obj .? "mixHash"    >>= decodeJsonHexString
        parentHash <- obj .? "parentHash" >>= decodeJsonHexString
        timestamp  <- obj .? "timestamp"  >>= decodeJsonBigNumber
        difficulty <- obj .? "difficulty" >>= decodeJsonBigNumber
        pure $ GenesisBlock { config, allocs, nonce, coinbase, extraData, gasLimit, mixHash, parentHash, timestamp, difficulty }

instance encodeJsonGenesisBlock :: EncodeJson GenesisBlock where
    encodeJson (GenesisBlock gen) =
         "config"     := encodeJson gen.config
      ~> "alloc"      := encodeJson gen.allocs
      ~> "nonce"      := encodeJsonBigNumber gen.nonce
      ~> "coinbase"   := encodeJsonAddress gen.coinbase
      ~> "extraData"  := encodeJson gen.extraData
      ~> "gasLimit"   := encodeJsonBigNumber gen.gasLimit
      ~> "mixHash"    := encodeJsonHexString gen.mixHash
      ~> "parentHash" := encodeJsonHexString gen.parentHash
      ~> "timestamp"  := encodeJsonBigNumber gen.timestamp
      ~> "difficulty" := encodeJsonBigNumber gen.difficulty
      ~> jsonEmptyObject

data GenesisGenerationError = CouldntLoadGenesisBlock FilePath String
                            | CouldntInjectLibraryAddress String String
                            | CouldntResolveLibrary String (Array (Tuple Network String))
                            | CouldntResolveLibraryNoNetworks String
                            | CouldntInjectLibrary String String
                            | CouldntCompileLibrary String CompileError
                            | MalformedProjectErrorG String
                            | NothingToDo String

