module Chanterelle.Internal.Types.Genesis where

import Prelude (bind, pure, show, ($), (<$>), (<<<), (<>), (=<<), (>>=))
import Control.Alt ((<|>))
import Chanterelle.Internal.Types.Compile (CompileError)
import Chanterelle.Internal.Utils.Json
import Control.Error.Util (note)
import Data.Argonaut (class DecodeJson, class EncodeJson, (:=), (~>), (.?), (.??), decodeJson, encodeJson, jsonEmptyObject)
import Data.Maybe (Maybe, maybe)
import Data.Monoid (mempty)
import Data.StrMap (StrMap)
import Data.StrMap as M
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Network.Ethereum.Core.BigNumber (parseBigNumber, toString, hexadecimal)
import Network.Ethereum.Web3 (Address, BigNumber, BlockNumber, HexString,  mkAddress, mkHexString, unAddress, unHex)
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
        where elems = [] <> ci <> bB <> e150B <> e150H <> e155B <> e158B <> hB <> cq
              ci    = maybe mempty (pure <<< Tuple "chainId"        <<< encodeJsonConfigBigNumber)   gc.chainId
              bB    = maybe mempty (pure <<< Tuple "byzantiumBlock" <<< encodeJsonConfigBlockNumber) gc.byzantiumBlock
              e150B = maybe mempty (pure <<< Tuple "eip150Block"    <<< encodeJsonConfigBlockNumber) gc.eip150Block
              e150H = maybe mempty (pure <<< Tuple "eip150Hash"     <<< encodeJsonHexString)   gc.eip150Hash
              e155B = maybe mempty (pure <<< Tuple "eip155Block"    <<< encodeJsonConfigBlockNumber) gc.eip155Block
              e158B = maybe mempty (pure <<< Tuple "eip158Block"    <<< encodeJsonConfigBlockNumber) gc.eip158Block
              hB    = maybe mempty (pure <<< Tuple "homesteadBlock" <<< encodeJsonConfigBlockNumber) gc.homesteadBlock
              cq    = maybe mempty (pure <<< Tuple "clique"         <<< encodeJson)            gc.clique

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
                                    , storage :: Maybe (StrMap String) -- this is a StrMap String to simplify parsing
                                    , balance :: BigNumber
                                    }
newtype GenesisAllocs = GenesisAllocs (StrMap GenesisAlloc)

lookupGenesisAllocs :: Address -> GenesisAllocs -> Maybe GenesisAlloc
lookupGenesisAllocs addr (GenesisAllocs allocs) =  M.lookup (show addr) allocs 
                                               <|> M.lookup (unHex $ unAddress addr) allocs

insertGenesisAllocs :: Address -> GenesisAlloc -> GenesisAllocs -> GenesisAllocs
insertGenesisAllocs addr alloc (GenesisAllocs allocs) = GenesisAllocs $ M.insert (unHex $ unAddress addr) alloc allocs

instance decodeJsonGenesisAlloc :: DecodeJson GenesisAlloc where
    decodeJson j = do
        obj <- decodeJson j
        code <- (mkHexString =<< _) <$> obj .?? "code"
        storage <- obj .?? "storage"
        balance <- obj .? "balance" >>= note "malformed balance" <<< (parseBigNumber hexadecimal)
        pure $ GenesisAlloc { code, storage, balance }

instance encodeJsonGenesisAlloc :: EncodeJson GenesisAlloc where
    encodeJson (GenesisAlloc alloc) = encodeJson $ M.fromFoldable elems
        where elems   = balance <> code <> storage
              code    = maybe mempty (pure <<< Tuple "code"    <<< encodeJsonHexString) alloc.code
              storage = maybe mempty (pure <<< Tuple "storage" <<< encodeJson         ) alloc.storage
              balance = [Tuple "balance" (encodeJson ("0x" <> toString hexadecimal alloc.balance))]

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
        coinbase   <- obj .? "coinbase"   >>= decodeJsonHexString >>= note "malformed coinbase"   <<< mkAddress
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
                            | CouldntInjectLibrary String String
                            | CouldntCompileLibrary String CompileError
                            | MalformedProjectErrorG String
                            | NothingToDo String
