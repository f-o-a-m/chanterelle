module Chanterelle.Genesis where

import Prelude
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (ChanterelleProject(..), ChanterelleProjectSpec(..), ChanterelleModule(..), Libraries(..), Library(..), runCompileM, logCompileError, isFixedLibrary)
import Control.Alt ((<|>))
import Control.Error.Util (note)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Random (RANDOM, randomRange)
import Control.Monad.Eff.Exception (try)
import Data.Argonaut as A
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, (:=), (~>), (.?), (.??), decodeJson, encodeJson, jsonEmptyObject)
import Data.Array ((!!), replicate, null, all)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, elem)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (mempty)
import Data.String as S
import Data.StrMap (StrMap)
import Data.StrMap as M
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync as FSS
import Node.Path (FilePath)
import Network.Ethereum.Web3 (Address, BigNumber, BlockNumber(..), HexString, embed, hexadecimal, mkAddress, mkHexString, parseBigNumber, toString, unAddress, unHex)

decodeJsonBlockNumber :: Json -> Either String BlockNumber
decodeJsonBlockNumber = map BlockNumber <<< decodeJsonBigNumber

encodeJsonBlockNumber :: BlockNumber -> Json
encodeJsonBlockNumber (BlockNumber n) = encodeJsonBigNumber n

decodeJsonBigNumber :: Json -> Either String BigNumber
decodeJsonBigNumber j = decodeFromString <|> decodeFromNumber <|> (Left "Value is neither a String nor Number")
    where decodeFromString = decodeJson j >>= (note "BigNumber is not a Hex String" <<< (parseBigNumber hexadecimal =<< _))
          decodeFromNumber = embed <$> (decodeJson j :: Either String Int)

encodeJsonBigNumber :: BigNumber -> Json
encodeJsonBigNumber n = encodeJson ("0x" <> toString hexadecimal n)

decodeJsonHexString :: Json -> Either String HexString
decodeJsonHexString j = decodeJson j >>= note "HexString is not a valid Hex String" <<< mkHexString

encodeJsonHexString :: HexString -> Json
encodeJsonHexString = encodeJson <<< show

encodeJsonAddress :: Address -> Json
encodeJsonAddress = encodeJson <<< show <<< unAddress

-- getFieldOptional (aka .??) with a manual decoder
gfoWithDecoder :: forall a. (Json -> Either String a) -> A.JObject -> String -> Either String (Maybe a)
gfoWithDecoder decode obj key = maybe (pure Nothing) decode' (M.lookup key obj)
    where decode' = map Just <<< decode

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
              ci    = maybe mempty (pure <<< Tuple "chainId"        <<< encodeJsonBigNumber)   gc.chainId
              bB    = maybe mempty (pure <<< Tuple "byzantiumBlock" <<< encodeJsonBlockNumber) gc.byzantiumBlock
              e150B = maybe mempty (pure <<< Tuple "eip150Block"    <<< encodeJsonBlockNumber) gc.eip150Block
              e150H = maybe mempty (pure <<< Tuple "eip150Hash"     <<< encodeJsonHexString)   gc.eip150Hash
              e155B = maybe mempty (pure <<< Tuple "eip155Block"    <<< encodeJsonBlockNumber) gc.eip155Block
              e158B = maybe mempty (pure <<< Tuple "eip158Block"    <<< encodeJsonBlockNumber) gc.eip158Block
              hB    = maybe mempty (pure <<< Tuple "homesteadBlock" <<< encodeJsonBlockNumber) gc.homesteadBlock
              cq    = maybe mempty (pure <<< Tuple "clique"         <<< encodeJson)            gc.clique

instance decodeJsonCliqueSettings :: DecodeJson CliqueSettings where
    decodeJson j = do
        obj <- decodeJson j
        period <- decodeJsonBigNumber =<< obj .? "period"
        epoch <- decodeJsonBigNumber =<< obj .? "epoch"
        pure $ CliqueSettings { period, epoch }

instance encodeJsonCliqueSettings :: EncodeJson CliqueSettings where
    encodeJson (CliqueSettings clique) =
         "period" := encodeJsonBigNumber clique.period
      ~> "epoch"  := encodeJsonBigNumber clique.epoch
      ~> jsonEmptyObject

newtype GenesisAlloc = GenesisAlloc { code    :: Maybe HexString
                                    , storage :: Maybe (StrMap String) -- this is a StrMap String to simplify parsing
                                    , balance :: BigNumber
                                    }
newtype GenesisAllocs = GenesisAllocs (StrMap GenesisAlloc)

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

substituteLibraryAddress :: HexString -> Address -> Either String HexString
substituteLibraryAddress hsBytecode target = ret
    where -- length of the asm PUSH20 <ownaddr>; ADDRESS; EQ
          -- which is part of the library's nonpayability guard
          -- so 27 bytes of opcodes *2 cause it's hex
          minBytecodeLength = 46

          -- evm opcodes
          op_push20'     = "73"
          op_push20 addr = op_push20' <> (unHex $ unAddress addr)
          op_addr        = "30"
          op_eq          = "14"

          -- work against the raw hex string
          bytecode = unHex hsBytecode

          -- split at minBytecodeLength since we have to edit before that
          bcsplit = case S.splitAt minBytecodeLength bytecode of
                      Nothing -> { preamble: "", code: "" }
                      Just s  -> { preamble: s.before, code: s.after }

          -- is the first instruction a PUSH20?
          firstByteIsPush20 = S.take 2 bcsplit.preamble == op_push20'

          -- are the last instructions in preamble ADDRESS; EQ?
          preambleEndsInAddrEq = S.takeRight 4 bcsplit.preamble == (op_addr <> op_eq)

          -- if yes to both then it's a library
          firstBytesAreLibPreamble = firstByteIsPush20 && preambleEndsInAddrEq

          newPreamble = (op_push20 target) <> op_addr <> op_eq

          ret | S.length bytecode <= minBytecodeLength = Left "Bytecode too short to be a library"
              | firstBytesAreLibPreamble == false      = Left "Bytecode does not look like a library"
              | otherwise                              = note "Couldn't make a valid HexString" $ mkHexString (newPreamble <> bcsplit.code)

generateAddress :: forall eff m f
                 . MonadEff (random :: RANDOM | eff) m
                => Foldable f
                => f Address
                -> m Address
generateAddress blacklist = do
  addr' <- randomAddress
  case mkHexAddress addr' of
    Nothing -> do
      log Debug $ "Generated address " <> show addr' <> " which isn't actually a valid address"
      generateAddress blacklist
    Just addr -> if addr `elem` blacklist
                   then do
                     log Debug $ "Generated address " <> show addr <> " which is blacklisted, retrying"
                     generateAddress blacklist
                   else do
                     log Debug $ "Successfully generated address " <> show addr
                     pure addr
  where randomHexDigit = toHex <<< floor <$> liftEff (randomRange (toNumber 0) (toNumber 16)) -- 0 inclusive, 16 exclusive, aka 0-F
        hexDigits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']
        randomAddress = S.fromCharArray <$> (sequence $ replicate 40 randomHexDigit)
        toHex n = fromMaybe '0' $ hexDigits !! n
        mkHexAddress s = mkHexString s >>= mkAddress

data GenesisGenerationError = CouldntLoadGenesisBlock FilePath String
                            | CouldntInjectLibraryAddress String String
                            | NothingToDo String

-- should ExceptT this stuff tbh
generateGenesis :: forall eff m
                 . MonadEff (random :: RANDOM, fs :: FS | eff) m
                => ChanterelleProject
                -> FilePath
                -> m (Either GenesisGenerationError GenesisBlock)
generateGenesis (ChanterelleProject project) genesisIn =
  if nothingToDo
    then loadGenesisIn
    else loadGenesisIn
  where (ChanterelleProjectSpec spec) = project.spec
        (Libraries libs)              = spec.libraries
        nothingToDo = null libs || all isFixedLibrary libs
        loadGenesisIn = (liftEff <<< try $ FSS.readTextFile UTF8 genesisIn) >>= case _ of
                          Left err -> pure $ Left (CouldntLoadGenesisBlock genesisIn $ show err)
                          Right gen' -> pure $ case (A.jsonParser gen' >>= A.decodeJson) of
                            Left err      -> Left $ CouldntLoadGenesisBlock genesisIn err
                            Right genesis -> Right genesis