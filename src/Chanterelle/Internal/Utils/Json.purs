module Chanterelle.Internal.Utils.Json where

import Prelude

import Chanterelle.Internal.Utils.Error (except')
import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut (class DecodeJson, Json, decodeJson, encodeJson, jsonParser, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Network.Ethereum.Core.BigNumber (hexadecimal, parseBigNumber, toString, unsafeToInt)
import Network.Ethereum.Web3 (Address, BigNumber, BlockNumber(..), HexString, embed, mkAddress, mkHexString, unAddress)

-- TODO(srghma): actually the decode and encode classes already exist for all these types (except of unsafe `encodeJsonConfigBigNumber` and `encodeJsonConfigBigNumber` of course)

decodeJsonBlockNumber :: Json -> Either String BlockNumber -- TODO(srghma): return Either JsonDecodeError BlockNumber
decodeJsonBlockNumber = map BlockNumber <<< decodeJsonBigNumber

encodeJsonBlockNumber :: BlockNumber -> Json
encodeJsonBlockNumber (BlockNumber n) = encodeJsonBigNumber n

decodeJsonBigNumber :: Json -> Either String BigNumber
decodeJsonBigNumber j = decodeFromString <|> decodeFromNumber <|> (Left "Value is neither a String nor Number")
    where decodeFromString = lmap printJsonDecodeError (decodeJson j) >>= (note "BigNumber is not a Hex String" <<< (parseBigNumber hexadecimal =<< _))
          decodeFromNumber = embed <$> (lmap printJsonDecodeError (decodeJson j) :: Either String Int)

encodeJsonBigNumber :: BigNumber -> Json
encodeJsonBigNumber n = encodeJson ("0x" <> toString hexadecimal n)

encodeJsonConfigBigNumber :: BigNumber -> Json
encodeJsonConfigBigNumber = encodeJson <<< unsafeToInt

encodeJsonConfigBlockNumber :: BlockNumber -> Json
encodeJsonConfigBlockNumber (BlockNumber n) = encodeJsonConfigBigNumber n

decodeJsonHexString :: Json -> Either String HexString
decodeJsonHexString j = lmap printJsonDecodeError (decodeJson j) >>= note "HexString is not a valid Hex String" <<< mkHexString

encodeJsonHexString :: HexString -> Json
encodeJsonHexString = encodeJson <<< show

encodeJsonAddress :: Address -> Json
encodeJsonAddress = encodeJson <<< show <<< unAddress

decodeJsonAddress :: Json -> Either String Address
decodeJsonAddress j = do
    s <- lmap printJsonDecodeError $ decodeJson j
    h <- note "Address is not a valid HexString" $ mkHexString s
    note "Address is malformed" $ mkAddress h

parseDecodeM
  :: forall m j
   . DecodeJson j
  => Monad m
  => MonadThrow String m
  => String
  -> m j
parseDecodeM = except' <<< (lmap printJsonDecodeError <<< decodeJson <=< jsonParser)
