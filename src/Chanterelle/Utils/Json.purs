module Chanterelle.Utils.Json where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut (class DecodeJson, Json, decodeJson, encodeJson, jsonParser, printJsonDecodeError, (.:), (.:!))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, note)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Foreign.Object (Object)
import Network.Ethereum.Core.BigNumber (fromInt, fromString, toString, unsafeToInt)
import Network.Ethereum.Web3 (Address, BigNumber, BlockNumber(..), HexString, mkAddress, mkHexString, unAddress)
import Partial.Unsafe (unsafePartial)

foreign import jsonStringifyWithSpaces :: Int -> Json -> String

decodeJsonBlockNumber :: Json -> Either String BlockNumber
decodeJsonBlockNumber = map BlockNumber <<< decodeJsonBigNumber

encodeJsonBlockNumber :: BlockNumber -> Json
encodeJsonBlockNumber (BlockNumber n) = encodeJsonBigNumber n

decodeJsonBigNumber :: Json -> Either String BigNumber
decodeJsonBigNumber j = decodeFromString <|> decodeFromNumber <|> (Left "Value is neither a String nor Number")
  where
  decodeFromString = lmap printJsonDecodeError (decodeJson j) >>= (note "BigNumber is not a Hex String" <<< fromString =<< _)
  decodeFromNumber = fromInt <$> (lmap printJsonDecodeError (decodeJson j) :: Either String Int)

encodeJsonBigNumber :: BigNumber -> Json
encodeJsonBigNumber n = encodeJson (unsafePartial $ fromJust $ mkHexString $ toString n)

encodeJsonConfigBigNumber :: BigNumber -> Json
encodeJsonConfigBigNumber = encodeJson <<< unsafeToInt

encodeJsonConfigBlockNumber :: BlockNumber -> Json
encodeJsonConfigBlockNumber (BlockNumber n) = encodeJsonConfigBigNumber (n)

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

-- getField (aka .?) with a manual decoder
gfWithDecoder :: forall a. (Json -> Either String a) -> Object Json -> String -> Either String a
gfWithDecoder decode obj k = lmap printJsonDecodeError (obj .: k) >>= decode

-- getFieldOptional (aka .??) with a manual decoder
gfoWithDecoder :: forall a. (Json -> Either String a) -> Object Json -> String -> Either String (Maybe a)
gfoWithDecoder decode obj key = lmap printJsonDecodeError (obj .:! key) >>= maybe (pure Nothing) (map Just <<< decode)

parseDecodeM
  :: forall m j
   . DecodeJson j
  => Monad m
  => MonadThrow String m
  => String
  -> m j
parseDecodeM = either throwError pure <<< (lmap printJsonDecodeError <<< decodeJson <=< jsonParser)
