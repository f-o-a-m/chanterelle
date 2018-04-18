module Chanterelle.Internal.Utils.Json where

import Prelude
import Control.Alt ((<|>))
import Control.Error.Util (note)
import Data.Argonaut (Json, JObject, decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M
import Network.Ethereum.Core.BigNumber (hexadecimal, parseBigNumber, toString, unsafeToInt)
import Network.Ethereum.Web3 (Address, BigNumber, BlockNumber(..), HexString, embed, mkHexString, unAddress)

foreign import jsonStringifyWithSpaces :: Int -> Json -> String

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

encodeJsonConfigBigNumber :: BigNumber -> Json
encodeJsonConfigBigNumber = encodeJson <<< unsafeToInt

encodeJsonConfigBlockNumber :: BlockNumber -> Json
encodeJsonConfigBlockNumber (BlockNumber n) = encodeJsonConfigBigNumber (n)

decodeJsonHexString :: Json -> Either String HexString
decodeJsonHexString j = decodeJson j >>= note "HexString is not a valid Hex String" <<< mkHexString

encodeJsonHexString :: HexString -> Json
encodeJsonHexString = encodeJson <<< show

encodeJsonAddress :: Address -> Json
encodeJsonAddress = encodeJson <<< show <<< unAddress

-- getFieldOptional (aka .??) with a manual decoder
gfoWithDecoder :: forall a. (Json -> Either String a) -> JObject -> String -> Either String (Maybe a)
gfoWithDecoder decode obj key = maybe (pure Nothing) decode' (M.lookup key obj)
    where decode' = map Just <<< decode
