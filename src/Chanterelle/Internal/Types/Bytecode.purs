module Chanterelle.Internal.Types.Bytecode 
  ( Bytecode(..)
  , LinkReference(..)
  , SolcBytecode(..)
  , linkLibrary
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, (:=), (~>), (.?), (.??), decodeJson, jsonEmptyObject)
import Data.Array (concatMap, length, uncons)
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..), maybe)
import Foreign.Object (Object)
import Foreign.Object as SM
import Data.String (splitAt)
import Data.Tuple (Tuple(..))
import Network.Ethereum.Core.HexString (HexString, mkHexString, unHex)
import Network.Ethereum.Core.Signatures (Address, unAddress)

newtype LinkReference = LinkReference { start :: Int, length :: Int }

data Bytecode = BCLinked { bytecode :: HexString }
              | BCUnlinked { rawBytecode :: String, linkReferences :: Object (Array LinkReference) }

newtype SolcBytecode = SolcBytecode Bytecode


instance decodeJsonSolcBytecode :: DecodeJson SolcBytecode where
  decodeJson o = do
    obj <- decodeJson o
    rawBytecode <- obj .? "object"
    solcLinkRefs <- maybe (SM.empty) identity <$> obj .?? "linkReferences"
    let linkReferences = flattenLinkReferences solcLinkRefs
    SolcBytecode <$> normalizeUnlinked (BCUnlinked { rawBytecode, linkReferences })

instance decodeJsonLinkReference :: DecodeJson LinkReference where
  decodeJson o = do
    obj <- decodeJson o
    start <- obj .? "start"
    length <- obj .? "length"
    pure (LinkReference { start, length })

instance encodeJsonLinkReference :: EncodeJson LinkReference where
  encodeJson (LinkReference { start, length })
    =  "start" := start
    ~> "length" := length
    ~> jsonEmptyObject

instance decodeJsonBytecode :: DecodeJson Bytecode where
  decodeJson o = do
    obj <- decodeJson o
    rawBytecode <- obj .? "object"
    linkReferences <- maybe (SM.empty) identity <$> obj .?? "linkReferences"
    normalizeUnlinked (BCUnlinked { rawBytecode, linkReferences })

instance encodeJsonBytecode :: EncodeJson Bytecode where
  encodeJson (BCLinked { bytecode }) 
    = "object" := unHex bytecode
    ~> jsonEmptyObject
  encodeJson (BCUnlinked { rawBytecode, linkReferences })
    =  "object" := rawBytecode
    ~> "linkReferences" := linkReferences
    ~> jsonEmptyObject

normalizeUnlinked :: Bytecode -> Either String Bytecode
normalizeUnlinked l@(BCLinked _) = Right l
normalizeUnlinked u@(BCUnlinked { rawBytecode, linkReferences }) =
  case length (SM.keys linkReferences) of
    0 -> case mkHexString rawBytecode of
            Nothing -> Left $ "Invalid bytecode hex, without any link references\n" <> rawBytecode
            Just bytecode -> Right (BCLinked { bytecode })
    _ -> Right u

linkLibrary :: String -> Address -> Bytecode -> Either String Bytecode
linkLibrary _ _ (BCLinked x) = Right (BCLinked x)
linkLibrary name address (BCUnlinked { rawBytecode, linkReferences })
  = case SM.lookup name linkReferences of
      Nothing -> Right (BCUnlinked { rawBytecode, linkReferences })
      Just refs -> do
        linkedBytecode <- spliceLinkRefs address refs rawBytecode
        let withoutLinked = SM.delete name linkReferences
            bcu = { rawBytecode: linkedBytecode, linkReferences: withoutLinked }
        normalizeUnlinked (BCUnlinked bcu)

spliceLinkRefs :: Address -> Array LinkReference -> String -> Either String String
spliceLinkRefs addr refs code = foldM (flip spliceLinkRef) code refs
  where addressHex = unHex (unAddress addr)
        spliceLinkRef (LinkReference { start, length }) = spliceString start length
        spliceString start length targetCode =
          let { before: clowns, after: rest } = splitAt (start * 2) targetCode -- *2 because solc's units are in bytes, not chars LOL
              { after: jokers }               = splitAt (length * 2) rest      -- ditto for *2
           in Right (clowns <> addressHex <> jokers)
          -- case splitAt (start * 2) targetCode of -- *2 because solc's units are in bytes, not chars LOL
          --   Nothing -> Left "couldn't splice code at offset start"
          --   Just { before: clowns, after: rest} -> case splitAt (length * 2) rest of -- ditto for *2
          --     Nothing -> Left "couldn't splice code at offset start+length"
          --     Just { after: jokers } -> Right (clowns <> addressHex <> jokers)

-- solc returns linkReferences as { "sourceFileName": "libraryContractName": [ { linkRef }., ..] } }
-- the case that there are different library contracts with the same name residing in different source files
-- is currently unsupported, and ideally never should be...
flattenLinkReferences :: Object (Object (Array LinkReference)) -> Object (Array LinkReference)
flattenLinkReferences solcLinkRefs = upsertElems (SM.empty) (concatMap unfoldLinkRefs $ SM.toUnfoldable solcLinkRefs)
  where
    unfoldLinkRefs (Tuple _ v) = SM.toUnfoldable v
    upsertElem theMap (Tuple k v) = 
      case SM.lookup k theMap of
        Nothing -> SM.insert k v theMap
        Just ex -> SM.insert k (ex <> v) theMap
    upsertElems theMap elems =
      case uncons elems of
        Nothing -> theMap
        Just { head, tail } -> upsertElems (upsertElem theMap head) tail