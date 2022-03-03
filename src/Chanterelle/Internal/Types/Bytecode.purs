module Chanterelle.Internal.Types.Bytecode
  ( Bytecode(..)
  , LibraryLinkReferences
  , fromSolidityBytecodeOutput
  , emptyBytecode
  , linkLibrary
  , unlinkedLibraryNames
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, (:=), (~>), (.:), (.:!), (.!=), decodeJson, jsonEmptyObject)
import Data.Array (concatMap, length, uncons)
import Data.Either (Either(..), note)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.String (splitAt)
import Data.Tuple (Tuple(..))
import Foreign.Object as SM
import Language.Solidity.Compiler.Types as ST
import Network.Ethereum.Core.HexString (HexString, fromAscii, mkHexString, unHex)
import Network.Ethereum.Core.Signatures (Address, unAddress)

-- | What unlinked code means:
-- |
-- | ```js
-- | var solc = require('solc');
-- |
-- | input = {
-- |   language: 'Solidity',
-- |   settings: {
-- |     libraries: {
-- |       'lib.sol': {
-- |         // Comment this out to make a bytecode with unlinked libraries
-- |         // L: '0x4200000000000000000000000000000000000001'
-- |       }
-- |     },
-- |     outputSelection: {
-- |       '*': {
-- |         '*': ['evm.bytecode']
-- |       }
-- |     }
-- |   },
-- |   sources: {
-- |     'lib.sol': {
-- |       content: 'library L { function f() public returns (uint) { return 7; } }'
-- |     },
-- |     'a.sol': {
-- |       content: 'import "lib.sol"; contract A { function g() public { L.f(); } }'
-- |     }
-- |   }
-- | };
-- |
-- | var output = JSON.parse(solc.compile(JSON.stringify(input)))
-- |
-- | console.log(output.contracts['a.sol']['A'].evm.bytecode)
-- | ```
-- |
-- | When the line is commented out, it will output unlinked code
-- |
-- | ```json
-- | {
-- |   linkReferences: { 'lib.sol': { L: [ { length: 20, start: 86 } ] } },
-- |   object: 'binary...__$7658e08c4e23aceed01ae97f6c6f1bccc3$__...',
-- |   opcodes: '... PUSH1 0x0 ...',
-- | }
-- | ```
-- |
-- | When the line is NOT commented out, it will output linked code
-- |
-- | ```json
-- | {
-- |   linkReferences: {},
-- |   object: 'binary...4200000000000000000000000000000000000001...',
-- |   opcodes: '... PUSH20 0x4200000000000000000000000000000000000001 ...',
-- | }
-- | ```
-- |
-- | For more info check https://github.com/ethereum/solc-js/blob/master/test/linker.ts

type LibraryLinkReferences = ST.ContractMapped (Array ST.LinkReference)

data Bytecode
  = BCLinked
    { bytecode :: HexString
    , linkReferences :: LibraryLinkReferences
    }
  | BCUnlinked
    { rawBytecode :: ST.BytecodeObject
    , remainingLinkReferences :: LibraryLinkReferences
    , linkReferences :: LibraryLinkReferences
    }
derive instance eqBytecode  :: Eq Bytecode
derive instance ordBytecode :: Ord Bytecode

emptyBytecode :: Bytecode
emptyBytecode = BCLinked { bytecode: fromAscii "", linkReferences: SM.empty }

fromSolidityBytecodeOutput :: ST.BytecodeOutput -> Either String Bytecode
fromSolidityBytecodeOutput (ST.BytecodeOutput o) = do
  rawBytecode <- note "Solidity bytecode output lacked an \"object\" field" o.object
  let linkReferences = maybe SM.empty (flattenLinkReferences <<< un ST.LinkReferences) o.linkReferences
  pure $ case rawBytecode of
    ST.BytecodeHexString bytecode -> BCLinked { bytecode, linkReferences }
    _ -> BCUnlinked { rawBytecode, linkReferences, remainingLinkReferences: linkReferences }

instance decodeJsonBytecode :: DecodeJson Bytecode where
  decodeJson o = do
    obj <- decodeJson o
    rawBytecode <- obj .: "object"
    linkReferences <- obj .:! "linkReferences" .!= SM.empty
    case rawBytecode of
      ST.BytecodeHexString bytecode -> pure $ BCLinked { bytecode, linkReferences }
      _ -> do
        remainingLinkReferences <- obj .:! "remainingLinkReferences" .!= linkReferences
        pure $ BCUnlinked { rawBytecode, linkReferences, remainingLinkReferences }

instance encodeJsonBytecode :: EncodeJson Bytecode where
  encodeJson (BCLinked { bytecode, linkReferences })
    =  "object" := unHex bytecode
    ~> "linkReferences" := linkReferences
    ~> jsonEmptyObject
  encodeJson (BCUnlinked { rawBytecode, linkReferences, remainingLinkReferences })
    =  "object" := rawBytecode
    ~> "linkReferences" := linkReferences
    ~> "remainingLinkReferences" := remainingLinkReferences
    ~> jsonEmptyObject

normalizeUnlinked :: Bytecode -> Either String Bytecode
normalizeUnlinked l@(BCLinked _) = Right l
normalizeUnlinked u@(BCUnlinked { rawBytecode, linkReferences, remainingLinkReferences }) =
  case length (SM.keys remainingLinkReferences) of
    0 -> case rawBytecode of
           ST.BytecodeHexString bytecode -> Right (BCLinked { bytecode, linkReferences })
           ST.BytecodeUnlinked s -> case mkHexString s of
            Nothing -> Left $ "Invalid bytecode hex, without any link references\n" <> s
            Just bytecode -> Right (BCLinked { bytecode, linkReferences })
    _ -> Right u

linkLibrary :: String -> Address -> Bytecode -> Either String Bytecode
linkLibrary _ _ (BCLinked x) = Right (BCLinked x)
linkLibrary name address (BCUnlinked ul)
  = case SM.lookup name ul.remainingLinkReferences of
      Nothing -> Right (BCUnlinked ul)
      Just refs ->
        let linkedBytecode = spliceLinkRefs address refs ul.rawBytecode
            withoutLinked  = SM.delete name ul.remainingLinkReferences
            bcu = { rawBytecode: linkedBytecode, linkReferences: ul.linkReferences, remainingLinkReferences: withoutLinked }
        in normalizeUnlinked (BCUnlinked bcu)

spliceLinkRefs :: Address -> Array ST.LinkReference -> ST.BytecodeObject -> ST.BytecodeObject
spliceLinkRefs addr refs = ST.mkBytecodeObject <<< go <<< ST.unBytecodeObject
  where go str = foldl (flip spliceLinkRef) str refs
        addressHex = unHex (unAddress addr)
        spliceLinkRef (ST.LinkReference { start, length }) = spliceString start length
        spliceString start length targetCode =
          let { before: clowns, after: rest } = splitAt (start * 2) targetCode -- *2 because solc's units are in bytes, not chars LOL
              { after: jokers }               = splitAt (length * 2) rest      -- ditto for *2
           in (clowns <> addressHex <> jokers)

-- solc returns linkReferences as { "libraryFile.sol": { "LibraryContractName": [ { "start": 0, "length": 20 }, ...] } }
-- the case that there are different library contracts with the same name residing in different source files
-- is currently unsupported, and ideally never should be...
flattenLinkReferences :: ST.FileMapped LibraryLinkReferences -> LibraryLinkReferences
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

unlinkedLibraryNames :: Bytecode -> Array String
unlinkedLibraryNames (BCLinked _) = []
unlinkedLibraryNames (BCUnlinked u) = SM.keys u.remainingLinkReferences
