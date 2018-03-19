module Deploy where

import Prelude
import Control.Error.Util ((??))
import Control.Monad.Aff (Aff)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Argonaut (_Object, _String)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Network.Ethereum.Web3 (HexString, mkHexString)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (FS, readTextFile)
import Node.Path (FilePath)


getBytecode :: forall eff. FilePath -> Aff (fs :: FS | eff) (Either String HexString)
getBytecode filename = runExceptT $ do
  artifact <- ExceptT $ jsonParser <$> readTextFile UTF8 filename
  bytecode <- (artifact ^? _Object <<< ix "bytecode" <<< _String) ?? "artifact missing 'bytecode' field."
  mkHexString bytecode ?? "bytecode not a valid hex string"

