module Foam where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except (runExceptT)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Web3 (ETH, defaultTransactionOptions, mkAddress, mkHexString, _from, _gas)
import Network.Ethereum.Web3.Types.BigNumber (parseBigNumber, decimal)
import Node.FS.Aff (FS)
import Node.Process (PROCESS, lookupEnv)
import Partial.Unsafe (unsafePartial)
import Control.Monad.Reader.Class (ask)
import Contracts.ParkingAuthority as ParkingAuthority

import Deploy (deployContractWithArgs)
import Utils (makeDeployConfig, validateDeployArgs)
import ContractConfig (makeParkingAuthorityConfig)
import Types (DeployConfig(..), runDeployM, logDeployError)

main :: forall e. Eff (console :: CONSOLE, eth :: ETH, fs :: FS, process :: PROCESS | e) Unit
main = void $ do
  foamCSRAddress <- unsafeCoerceEff $ lookupEnv "FOAM_CSR"
  launchAff $ do
    edeployConfig <- runExceptT $ makeDeployConfig
    case edeployConfig of
      Left err -> logDeployError err *> pure unit
      Right deployConfig -> do
        eRes <- flip runDeployM deployConfig $ do
          deployCfg@(DeployConfig {primaryAccount}) <- ask
          let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "4712388"
              txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                                 # _gas ?~ bigGasLimit
          let foamCSR = unsafePartial fromJust $ mkAddress =<< mkHexString =<< foamCSRAddress
          let parkingAuthorityConfig = makeParkingAuthorityConfig {foamCSR}
          _ <- deployContractWithArgs parkingAuthorityConfig $ ParkingAuthority.constructor txOpts
          pure unit
        case eRes of
          Left err -> logDeployError err
          Right _ -> pure unit
