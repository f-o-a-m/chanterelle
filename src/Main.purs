module Main where

import Prelude
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Web3 (ETH, defaultTransactionOptions, _from, _gas)
import Network.Ethereum.Web3.Types.BigNumber (parseBigNumber, decimal)
import Node.FS.Aff (FS)
import Node.Process (PROCESS)
import Partial.Unsafe (unsafePartial)
import Control.Monad.Reader.Class (ask)
import Contracts.SimpleStorage as SimpleStorage
import Contracts.ParkingAuthority as ParkingAuthority
import Deploy (deployContractWithArgs, deployContractNoArgs)
import Utils (makeDeployConfig, validateDeployArgs)
import ContractConfig (simpleStorageConfig, foamCSRConfig, makeParkingAuthorityConfig)
import Types (DeployConfig(..), runDeployM, logDeployError)

main :: forall e. Eff (console :: CONSOLE, eth :: ETH, fs :: FS, process :: PROCESS, exception :: EXCEPTION | e) Unit
main = void $ launchAff $ mainDeploy


mainDeploy :: forall e. Aff (console :: CONSOLE, eth :: ETH, fs :: FS, process :: PROCESS | e) Unit
mainDeploy = void $ do
  edeployConfig <- runExceptT $ makeDeployConfig
  case edeployConfig of
    Left err -> logDeployError err *> pure unit
    Right deployConfig -> do
      eRes <- flip runDeployM deployConfig $ do
        deployCfg@(DeployConfig {primaryAccount}) <- ask
        let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "9000000"
            txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                               # _gas ?~ bigGasLimit
        ssConfig <- validateDeployArgs simpleStorageConfig
        _ <- deployContractWithArgs ssConfig $ SimpleStorage.constructor txOpts
        foamCSR <- deployContractNoArgs foamCSRConfig txOpts
        let parkingAuthorityConfig = makeParkingAuthorityConfig {foamCSR}
        _ <- deployContractWithArgs parkingAuthorityConfig $ ParkingAuthority.constructor txOpts
        pure unit
      case eRes of
        Left err -> logDeployError err
        Right _ -> pure unit
