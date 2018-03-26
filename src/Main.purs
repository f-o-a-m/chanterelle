module Main where

import Prelude
import Control.Monad.Aff (launchAff, liftEff')
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Web3 (ETH, defaultTransactionOptions, _from, _gas)
import Network.Ethereum.Web3.Types.BigNumber (parseBigNumber, decimal)
import Node.FS.Aff (FS)
import Partial.Unsafe (unsafePartial)
import Control.Monad.Reader.Class (ask)
import Contracts.SimpleStorage as SimpleStorage
import Contracts.ParkingAuthority as ParkingAuthority

import Deploy (deployContractWithArgs, deployContractNoArgs, runDeployM)
import Utils (makeDeployConfig, validateDeployArgs)
import ContractConfig (simpleStorageConfig, foamCSRConfig, makeParkingAuthorityConfig)
import Types (DeployConfig(..))


-- | TODO: This passing of config indicates a ReaderMonad
main :: forall e. Eff (eth :: ETH, console :: CONSOLE, fs :: FS | e) Unit
main = void <<< launchAff $ do
  deployConfig <- makeDeployConfig
  flip runDeployM deployConfig $ do
    deployCfg@(DeployConfig {primaryAccount}) <- ask
    let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "9000000"
        txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                           # _gas ?~ bigGasLimit
    ssConfig <- liftAff <<< liftEff' $ validateDeployArgs simpleStorageConfig
    _ <- deployContractWithArgs ssConfig $ SimpleStorage.constructor txOpts
    foamCSR <- deployContractNoArgs foamCSRConfig txOpts
    let parkingAuthorityConfig = makeParkingAuthorityConfig {foamCSR}
    _ <- deployContractWithArgs parkingAuthorityConfig $ ParkingAuthority.constructor txOpts
    pure unit
