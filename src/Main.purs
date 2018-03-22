module Main where

import Prelude
import Control.Monad.Aff (launchAff, liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Web3 (ETH, defaultTransactionOptions, _from, _gas)
import Network.Ethereum.Web3.Types.BigNumber (parseBigNumber, decimal)
import Node.FS.Aff (FS)
import Partial.Unsafe (unsafePartial)

import Contracts.SimpleStorage as SimpleStorage
import Contracts.ParkingAuthority as ParkingAuthority

import Deploy (deployContractWithArgs, deployContractNoArgs)
import Utils (makeDeployConfig, validateDeployArgs)
import ContractConfig (simpleStorageConfig, foamCSRConfig, makeParkingAuthorityConfig)


-- | TODO: This passing of config indicates a ReaderMonad
main :: forall e. Eff (eth :: ETH, console :: CONSOLE, fs :: FS | e) Unit
main = void <<< launchAff $ do
  deployCfg <- makeDeployConfig
  let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "9000000"
      txOpts = defaultTransactionOptions # _from ?~ deployCfg.primaryAccount
                                         # _gas ?~ bigGasLimit
  ssConfig <- liftEff' $ validateDeployArgs simpleStorageConfig
  _ <- deployContractWithArgs deployCfg ssConfig $ SimpleStorage.constructor txOpts
  foamCSR <- deployContractNoArgs deployCfg foamCSRConfig txOpts
  let parkingAuthorityConfig = makeParkingAuthorityConfig {foamCSR}
  _ <- deployContractWithArgs deployCfg parkingAuthorityConfig $ ParkingAuthority.constructor txOpts
  pure unit
