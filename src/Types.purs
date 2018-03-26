module Types
  ( DeployM
  , runDeployM
  , DeployConfig(..)
  , ContractConfig
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Network.Ethereum.Web3 (ETH, Address, BigNumber)
import Network.Ethereum.Web3.Types.Provider (Provider)
import Node.FS.Aff (FS)
import Node.Path (FilePath)



--------------------------------------------------------------------------------
-- | DeployM
--------------------------------------------------------------------------------

newtype DeployM eff a = DeployM (ReaderT DeployConfig (Aff (eth :: ETH, fs :: FS, console :: CONSOLE | eff)) a)

runDeployM :: forall eff a. DeployM eff a -> DeployConfig -> Aff (fs :: FS, console :: CONSOLE, eth :: ETH | eff) a
runDeployM (DeployM deploy) = runReaderT deploy

derive newtype instance functorDeployM :: Functor (DeployM eff)
derive newtype instance applyDeployM :: Apply (DeployM eff)
derive newtype instance applicativeDeployM :: Applicative (DeployM eff)
derive newtype instance bindDeployM :: Bind (DeployM eff)
derive newtype instance monadDeployM :: Monad (DeployM eff)
derive newtype instance monadAskDeployM :: MonadAsk DeployConfig (DeployM eff)
derive newtype instance monadEffDeployM :: MonadEff (eth :: ETH, fs :: FS, console :: CONSOLE | eff) (DeployM eff)
derive newtype instance monadAffDeployM :: MonadAff (eth :: ETH, fs :: FS, console :: CONSOLE | eff) (DeployM eff)


--------------------------------------------------------------------------------
-- | Config Types
--------------------------------------------------------------------------------

-- | primary deployment configuration
newtype DeployConfig =
  DeployConfig { networkId :: BigNumber
               , primaryAccount :: Address
               , provider :: Provider
               }

-- | configuration for deployment of a single contract
type ContractConfig args =
  { filepath :: FilePath
  , name :: String
  | args
  }
