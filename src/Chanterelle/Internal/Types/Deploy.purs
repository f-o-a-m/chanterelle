module Chanterelle.Internal.Types.Deploy where

import Prelude
import Control.Monad.Aff (Aff, Milliseconds, liftEff')
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (Error, throwException)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either)
import Data.Lens ((?~))
import Data.Validation.Semigroup (V)
import Network.Ethereum.Web3 (Address, BigNumber, ETH, HexString, TransactionOptions, Web3, _data, _value, fromWei)
import Network.Ethereum.Web3.Api (eth_sendTransaction)
import Network.Ethereum.Web3.Types (NoPay)
import Network.Ethereum.Web3.Types.Provider (Provider)
import Node.FS (FS)
import Node.Path (FilePath)
--------------------------------------------------------------------------------
-- | DeployM Deployment monad
--------------------------------------------------------------------------------

-- | Monad Stack for contract deployment.
newtype DeployM eff a =
  DeployM (ReaderT DeployConfig (ExceptT DeployError (Aff (eth :: ETH, fs :: FS, console :: CONSOLE | eff))) a)

runDeployM
  :: forall eff a.
     DeployM eff a
  -> DeployConfig
  -> Aff (fs :: FS, console :: CONSOLE, eth :: ETH | eff) (Either DeployError a)
runDeployM (DeployM deploy) = runExceptT <<< runReaderT deploy

derive newtype instance functorDeployM :: Functor (DeployM eff)
derive newtype instance applyDeployM :: Apply (DeployM eff)
derive newtype instance applicativeDeployM :: Applicative (DeployM eff)
derive newtype instance bindDeployM :: Bind (DeployM eff)
derive newtype instance monadDeployM :: Monad (DeployM eff)
derive newtype instance monadAskDeployM :: MonadAsk DeployConfig (DeployM eff)
derive newtype instance monadThrowDeployM :: MonadThrow DeployError (DeployM eff)
derive newtype instance monadEffDeployM :: MonadEff (eth :: ETH, fs :: FS, console :: CONSOLE | eff) (DeployM eff)
derive newtype instance monadAffDeployM :: MonadAff (eth :: ETH, fs :: FS, console :: CONSOLE | eff) (DeployM eff)

--------------------------------------------------------------------------------
-- | Error Types
--------------------------------------------------------------------------------

data DeployError = ConfigurationError String
                 | OnDeploymentError {name :: String, message :: String}
                 | PostDeploymentError {name :: String, message :: String}

-- | Throw an `Error` Exception inside DeployM.
throwDeploy :: forall eff a
             . Error
            -> DeployM eff a
throwDeploy = liftAff <<< liftEff' <<< throwException

--------------------------------------------------------------------------------
-- | Config Types
--------------------------------------------------------------------------------

-- | primary deployment configuration
newtype DeployConfig =
  DeployConfig { networkId :: BigNumber
               , primaryAccount :: Address
               , provider :: Provider
               , timeout :: Milliseconds
               }

-- | Contract Config

-- | Represents a contract constructor with input type `args`.
type Constructor args =
  forall eff. TransactionOptions NoPay -> HexString -> Record args -> Web3 eff HexString

-- | Type alias for the empty args
type NoArgs = ()

-- | Value representing empty args
noArgs :: V (Array String) {}
noArgs = pure {}

-- | A constructor that deploys a contract with no constructor args.
constructorNoArgs :: Constructor NoArgs
constructorNoArgs txOpts bytecode _ =
  eth_sendTransaction $ txOpts # _data ?~ bytecode
                               # _value ?~ fromWei zero

type ConfigR args =
  ( filepath :: FilePath
  , name :: String
  , constructor :: Constructor args
  , unvalidatedArgs :: V (Array String) (Record args)
  )

-- | Configuration for deployment of a single contract
type ContractConfig args = Record (ConfigR args)
