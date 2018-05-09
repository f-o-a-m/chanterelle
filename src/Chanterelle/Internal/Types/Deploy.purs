module Chanterelle.Internal.Types.Deploy where

import Prelude

import Control.Monad.Aff (Aff, Fiber, Milliseconds, forkAff, liftEff', joinFiber)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (Error, throwException)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Either (Either)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V, invalid)
import Network.Ethereum.Web3 (Address, ETH, HexString, TransactionOptions, Web3, _data, _value, fromWei)
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

-- | Fork a DeployM to run concurrently, returning the fiber that was created
forkDeployM
  :: forall eff a.
     DeployM eff a
  -> DeployM eff ( Fiber ( eth     :: ETH
                         , fs      :: FS
                         , console :: CONSOLE
                         | eff
                         )
                         (Either DeployError a)
                 )
forkDeployM m =
  ask >>= (liftAff <<< forkAff <<< runDeployM m)

joinDeployM
  :: forall eff a.
    Fiber ( eth     :: ETH
          , fs      :: FS
          , console :: CONSOLE
          | eff
          )
          (Either DeployError a)
  -> DeployM eff (Either DeployError a)
joinDeployM = liftAff <<< joinFiber

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
  DeployConfig { networkId :: String
               , primaryAccount :: Address
               , provider :: Provider
               , timeout :: Milliseconds
               , writeArtifacts :: Boolean
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

-- | Validation helpers
validateWithError :: forall a. Maybe a -> String -> V (Array String) a
validateWithError mres msg = case mres of
  Nothing -> invalid [msg]
  Just res -> pure res

infixl 9 validateWithError as ??
