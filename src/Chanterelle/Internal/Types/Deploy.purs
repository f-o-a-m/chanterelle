module Chanterelle.Internal.Types.Deploy where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Parallel.Class (class Parallel, sequential)
import Control.Plus (class Plus)
import Data.Either (Either)
import Data.Functor.Compose (Compose)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V, invalid)
import Effect.Aff (Aff, Fiber, Milliseconds, ParAff, forkAff, joinFiber, parallel)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throwException)
import Network.Ethereum.Web3 (Address, HexString, TransactionOptions, Web3, _data, _value, fromMinorUnit)
import Network.Ethereum.Web3.Api (eth_sendTransaction)
import Network.Ethereum.Web3.Types (NoPay)
import Network.Ethereum.Web3.Types.Provider (Provider)
import Node.Path (FilePath)

--------------------------------------------------------------------------------
-- | DeployM Deployment monad
--------------------------------------------------------------------------------

-- | Monad Stack for contract deployment.
newtype DeployM a =
  DeployM (ReaderT DeployConfig (ExceptT DeployError Aff) a)

runDeployM
  :: forall a.
     DeployM a
  -> DeployConfig
  -> Aff (Either DeployError a)
runDeployM (DeployM deploy) = runExceptT <<< runReaderT deploy

derive newtype instance functorDeployM :: Functor DeployM
derive newtype instance applyDeployM :: Apply DeployM
derive newtype instance applicativeDeployM :: Applicative DeployM
derive newtype instance bindDeployM :: Bind DeployM
derive newtype instance monadDeployM :: Monad DeployM
derive newtype instance monadAskDeployM :: MonadAsk DeployConfig DeployM
derive newtype instance monadThrowDeployM :: MonadThrow DeployError DeployM
derive newtype instance monadEffDeployM :: MonadEffect DeployM
derive newtype instance monadAffDeployM :: MonadAff DeployM

-- | Fork a DeployM to run concurrently, returning the fiber that was created
forkDeployM
  :: forall a.
     DeployM a
  -> DeployM (Fiber (Either DeployError a))
forkDeployM m =
  ask >>= (liftAff <<< forkAff <<< runDeployM m)

joinDeployM
  :: forall a.
    Fiber (Either DeployError a)
  -> DeployM (Either DeployError a)
joinDeployM = liftAff <<< joinFiber

newtype DeployMPar a =
  DeployMPar (ReaderT DeployConfig (Compose ParAff (Either DeployError)) a)

derive newtype instance functorDeployMPar :: Functor DeployMPar

derive newtype instance applyDeployMPar :: Apply DeployMPar

derive newtype instance applicativeDeployMPar :: Applicative DeployMPar

instance monadParDeployM :: Parallel DeployMPar DeployM where
  parallel (DeployM m) = DeployMPar (parallel m)
  sequential (DeployMPar m) = DeployM (sequential m)

derive newtype instance altParDeployM :: Alt DeployMPar

derive newtype instance plusParDeployM :: Plus DeployMPar

derive newtype instance alternativeParDeployM :: Alternative DeployMPar

--------------------------------------------------------------------------------
-- | Error Types
--------------------------------------------------------------------------------

data DeployError = ConfigurationError String
                 | OnDeploymentError {name :: String, message :: String}
                 | PostDeploymentError {name :: String, message :: String}

-- | Throw an `Error` Exception inside DeployM.
throwDeploy :: forall a
             . Error
            -> DeployM a
throwDeploy = liftEffect <<< throwException

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
type Constructor args = TransactionOptions NoPay -> HexString -> Record args -> Web3 HexString

-- | Type alias for the empty args
type NoArgs = ()

-- | Value representing empty args
noArgs :: V (Array String) {}
noArgs = pure {}

-- | A constructor that deploys a contract with no constructor args.
constructorNoArgs :: Constructor NoArgs
constructorNoArgs txOpts bytecode _ =
  eth_sendTransaction $ txOpts # _data ?~ bytecode
                               # _value ?~ fromMinorUnit zero

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
