module Chanterelle.Internal.Types.Deploy where

import Prelude

import Chanterelle.Internal.Types.Artifact (Artifact)
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
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V, invalid)
import Effect.Aff (Aff, Fiber, Milliseconds, ParAff, forkAff, joinFiber, parallel)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throwException)
import Effect.Ref as Ref
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
  DeployM ((ReaderT DeployConfig (ExceptT DeployError Aff)) a)

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

-- Note:
-- using `Compose` instead of `DeployMPar (ReaderT DeployConfig (ExceptT DeployError ParAff) a)`
-- to prevent
-- ```
--   No type class instance was found for
--     Data.Monoid.Monoid DeployError
-- while solving type class constraint
--   Control.Plus.Plus (ExceptT DeployError ParAff)
-- ```
-- because
-- (Monoid e, Monad m) => Plus (ExceptT e m)
-- but
-- (Plus f, Functor g) => Plus (Compose f g)
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
                 | DeployingUnlinkedBytecodeError { name :: String, libs :: Array String }
                 | LinkingLinkedBytecodeError { name :: String, libraryName :: String, bytecodeKind :: String }
                 | LinkingError { contractName :: String, libraryName :: String, libraryAddress :: Address, bytecodeKind :: String, msg :: String }
                 | Impossibility String

-- | Throw an `Error` Exception inside DeployM.
throwDeploy :: forall a
             . Error
            -> DeployM a
throwDeploy = liftEffect <<< throwException

getArtifactCache
  :: forall m
   . MonadAsk DeployConfig m
  => MonadEffect m
  => m (Map.Map (LibraryConfig ()) Artifact)
getArtifactCache = do
  DeployConfig { artifactCache } <- ask
  liftEffect (Ref.read artifactCache)

setArtifactCache
  :: forall m
   . MonadAsk DeployConfig m
  => MonadEffect m
  => Map.Map (LibraryConfig ()) Artifact
  -> m Unit
setArtifactCache newCache = do
  DeployConfig { artifactCache } <- ask
  liftEffect $ Ref.modify_ (const newCache) artifactCache

--------------------------------------------------------------------------------
-- | Config Types
--------------------------------------------------------------------------------

-- | An Ethereum Chain ID
type NetworkID = Int

-- | primary deployment configuration
newtype DeployConfig =
  DeployConfig { networkID :: NetworkID
               , primaryAccount :: Address
               , provider :: Provider
               , timeout :: Milliseconds
               , writeArtifacts :: Boolean -- if true, artifacts will be persisted as deploy scripts modify them by linking or deploying. false is useful for testing
               , ignoreNetworksInArtifact :: Boolean -- if true, artifacts that are loaded will not have networks fields populated, useful for testing.
               , artifactCache :: Ref.Ref (Map.Map (LibraryConfig ()) Artifact)
               }

-- | Contract Config

-- | Represents a contract constructor with input type `args`.
type Constructor args = TransactionOptions NoPay -> HexString -> Record args -> Web3 HexString

-- | Type alias for the empty args
type NoArgs :: forall k. Row k
type NoArgs = ()

-- | Value representing empty args
noArgs :: V (Array String) {} -- TODO(srghma): rename to noArgsValidator?
noArgs = pure {}

-- | A constructor that deploys a contract with no constructor args.
constructorNoArgs :: Constructor NoArgs
constructorNoArgs txOpts bytecode _ =
  eth_sendTransaction $ txOpts # _data ?~ bytecode
                               # _value ?~ fromMinorUnit zero

type LibraryR r =
  ( filepath :: FilePath
  , name :: String
  | r
  )

type LibraryConfig r = Record (LibraryR r)

type ConfigR args = LibraryR
  ( constructor :: Constructor args
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
