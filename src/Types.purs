module Types
  ( DeployM
  , runDeployM
  , DeployError(..)
  , logDeployError
  , throwDeploy
  , DeployConfig(..)
  , ContractConfig
  ) where

import Prelude
import Ansi.Codes (Color(Red))
import Ansi.Output (withGraphics, foreground)
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Console as C
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (Error, throwException)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Network.Ethereum.Web3 (ETH, Address, BigNumber)
import Network.Ethereum.Web3.Types.Provider (Provider)
import Node.FS.Aff (FS)
import Node.Path (FilePath)


--------------------------------------------------------------------------------
-- | DeployM
--------------------------------------------------------------------------------

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

data DeployError =
    ConfigurationError String
  | OnDeploymentError String
  | PostDeploymentError String

derive instance genericError :: Generic DeployError _

instance showDeployError :: Show DeployError where
  show = genericShow

logDeployError
  :: forall eff m.
     MonadAff (console :: CONSOLE | eff) m
  => DeployError
  -> m Unit
logDeployError err = liftAff $ case err of
    ConfigurationError errMsg -> C.error $ makeRed "Configuration Error: " <> errMsg
    OnDeploymentError errMsg -> C.error $ makeRed "On Deployment Error: " <> errMsg
    PostDeploymentError errMsg -> C.error $ makeRed "Post Deployment Error: " <> errMsg
  where
    makeRed :: String -> String
    makeRed = withGraphics (foreground Red)

throwDeploy
  :: forall eff a.
     Error
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
               }

-- | configuration for deployment of a single contract
type ContractConfig args =
  { filepath :: FilePath
  , name :: String
  | args
  }
