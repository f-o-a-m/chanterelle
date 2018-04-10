## Module Chanterelle.Internal.Types

#### `Dependency`

``` purescript
newtype Dependency
  = Dependency String
```

Chanterelle Project Types

##### Instances
``` purescript
Eq Dependency
EncodeJson Dependency
DecodeJson Dependency
```

#### `ChanterelleModule`

``` purescript
data ChanterelleModule
  = ChanterelleModule { moduleName :: String, solContractName :: String, solPath :: FilePath, jsonPath :: FilePath, pursPath :: FilePath }
```

#### `ChanterelleProjectSpec`

``` purescript
newtype ChanterelleProjectSpec
  = ChanterelleProjectSpec { name :: String, version :: String, sourceDir :: FilePath, modules :: Array String, dependencies :: Array Dependency, solcOutputSelection :: Array String, psGen :: { exprPrefix :: String, modulePrefix :: String, outputPath :: String } }
```

##### Instances
``` purescript
Eq ChanterelleProjectSpec
EncodeJson ChanterelleProjectSpec
DecodeJson ChanterelleProjectSpec
```

#### `ChanterelleProject`

``` purescript
data ChanterelleProject
  = ChanterelleProject { root :: FilePath, srcIn :: FilePath, jsonOut :: FilePath, psOut :: FilePath, spec :: ChanterelleProjectSpec, modules :: Array ChanterelleModule }
```

#### `DeployM`

``` purescript
newtype DeployM eff a
  = DeployM (ReaderT DeployConfig (ExceptT DeployError (Aff (eth :: ETH, fs :: FS, console :: CONSOLE | eff))) a)
```

DeployM Deployment monad
Monad Stack for contract deployment.

##### Instances
``` purescript
Functor (DeployM eff)
Apply (DeployM eff)
Applicative (DeployM eff)
Bind (DeployM eff)
Monad (DeployM eff)
MonadAsk DeployConfig (DeployM eff)
MonadThrow DeployError (DeployM eff)
MonadEff (eth :: ETH, fs :: FS, console :: CONSOLE | eff) (DeployM eff)
MonadAff (eth :: ETH, fs :: FS, console :: CONSOLE | eff) (DeployM eff)
```

#### `runDeployM`

``` purescript
runDeployM :: forall eff a. DeployM eff a -> DeployConfig -> Aff (fs :: FS, console :: CONSOLE, eth :: ETH | eff) (Either DeployError a)
```

#### `DeployError`

``` purescript
data DeployError
  = ConfigurationError String
  | OnDeploymentError String
  | PostDeploymentError String
```

Error Types

##### Instances
``` purescript
MonadThrow DeployError (DeployM eff)
Generic DeployError _
Show DeployError
```

#### `logDeployError`

``` purescript
logDeployError :: forall eff m. MonadAff (console :: CONSOLE | eff) m => DeployError -> m Unit
```

#### `throwDeploy`

``` purescript
throwDeploy :: forall eff a. Error -> DeployM eff a
```

Throw an `Error` Exception inside DeployM.

#### `DeployConfig`

``` purescript
newtype DeployConfig
  = DeployConfig { networkId :: BigNumber, primaryAccount :: Address, provider :: Provider }
```

Config Types
primary deployment configuration

##### Instances
``` purescript
MonadAsk DeployConfig (DeployM eff)
```

#### `Constructor`

``` purescript
type Constructor args = forall eff. TransactionOptions NoPay -> HexString -> {  | args } -> Web3 eff HexString
```

Contract Config
Represents a contract constructor with input type `args`.

#### `NoArgs`

``` purescript
type NoArgs = ()
```

Type alias for the empty args

#### `noArgs`

``` purescript
noArgs :: V (Array String) {  }
```

Value representing empty args

#### `constructorNoArgs`

``` purescript
constructorNoArgs :: Constructor NoArgs
```

A constructor that deploys a contract with no constructor args.

#### `ConfigR`

``` purescript
type ConfigR args = (filepath :: FilePath, name :: String, constructor :: Constructor args, unvalidatedArgs :: V (Array String) ({  | args }))
```

#### `ContractConfig`

``` purescript
type ContractConfig args = {  | ConfigR args }
```

Configuration for deployment of a single contract


