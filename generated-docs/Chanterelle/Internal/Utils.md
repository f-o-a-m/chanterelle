## Module Chanterelle.Internal.Utils

#### `makeProvider`

``` purescript
makeProvider :: forall eff m. MonadEff (eth :: ETH, process :: PROCESS | eff) m => MonadThrow DeployError m => m Provider
```

Make an http provider with address given by NODE_URL, falling back
to localhost.

#### `makeDeployConfig`

``` purescript
makeDeployConfig :: forall eff m. MonadAff (eth :: ETH, console :: CONSOLE, process :: PROCESS | eff) m => MonadThrow DeployError m => m DeployConfig
```

#### `getPrimaryAccount`

``` purescript
getPrimaryAccount :: forall eff. Web3 (console :: CONSOLE | eff) Address
```

get the primary account for the ethereum client

#### `pollTransactionReceipt`

``` purescript
pollTransactionReceipt :: forall eff m. MonadAff (eth :: ETH | eff) m => HexString -> Provider -> m TransactionReceipt
```

indefinitely poll for a transaction receipt, sleeping for 3
seconds in between every call.

#### `withTimeout`

``` purescript
withTimeout :: forall eff a. Milliseconds -> Aff eff a -> Aff eff a
```

try an aff action for the specified amount of time before giving up.

#### `reportIfErrored`

``` purescript
reportIfErrored :: forall err a eff. Show err => String -> Either err a -> Aff (console :: CONSOLE | eff) a
```

#### `validateDeployArgs`

``` purescript
validateDeployArgs :: forall m args. MonadThrow DeployError m => ContractConfig args -> m ({  | args })
```

#### `unparsePath`

``` purescript
unparsePath :: forall p. { dir :: String, name :: String, ext :: String | p } -> FilePath
```

#### `assertDirectory`

``` purescript
assertDirectory :: forall eff m. MonadAff (fs :: FS | eff) m => MonadThrow Error m => FilePath -> m Unit
```


