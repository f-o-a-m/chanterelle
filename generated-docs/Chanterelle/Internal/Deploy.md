## Module Chanterelle.Internal.Deploy

#### `deployContract`

``` purescript
deployContract :: forall eff args m. MonadThrow DeployError m => MonadAsk DeployConfig m => MonadAff (console :: CONSOLE, eth :: ETH, fs :: FS | eff) m => TransactionOptions NoPay -> ContractConfig args -> m Address
```

Deploy a contract using its ContractConfig object.

#### `readDeployAddress`

``` purescript
readDeployAddress :: forall eff m. MonadThrow DeployError m => MonadAff (fs :: FS | eff) m => FilePath -> BigNumber -> m Address
```

Read the deployment address for a given network id from the solc artifact.


