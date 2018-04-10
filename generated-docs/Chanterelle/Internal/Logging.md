## Module Chanterelle.Internal.Logging

#### `LogLevel`

``` purescript
data LogLevel
  = Debug
  | Info
  | Warn
  | Error
```

##### Instances
``` purescript
Eq LogLevel
Ord LogLevel
Show LogLevel
```

#### `log`

``` purescript
log :: forall eff m. MonadEff eff m => LogLevel -> String -> m Unit
```

#### `setLogLevel`

``` purescript
setLogLevel :: forall eff m. MonadEff eff m => LogLevel -> m Unit
```


