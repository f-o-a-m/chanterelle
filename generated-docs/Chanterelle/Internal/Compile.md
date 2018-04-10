## Module Chanterelle.Internal.Compile

#### `compile`

``` purescript
compile :: forall eff m. MonadAff (fs :: FS, process :: PROCESS | eff) m => ChanterelleProject -> m (StrMap (Tuple ChanterelleModule Json))
```

compile and write the artifact


