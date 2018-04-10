## Module Chanterelle.Main

#### `loadProject`

``` purescript
loadProject :: forall m eff. MonadAff (fs :: FS | eff) m => FilePath -> m ChanterelleProject
```

#### `main`

``` purescript
main :: forall e. Eff (console :: CONSOLE, fs :: FS, exception :: EXCEPTION, process :: PROCESS | e) Unit
```


