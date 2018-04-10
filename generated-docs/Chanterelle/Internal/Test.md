## Module Chanterelle.Internal.Test

#### `takeEvent`

``` purescript
takeEvent :: forall eff a ev i ni. DecodeEvent i ni ev => Show ev => EventFilter ev => Proxy ev -> Address -> Web3 (console :: CONSOLE, avar :: AVAR | eff) a -> Web3 (console :: CONSOLE, avar :: AVAR | eff) (Tuple a ev)
```

Run a `Web3` action which will dispatch a single event, wait for the event,
then return the action's result and the event.

#### `assertWeb3`

``` purescript
assertWeb3 :: forall eff a. Provider -> Web3 eff a -> Aff (eth :: ETH | eff) a
```

Assert the `Web3` action's result, crash the program if it doesn't succeed.


