An experiment with extensible effects.

Here's what this library can do:

```haskell
foo :: (Has (Reader Int) e, Has (Reader String) e, Has (State Int) e) => Eff e ()
foo = do
  x :: Int <- ask
  y :: String <- ask
  modify (\s -> s + x + length y)

runFoo :: Int
runFoo =
  flip S.execState 5 $

  -- Use mtl's MonadState as State Int effect
  runEff (FlipApply mtlState :& V.RNil) $

  -- Provide Reader Int based on State Int
  withEffect (readOnlyState (Proxy :: Proxy Int)) $

  -- Provide Reader String based in Reader String
  withEffect (zoomReader (show :: Int -> String)) $

  foo
```
