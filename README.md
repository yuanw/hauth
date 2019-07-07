# hauth

Walkthrough the example from Practical Web Developement with Haskell


## Pro and Cons of alternative `Prelude`

## Port and Adapter

## STM

```
:t newTVarIO :: a -> IO (TVar a)
```
## data.has

## why ConstraintKinds

```haskell
type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)
```

```
    • Illegal constraint synonym of kind: ‘Constraint’
        (Use ConstraintKinds to permit this)
    • In the type synonym declaration for ‘InMemory’
```

```
    • Non type-variable argument in the constraint: Has (TVar State) r
      (Use FlexibleContexts to permit this)
    • In the type synonym declaration for ‘InMemory’
  |
8 | type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```
