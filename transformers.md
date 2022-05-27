# Issues with the `transformers`/`mtl` library

TL;DR:

From monad transformers used the majority of time:

- `ExceptT` can't produce stack traces and its errors are not runtime
  exceptions, which can easily introduce subtle bugs leading to resource
  exhaustion.

- `StateT` discards state updates when interacting with runtime exceptions and
  `ExceptT`-specific errors in surprising ways.

- `WriterT` has too many variants, choosing a right variant for your use case is
  extremely tricky unless you're an expert in the language and it overall has
  niche applications.

- `RWST` inherits issues of both `StateT` and `WriterT`, which makes it resemble
  a spike trap.

That leaves `ReaderT`, which is the only one with predictable behavior.

`effectful` fixes these issues, namely:

- Errors of the `Error` effect are implemented as runtime exceptions underneath,
  which allows the library to provide stack traces and clients of the library to
  treat `Error`-specific errors and runtime exceptions uniformly.

- `State` effects never lose updates and they're not affected by the order of
  effects on the stack in any way.

- `Writer` effects are properly strict (but still niche).

- There is no `RWST` equivalent because stacking effects is cheap.

## ExceptT

Errors returned by `ExceptT` lack a very important feature: the ability to
obtain associated stack traces. It is simply impossible to get them with errors
produced by `ExceptT` in an automatic manner, which combined with wonky
interactions with various libraries (as demonstrated below) makes its usability
extremely limited.

### Interaction with the `exceptions` library

Consider the following:

```haskell
{-# LANGUAGE TypeApplications #-}
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class

data Resource = Resource

acquireResource :: MonadIO m => m Resource
acquireResource = do
  liftIO $ putStrLn "acquireResource"
  pure Resource

releaseResourceOnSuccess :: MonadIO m => Resource -> m ()
releaseResourceOnSuccess _ = liftIO $ putStrLn "releaseResourceOnSuccess"

releaseResourceOnFailure :: MonadIO m => Resource -> m ()
releaseResourceOnFailure _ = liftIO $ putStrLn "releaseResourceOnFailure"

withResource :: (MonadMask m, MonadIO m) => (Resource -> m a) -> m a
withResource action = mask $ \unmask -> do
  r <- acquireResource
  a <- unmask (action r) `onException` do
    releaseResourceOnFailure r
  releaseResourceOnSuccess r
  pure a

main :: IO ()
main = do
  putStrLn "1. IO - no exception"
  test . withResource $ \Resource -> pure ()
  putStrLn "2. IO - exception"
  test . withResource $ \Resource -> error "oops"
  putStrLn "3. ExceptT IO - no exception"
  test . runExceptT @String . withResource $ \Resource -> pure ()
  putStrLn "4. ExceptT IO - exception"
  test . runExceptT @String . withResource $ \Resource -> error "oops"
  putStrLn "4. ExceptT IO - error"
  test . runExceptT @String . withResource $ \Resource -> throwError "oops"
  where
    test :: IO a -> IO ()
    test = void . try @_ @SomeException
```

Does `withResource` correctly handle resource management in all cases?

**No**.

Here's the output:

```
1. IO - no exception
acquireResource
releaseResourceOnSuccess
2. IO - exception
acquireResource
releaseResourceOnFailure
3. ExceptT IO - no exception
acquireResource
releaseResourceOnSuccess
4. ExceptT IO - exception
acquireResource
releaseResourceOnFailure
4. ExceptT IO - error
acquireResource
```

Note that the resource is never released when an `ExceptT`-specific error is
raised.

The issue here is the use of `onException` as it doesn't capture transformer
specific errors **because they are not exceptions**.

Granted, this is more of a problem with the API of the `exceptions` library. It
mentions this caveat in the documentation and provides a function `onError` that
should be used instead, but overall the library makes it far too easy to write
code that looks correct, but is subtly broken and will sneakily leak resources
until they are exhausted and your application grinds to a halt.

## StateT

### Interaction with the `exceptions` library

What is the output of the following program?

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception (ErrorCall)
import Control.Monad.Catch
import Control.Monad.Trans.State

main :: IO ()
main = do
  s <- (`execStateT` (0::Int)) $ do
    (modify (+1) >> error "oops") `catch` \(e::ErrorCall) -> modify (+2)
  putStrLn $ show s
```

It would be reasonable to expect `3`, but that's not the case.

```
$ ./test
2
```

The problem is that state updates tracked by `StateT` within a computation
wrapped in `catch` are discarded when an exception is raised. This is confusing
and will lead to bugs if one doesn't know about this subtle behavior.

The same thing happens when `lifted-base` (backed by `monad-control`) is used:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception.Lifted
import Control.Monad.Trans.State

main :: IO ()
main = do
  s <- (`execStateT` (0::Int)) $ do
    (modify (+1) >> error "oops") `catch` \(e::ErrorCall) -> modify (+2)
  putStrLn $ show s
```

```
$ ./test
2
```

### Interaction with `EitherT`

The initial state is `0`. What is the value of the state after `test` runs?

```haskell
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Except
import Control.Monad.State

test :: (MonadError String m, MonadState Int m) => m ()
test = (modify (+1) >> throwError "oops") `catchError` \_ -> modify (+2)
```

After previous section you will most likely be cautious about the
answer. However, neither `2` nor `3` is correct!

It depends on the order of the transformer stack:

```haskell
main :: IO ()
main = do
  putStrLn $ "1. StateT Int (ExceptT IO)"
  putStrLn . show =<< (runExceptT . (`runStateT` (0::Int)) $ test)
  putStrLn $ "2. ExceptT (StateT Int IO)"
  putStrLn . show =<< ((`runStateT` (0::Int)) . runExceptT $ test)
```

```
$ ./test
1. StateT Int (ExceptT IO)
Right ((),2)
2. ExceptT (StateT Int IO)
(Right (),3)
```

This is even worse than the previous section, because:

- You can't predict the behavior of code based on the definition of `test`
  alone.

- Seemingly unrelated code change, i.e. rearranging the order of monad
  transformers in the stack will lead to subtle change of behavior in a
  completely different part of the application.

## WriterT

### Control.Monad.Trans.Writer.Lazy

The excerpt of its definition:

```haskell
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

writer :: Monad m => (a, w) -> WriterT w m a
writer = WriterT . return

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  m >>= k = WriterT $ do
    ~(a, w)  <- runWriterT m
    ~(b, w') <- runWriterT (k a)
    pure (b, w `mappend` w')
```

Usage of lazy `WriterT w m` makes sense only when:

1. The bind of `m` is lazy (e.g. `Identity` qualifies, `IO` doesn't) as then the
   bind of `WriterT w m` is also lazy.

2. `w` can be produced and consumed lazily (e.g. `[a]` qualifies, `Sum Int`
   doesn't).

If both conditions are met, it'll run in constant space:

```haskell
import Control.Monad.Trans.Writer.Lazy
import Data.Foldable

main :: IO ()
main = do
  let xs = execWriter $ forM_ [1..1000000::Int] $ \n -> tell [n]
  putStrLn . show $ sum xs
```

```
$ ./test +RTS -s
500000500000
     456,052,432 bytes allocated in the heap
           1,792 bytes copied during GC
          44,328 bytes maximum residency (2 sample(s))
          29,400 bytes maximum slop
               5 MiB total memory in use (0 MB lost due to fragmentation)
```

If the first one is not met, it'll leak space:

```haskell
import Control.Monad.Trans.Writer.Lazy
import Data.Foldable

main :: IO ()
main = execWriterT $ forM_ [1..1000000::Int] $ \_ -> pure ()
```

```
$ ./test +RTS -s
     520,687,800 bytes allocated in the heap
           2,640 bytes copied during GC
      87,127,800 bytes maximum residency (8 sample(s))
       1,706,248 bytes maximum slop
             235 MiB total memory in use (0 MB lost due to fragmentation)
```

If the second one is not met, it'll leak space:

```haskell
import Control.Monad.Trans.Writer.Lazy
import Data.Foldable
import Data.Monoid

main :: IO ()
main = do
  let Sum xs = execWriter $ forM_ [1..1000000::Int] $ \n -> tell $ Sum n
  putStrLn $ show xs
```

```
$ ./test +RTS -s
500000500000
     473,892,576 bytes allocated in the heap
           2,360 bytes copied during GC
      51,145,184 bytes maximum residency (6 sample(s))
          29,400 bytes maximum slop
             100 MiB total memory in use (0 MB lost due to fragmentation)
```

### Control.Monad.Trans.Writer.Strict

The excerpt of its definition:

```haskell
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

writer :: (Monad m) => (a, w) -> WriterT w m a
writer = WriterT . return

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  m >>= k = WriterT $ do
    (a, w)  <- runWriterT m
    (b, w') <- runWriterT (k a)
    pure (b, w `mappend` w')
```

Usage of the strict `WriterT` **never makes sense**. It always leaks space,
because:

1. Its bind is not tail recursive and strict pattern matches force computation
   of `k` even if the bind of `m` is lazy.

2. `w ``mappend`` w'` is never evaluated, which results in accumulation of
   thunks.

All three tests leak space:

```haskell
import Control.Monad.Trans.Writer.Strict
import Data.Foldable

main :: IO ()
main = do
  let xs = execWriter $ forM_ [1..1000000::Int] $ \n -> tell [n]
  putStrLn . show $ sum xs
```

```
$ ./test +RTS -s
500000500000
     433,958,096 bytes allocated in the heap
           1,808 bytes copied during GC
     104,548,664 bytes maximum residency (6 sample(s))
         247,496 bytes maximum slop
             187 MiB total memory in use (0 MB lost due to fragmentation)
```

```haskell
import Control.Monad.Trans.Writer.Strict
import Data.Foldable

main :: IO ()
main = execWriterT $ forM_ [1..1000000::Int] $ \_ -> pure ()
```

```
$ ./test +RTS -s
     472,687,776 bytes allocated in the heap
           2,448 bytes copied during GC
      57,333,760 bytes maximum residency (7 sample(s))
          83,968 bytes maximum slop
             116 MiB total memory in use (0 MB lost due to fragmentation)
```

```haskell
import Control.Monad.Trans.Writer.Strict
import Data.Foldable
import Data.Monoid

main :: IO ()
main = do
  let Sum xs = execWriter $ forM_ [1..1000000::Int] $ \n -> tell $ Sum n
  putStrLn $ show xs
```

```
$ ./test +RTS -s
500000500000
     435,217,632 bytes allocated in the heap
           9,720 bytes copied during GC
      79,255,400 bytes maximum residency (7 sample(s))
         346,264 bytes maximum slop
             195 MiB total memory in use (0 MB lost due to fragmentation)
```

### Control.Monad.Trans.Writer.CPS

What the strict `WriterT` should have been.

Here is the excerpt of its definition:

```haskell
newtype WriterT w m a = WriterT { unWriterT :: w -> m (a, w) }

writer :: (Monoid w, Monad m) => (a, w) -> WriterT w m a
writer (a, w') = WriterT $ \ w ->
    let wt = w `mappend` w' in wt `seq` return (a, wt)

instance Monad m => Monad (WriterT w m) where
  m >>= k = WriterT $ \ w -> do
    (a, w') <- unWriterT m w
    unWriterT (k a) w'
```

It's essentially a `StateT` with a restricted API.

1. Its bind is tail recursive and strict, so will run in constant space.

2. `w ``mappend`` w'` is continuously evaluated, so thunks will not be
   accumulated.

The downside of (2) is that time complexity of the first test degrades to
`O(n^2)` because each `tell` has to append to the end of evaluated list.

However, the rest run in constant space:

```haskell
import Control.Monad.Trans.Writer.CPS
import Data.Foldable

main :: IO ()
main = execWriterT $ forM_ [1..1000000::Int] $ \_ -> pure ()
```

```
$ ./test +RTS -s
     352,041,696 bytes allocated in the heap
           1,360 bytes copied during GC
          35,984 bytes maximum residency (2 sample(s))
          29,552 bytes maximum slop
               5 MiB total memory in use (0 MB lost due to fragmentation)
```

```haskell
import Control.Monad.Trans.Writer.CPS
import Data.Foldable
import Data.Monoid

main :: IO ()
main = do
  let Sum xs = execWriter $ forM_ [1..1000000::Int] $ \n -> tell $ Sum n
  putStrLn $ show xs
```

```
$ ./test +RTS -s
500000500000
     376,052,496 bytes allocated in the heap
           1,464 bytes copied during GC
          44,328 bytes maximum residency (2 sample(s))
          29,400 bytes maximum slop
               5 MiB total memory in use (0 MB lost due to fragmentation)
```

### Summary

1. Lazy `WriterT` makes sense in niche scenarios like lazy production and
   consumption of `w` (arguably in such case it's better to use a dedicated
   streaming library instead of relying on laziness, which is quite fragile).

2. Strict `WriterT` never makes sense.

3. CPS `WriterT` makes sense if the left-associated chain of `mappendS` is
   efficient for the `w` of your choice (in particular it's not for `[a]`, which
   tends to be often used with `WriterT` by inexperienced users).

In conclusion, `WriterT` flavors range from "useless" to "full of traps", so
they are best avoided.

## RWST

Inherits all issues of `StateT` and `WriterT`, best avoided.
