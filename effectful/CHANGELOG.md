# effectful-core-2.6.0.0 (????-??-??)
* Adjust `generalBracket` with `base >= 4.21` to make use of the new exception
  annotation mechanism.
* Add `withException` to `Effectful.Exception`.
* Deprecate `Effectful.Reader.Dynamic.withReader` as it doesn't work correctly
  for all potential interpreters.
* Re-export `ThreadId` from `Effectful.Concurrent` for convenience.
* **Breaking changes**:
  - Change the order of type parameters in `raise` for better usability.
  - `Effectful.Error.Static.ErrorWrapper` is no longer caught by `catchSync`.
  - Remove deprecated function `Effectful.withConcEffToIO`.

# effectful-2.5.1.0 (2024-11-27)
* Add `passthrough` to `Effectful.Dispatch.Dynamic` for passing operations to
  the upstream handler within `interpose` and `impose` without having to fully
  pattern match on them.
* **Bugfixes**:
  - Fix a potential space leak related to `HasCallStack` quirks (see
    https://gitlab.haskell.org/ghc/ghc/-/issues/25520 for more information).

# effectful-2.5.0.0 (2024-10-23)
* Add `plusEff` (specialized version of `<|>`) to `Effectful.NonDet` and make
  `emptyEff` and `sumEff` generate better call stacks.
* Explicitly define `setByteArray#` and `setOffAddr#` in the `Prim` instance of
  `Ref` for `primitive` < 0.9.0.0.
* **Bugfixes**:
  - `OnEmptyRollback` strategy of the `NonDet` effect is no longer broken.
* **Breaking changes**:
  - Remove `restoreEnv` function from `Effectful.Dispatch.Static.Primitive`
    since it was broken.
  - Base `Effectful.Exception` on `Control.Exception` instead of the
    `safe-exceptions` library for consistency with provided `MonadThrow` and
    `MonadCatch` instances.

# effectful-2.4.0.0 (2024-10-08)
* Add utility functions for handling effects that take the effect handler as the
  last parameter to `Effectful.Dispatch.Dynamic`.
* Add utility functions for handling first order effects to
  `Effectful.Dispatch.Dynamic`.
* Improve `Effectful.Labeled`, add `Effectful.Labeled.Error`,
  `Effectful.Labeled.Reader`, `Effectful.Labeled.State` and
  `Effectful.Labeled.Writer`.
* Add `throwErrorWith` and `throwError_` to `Effectful.Error.Static` and
  `Effectful.Error.Dynamic`.
* Add `HasCallStack` constraints where appropriate for better debugging
  experience.
* Add a `SeqForkUnlift` strategy to support running unlifting functions outside
  of the scope of effects they capture.
* Add `Effectful.Exception` with appropriate re-exports from the
  `safe-exceptions` library.
* Add `Effectful.Concurrent.Chan.Strict`.
* Add `Effectful.Prim.IORef` and `Effectful.Prim.IORef.Strict`.
* **Bugfixes**:
  - Ensure that a `LocalEnv` is only used in a thread it belongs to.
  - Properly roll back changes made to the environment when `OnEmptyRollback`
    policy for the `NonDet` effect is selected.
  - Fix a bug in `stateM` and `modifyM` of thread local `State` effect that
    might've caused dropped state updates
    ([#237](https://github.com/haskell-effectful/effectful/issues/237)).
* **Breaking changes**:
  - `localSeqLend`, `localLend`, `localSeqBorrow` and `localBorrow` now take a
    list of effects instead of a single one.
  - `Effectful.Error.Static.throwError` now requires the error type to have a
    `Show` constraint. If this is not the case for some of your error types, use
    `throwError_` for them.
  - `ThrowError` operation from the dynamic version of the `Error` effect was
    replaced with `ThrowErrorWith`.
  - `stateEnv` and `modifyEnv` now take pure modification functions. If you rely
    on their old forms, switch to a combination of `getEnv` and `putEnv`.
  - `runStateMVar`, `evalStateMVar` and `execStateMVar` now take a strict
    `MVar'` from the `strict-mutable-base` package.
  - `Effectful.Concurrent.MVar.Strict` is now a lifted version of
    `Control.Concurrent.MVar.Strict` from `strict-mutable-base`. The original
    module was renamed to `Effectful.Concurrent.MVar.Strict.Compat` and
    deprecated.

# effectful-2.3.1.0 (2024-06-07)
* Drop support for GHC 8.8.
* Remove inaccurate information from the `Show` instance of `ErrorWrapper`.
* Add `Effectful.Provider.List`, generalization of `Effectful.Provider`.
* Respect `withFrozenCallStack` used by callers of `send`.
* Support exchange of effects between the environment of the handler and the
  local one via `localSeqLend`, `localLend`, `localSeqBorrow` and `localBorrow`
  from `Effectful.Dispatch.Dynamic`.

# effectful-2.3.0.0 (2023-09-13)
* Deprecate `withConcEffToIO`.
* Make `withEffToIO` take an explicit unlifting strategy for the sake of
  consistency with unlifting functions from `Effectful.Dispatch.Dynamic` and
  easier to understand API.
* Add support for turning an effect handler into an effectful operation via the
  `Provider` effect.
* Add `runErrorWith` and `runErrorNoCallStackWith` to `Effectful.Error.Dynamic`
  and `Effectful.Error.Static`.
* Add support for having multiple effects of the same type in scope via the
  `Labeled` effect.
* Add various `ByteString` related functions to the `FileSystem` effect.
* Add the `Console` effect.

# effectful-2.2.2.0 (2023-01-11)
* Add `withSeqEffToIO` and `withConcEffToIO` to `Effectful`.
* Use strict `IORef` and `MVar` variants where appropriate.
* Make `inject` work with effect stacks sharing a polymorphic suffix.

# effectful-2.2.1.0 (2022-11-09)
* Add `localSeqLift` and `localLift` to `Effectful.Dispatch.Dynamic`.

# effectful-2.2.0.0 (2022-10-24)
* Change `PrimState` for `Eff` from `RealWorld` to `PrimStateEff` to prevent the
  `Prim` effect from executing arbitrary `IO` actions via `ioToPrim`.
* Deprecate `(:>>)` as [GHC can't efficiently deal with type
  families](https://github.com/haskell-effectful/effectful/issues/52#issuecomment-1269155485).
* Add support for the `Alternative` and `MonadPlus` instances for `Eff` via the
  `NonDet` effect.

# effectful-2.1.0.0 (2022-08-22)
* Include the `e :> localEs` constraint in the `EffectHandler` to allow more
  flexibility in handling higher order effects.
* Do not include internal stack frames in `throwError` from
  `Effectful.Error.Dynamic`.

# effectful-2.0.0.0 (2022-08-12)
* Make storage references in the environment immutable.
* Remove `checkSizeEnv` and `forkEnv` from
  `Effectful.Dispatch.Static.Primitive`.
* Add internal versioning of effects to prevent leakage of `unsafeCoerce`.
* Make `interpose` and `impose` properly interact with other handlers.

# effectful-1.2.0.0 (2022-07-28)
* Change `SuffixOf` to `SharedSuffix` and make it behave as advertised.
* Add `raiseWith`.

# effectful-1.1.0.0 (2022-07-19)
* Don't reset the `UnliftStrategy` to `SeqUnlift` inside the continuation of
  `withEffToIO`.
* Add `withReader`.

# effectful-1.0.0.0 (2022-07-13)
* Initial release.
