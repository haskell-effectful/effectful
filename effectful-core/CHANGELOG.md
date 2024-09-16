# effectful-core-2.4.0.0 (????-??-??)
* Add utility functions for handling effects that take the effect handler as the
  last parameter to `Effectful.Dispatch.Dynamic`.
* Add utility functions for handling first order effects to
  `Effectful.Dispatch.Dynamic`.
* Improve `Effectful.Labeled`, add `Effectful.Labeled.Error`,
  `Effectful.Labeled.Reader`, `Effectful.Labeled.State` and
  `Effectful.Labeled.Writer`.
* Add `throwErrorWith` and `throwError_` to `Effectful.Error.Static` and
  `Effectful.Error.Dynamic`.
* Fix a bug in `stateM` and `modifyM` of thread local `State` effect that
  might've caused dropped state updates
  ([#237](https://github.com/haskell-effectful/effectful/issues/237)).
* Add `HasCallStack` constraints where appropriate for better debugging
  experience.
* Properly roll back changes made to the environment when `OnEmptyRollback`
  policy for the `NonDet` effect is selected.
* Add a `SeqForkUnlift` strategy to support running unlifting functions outside
  of the scope of effects they capture.
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

# effectful-core-2.3.1.0 (2024-06-07)
* Drop support for GHC 8.8.
* Remove inaccurate information from the `Show` instance of `ErrorWrapper`.
* Add `Effectful.Provider.List`, generalization of `Effectful.Provider`.
* Respect `withFrozenCallStack` used by callers of `send`.
* Support exchange of effects between the environment of the handler and the
  local one via `localSeqLend`, `localLend`, `localSeqBorrow` and `localBorrow`
  from `Effectful.Dispatch.Dynamic`.

# effectful-core-2.3.0.1 (2023-11-13)
* Prevent internal functions from appending call stack frames to handlers.

# effectful-core-2.3.0.0 (2023-09-13)
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

# effectful-core-2.2.2.2 (2023-03-13)
* Allow `inject` to turn a monomorphic effect stack into a polymorphic one.
* Use C sources only with GHC < 9.
* Force inlining of `bracket` early to work around excessive inlining problem
  with GHC 9.6 (https://gitlab.haskell.org/ghc/ghc/-/issues/22824).

# effectful-core-2.2.2.1 (2023-01-12)
* Stop using the internal library because of bugs in `stack`.

# effectful-core-2.2.2.0 (2023-01-11)
* Add `withSeqEffToIO` and `withConcEffToIO` to `Effectful`.
* Use strict `IORef` and `MVar` variants where appropriate.
* Make `inject` work with effect stacks sharing a polymorphic suffix.

# effectful-core-2.2.1.0 (2022-11-09)
* Add `localSeqLift` and `localLift` to `Effectful.Dispatch.Dynamic`.

# effectful-core-2.2.0.0 (2022-10-24)
* Change `PrimState` for `Eff` from `RealWorld` to `PrimStateEff` to prevent the
  `Prim` effect from executing arbitrary `IO` actions via `ioToPrim`.
* Deprecate `(:>>)` as [GHC can't efficiently deal with type
  families](https://github.com/haskell-effectful/effectful/issues/52#issuecomment-1269155485).
* Add support for the `Alternative` and `MonadPlus` instances for `Eff` via the
  `NonDet` effect.

# effectful-core-2.1.0.0 (2022-08-22)
* Include the `e :> localEs` constraint in the `EffectHandler` to allow more
  flexibility in handling higher order effects.
* Do not include internal stack frames in `throwError` from
  `Effectful.Error.Dynamic`.

# effectful-core-2.0.0.0 (2022-08-12)
* Make storage references in the environment immutable.
* Remove `checkSizeEnv` and `forkEnv` from
  `Effectful.Dispatch.Static.Primitive`.
* Add internal versioning of effects to prevent leakage of `unsafeCoerce`.
* Make `interpose` and `impose` properly interact with other handlers.

# effectful-core-1.2.0.0 (2022-07-28)
* Change `SuffixOf` to `SharedSuffix` and make it behave as advertised.
* Add `raiseWith`.

# effectful-core-1.1.0.0 (2022-07-19)
* Don't reset the `UnliftStrategy` to `SeqUnlift` inside the continuation of
  `withEffToIO`.
* Add `withReader`.

# effectful-core-1.0.0.0 (2022-07-13)
* Initial release.
