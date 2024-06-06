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
