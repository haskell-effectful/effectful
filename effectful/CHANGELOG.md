# effectful-2.0.0.0 (2022-??-??)
* Make storage references in the environment immutable.
* Remove `checkSizeEnv` and `forkEnv` from
  `Effectful.Dispatch.Static.Primitive`.
* Add internal versioning of effects to prevent leakage of `unsafeCoerce`.

# effectful-1.2.0.0 (2022-07-28)
* Change `SuffixOf` to `SharedSuffix` and make it behave as advertised.
* Add `raiseWith`.

# effectful-1.1.0.0 (2022-07-19)
* Don't reset the `UnliftStrategy` to `SeqUnlift` inside the continuation of
  `withEffToIO`.
* Add `withReader`.

# effectful-1.0.0.0 (2021-07-13)
* Initial release.
