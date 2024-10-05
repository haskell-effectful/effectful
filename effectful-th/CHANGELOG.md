# effectful-th-1.0.0.3 (2024-??-??)
* Make `makeEffect` reuse Haddock descriptions of effect operations for
  corresponding functions it generates (GHC >= 9.2).

# effectful-th-1.0.0.2 (2024-06-07)
* Add support for GHC 9.10.
* Drop support for GHC 8.8.
* `makeEffect` no longer generates `Wunused-type-patterns` warning ([#200](https://github.com/haskell-effectful/effectful/pull/200)).

# effectful-th-1.0.0.1 (2023-01-13)
* Depend on `effectful-core`, not `effectful`.

# effectful-th-1.0.0.0 (2022-07-13)
* Initial release.
