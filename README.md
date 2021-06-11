# effective

[![Build Status](https://github.com/arybczak/effective/workflows/Haskell-CI/badge.svg?branch=master)](https://github.com/arybczak/effective/actions?query=branch%3Amaster)

An effect system based on the [ReaderT design pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/).

Main features:

1. Very fast.

2. Internals of the library are easy to reason about.

3. Correct semantics in presence of runtime exceptions (no more lost or
   discarded state).

4. Seamless integration with existing Haskell ecosystem (`exceptions`,
   `monad-control`, `unliftio-core`, `resourcet`).

5. Effects can be defined for either static (as fast as it gets, single
   interpretation) or dynamic (slower, multiple interpretations) dispatch
   depending on your needs.

## Example

A `Filesystem` effect with two handlers, one that runs in `IO` and another that
uses an in-memory virtual file system can be found
[here](https://github.com/arybczak/effective/blob/master/examples/FileSystem.hs).
