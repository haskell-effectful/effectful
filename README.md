# effectful

[![Build Status](https://github.com/arybczak/effectful/workflows/Haskell-CI/badge.svg?branch=master)](https://github.com/arybczak/effectful/actions?query=branch%3Amaster)
[![Documentation](https://img.shields.io/static/v1?label=docs&message=effectful-core-0.1&color=informational)](https://rybczak.net/files/effectful/effectful-core-0.1-docs)
[![Documentation](https://img.shields.io/static/v1?label=docs&message=effectful-0.1&color=informational)](https://rybczak.net/files/effectful/effectful-0.1-docs)

<img src="https://user-images.githubusercontent.com/387658/127747903-f728437f-2ee4-47b8-9f0c-5102fd44c8e4.png" width="128">

*Note:* this is a pre-release of the 0.1 version. Please disregard the 0.0.0.0
version available on Hackage as the API has been completely redesigned since
then.

An easy to use, fast extensible effects library with seamless integration with
the existing Haskell ecosystem.

Main features:

1. Very fast
   ([benchmarks](https://github.com/haskell-effectful/effectful/tree/master/benchmarks)).

2. Easy to use API (if you know how to use the
   [MonadUnliftIO](https://hackage.haskell.org/package/unliftio-core/docs/Control-Monad-IO-Unlift.html#t:MonadUnliftIO)
   class, you know how to write effects).

3. Correct semantics in presence of runtime exceptions (no more discarded state
   updates).

4. Seamless integration with the existing ecosystem (`exceptions`,
   `monad-control`, `unliftio-core`, `resourcet` etc.).

5. Support for thread local and shared state (e.g. `StateT` provides a thread
   local state, while `MVar` holds a shared state, both approaches have their
   merits).

6. Support for statically (implementation determined at compile time) and
   dynamically (implementation determined at run time) dispatched effects.

## Motivation

Do we really need yet another library for handling effects? There's
[freer-simple](https://hackage.haskell.org/package/freer-simple),
[fused-effects](https://hackage.haskell.org/package/fused-effects),
[polysemy](https://hackage.haskell.org/package/polysemy),
[eff](https://github.com/hasura/eff) and probably a few more.

Unfortunately, of all of them only `eff` is a promising proposition because of
reasonable performance characteristics (see the talk "Effects for Less" linked
below for more information) and potential for good interoperability with the
existing ecosystem.

The second point is arguably the most important, because it allows focusing on
things that matter instead of reinventing all kinds of wheels, hence being a
necessary condition for broader adoption of the library.

However, `eff` uses delimited continuations underneath, which:

- Are not yet supported by GHC (though [the
proposal](https://github.com/ghc-proposals/ghc-proposals/pull/313) for including
support for them has been accepted).

- Are quite hard to understand.

- Make the library "too powerful" in a sense as it faces
  [a](https://github.com/hasura/eff/issues/13)
  [few](https://github.com/hasura/eff/issues/7)
  [issues](https://github.com/hasura/eff/issues/12) with no clear path towards
  their resolution.

### What about `mtl`?

It's true that its "effects as classes" approach is widely known and used often.

However:

- `mtl` style effects are
  [slow](https://github.com/haskell-effectful/effectful/tree/master/benchmarks).

- All of most often used monad transformers (except `ReaderT`) used for effect
  implementations are rife with [subtle
  issues](https://github.com/haskell-effectful/effectful/tree/master/transformers.md).

These issues are problematic enough that the [ReaderT design
pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/) was
invented. Its fundamentals are solid, but it's not an effect system.

The solution? Use the `ReaderT` pattern as a base and build around it to make it
an effect system! This is where `effectful` comes in. The `Eff` monad it uses is
essentially a `ReaderT` over `IO` on steroids, allowing us to dynamically extend
its environment with data types representing effects.

This concept is quite simple, so:

- It's reasonably easy to understand what is going on under the hood.

- The `Eff` monad being a reader allows for seamless interoperability with
  ubiquitous classes such as `MonadBaseControl` and `MonadUnliftIO` and solves
  [issues](https://github.com/haskell-effectful/effectful/tree/master/transformers.md)
  of monad transformers mentioned above.

What is more, the `Eff` monad is concrete, so GHC has many possibilities for
optimization, which results in a very fast code at a default optimization
level. There is no need to mark every function `INLINE` or enable additional
optimization passes, it just works.

### Any downsides?

As always, there's no free lunch. `Eff` doesn't support `NonDet` nor `Coroutine`
effects. However, the `NonDet` effect in existing libraries is
[broken](https://github.com/lexi-lambda/eff/blob/master/notes/semantics-zoo.md)
and none of the ones with support for higher order effects provide the
`Coroutine` effect, so arguably it's not a big loss.

If you need such capability in your application, there are well established
libraries such as [conduit](https://hackage.haskell.org/package/conduit) or
[list-t](https://hackage.haskell.org/package/list-t) that can be used with
`effectful` without any issues.

### Summary

`effectful` aims to replace "boring" transformer stacks (which 99% of time
consist of a dozen of newtype'd `ExceptT`, `ReaderT`, `StateT` and `WriterT`
transformers) by providing equivalent effects with much improved semantics,
performance and usability. It doesn't try to make monad transformers obsolete,
so you're free to use it with `ConduitT`, `ContT`, `ListT` etc. when necessary.

## Usage

The effect system is split among several libraries:

- The `effectful-core` library contains the main machinery of the effect system
  itself and basic effects. It aims for a small dependency footprint and
  provides building blocks for more advanced effects.

- The `effectful-th` library provides utilities for generating bits of
  effect-related boilerplate via Template Haskell.

- The `effectful` library re-exports public modules of `effectful-core` and
  additionally provides most features of the `unliftio` library divided into
  appropriate effects.

## Example

A `Filesystem` effect with two handlers, one that runs in `IO` and another that
uses an in-memory virtual file system can be found
[here](https://github.com/arybczak/effectful/blob/master/effectful/examples/FileSystem.hs).

## Resources

Resources that inspired the rise of this library and had a lot of impact on its
design.

Talks:

* [Effects for Less](https://www.youtube.com/watch?v=0jI-AlWEwYI) by Alexis King.

* [Monad Transformer State](https://www.youtube.com/watch?v=KZIN9f9rI34) by Michael Snoyman.

Blog posts:

* [ReaderT design pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/) by Michael Snoyman.

* [Exceptions Best Practices](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell/) by Michael Snoyman.

----------------------------------------

<div>Icons made by <a href="https://www.freepik.com" title="Freepik">Freepik</a> from <a href="https://www.flaticon.com/" title="Flaticon">www.flaticon.com</a></div>
