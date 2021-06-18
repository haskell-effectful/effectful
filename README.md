# effectful

[![Build Status](https://github.com/arybczak/effectful/workflows/Haskell-CI/badge.svg?branch=master)](https://github.com/arybczak/effectful/actions?query=branch%3Amaster)
[![Hackage](https://img.shields.io/hackage/v/effectful.svg)](https://hackage.haskell.org/package/effectful)

*Note:* this is a pre-release.

A simple, performant extensible effects library with seamless integration with
the existing ecosystem.

Main features:

1. Very fast.

2. Internals of the library are easy to reason about.

3. Correct semantics in presence of runtime exceptions (no more lost or
   discarded state).

4. Seamless integration with the existing ecosystem (`exceptions`,
   `monad-control`, `unliftio-core`, `resourcet`).

5. Effects can be defined for either

   - static dispatch (as fast as it gets, single interpretation) or

   - dynamic dispatch (slower, multiple interpretations are possible),

   depending on your needs.

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
things that matter instead of reinventing all kinds of wheels and is crucial for
adoption of the library.

However, `eff` uses delimited continuations underneath, which:

- Are not yet supported by GHC (though [the
proposal](https://github.com/ghc-proposals/ghc-proposals/pull/313) for including
support for them has been accepted).

- Are somewhat hard to understand.

- Make the space of possible effects "too big", i.e. some of their interactions
  might no longer make sense. For example, there is no `MonadWriter` instance
  for the `ContT` transformer in the `mtl` library, because [it can't be
  properly
  implemented](https://www.reddit.com/r/haskell/comments/hai9kb/why_is_there_no_monadwriter_for_contt_in_mtl/). It's
  not clear how `eff` can detect and prevent such interactions.

On the other hand, if support for continuations is excluded from the handler
monad, then the ability to define effects with non-linear control flow (such as
`NonDet`) is lost. Arguably it's a small price to pay for predictability,
because such specialized effects are needed rarely and locally, at which point a
dedicated, well established solution such as
[conduit](https://hackage.haskell.org/package/conduit),
[list-t](https://hackage.haskell.org/package/list-t) or
[logict](https://hackage.haskell.org/package/logict) can be used.

This is where `effectful` comes in. The `Eff` monad it uses is essentially a
`ReaderT` over `IO` on steroids, allowing us to dynamically extend its
environment with data types that represent effects.

Because this concept is so simple:

- It's reasonably easy to understand what is going on under the hood.

- The `Eff` monad being a reader allows for seamless interoperability with
  ubiquitous classes such as `MonadBaseControl` and `MonadUnliftIO` as well as
  support for handling runtime exceptions without worrying about lost or
  discarded state (see the talk "Monad Transformer State" linked below for more
  information).

What is more:

- The `Eff` monad is concrete, so GHC has many possibilities for optimization,
  which results in a very fast code at a default optimization level. There is no
  need to mark every function `INLINE` or enable additional optimization passes,
  it just works.

- If an advanced effect with non-linear control flow is needed, you can always
  stick a transformer that implements it on top of `Eff` in a local context.

In conclusion, `effectful` aims to reduce duplication and bring back performance
to "boring" transformer stacks, most of which are a dozen of newtype'd `StateT`
or `ReaderT` transformers, each with a few associated operations (usually tied
to a type class), not to replace monad transformers altogether.

## Example

A `Filesystem` effect with two handlers, one that runs in `IO` and another that
uses an in-memory virtual file system can be found
[here](https://github.com/arybczak/effectful/blob/master/examples/FileSystem.hs).

## Resources

Resources that inspired the rise of this library and had a lot of impact on its
design.

Talks:

* [Effects for Less](https://www.youtube.com/watch?v=0jI-AlWEwYI) by Alexis King.

* [Monad Transformer State](https://www.youtube.com/watch?v=KZIN9f9rI34) by Michael Snoyman.

Blog posts:

* [ReaderT design pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/) by Michael Snoyman.

* [Exceptions Best Practices](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell/) by Michael Snoyman.
