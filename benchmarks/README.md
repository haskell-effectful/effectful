# Benchmarks

## Introduction

The benchmark suite of `effectful` compares performance of the most popular
extensible effects libraries in several scenarios. It implements two benchmarks:

- **countdown** - a microbenchmark that effectively measures performance of
  monadic binds and the effect dispatch.

- **filesize** - a more down to earth benchmark that does various things,
  including I/O.
   
Each benchmark has two flavours that affect the amount of effects available in
the context:

- **shallow** - only effects necessary for the benchmark.

- **deep** - necessary effects + 5 redundant effects put into the context before
  and after the relevant ones (10 in total). This simulates a typical scenario
  in which the code uses only a portion of the total amount of effects available
  to the application.

Moreover, the benchmarked code was annotated with `NOINLINE` pragmas to prevent
GHC from inlining it and/or specializing away type class constraints related to
effects. This is crucial in order to get realistic results, as for any
non-trivial, multi-module application the compiler will not be able to do this
as that would essentially mean performing whole program specialization.

## Results

The code was compiled with GHC 9.0.2 and run on a Ryzen 9 5950x.

### Countdown

<img src="https://raw.githubusercontent.com/haskell-effectful/effectful/master/benchmarks/bench_countdown_1000.png">

Analysis:

1. `effectful` takes the lead. Its static dispatch is on par with the reference
   implementation that uses the `ST` monad, so it offers no additional
   overhead. Its dynamic dispatch is also the fastest.

2. `cleff` uses very similar implementation techniques as `effectful` and is on
   par with it for the shallow version, but gets slower for the deep one. This
   is because it uses `IntMap` for the effect dispatch underneath, so it's not
   quite constant size in terms of the effect stack. For comparison, `effectful`
   uses arrays.

3. `freer-simple` does surprisingly well for a solution that's based on free
   monads.
   
4. `mtl` comes next and unfortunately here's when the conventional wisdom stating
   that it is fast crumbles. The deep version is **50 times** slower than the
   reference implementation!
   
   This is a direct consequence of how type classes are compiled. To be more
   precise, during compilation type class constraints are translated by the
   compiler to regular arguments. These arguments are class dictionaries,
   i.e. data types containing all functions that the type class contains.
   
   Now, because usage of `mtl` style effects requires the monad to be
   polymorphic, such functions at runtime are passed a dictionary of `Monad`
   specific methods and have to call them. **In particular, this applies to the
   monadic bind**. That's the crux of a problem - bind is called in between
   every monadic operation, so making it a function call has a disastrous effect
   on performance.
   
   Why is the result for the deep stack so much worse than for the shallow one
   though? It's because in reality, each call to bind performs *O(n)* function
   calls, where *n* is the number of monad transformers on the stack. That's
   because the implementation of bind for every monad transformer refers to the
   bind of a monad it transforms.
   
   Compare that to `effectful`, where monadic binds are known function calls and
   can be eliminated by the compiler. What is more, the only piece of data
   passed via class constraints are dictionaries of `:>`, each represented by a
   single `Int` pointing at the place in the stack where the relevant effect is
   located.

5. `fused-effects` exhibits similar behavior as `mtl`. This comes as no surprise
   since it uses the same implementation techniques. It augments them with
   additional machinery for convenience, which seems to add even more overhead
   though.

6. `polysemy` is based on free monads just as `freer-simple` and performs
   similarly, though with a much higher initial overhead.

### Filesize

<img src="https://raw.githubusercontent.com/haskell-effectful/effectful/master/benchmarks/bench_filesize_1000.png">

The results are similar to the ones of the *countdown* benchmark. It's worth
noting though that introduction of other effects and I/O makes the difference in
performance between libraries not nearly as pronounced.
