# effectful-plugin

## Overview

A typechecker plugin that can disambiguate "obvious" uses of effects in
[`effectful`](https://hackage.haskell.org/package/effectful).


## Example

Consider the following program:

```haskell
foo :: State Int :> es => Eff es ()
foo = put 10
```

What does this program do? Any human will tell you that it changes the state of
the `Int` to 10, which is clearly what's meant.

Unfortunately, `effectful` can't work this out on its own. Its reasoning is
"maybe you wanted to change some other `State` effect which is *also* a `Num`,
but you just forgot to add a `:>` constraint for it."

This is obviously insane, but it's the way the cookie crumbles.
`effectful-plugin` is a typechecker plugin which will disambiguate the above
program (and others) so the compiler will do what you want.


## Usage

Add the following line to your package configuration:

```
ghc-options: -fplugin=Effectful.Plugin
```


## Limitations

The `effectful-plugin` will only disambiguate effects if there is exactly one
relevant constraint in scope. For example, it will *not* disambiguate the
following program:

```haskell
bar :: '[ State Int
        , State Double
        ] :>> es => Eff es ()
bar = put 10
```

because it is now unclear whether you're attempting to set the `Int` or the
`Double`. Instead, you can manually write a type application in this case.

```haskell
bar :: '[ State Int
        , State Double
        ] :>> es => Eff es ()
bar = put @Int 10
```


## Acknowledgments

This plugin is copied almost verbatim from [`polysemy-plugin`](https://github.com/polysemy-research/polysemy/tree/master/polysemy-plugin).

