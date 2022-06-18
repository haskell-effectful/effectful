# `effectful-plugin`

A GHC plugin for improving disambiguation of effects.

## Usage

To enable the plugin, add the following GHC option to your project file:

```
ghc-options: -fplugin=Effectful.Plugin
```

## What it does

The following code:

```haskell
action :: [State Int, State String] :>> es => Eff es ()
action = do
  x <- get
  put (x + 1)
```

will not compile out of the box because `GHC` doesn't know that you meant to
`get` an `Int` since the function `+` as well as the literal `1` are
polymorphic. You have to write:

```haskell
action :: [State Int, State String] :>> es => Eff es ()
action = do
  x <- get @Int
  put (x + 1)
```

Which is slightly annoying. This plugin tells `GHC` extra information so code
like this can type-check without having to spell types to the compiler.

## Acknowledgements

Thanks to Xy Ren for her work on
[cleff-plugin](https://hackage.haskell.org/package/cleff-plugin)
`effectful-plugin` is based on.
