# Monads

## The Reader Monad

```Haskell
{{#include ../haskell/readerstate.hs:Reader}}
```

```Haskell
{{#include ../haskell/readerstate.hs:Monad_Reader}}
```

```Haskell
{{#include ../haskell/readerstate.hs:Functor_Reader}}
{{#include ../haskell/readerstate.hs:Applicative_Reader}}
```

## The State Monad

```Haskell
{{#include ../haskell/readerstate.hs:State}}
```

```Haskell
{{#include ../haskell/readerstate.hs:Monad_State}}
```

```Haskell
{{#include ../haskell/readerstate.hs:Functor_State}}
{{#include ../haskell/readerstate.hs:Applicative_State}}
```

## Combining Reader and State

One limitation of monads - in particular the simple form we study in
AP - is that they do not compose well. We cannot in general take two
monads, such as `Reader` and `State`, and combine them into a single
monad that supports both of their functionalities. There are
techniques that allow for this, such as *monad transformers*, but they
are somewhat complex and outside the scope of AP. Instead, if we wish
to have a monad that supports both a read-only environment (such as
with `Reader`) and a mutable store (such as with `State`), then we
must write a monad from scratch, such as the following `RS` monad.

```Haskell
{{#include ../haskell/readerstate.hs:RS}}
```

See how the function we use to represent the monad takes two
arguments, `env` and `s`, corresponding to the environment and store
respectively, but returns only a new store.

The `Monad` instance itself is a little intricate, but it just
combines the dataflow that we also saw for the `Reader` and `State`
monads above:

```Haskell
{{#include ../haskell/readerstate.hs:Monad_RS}}
```

The `Functor` and `Applicative` instances are then just the usual
boilerplate.

```Haskell
{{#include ../haskell/readerstate.hs:Functor_RS}}
{{#include ../haskell/readerstate.hs:Applicative_RS}}
```
