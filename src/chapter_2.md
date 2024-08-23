# Monads

Monads are a way to describe effectful computation in a functional
setting. In Haskell, they are unavoidable as they are used to support
true side effects (writing to files etc) with the built-in IO monad.
However, monads can also be used as a powerful program structuring
technique, even for programs that do not directly interact with the
outside world.

To motivate the value of monads (and some of the supporting
machinery), consider if we have a value `x :: Maybe Int`. We can see
such a value as a computation that is either an `Int` or `Nothing`,
with the latter case representing some kind of failure. We can
interpret failure as a kind of *effect*, that is separate from the
functional value (`Int`), although of course they are all just normal
Haskell types.

We are often in a situation where we want to perform a computation on
the result (if it exists), or otherwise propagate the failure. We can
do this with explicit pattern matching:

```Haskell
case x of
  Nothing -> Nothing
  Just x' -> Just (x'+1)
```

To make this more concise, we can use the `Functor` instance for
`Maybe` that we saw last week:

```Haskell
fmap (+1) x
```

The above works because we are applying a pure function of type `Int
-> Int` to the value in the `Maybe`. But what if the function is also
produced from some potentially failing computation, e.g. what if `f ::
Maybe (Int -> Int)`? Then `fmap f x` will be ill-typed, because `f` is
not a function - it is a function contained in an *effectful
computation* (or less abstractly, stored in a `Maybe` container).

We can write it using pattern matching, of course:

```Haskell
case f of
  Just f' ->
    case x of
      Just x' -> Just (f' x')
      Nothing -> Nothing
  Nothing -> Nothing
```

But all this checking for `Failure` becomes quite verbose. Our
salvation comes in the form of another typeclass, `Applicative`, which
describes *applicative functors*. Any applicative functor must also be
an ordinary functor, which we can add as a *superclass constraint*:

```Haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

The `pure` method injects a pure value into an effectful computation.
The `(<*>)` method applies a function stored in an applicative functor
to a value stored in an applicative functor, yielding an applicative
functor. This sounds (and is) abstract, and is perhaps best understood
by looking at an example instance:

```Haskell
instance Applicative Maybe where
  pure x = Just x
  f <*> x = case f of
              Just f' ->
                case x of
                  Just x' -> Just (f x)
                  Nothing -> Nothing
              Nothing -> Nothing
```

Or equivalently:

```Haskell
instance Applicative Maybe where
  pure x = Just x
  Just f <*> Just x = Just (f x)
  _ <*> _ = Nothing
```

Now we can write `f <*> x` rather than writing out the `case` by hand.
Even better, this will work not just when `f` and `x` make use of
`Maybe` specifically, but *any type* that is an applicative functor -
and as we shall see, that includes every monad.

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
