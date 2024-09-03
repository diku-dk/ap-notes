# Monads

Monads are a way to describe effectful computation in a functional
setting. In Haskell, they are unavoidable as they are used to support
true side effects (writing to files etc) with the built-in IO monad.
However, monads can also be used as a powerful program structuring
technique, even for programs that do not directly interact with the
outside world.

## Applicative Functors

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
functor. Essentially, it is an extension of the notion of `Functor` to
also allow the function to be in a "container".

This sounds (and is) abstract, and is perhaps best understood by
looking at an example instance:

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
and as we shall see, any monad is also an applicative functor.

## Monads

Consider now the case where we have a value `x :: Maybe Int` and a
function `f :: Int -> Maybe Int`. That is, the function now also has a
effect - in this case, it can fail.

If we use `fmap f x`, we get something that is well-typed, but the
result has type `Maybe (Maybe Int)`, which is unlikely to be what we
desire. What we need is this function:

```Haskell
maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind (Just x) f = f x
```

The `maybeBind` function passes a value of type `a` to a provided
function that operates on `a`, but returns a potentially failing
computation (`Maybe b`). If the original value is `Nothing`, then the
final result is `Nothing`. We can now write our application as

```Haskell
maybeBind x f
```

or using backticks to make the operator infix:

```Haskell
x `maybeBind` f
```

The intuition here is "first execute `x`, then apply the pure result
(if any) to `f`".

It turns out that functions with the same "shape" as `maybeBind` are
pretty common. For example, we can define a similar one for `Either`,
where in the type we simply replace `Maybe a` with `Either e a`:

```Haskell
eitherBind :: Either e a -> (a -> Either e b) -> Either e b
eitherBind (Left e) _ = (Left e)
eitherBind (Right x) f = f x
```

Note that we operate only on the `Right` part of the `Either` value -
the `Left` part, which usually represents some kind of error case, is
undisturbed.

We can even define such a function for linked lists:

```Haskell
listBind :: [a] -> (a -> [b]) -> [b]
listBind [] _ = []
listBind (x : xs) f = f x ++ listBind xs f
```

The type looks a bit different than `maybeBind` and `eitherBind`, but
that is just because of the syntactic sugar that lets us write `[a]`
instead of `List a`.

It seems that when we have things that behave a bit like "containers"
(in a general sense), we can define these "bind" functions that all
have some similar behaviour. When we observe such similarities, we can
put them into a typeclass. In this case, the typeclass is `Monad`, and
the "bind" function is named the much more intuitive and
easy-to-pronounce `>>=`:

```Haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
```

Haskell already provides a `Monad` instance for `Maybe`, but if it did
not, we would define it as follows:

```Haskell
instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just x >>= f = f x
```

Note that this definition is equivalent to the `maybeBind` above.
Indeed, we could also write this definition like so:

```Haskell
instance Monad Maybe where
  (>>=) = maybeBind
```

## Intuition, nomenclature, and slang

Monads when viewed as an abstract interface can be quite tricky to get
a grasp of. They are so abstract and general that it can be difficult
to understand what the general concept *means*. One good approach to
learning is to essentially disregard the general concept, and focus
only on specific monads. It is not so difficult to understand
operationally what the `Monad` instances for `Option` and `Either` do,
and in this we will mostly be working with *specific* monads.

From a pedantic viewpoint, a function `f` of type `a -> m b`, where
`m` is some monad (such as `Option`), returns a *monadic value* of
type `m b`. We also sometimes say that it returns a *command in the
monad `m`* which can produce a value of type `b` when "executed" (this
term will make a bit more sense for the monads discussed below). We
often also say that `f` is a *monadic function*. This is technically
an abuse of nomenclature, but functions that "run within a specific
monad" are such a common concept in Haskell that terse nomenclature is
useful.

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
