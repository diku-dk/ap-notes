# More Monads

## Free Monads

This section explains *free monads*. A free monad is a construction
that lets us construct a Monad from any Functor. Although this seems
obscure at first, it allows a style of programming design that clearly
distinguishes the *use* of effects from the *interpretation* of
effects.

### Motivation

As an example, imagine writing a function that needs to perform HTTP
requests to do its work. For usage, these network requests really must
be done, which requires the function to be in the `IO` monad, which
allows it to do *anything*, not just issue network requests. Further,
when unit testing, we may want to "mock" these network requests and
return synthetic data instead. It would be better if the function
could precisely describe that it depends upon being able to perform
HTTP requests (and no other IO operations), but let whoever executes
the function decide how exactly such requests are *implemented*:
either by actually performing physical network communication, or by
returning synthetic data (for testing and debugging).

In other languages, this problem is solved using ideas such as
[dependency
injection](https://en.wikipedia.org/wiki/Dependency_injection),
[mocking](https://microsoft.github.io/code-with-engineering-playbook/automated-testing/unit-testing/mocking/),
and [object
capabilities](https://en.wikipedia.org/wiki/Object-capability_model).
As we shall see, free monads provide a rather simple (although
sophisticated!) solution to these problems, and it is an approach that
sees fairly wide use among functional programmers.

The actual *definition* of the general concept of a free monad is
rather abstract, and may be skimmed on a first reading. The *use* of a
free monad is somewhat more concrete, and is more important for the
course.

### Defining `Free`

We will be defining a monad `Free f a` that represents a computation
that produces a value of type `a` (like in `IO a`), and `e` describes
the possible effects. The definition is as follows[^church]:

```Haskell
{{#include ../haskell/free_monads.hs:Free}}
```

This looks quite cryptic, but it is possible to understand based on
what we already know. The `Pure` constructor is straightforward: it
represents a computation that has finished with a value of type `a`.

The `Free` constructor is more interesting - it represents an
*effectful* computation. Note how `e` is *applied as a type
constructor* to a type `Free f a` (which is also the type we are
defining). Intuitively, this constructor encodes the idea of "first do
the effect `e`, then continue executing a `Free f a`". The meaning of
"do the effect" will be specified by the *interpretation function* we
define for the monad (we'll return to this below), and importantly we
can define multiple different interpretation functions for the same
monad. This is what allows separation of concerns.

The `f (Free f a)` part merits elaboration, as this is the first time
we have seen a datatype that applies one of its type parameters to
another type. This is an instance of "higher-rank polymorphism", where
we abstract not over *types*, but *type constructors*. While this is
the first time we have seen this in a data type definition, it is not
the first time we see higher-rank polymorphism at all. Recall the
`Functor` type class:

```Haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Intuitively, if a data type is a `Functor`, that means it is a kind of
"container" with "elements", and we can apply an arbitrary function on
these elements. But importantly, `e` by itself *is not a
type*[^higher-rank], but a *type constructor* that must be applied to
an argument (such as in `f a` and `f b`) to form a type. Our use of
`e` in the definition of `Free` works exactly the same way.

[^church]: This is not the only possible way to define free monads in
Haskell, but it is the simplest one. More efficient definitions exist
(such as
[Control.Monad.Free.Church](https://hackage.haskell.org/package/free-5.2/docs/Control-Monad-Free-Church.html)),
but they are more complicated, and unnecessary for our purposes.

[^higher-rank]: Just like how values can be classified with types, so
can types be classified with "types of types", which in Haskell are
called *kinds*. A normal type such as `Int` has kind `Type` (`Type` is
often written `*` for historical reasons), while a type constructor
such as `Maybe` has kind `Type -> Type`, meaning it is essentially a
function at the type level. The type constructor `Free` then has kind
`(Type -> Type) -> Type -> Type`. Actual type-level programming is a
fascinating topic, but beyond the scope of AP.

### Implementing `Reader` in terms of `Free`

We will have to construct a bit more machinery before `Free` will work
as a monad, but to skip ahead a bit, here is an idea of how we will
use it to to implement a `Reader` monad. The `Reader` monad supports a
single effect: we can ask for the value of an *environment* (called
`ask` in the standard `Reader` monad). We can define a datatype
`ReadOp r a` that encodes the notion of asking for a value of type
`r`, then producing a value of type `a`:

```Haskell
{{#include ../haskell/free_monads.hs:ReadOp}}
```

Actually, `ReadOp` can be made a `Functor`. We will see later that
this is necessary in order to make it usable with `Free`.

```Haskell
{{#include ../haskell/free_monads.hs:Functor_ReadOp}}
```

We can use this to construct a `Reader` monad using `Free`:

```Haskell
{{#include ../haskell/free_monads.hs:Reader}}
```

Once we have defined `Monad` instances and such for `Free`, we will be
able to write monadic code that makes use of it. But we also have to
define an interpretation function that actually *runs* the monad and
gives meaning to its effects? We want a function of the following
type:

```Haskell
{{#include ../haskell/free_monads.hs:RunReader}}
```

That is, given an initial value of type `r` and a computation of type
`Reader r a`, run that computation and produce a value of type `a`.
Since we don't know anything about `r`, our only option is to pattern
match on the `Reader r a` value itself. The `Pure` case is trivial, as
it represents a computation without any effects:

```Haskell
{{#include ../haskell/free_monads.hs:RunReader_Pure}}
```

For the second case, we are considering a value `Free (ReadOp f)`,
where `e` is of type `r -> Reader r a`. We can apply `e` to the
environment to obtain a `Reader r a`, which we can then execute with a
recursive application of `runReader`:

```Haskell
{{#include ../haskell/free_monads.hs:RunReader_Free}}
```

We can also define Haskell functions that hide the specific encoding
of `Reader` behind a more familiar interface:

```Haskell
{{#include ../haskell/free_monads.hs:ask}}
```

While it is perhaps not terribly interesting to define other
interpretations of the `Reader` monad, it is possible to do so; for
example by storing the environment in a global variable or in a
database, and defining an interpretation function that runs in `IO`
and fetches the environment from there. The important thing is that we
have decoupled the notion of an effect from its interpretation.

### Making `Free` a `Monad`

The above skipped ahead quite a bit, as we have yet to show that
`Free` is actually a `Monad`. Any `Monad` must also be a `Functor` and
an `Applicative`, so let us start with `Functor`. For a value of type
`Free f a`, the `Functor` instance will be about transforming the `a`
part.

```Haskell
{{#include ../haskell/free_monads.hs:Functor_Free}}
```

The `Pure` case is straightforward. For the `Free` case, we have a
value `e` of type `e (Free e a)`, and we need to somehow transform
that `a` inside of it. The only way we can possibly operate inside of
that `e` is if `e` *itself* is also a `Functor`, so we add that as a
premise of the instance definition. This is the reason why the effect
representation we use with `Free` must always be a `Functor` (such as
with `ReadOp` above). Usually these `Functor` instances are quite
mechanical. Note that the two `fmap`s we use are on different types:
the outermost one uses the `Functor` instance for `e`, and the
innermost one uses the `Functor` instance for `Free e` (recursively).

We then move on to definition an `Applicative` instance for `Free`.
The `pure` method is simple -- it is simply the `Pure` constructor.
Because we know that we will also be making `Free` a `Monad`, we can
define the `<*>` method as `ap` from `Control.Monad.`

```Haskell
{{#include ../haskell/free_monads.hs:1}}
```

```Haskell
{{#include ../haskell/free_monads.hs:Applicative_Free}}
```

Finally we can define the `Monad` instance and the `>>=` method
itself.

```Haskell
{{#include ../haskell/free_monads.hs:Monad_Free}}
```

This definition can also be constructed largely by following the
structure of the types. In the `Pure` case we have the following:

```Haskell
x :: a
f :: a -> Free e b
```

and we must produce a result of type `Free e b`. This is clearly done
simply by applying `f` to `x`.

In the `Free` case we have the following:

```Haskell
g :: e (Free e a)
f :: a -> Free e b
```

We ultimately want to apply `f` to something of type `a`, but we don't
have an `a`. All we have is a `Free e a` hidden behind an effect `e`.
However, since we require `e` to be a `Functor`, it is possible for us
to apply a function to the `Free e a` that is inside the `e`, meaning
that if we have a function `h :: Free e a -> Free e b`, we can say
`fmap h e` to obtain an `e c`. And it turns out that such an `h` is
defined as a recursive invocation of the `>>=` method for `Free a`.
