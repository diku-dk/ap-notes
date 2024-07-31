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

### Defining a Free Monad

Recall the `Functor` type class:

```Haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Intuitively, if a data type is a `Functor`, that means it is a kind of
"container" with "elements", and we can apply an arbitrary function on
these elements.
