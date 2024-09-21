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

We will be defining a monad `Free e a` that represents a computation
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
constructor* to a type `Free e a` (which is also the type we are
defining). Intuitively, this constructor encodes the idea of "first do
the effect `e`, then continue executing a `Free e a`". The meaning of
"do the effect" will be specified by the *interpretation function* we
define for the monad (we'll return to this below), and importantly we
can define multiple different interpretation functions for the same
monad. This is what allows separation of concerns.

The `e (Free e a)` part merits elaboration, as this is the first time
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

For the second case, we are considering a value `Free (ReadOp g)`,
where `g` is of type `r -> Reader r a`.  To see this, recall that the
`Free` data constructor takes something of type `e (Free e a)` as a
payload; setting `e = ReaderOp r`, this type becomes `ReaderOp r (Free
(ReaderOp r) a)`, which is the same as `ReaderOp r (Reader r
a)`. Referring back to the definition of `ReaderOp`, we conclude that
`g` must have type `r -> Reader r a`.

We can now apply `g` to the environment to obtain a `Reader r a`, which we
can then execute with a recursive application of `runReader`:

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
`Free e a`, the `Functor` instance will be about transforming the `a`
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

## The IO Monad

As we have seen in previous chapters, monads are not a special
language construct in Haskell. While the `Monad` typeclass is part of
the standard library, we could have defined it ourselves if not, and
indeed that is how it was originally done. The only special affordance
that Haskell provides is syntactic sugar in the form of `do` notation,
and while it is certainly very convenient, it does not let us do
anything we could not otherwise do. Ultimately, the monads you have
seen have merely been convenient and abstract ways of doing things
that could also be done in non-monadic Haskell, and they are indeed
all ultimately expressed in terms non-monadic code.

There is one exception, however: the `IO` monad is truly built into
the language, and cannot be expressed using normal Haskell. It is the
ultimate mechanism by which Haskell programs interact with the
surrounding world. This is evident in the type of `main`; the
canonical entry point for Haskell programs:

```Haskell
main :: IO ()
```

We can imagine that the Haskell runtime system has some kind of
interpreter for `IO`, the same way we write interpreters for other
monads, but there is no way to express this interpretation in pure
Haskell.

In the assignments and exercises you have worked with, the `main`
function is in the `runtests.hs` file, and serves as the entry point
to the unit tests. We can also simply type in `IO` actions at the
`ghci` prompt, after which `ghci` will execute them.

There are various metaphors for how to understand `IO`. One is that it
is a kind of state monad that passes around the entire state of the
universe, with functions like `putStr` and `readFile` modifying the
state, the same way `put` and `get` modify the state of the `State`
monad. This interpretation is useful to an extent, but break downs
when considering concurrency, which we will look at later in the
course. Ultimately, it is most useful to simply consider `>>=` for the
IO monad as straight up impure and executing side effects.

### Programming with IO

Programming with the `IO` monad in Haskell is very similar to
programming in a conventional imperative language, and the same as
with programming with any other monad. However, the fact that it so
similar to other languages means that our intuition can sometimes
betray us. For example, consider the function `putStrLn`, which prints
a given string to stdout, and has the following type:

```
putStrLn :: String -> IO ()
```

Simply putting `putStrLn` somewhere in our program will not cause
anything to be printed, even when it is evaluated. For example,
evaluating this expression will produce no output:

```Haskell
let x = putStrLn "hello world"
in ()
```

There is nothing here you haven't seen before. All this does is create
a binding `x` of type `IO ()`, which is not used for anything, and
then returns the unit value. In fact, this expression just has type
`()` - it is not monadic at all. In order to actually *execute* an
effect, we must pass it to `>>=` somehow, putting together an even
larger `IO` operation, which must ultimately be the definition of the
program `main` function:

```Haskell
main :: IO ()
main = putStrLn "hello world" >>= \_ -> pure ()

-- or equivalently

main :: IO ()
main = do putStrLn "hello world"
          pure ()
```

The fact that IO operations are normal Haskell values, that just
happen to be executable, means we can manipulate them in various ways.
As a particularly trivial example:

```Haskell
main :: IO ()
main = do let x = putStrLn "hello world"
          x
          x
```

This will print twice, because we are executing the action twice.

### Exceptions and IO

We have previously seen how to use monads to encode and propagate
errors, such as with `Maybe` and `Either`. The advantage of this
approach is that the potential errors are directly visible in the
types of functions. However, not all errors are represented, or *can*
be represented, in this way. For example, the `div` function has the
following type:

```
div :: Integral a => a -> a -> a
```

Yet if we try to divide by zero, we will get an error:

```
> div 1 0
*** Exception: divide by zero
```

This is an *exception*. Despite Haskell being a pure language, it is
indeed the case that some ostensibly pure functions, such as `div`,
can raise exceptions. One common cause of exceptions is the function
`error`, which we often use to indicate program bugs, or `undefined`,
which we often use during development. Many prelude functions such as
`head` or `tail` are also *partial*, meaning they are not defined on
their entire domain. Taking the `head` of an empty list will also
raise an exception:

```
> head []
*** Exception: Prelude.head: empty list
```

Exceptions raised by pure code are often called *imprecise
exceptions*, because they are not evident in the type.

Today, partial functions are largely considered a bad idea by most
Haskell programmers, because they make the types unreliable. Instead
`head` should perhaps return a `Maybe` value. Yet even adherents of
this approach may hesitate to make functions such as `div` return
`Maybe`, due to the sheer amount of boilerplate this would require
(even when using monads to propagate the error situaton).

Further, other exceptions are harder to avoid: they are also raised
for out-of-memory situations or various asynchronous signals. Most
functions that perform IO, such as opening or writing to a file, will
also use exceptions to report errors. As a result, we need a way to
*handle* exceptions.

~~~admonish info

*Exception* is an overloaded term. In this section we discuss solely
the kinds of exceptions that are thrown by functions like `error`. It is
common to use the word "exception" to refer to the kind of error handling
done with `Either` or similar monads.

~~~


#### Handling Exceptions

Exceptions are undeniably an effect, and while they can be thrown in
pure code, it would be a violation of referential transparency to also
*handle* them in pure code. As a result, exceptions can only be caught
in the IO monad. The facilities for working with exceptions are found
in
[Control.Exception](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Exception.html).
This is a rather rich and complicated module, and we will not need
much of what it provides. The main things we will need is the `catch`
function:

```Haskell
catch :: Exception e => IO a -> (e -> IO a) -> IO a
```

The Haskell exception handling machinery is fairly similar to that
which you might be familiar with from other languages. The `catch`
function takes two arguments. It tries to run the provided `IO`
action, and if an exception is thrown during that action, it calls the
provided handler function with the exception. The wrinkle is that
`catch` is *polymorphic*, while an exception is any value that
implements the `Exception` typeclass, any use of `catch` must somehow
specify *exactly* which type of exception is caught by this specific
`catch`. This may sound unclear, so here is an example where we try to
handle a division by zero:

```Haskell
handleDivByZero :: IO ()
handleDivByZero =
  print (div 1 0) `catch` (\e -> putStrLn "I am a handler")
```

This will give us a rather long (here abbreviated) error message:

```
    • Ambiguous type variable ‘e0’ arising from a use of ‘catch’
      prevents the constraint ‘(Exception e0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘e0’ should be.
```

The problem is that `catch` can handle *any* exception, so how is
Haskell to know which one we know? We need to put in a type annotation
to specify the one we are interested in. For AP we will mainly use the
type `SomeException`, which acts as a "root type" for all other kinds
of exceptions. In general, in AP we will not discriminate between
different types of exceptions, although Haskell provides facilities
for doing so. The easiest way to indicate that this is the exception
we want to catch is to make the handler a local function with an
explicit type ascription:

```Haskell
handleDivByZero :: IO ()
handleDivByZero = do
  let handler :: SomeException -> IO ()
      handler e = putStrLn "I am a handler"
  print (div 1 0) `catch` handler
```

```
> handleDivByZero
I am a handler
```

We can turn a `SomeException` into a (hopefully) human-readable string
by using its `Show` instance:

```Haskell
handleDivByZero :: IO ()
handleDivByZero = do
  let handler :: SomeException -> IO ()
      handler e = putStrLn $ "It went wrong: " ++ show e
  print (div 1 0) `catch` handler
```

#### Example: Safely Reading Files

The Haskell prelude provides the function `readFile`. Given a
`FilePath` (a synonym for `String`), it returns the contents of a
file:

```Haskell
readFile :: FilePath -> IO String
```

If the file cannot be read, it is reported with an exception:

```Haskell
> readFile "doesnotexist"
*** Exception: doesnotexist: openFile: does not exist (No such file or directory)
```

Since the exception handling machinery in Haskell is somewhat
cumbersome, we may want to write a wrapper for `readFile` that returns
a proper sum type with error conditions instead. Reading a file can go
wrong in many ways (perhaps someone cuts a disk cable at an
inopportune time), but there are often some common we want to handle
specially, such as the file not existing. We define a sum type that
captures the result of attempting to read a file:

```Haskell
data FileContents
  = FileNotFound
  | CouldNotRead String
  | FileContents String
  deriving (Show)
```

The `CouldNotRead` constructor is used as a catch-all for all errors
except for file-not-found, and `FileContents` represents success.

When `readFile` fails, it throws an exception of type `IOError`. Using
the function `isDoesNotExistError` from `System.IO.Error`, we can
detect whether such an `IOError` corresponds to the case where a file
of the given name does not exist. We can put all this together into a
function for reading a file safely (in this case, "safely" means "does
not throw exceptions"):

```Haskell
import System.IO.Error (isDoesNotExistError)

readFileSafely :: FilePath -> IO FileContents
readFileSafely f = (FileContents <$> readFile f) `catch` onException
  where
    onException :: IOError -> IO FileContents
    onException e =
      if isDoesNotExistError e
        then pure FileNotFound
        else pure $ CouldNotRead $ show e
```

And observe how well it works:

```
> readFileSafely "doesnotexist"
FileNotFound
> readFileSafely "/root/secrets"
CouldNotRead "/root/secrets: openFile: permission denied (Permission denied)"
> readFileSafely "ap-exam-solution.hs"
FileContents "module Solution where[...]"
```

#### Laziness and Exceptions

Haskell's laziness can sometimes make it difficult to handle
exceptions in pure code. The reason is that exceptions are not thrown
until the associated computation is forced, which may not be when you
expect. For example, we may write code like this, with the intent of
replacing a division-by-zero error with an appropriate dummy value:

```Haskell
doesNotWork :: IO Int
doesNotWork = do
  let handler :: SomeException -> IO Int
      handler e = do
        putStrLn $ "It went wrong: " ++ show e
        pure 42
  pure (div 1 0) `catch` handler
```

But we receive an unpleasant surprise:

```
> doesNotWork
*** Exception: divide by zero
```

The reason is that the expression `div 1 0` is not actually fully
evaluated inside the computation protected by `catch` - instead it is
simply returned un-evaluated, and not until `ghci` tries to print the
result of the computation (after `catch` is done) will be division
actually be attempted and the exception thrown.

One solution is to use the `evaluate` function, also from
`Control.Exception`, which has this signature:

```
evaluate :: a -> IO a
```

An expression `evaluate x` is much like `pure x`, but evaluates its
argument to *weak head normal form* (*WHNF*) before injecting it into
the monad. Intuitively, it will evaluate the provided expression up to
the *first* constructor, hopefully uncovering any exceptions
immediately. For `Int`, that will be the entire value, but for a
lists, it will only be up to the first cons cell. However, this is
enough to make this simple example work:

```Haskell
doesWork :: IO Int
doesWork = do
  let handler :: SomeException -> IO Int
      handler e = do
        putStrLn $ "It went wrong: " ++ show e
        pure 42
  evaluate (div 1 0) `catch` handler
```

```
> doesWork
It went wrong: divide by zero
42
```

### IO References

One of the basic facilities provided by the IO monad is a form of
mutable references, called `IORef`s, which can be found in the module
[Data.IORef](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-IORef.html).
The type `IORef a` denotes a mutable reference to a value of type `a`.
We can create an `IORef` using the function `newIORef`:

```Haskell
newIORef :: a -> IO (IORef a)
```

When creating an `IORef`, we must provide an initial value. Reading
and writing is done through the following functions:

```Haskell
readIORef :: IORef a -> IO a

writeIORef :: IORef a -> a -> IO ()
```

While other utility functions exist, this interface is all we need in
order to interact with IORefs.

```Haskell
> r <- newIORef True
> readIORef r
True
> writeIORef r False
> readIORef r
False
```

~~~admonish warning

`IORef`s are *not* thread safe. When we discuss concurrent programming
later in the course we must be careful not to access them in
unstructured ways from multiple concurrent threads. Programming with
`IORef`s in Haskell is vulnerable to all the usual tragedies of concurrent
programming with mutable state.

~~~
