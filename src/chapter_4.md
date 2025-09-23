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
{{#include ../haskell/Week4/Free.hs:Free}}
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
another type. This is an instance of "higher-order polymorphism", where
we abstract not over *types*, but *type constructors*. While this is
the first time we have seen this in a data type definition, it is not
the first time we see higher-order polymorphism at all. Recall the
`Functor` type class:

```Haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Intuitively, if a data type is a `Functor`, that means it is a kind of
"container" with "elements", and we can apply an arbitrary function on
these elements. But importantly, `f` by itself *is not a
type*[^higher-order], but a *type constructor* that must be applied to
an argument (such as in `f a` and `f b`) to form a type. Our use of
`e` in the definition of `Free` works exactly the same way.

[^church]: This is not the only possible way to define free monads in
Haskell, but it is the simplest one. More efficient definitions exist
(such as
[Control.Monad.Free.Church](https://hackage.haskell.org/package/free-5.2/docs/Control-Monad-Free-Church.html)),
but they are more complicated, and unnecessary for our purposes.

[^higher-order]: Just like how values can be classified with types, so
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
{{#include ../haskell/Week4/Free.hs:ReadOp}}
```

The `r -> a` value is called a *continuation*. It is a function that
is called to resume evaluation once the requested value is ready.

Further, `ReadOp` can be made a `Functor`. We will see later that this
is necessary in order to make it usable with `Free`.

```Haskell
{{#include ../haskell/Week4/Free.hs:Functor_ReadOp}}
```

We can use this to construct a `Reader` monad using `Free`:

```Haskell
{{#include ../haskell/Week4/Free.hs:Reader}}
```

Once we have defined `Monad` instances and such for `Free`, we will be
able to write monadic code that makes use of it. But we also have to
define an interpretation function that actually *runs* the monad and
gives meaning to its effects? We want a function of the following
type:

```Haskell
{{#include ../haskell/Week4/Free.hs:RunReader}}
```

That is, given an initial value of type `r` and a computation of type
`Reader r a`, run that computation and produce a value of type `a`.
Since we don't know anything about `r`, our only option is to pattern
match on the `Reader r a` value itself. The `Pure` case is trivial, as
it represents a computation without any effects:

```Haskell
{{#include ../haskell/Week4/Free.hs:RunReader_Pure}}
```

For the second case, we are considering a value `Free (ReadOp g)`,
where `g` is of type `r -> Reader r a`.  To see this, recall that the
`Free` data constructor takes something of type `e (Free e a)` as a
payload; setting `e = ReadOp r`, this type becomes `ReadOp r (Free
(ReadOp r) a)`, which is the same as `ReadOp r (Reader r
a)`. Referring back to the definition of `ReadOp`, we conclude that
`g` must have type `r -> Reader r a`.

We can now apply `g` to the environment to obtain a `Reader r a`, which we
can then execute with a recursive application of `runReader`:

```Haskell
{{#include ../haskell/Week4/Free.hs:RunReader_Free}}
```

We can also define Haskell functions that hide the specific encoding
of `Reader` behind a more familiar interface:

```Haskell
{{#include ../haskell/Week4/Free.hs:ask}}
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
{{#include ../haskell/Week4/Free.hs:Functor_Free}}
```

The `Pure` case is straightforward. For the `Free` case, we have a
value `g` of type `e (Free e a)`, and we need to somehow transform
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
{{#include ../haskell/Week4/Free.hs:3}}
```

```Haskell
{{#include ../haskell/Week4/Free.hs:Applicative_Free}}
```

Finally we can define the `Monad` instance and the `>>=` method
itself.

```Haskell
{{#include ../haskell/Week4/Free.hs:Monad_Free}}
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

### Implementing `State` in Terms of `Free`

Similarly to `Reader`, it is also straightforward to define a `State`
monad using `Free`. A state monad supports two effects: reading the
state and writing the state:

```Haskell
{{#include ../haskell/Week4/Free.hs:StateOp}}
```

Defining a `Functor` instance for `StateOp` is similar to `ReadOp`,
and can be done in the usual style by looking at which variables of
which types we have available, and which must be constructed:

```Haskell
{{#include ../haskell/Week4/Free.hs:Functor_StateOp}}
```

Now we can define the monad `State` simply as `Free` applied to the
state effects:

```Haskell
{{#include ../haskell/Week4/Free.hs:State}}
```

Evaluation of a `State` computation is also very similar to the case
for `Reader`, and takes the form of a recursive function that
interprets the `StateOp` effcts. When we encounter a `StatePut`, we
discard the current state and use the provided one.

```Haskell
{{#include ../haskell/Week4/Free.hs:runState}}
```

Finally, we can define the usual `put`/`get` accessor functions.

```Haskell
{{#include ../haskell/Week4/Free.hs:put_get}}
```

On top of these, we can define the usual helper functions, such as
`modify`, in the way discussed in chapter 2.

### Implementing an Error Monad

As another example, consider a free monad with error handling, very
similar to that provided by `Either`. Here we support two effects:
throwing an error and catching an error:

```Haskell
{{#include ../haskell/Week4/Free.hs:Error}}
```

The interpretation function `runError` is a little more sophisticated
than the ones we saw before, as some control flow is now required to
handle the error cases. However, it is fundamentally very similar to
the bind method we have seen previously for the `Either` monad.

```Haskell
{{#include ../haskell/Week4/Free.hs:runError}}
```

Finally, we can define the usual boilerplate accessor functions for
using the effects:

```Haskell
{{#include ../haskell/Week4/Free.hs:throw_catch}}
```

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

```Haskell
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
inopportune time), but there are often some common errors we want to
handle specially, such as the file not existing. We define a sum type
that captures the result of attempting to read a file:

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

## Free Monads with IO

The example of free monads we saw above are perhaps a bit contrived,
as they merely involved replicating existing monads. In practice, we
often use free monads to abstract over complicated effects, typically
those in IO. Let us look at some use cases. By the constraints of
these notes, they will still be somewhat contrived (we can't fit an
actual production system here), but they will be more interesting than
spelling `State` in a new way.

### An Uncontrived Real World Example

To start out with, let us consider one of the most interesting and
useful functions, the recursive Fibonacci function:

```Haskell
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + (n - 2)
```

One common requirement when writing software is *logging*, yet we do
not wish every function to depend on some specific implementation of a
logging system. In particular, logging typically requires IO, and we
don't want every single function to live in the `IO` monad. Free
monads are a handy way to abstract out the notion of logging. Let us
define a type `FibOp` that encapsulates the effects that we need in
our `fib` function; currently restricted to merely logging.

```Haskell
data FibOp a = FibLog String a

instance Functor FibOp where
  fmap f (FibLog s x) = FibLog s $ f x
```

Now we can define a `FibM` monad that supports `FibOp` effects, with
an accessor function `fibLog`:

```Haskell
type FibM a = Free FibOp a

fibLog :: String -> FibM ()
fibLog s = Free $ FibLog s $ pure ()
```

And finally we can use it in our definition of `fib`:

```Haskell
fib :: Int -> FibM Int
fib 0 = pure 1
fib 1 = pure 1
fib n = do
  fibLog $ "fib(" ++ show n ++ ")"
  x <- fib (n - 1)
  y <- fib (n - 2)
  pure $ x + y
```

One of the interesting parts of the `FibM` monad is that there are
many legitimate and interesting ways to interpret it (in contrast to
`Reader` or `State`, which have only a single sensible
interpretation). One obvious one is to interpret it in the `IO` monad,
where the logging messages are printed as lines:

```Haskell
ioFibM :: FibM a -> IO a
ioFibM (Pure x) = pure x
ioFibM (Free (FibLog s x)) = do
  putStrLn s
  ioFibM x
```

Example use:

```
> ioFibM $ fib 5
fib(5)
fib(4)
fib(3)
fib(2)
fib(2)
fib(3)
fib(2)
8
```

In a real application, we might log to the file system or some
dedicated logging daemon (and probably, we would be computing
something more interesting than Fibonacci numbers), but this is
decoupled from the *users* of the `fibLog` effect.

But another useful interpretation function is one that just discards
the logging messages - and is pure:

```Haskell
pureFibM :: FibM a -> a
pureFibM (Pure x) = x
pureFibM (Free (FibLog _ c)) = pureFibM c
```

```
> pureFibM $ fib 5
8
```

And yet another useful interpretation is a pure one that accumulates
the log messages in a list.

```Haskell
logFibM :: FibM a -> (a, [String])
logFibM (Pure x) = (x, [])
logFibM (Free (FibLog s c)) =
  let (x', msgs) = logFibM c
   in (x', msgs ++ [s])
```

```
> logFibM $ fib 5
(8,["fib(2)","fib(3)","fib(2)","fib(2)","fib(3)","fib(4)","fib(5)"])
```

It is easy to imagine how this could be useful for testing the pure
logic for logging, without actually interacting with a complicated
logging infrastructure.


~~~admonish warning
The list concatenation `msgs ++ [s]` is quite inefficient, and means that
logging has quadratic cost in the number of log messages. An efficient
implementation would either prepend the log messages and reverse at the end, or
use a data structure with more efficient support for appending new elements.
This is however orthorgonal to the issue of free monads.
~~~

### Adding Another Effect

Above we saw how we could interpret the same effectful function
(`fib`) in three different ways, without modifying `fib` at all. Let
us now add another effect. One problem with the recursive Fibonacci
function is that it is very slow, as it redundantly recomputes the
same recursive invocations over and over again. If you execute
`pureFibM $ fib 25` at the `ghci` prompt, you will likely wait several
seconds before you get a response.

One way to improve the performance of recursive computations with many
shared subresults is *memoisation*, where we maintain a cache mapping
function arguments to results. Then, whenever we encounter an argument
we have seen before, we merely retrieve the result that was computed
last time again.

Memoisation is notoriously inconvenient to implement in pure
languages, because of the need to maintain a state. The idea behind
memoisation is that the effect of the cache is not *observable*, but
merely speeds up the computation, but Haskell does not know that.
Instead, we have to manually manage the cache of previous results,
which raises additional questions, such as when to expire cache
entries in order to avoid space leaks. It's a rather complicated
space, and intermingling memoisation logic with algorithmic logic is
likely to result in a mess.

Instead, let us augment the `FibM` monad to handle memoisation. First,
we add a new kind of effect to `FibOp`, namely `FibMemo`:

```Haskell
data FibOp a
  = FibLog String a
  | FibMemo Int (FibM Int) (Int -> a)
```

The `FibMemo` constructor has three components:

1. An integer `n` denoting that this effect refers to the result of
   computing `fib(n)`.

2. A `FibM Int` computation that computes `fib(n)` if executed.

3. A continuation `Int -> a` that should be invoked with the result of
   the computation stored in the `FibM Int` - or a memoised version if
   available.

The instance definition and the accessor functions are fairly
straightforward; strongly resembling those we have seen before.

```Haskell
instance Functor FibOp where
  fmap f (FibLog s x) = FibLog s $ f x
  fmap f (FibMemo n m c) = FibMemo n m $ \y -> f (c y)

fibMemo :: Int -> FibM Int -> FibM Int
fibMemo n m = Free $ FibMemo n m pure
```

The idea behind `fibMemo` is that we use it to wrap a computation, for
example like so:

```Haskell
fib :: Int -> FibM Int
fib 0 = pure 1
fib 1 = pure 1
fib n = fibMemo n $ do
  fibLog $ "fib(" ++ show n ++ ")"
  x <- fib (n - 1)
  y <- fib (n - 2)
  pure $ x + y
```

The operational idea is that whenever the `fib n` case is reached, we
want to look if an existing result for `n` has already been computed.
If so, we return it. If not, we compute the result using the provided
computation. Of course, whether that is *actually* what happens
depends on how we write our interpreter function for `FibM`. For
example, we can add support for `FibMemo` to `pureFibM` in a way that
does not actually perform any memoisation:

```Haskell
pureFibM :: FibM a -> a
pureFibM (Pure x) = x
pureFibM (Free (FibMemo _ x c)) = pureFibM $ c $ pureFibM x
pureFibM (Free (FibLog _ c)) = pureFibM c
```

But of course, that rather defeats the purpose of `FibMemo`. Instead,
we can write an interpretation function `memoFibM` that carries around
a cache of type `[(Int,Int)]`, in which memoisation results are
stored. We implement this by using a local helper function that
accepts and returns the state, and then we discard the final state at
the end.

```Haskell
memoFibM :: FibM a -> a
memoFibM m = fst $ memo [] m
  where
    memo :: [(Int, Int)] -> FibM a -> (a, [(Int, Int)])
    memo cache (Pure x) = (x, cache)
    memo cache (Free (FibMemo n fn c)) =
      case lookup n cache of
        Just res -> memo cache $ c res
        Nothing ->
          let (fn', cache') = memo cache fn
           in memo ((n, fn') : cache') (c fn')
    memo cache (Free (FibLog _ x)) =
      memo cache x
```

In the `FibMemo` case, we check whether a result for `n` is already
known, and if so, we return it. Otherwise we compute it by executing
`fn`. Note that `memo` could also be implemented using the `State`
monad, as the way we handle the cache is identical to how `State`
handles state.

Now even large applications of `fib` finish almost instantaneously, as
memoisation transforms the exponential number of recursive calls to a linear
number of recursive calls:

```
> memoFibM $ fib 1000
9079565065540428013
```

Although `memoFibM` is a rather simplistic way of handling
memoisation, in that the cache is not persistent across multiple
invocations, it is not difficult to conceive of a variant that uses
`IO` to store memoisation results in a database or on the file system,
perhaps with limitations on the maximum size of stored results, or
some expiry policy that removes entries after a time. All of these
changes can be made without modifying `fib` itself.

Another useful change would be to allow memoisation of arguments and
results that are not exclusively of type `Int` (or some other fixed
type). This is not particularly difficult, although somewhat more
verbose, and so we have left it out of the exposition here.

### Asynchronous Programming with Free Monads

Asynchronous programming styles have become common in languages
targeted towards network programming, often support by language
features such as async/await facilities. The purpose of such language
features is to *hide* the somewhat contorted control flow otherwise
required by asynchronous programming. For example, a program often
needs to *wait* for an event to happen. Operationally, this happens by
suspending the current computation, recording its state in a data
structure somewhere. Whenever an event comes in, it is checked whether
any suspended computations depend on it, and if so, they are resumed
until the next time they need to be suspended. However, exposing all
this complexity leads to a very awkward programming experience.
Instead, we would like to simply have a seemingly normal function that
*blocks* until the requested event arrives - but of course without
suspending the entire system. We shall now see how this can be
accomplished using a design based on free monads.

We start by defining a *very* simple model of events. An event is a
name paired with a value, and the value is always an integer.

```Haskell
type EventName = String

type EventValue = Int

type Event = (EventName, EventValue)
```

Events come from the outside world, in unpredictable order and with
uncertain timing, and are in practice often the result of
communication with other programs. For testing purposes, we can model
sequences of events as Haskell lists, but baking such an assumption
into our systems would make them useless in practice. At any given
time, many computations may be suspended waiting for events to happen;
some of them perhaps waiting for the same events.

To support his style of programming, we define an effect type for our
asynchronous programming model, with support for two effects:

1. Waiting for an event of a given name.

2. Logging a message. This is solely so we can observe execution of
   our asynchronous programs through their side effects (printing to
   the console), and is in principle unrelated to events.

The definition, along with its `Functor` instance, is as follows:

```Haskell
data EventOp a
  = WaitFor EventName (EventValue -> a)
  | Log String a

instance Functor EventOp where
  fmap f (WaitFor s c) = WaitFor s $ \x -> f (c x)
  fmap f (Log s c) = Log s $ f c

type EventM a = Free EventOp a
```

And it is all packed together under the name `EventM` with two
accessor functions `waitFor` and `logMsg`:

```Haskell
waitFor :: String -> EventM EventValue
waitFor s = Free (WaitFor s pure)

logMsg :: String -> EventM ()
logMsg s = Free $ LogMsg s $ pure ()
```

Here are three examples of how to use the monad. All of these
functions listen for events and do something (fairly trivial) with the
result. The purpose of the `divider` example is solely to illustrate
that control flow and looping is possible.

```Haskell
adder :: EventM ()
adder = do
  logMsg "starting adder"
  x <- waitFor "add"
  y <- waitFor "add"
  logMsg $ unwords [show x, "+", show y, "=", show $ x + y]

multiplier :: EventM ()
multiplier = do
  logMsg "starting multiplier"
  x <- waitFor "mul"
  y <- waitFor "mul"
  logMsg $ unwords [show x, "*", show y, "=", show $ x * y]

divider :: EventM ()
divider = do
  logMsg "starting divider"
  x <- waitFor "div"
  y <- waitForDivisor
  logMsg $ unwords [show x, "/", show y, "=", show $ x `div` y]
  where
    waitForDivisor = do
      y <- waitFor "div"
      if y == 0
        then do
          logMsg $ "Cannot divide by zero"
          waitForDivisor
        else pure y
```

Once you have finished with this section, and you have seen how the
sausage is made, I suggest returning to these definitions and normal
how *normal* they look. The complexity of how they are actually
executed is completely hidden by the monad abstraction.

A definition such as `adder` represents a *process*. It runs for as
far as possible until the value of an event is needed, at which point
it is suspended. We can write an interpretation function that does
just that; evaluating as many of the effects as possible until
reaching a `WaitFor`:

```Haskell
stepUntilWait :: EventM a -> IO (EventM a)
stepUntilWait (Pure x) = pure $ Pure x
stepUntilWait (Free (LogMsg s c)) = do
  putStrLn $ s
  stepUntilWait c
stepUntilWait (Free (WaitFor s c)) =
  pure $ Free $ WaitFor s c
```

It is possible to use `stepUntilWait` directly in `ghci`, but the
result is not terribly interesting:

```
> a <- stepUntilWait adder
starting adder
```

Now we have a name `a` representing a suspended execution.

```
:t a
a :: EventM ()
```

Unfortunately we cannot inspect its *structure*, because `EventOp` is
not an instance of `Show`, but we can be pretty sure it is currently
stuck on a `WaitFor` effect.

At some point, an event may arrive. We can then check whether the
event name matches what the suspended execution is waiting for, and if
so, call the continuation with the value. If the event name does not
match, we do nothing. We can encapsulate this in a function:

```Haskell
stepSingleEvent :: EventM () -> Event -> IO (EventM ())
stepSingleEvent (Free (WaitFor waiting_for c)) (event_name, event_val) =
  if waiting_for == event_name
    then stepUntilWait $ c event_val
    else pure $ Free $ WaitFor waiting_for c
stepSingleEvent p _ = pure p
```

After invoking the continuation, we use `stepUntilWait` to evaluate
any subsequent non-`WaitFor` events, but we do *not* recursively call
`stepSingleEvent`. This is because events are distinguishable: we want
the `adder` process to process two *distinct* events, not have the
same event with name `"add"` to provide values for both `waitFor`s.

Here is how we can use `stepSingleEvent`, continuing execution of the
`a` above:

```
> b <- stepSingleEvent a ("add", 1)
> c <- stepSingleEvent b ("add", 2)
1 + 2 = 3
```

Note how execution returns to our control after every invocation to
`stepSingleEvent`. This allows us to use arbitrary logic to retrieve
events (such as reading them from the network or a file), without the
process definitions (`adder`, `multiplier`, `divider`) having to care
about the details.

This is often called an *event pump*, by analogy to old-fashioned
water pumps. We continue cranking the handle (calling
`stepSingleEvent`), which lets the process continue through its
execution. The interesting thing is that the suspended computations,
the `a`, `b`, and `c` values above, are ordinary Haskell values, that
we can manipulate like any other Haskell value. One slightly dubious
thing we can do is to keep reusing the same suspended computation
multiple times:

```
> stepSingleEvent b ("add", 2)
1 + 2 = 3
> stepSingleEvent b ("add", 2)
1 + 2 = 3
> stepSingleEvent b ("add", 2)
1 + 2 = 3
```

Another more useful thing we can do is to keep *multiple* suspended
processes in a list. Whenever an event arrives, we crank the pump once
on each of them. If a process is truly finished, represented by the
`Pure` constructor, we remove it from the list. This can be expressed
as a fairly simple recursive function:

```Haskell
stepEventM :: [EventM ()] -> Event -> IO [EventM ()]
stepEventM [] _ = pure []
stepEventM (p : ps) event = do
  p' <- stepUntilWait p
  case p' of
    Pure () -> stepEventM ps event
    _ -> do
      p'' <- stepSingleEvent p' event
      ps' <- stepEventM ps event
      pure $ p'' : ps'
```

And finally, we can write another straightforward function that simply
calls `stepEventM` once for every event in a list of events:

```Haskell
runEventM :: [EventM ()] -> [Event] -> IO [EventM ()]
runEventM ps [] = do
  pure ps
runEventM ps (e : es) = do
  ps' <- stepEventM ps e
  runEventM ps' es
```

This lets us have interleaved execution of asynchronous operations - a
programming technique that in most languages is either hopelessly
complicated, or requires direct runtime support.

```
> runEventM [adder, multiplier, divider]
            [("add", 1),
             ("mul", 2),
             ("div", 3),
             ("add", 4),
             ("div", 0),
             ("mul", 5),
             ("div", 6)]
starting adder
starting multiplier
starting divider
1 + 4 = 5
Cannot divide by zero
2 * 5 = 10
3 / 6 = 0
```

As an example that truly demonstrates how decoupled the processes are
from how events are read, the following function reads events
interactively from the console, through the the `readLn` function
which reads a value in Haskell syntax.

```Haskell
interactivelyRunEventM :: [EventM ()] -> IO ()
interactivelyRunEventM [] = pure ()
interactivelyRunEventM ps = do
  ps' <- mapM stepUntilWait ps
  event <- readLn
  ps'' <- stepEventM ps' event
  interactivelyRunEventM ps''
```

Here is an example of using it, where the output from the processes is
intermixed with my typed input:

```
> interactivelyRunEventM [adder, multiplier, divider]
starting adder
starting multiplier
starting divider
("add", 1)
("add", 2)
1 + 2 = 3
("div", 3)
("mul", 2)
("mul", 4)
2 * 4 = 8
("div", -1)
3 / -1 = -3
```
