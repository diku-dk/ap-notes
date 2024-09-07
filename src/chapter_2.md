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

## Deriving `fmap` and `<*>`

It turns out that when some type is an instance of `Monad`, the `fmap`
and `<*>` methods from `Functor` and `Applicative` can be expressed in
terms of `>>=` and `pure`. This means that when we implement this
trifecta of instances, we only really have to think about `>>=` and
`pure`. Specifically, we can define a function `liftM` that behaves
like `fmap` for any monad:

```Haskell
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f x = x >>= \x' -> pure (f x')
```

And similarly a function `ap` that behaves like `<*>`:

```Haskell
ap :: m (a -> b) -> m a -> m b
ap f x = f >>= \f' ->
         x >>= \x' ->
           pure (f' x')
```

It can be shown that these are actually the only law-abiding
definitions for these functions. Further, these functions are
available from the builtin `Control.Monad` module. This means that
when defining the instances for `Maybe`, we can take the following
shortcuts:

```Haskell
import Control.Monad (liftM, ap)

instance Applicative Maybe where
  (<*>) = ap

instance Functor Maybe where
  fmap = liftM
```

## `do`-notation

Monads are particularly ergonomic to use in Haskell, and the main
reason for this is a bit of syntactic sugar called `do`-notation,
which allows us to imitate imperative programming.

Roughly speaking, the keyword `do` begins a block wherein every line
corresponds to a monadic action, with the actions combined with `>>=`,
and each statement after the first beginning a lambda. As an example,

```Haskell
do x <- foo
   y <- bar
   baz x y
```

is syntactic sugar for

```Haskell
foo >>=
(\x -> bar >>=
 (\y -> baz x y))
```

We can break a single statement over multiple lines if we are careful
about how we indent them: the continuation lines must be indented more
deeply than the first:

```Haskell
do x <- foo one_argument
          more arguments...
   y <- x
   ...
```

## Examples of defining and using monads

In the following we will look at examples of how to define and use our
own monads. Most of these correspond to monads that are available and
commonly used in standard Haskell libraries.

### The Reader Monad

The Reader monad is a commonly used pattern for implicitly passing an
extra argument to functions. It is often used to maintain
configuration data or similar context that would be annoyingly verbose
to handle manually, perhaps because it is not used in all cases of a
function. We will call this implicit value an *environment*.

Operationally, a Reader monad with environment `env` and producing a
value of type `a` is represented as a function accepting an `env` and
returning `a`.

```Haskell
{{#include ../haskell/readerstate.hs:Reader}}
```

Coming up with a definition of the Reader type itself requires actual
creativity. On the other hand, the `Functor`/`Applicative`/`Monad`
instances can almost be derived mechanically. I recommend starting
with the the following template code:

```Haskell
instance Functor (Reader env) where
  fmap = liftM

instance Applicative (Reader env) where
  (<*>) = ap
  pure x = undefined

instance Monad (Reader env) where
  m >>= f = undefined
```

The `fmap` and `(<*>)` definitions are done, as discussed above. All
we have to do is fill out the definitions of `pure` and `>>=`. I
usually start with `pure`, because it is simpler. We start with this:

```Haskell
pure x = undefined
```

The `Applicative` class requires that the `pure` instance for `Reader
env` has type `a -> Reader env a`. This means we know two things:

1. `x :: a`.
2. The right-hand side (currently `undefined`) must have type `Reader
   env a`.

Looking at the type definition of `Reader` above, we see that *any*
value of type `Reader` takes the form of a `Reader` constructor
followed by a payload. So we write this:

```Haskell
pure x = Reader undefined
```

Looking at the definition of `Reader` again, we see that this
`undefined` must have type `env -> a`.

~~~admonish hint

Instead of using `undefined`, we can also use `_`. This is a so-called
*hole*, and will cause the compiler to emit an error message
containing the type of the expression that is supposed to be located at the hole.

~~~

How do we construct a value of type `env -> a`? Well, we have a
variable of type `a` (namely `x`), so we can simply write an anonymous
function that ignores its argument (of type `env`) and returns `x`:

```Haskell
pure x = Reader $ \_env -> x
```

This concludes the definition of `pure`. We now turn our attention to
`>>=`. The line of thinking is the same, where we systematically
consider the types values we have available to us, and the types of
values we are supposed to construct. This is our starting point:

```Haskell
m >>= f = undefined
```

We know:

1. `m :: Reader env a`
2. `f :: a -> Reader env b`
3. `undefined :: Reader env b`

We don't have anything of type `a`, so we cannot apply the function
`f`. One we can do is deconstruct the value `m`, since we know this is
a single-constructor datatype:

```Haskell
Reader x >>= f = undefined
```

Now we have the following information:


1. `x :: env -> a`
2. `f :: a -> Reader env b`
3. `undefined :: Reader env b`

The values `x` and `f` are functions for which we do not have values
of the required argument type, so we cannot do anything to. But we can
still start adding a constructor to the right-hand side, just as we
did above for `pure`:

```Haskell
Reader x >>= f = Reader undefined
```

And again, we know that the `undefined` here must be a function taking
an `env` as argument:

```Haskell
Reader x >>= f = Reader $ \env -> undefined
```

So far we have not used any creativity. We have simply done the only
things possible given the structure of the types we have available.
We now have this:

1. `x :: env -> a`
2. `f :: a -> Reader env b`
3. `env :: env`
3. `undefined :: b`

Since we now have a variable of type `env`, we can apply the function
`x`. We do so:

```Haskell
Reader x >>= f = Reader $ \env ->
                   let x' = x env
                   in undefined
```

Now we have `x' :: a`, which allows us to apply the function `f`:

```Haskell
Reader x >>= f = Reader $ \env ->
                   let x' = x env
                   let f' = f env
                   in undefined
```

We have `f' :: Reader env b`, so we can pattern match on `f'` to
extract the payload. When a type has only a single constructor, we can
do this directly in `let`, without using `case`:

```Haskell
Reader x >>= f = Reader $ \env ->
                   let x' = x env
                   let Reader f' = f env
                   in undefined
```

Now we have `f' :: b`, which is exactly what we need to finish the
definition:

```Haskell
Reader x >>= f = Reader $ \env ->
                   let x' = x env
                   let Reader f' = f env
                   in f'
```

This finishes the `Monad` instance for `Reader`. However, we still
need to define the programming interface for the monad. Some monads
(such as `Maybe`, `Either`, or lists) directly expose their type
definition. But for more effect-oriented monads like `Reader`, we
usually want to hide their definition and instead provide an abstract
interface. This usually takes the form of a function for executing a
monadic computation, as well as various functions for constructing
monadic computations. For `Reader`, we will implement the following
interface:

```Haskell
runReader :: env -> Reader env a -> a

ask :: Reader env env

local :: (env -> env) -> Reader env a -> Reader env a
```

The `runReader` function is used to execute a `Reader` computation,
given an initial environment. It has the following definition:

```Haskell
runReader env (Reader f) = f env
```

The `ask` command is used to retrieve the environment. It has the
following definition:

```Haskell
ask = Reader $ \env -> env
```

The `local` function executes a given `Reader` command in a modified
environment. This does not allow stateful mutation, as the environment
is only modified while executing the provided command, not any
subsequent ones:

```Haskell
local f (Reader g) = Reader $ \env -> g (f env)
```

#### Using the Reader Monad

The `Reader` monad is mostly useful when writing functions with many
cases, where only some need to make use of the environment. This means
compelling examples are relatively verbose. You will see such examples
in the course exercises, but for now, we will use a somewhat contrived
example of modifying a binary tree of integers, such that every node
is incremented with its distance from the root.

First we define the datatype.

```Haskell
data Tree
  = Leaf Int
  | Inner Tree Int Tree
  deriving (Show)
```

```
> (Leaf 0 `Inner` Leaf 0) `Inner` Leaf 0
Inner (Inner (Leaf 0) (Leaf 0)) (Leaf 0)
```

Then we can define a monadic recursive function over `Tree`:

```Haskell
incLeaves :: Tree -> Reader Int Tree
incLeaves (Leaf x) = do
  depth <- ask
  pure $ Leaf $ x + depth
incLeaves (Inner l r) = do
  l' <- local (+ 1) $ incLeaves l
  r' <- local (+ 1) $ incLeaves r
  pure $ Inner l' r'
```

And then we can run our contrived function on some provided tree:

```
> runReader 0 $ incLeaves $ (Leaf 0 `Inner` Leaf 0) `Inner` Leaf 0
Inner (Inner (Leaf 2) (Leaf 2)) (Leaf 1)
```

### The State Monad

The State monad is similar to the Reader monad, except now we allow
subsequent commands to modify the state. We represent a stateful
computation as a function that accepts a state (of some abstract type
`s`) and returns a new state, along with a value.

```Haskell
{{#include ../haskell/readerstate.hs:State}}
```

The definitions of the type class instances follow similarly to the
ones for Reader, and can be derived largely through the same technique
of considering the types of values we have available to us.

```Haskell
{{#include ../haskell/readerstate.hs:Monad_State}}
```

Note that the have the opportunity to make a mistake here, by using
the original `state` instead of the modified `state'` produced by `m`.

```Haskell
{{#include ../haskell/readerstate.hs:Functor_State}}
{{#include ../haskell/readerstate.hs:Applicative_State}}
```

We also provide the following API for executing and interacting with
stateful computations. First, computation requires that we provide an
initial state and returns the final state:

```Haskell
runState :: s -> State s a -> (a, s)
runState s (State f) = f s
```

The `put` and `get` functions are for reading and writing the state.

```Haskell
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)
```

If we want to store more than a single value as state, we simply store
a tuple, record, or some other compound structure.

#### Using the State monad

We will use the State monad to implement a function that renumbers the
leaves of a tree with their left-to-right traversal ordering.

```Haskell
numberLeaves :: Tree -> State Int Tree
numberLeaves (Leaf _) = do
  i <- get
  put (i + i)
  pure $ Leaf $ i + 1
numberLeaves (Inner l r) = do
  l' <- numberLeaves l
  r' <- numberLeaves r
  pure $ Inner l' r'
```

We may now use it as follows:

```Haskell
> runState 0 $ numberLeaves $ (Leaf 0 `Inner` Leaf 0) `Inner` Leaf 0
(Inner (Inner (Leaf 0) (Leaf 1)) (Leaf 2),3)
```

### Combining Reader and State

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
