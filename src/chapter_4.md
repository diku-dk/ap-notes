# More Monads

Before we build new monads, it is worth recalling what a monad
is. We then show how monad parameterisation and free monads can be used to write code 
that generates computations once and then executes computations using different concrete monads. 
Finally, we provide examples of applying free monads to a selection of programming problems, 
ranging from various implementations of the state monad to memoisation, exception handling and
a form of asynchronous programming.

## Monads Revisited

Recall that an *abstract* monad is an *abstract data type*. It has the 
*interface* consisting of the operation signatures

```Haskell
  -- type M a
  fmap   :: (a -> b) -> M a -> M b
  pure   :: a -> M a                   
  (>>=)  :: M a -> (a -> M b) -> M b
```

and certain properties these operations must satisfy. The operation `(>>=)` is called *bind*.  
We can optionally use the name `return` instead of `pure` in monads, which emphasises that the operation does 
nothing else but return its argument.  We will stick to `pure` in the code in these notes, however.

A generally useful function is *Kleisli composition* `(>=>)`. It is defined using bind by

```Haskell
(>=>)  :: (a -> M b) -> (b -> M c) -> (a -> M c)
(f >=> g) x = f x >>= g
```

The monad laws are then most easily expressed in 
terms of Kleisli composition:

```Haskell
(f >=> g) >=> h   =  f >=> (g >=> h)
pure >=> g        =  g
f >=> pure        =  f 
```

The laws say that Kleisli composition is associative with `pure` as the neutral element,
just like ordinary function composition `.` is associative with `id` as its neutral element.

Another useful function is `join`. It is defined by using `(>>=)` by
 
```Haskell
join :: M (M a) -> M a 
join m = m >>= id
```

Recall that once we have defined `pure` and `>>=`, the definition of `fmap` is essentially forced to satisfy

```Haskell
fmap f m   =  m >>= pure . f 
```

Similarly, `(<*>)` is typically defined using `(>>=)` by

```Haskell
mf <*> ma = mf >>= \f -> ma >>= pure . f
```

The upshot is that defining a monad instance in Haskell can be done by using the template below, 
filling only the `...` in the definitions of `pure` and `>>=`.

```Haskell
instance Functor M where  
   fmap f m   =  m >>= pure . f  
   
instance Applicative M where 
   pure =  ...    -- definition of pure 
   mf <*> ma = mf >>= \f -> ma >>= pure . f
   
instance Monad M where 
   m >>= f = ...    -- definition of bind
```

Note that `(>=>)` and `join` are available as monad-parameterised functions in `Control.Monad`.  

## Subclasses of monads

Subclasses of monads have operations beyond this interface, with
additional properties. The abstract state monad `M s` over states of type `s` has operations `get` and `put`
with the following types.

```Haskell
class (Monad m) => StateMonad m s | m -> s where
  get :: m s
  put :: s -> m ()
```
and properties

```Haskell
put s >> get      ==  put s >> pure s      -- put-get
put s >> put s'   ==  put s'               -- put-put
get >>= put       ==  pure ()              -- get-put
```

(The functional dependency `m -> s` says: the state monad `m` determines the type `s` of state it depends on. 
Since Haskell insists on passing a class instance implicitly to a function---the programmer *must not* 
write it as an explicit argument---Haskell has to make sure there is a unique instance whenever such an argument  
is needed.  Using `newtype` declarations and functional dependencies helps in disambiguating 
when multiple instances might fit the bill.  Since instance inference is Haskell-specific trickery rather than a general 
programming technique we will usually try to stay away from having to use them and getting into the details of how they aid 
instance inference.) 

The properties express that storing a state and then reading it is the same as storing it and returning it; 
storing a state and then another state is the same as just storing the second state; and 
reading the state and then storing it again is the same as doing nothing at all.

Note that `StateMonad` is an abstract data type: It does not contain an implementation, but it tells us what is required of any 
implementation of a state monad--we will see several below--and what a user of *any* implementation can rely on. 
As such, an abstract data type, including its properties, is a contract.   It obliges the implementor of a particular implementation 
to provide the operations and ensure they satisfy the requisite properties, and it obliges the user to write code that works with *any* 
implementation of the abstract data type, not only a particular one.  

## Abstracting over monads

Suppose we want to write code that uses state, without committing to
how the state is implemented. 

We can write functions that are parameterised by the state monad class.  Here is a
counter, and a small stack machine:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:tick}}
```

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:stack}}
```

The purely functional implementation `State` from chapter 2 is one instance of `StateMonad`.

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:State}}
```

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:State_instances}}
```

We will meet more implementations in the next section. All of them run the *same* `tick` and
`stackExample`, with no change to their source text:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:four_targets}}
```

```
> runState 0 tickF 
(0,1)
> runFreeStateF tickFree 0
(0,1)
```

Both return the pair of the value `tick` returned and the final state.

This technique — write against an interface, pick the implementation at the
call site — is the first of two ways of decoupling the use of an effect
from its interpretation. It is simple and it is what most Haskell libraries
do.

Once we pick a state monad instance `m`, the only way of
observing an element of `m a` is by *running* it — that is, by passing it to
a `run...` function. Nothing happens until something kicks off execution.  
The monad operations `pure`, `(>>=)` and 
any primitive operations producing a computation such as `get` and `put s` only compose 
computations; they don't execute any of the effects in them. 

Consider `tickF` above. Its type is `State Int Int`, a 
monad instance, and the value is a `newtype` around a function `Int -> (Int, Int)`.
The `run...` function is `runState s m`.  It executes the entire computation `m` in an initial state `s`.  
The pair it returns is everything about `tickF` one can
observe. Which operations it performed on the way, and in which order, is opaque.

## Free Monads

A *free monad* is a tree of constructors that *name* `pure` and the primitive
operations of a monad without binding them to any particular implementation. 
An element of a free monad is essentially data.  We can provide multiple *interpretations* 
of the named operations.  An *interpretation* that maps the constructors to operations in a monad 
can be automatically extended to mapping entire computations into that monad.   
This way we achieve the same effect as monad-abstracted code.

Additionally, free monad elements are *data*, which we can analyse and manipulate at 
run time by pattern matching on the constructors used to name the primitive operations.
A half-run computation is a value that can be stored and resumed, rewriting a
computation yields another computation, and an operation can carry a
subcomputation as data. 

### The Free State Monad, Directly

Before giving the general construction of a free monad for any set of primitive operations, 
let us build one instance of it by
hand, for state. The construction is short and natural, and the
general construction will then be a matter of separating the steps into what is common and 
what needs to be done for each set of primitive operations separately. 

Consider a state monad, which has primitive operations `get` and `put`. Give each operation its continuation as an
extra argument to arrive at their *continuation-passing style* versions:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:putk_getk}}
```

Both directions relate the continuation-passing forms to the originals:

```Haskell
put s = putk s pure         put s >>= m  =  putk s m
get   = getk pure           get   >>= m  =  getk m
```

The left-hand column is the right unit law `m >>= pure == m`, so this uses
no property of the implementation beyond the monad laws. The point of the exercise is the
right-hand column: in a computation built from `getk` and `putk`, it can be shown that bind is no longer needed.
Every occurrence of bind can be transformed into one that has a `get` or `put` or `pure` on the left.
In the first two cases we can use `getk` and `putk`, and in the case of `pure` we can eliminate it using
the monad law `pure x >>= f  =  f x`.  The bind is no longer needed as a separate operation;
it has been pushed into the continuation arguments of primitive operations themselves.

Now we make `getk` and `putk` *uninterpreted* named functions, that is, constructors `Getk` and `Putk`. Their
types are

```Haskell
Return :: a -> m a
Getk   :: (s -> m a) -> m a
Putk   :: s -> m a -> m a
```
where `() -> m a` has been replaced by `m a` since `() -> m a` and `m a` are essentially isomorphic.  
We now build a particular `m` as the data type built by these constructors.  Let us call it `FSM s`:

```Haskell
data FSM s a =
    Return a
  | Getk (s -> FSM s a)
  | Putk s (FSM s a)
```
 
We now show that `FSM s` can be made into a state monad instance by providing definitions of `pure` and `(>>=)`. 
`pure` is just the constructor `Return`.  
Because each primitive operation carries a continuation, `(>>=)` can be defined by 
composing the continuation attached to a primitive operation with the continuation serving as the second argument of bind using
Kleisli composition. Specifically, `Getk k >>= f  =  Getk (k >=> f)`.  
Since we have `FSM s a` as the continuation argument type to `Putk` instead of `() -> FSM s a`, the corresponding
definition becomes `Putk s m >>= f = Putk s (m >>= f)`.  Finally, for `Pure` we get `Pure x >>= f  =  f x`.

We arrive at the following definition of `FSM s` as a `StateMonad` instance.

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:FSM_monad}}
```

We can check that the continuation-passing equalities we started from hold by calculation:

```Haskell
get >>= f    =  Getk Return >>= f
             =  Getk (\s -> Return s >>= f)
             =  Getk f

put s >>= f  =  Putk s (Return ()) >>= f
             =  Putk s (Return () >>= f)
             =  Putk s (f ())
```

We can now provide a `run` function that interprets the constructor functions 
as the corresponding functions in `State`.  

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:FSM_state}}
```

The point of `FSM s` is that this interpretation is in its `run` function, in contrast to `State`, 
where it is hardwired into the definitions of `get` and `put`.  In particular, after having built 
an `FSM s a` element we can run it with another and indeed multiple implementations of the primitive operations.  


### Abstracting the Primitive Operations

`FSM s` is the free state monad.
To get the general construction, we separate what is specific to the state monad, 
the operations `Getk` and `Putk`, from what all monads have in common, `Return`.

To do this we abstract over `FSM s a` by replacing it with a type variable `r` in the 
constructor arguments of `Getk` and `Putk`.  The point is that instantiation of `r` to `FSM s a`
is deferred to the point where we add `Return`.  We get a data type

```Haskell
data StateOp s r = StateGet (s -> r) | StatePut s r
```

so that, as a type isomorphism, we have 

```Haskell
X  =  a +  StateOp s X
```

where `+` is the sum type constructor.  In Haskell this means we want `X` to be a type `Free (StateOp s) a`
that is isomorphic to `Pure a | Free (StateOp s (Free (StateOp s) a))`.   
Replacing `StateOp s` by any type function `e` yields the definition for the free monad type:

```Haskell
data Free e a
  = Pure a
  | Free (e (Free e a))
```

To define `(>>=)` for `Free e a` we need to equip `e` with a way of 
performing Kleisli composition.  Here it is useful that 

```Haskell
k >=> f  = (>>= f) . k
```

That is, we can take the current continuation `k` of a primitive operation 
and *functionally* compose it with `(>>= f)`.  This is useful since in the definition of `e r`
the `r` is a type variable, not the eventual free monad it is instantiated to. 
It turns out that making `e` a functor by providing an `fmap` is exactly what is needed to 
make `Free e a` a monad. For `StateOp` we arrive at 

```Haskell
{{#include ../haskell/Week4/Free.hs:Functor_StateOp}}
```

We are now ready to provide the definition of `Free e a` as a monad:

```Haskell
instance Functor e => Functor (Free e) where
  fmap f m = m >>= pure . f                -- standard

instance Functor e => Applicative (Free e) where
  pure = Pure
  mf <*> ma = mf >>= \f -> ma >>= pure . f  -- standard

instance Functor e => Monad (Free e) where
  Pure x >>= f = f x
  Free g >>= f = Free $ fmap (>>= f) g     -- only interesting line
```

Note that the definitions of `fmap` and `(>>=)` are *not* mutually recursive.  
The occurrence of `fmap` in the definition of `(>>=)` is the `fmap` of functor `e`, 
not of functor `Free e`. 

By inlining the definition of `(>>=)` into `fmap` we arrive 
at its recursive form, which is commonly seen:

```Haskell
instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free g) = Free $ fmap (fmap f) g
```
This duplicates the code for `(>>=)` and camouflages 
that `fmap` can be defined the standard way, though.

### Example: The Free Reader Monad

To construct a free monad for a given set of primitive operations 
we need to provide a data type of constructors corresponding 
to the operations in continuation-passing style and equip it with `fmap` 
by systematically composing the continuation argument of each constructor with 
the argument to `fmap`.
   
The reader monad has the primitive operation `ask :: Reader r r`.  Its continuation-passing form 
is `askk :: (r -> Reader r a) -> Reader r a`.  Abstracting over `Reader r a` in the argument 
we get the constructor `ReadOp` with argument of type `r -> a`:  
```Haskell
{{#include ../haskell/Week4/Free.hs:ReadOp}}
```
with `fmap` forced to be the composition:
```Haskell
{{#include ../haskell/Week4/Free.hs:Functor_ReadOp}}
```
We can use this to construct the free reader monad using `Free`:

```Haskell
{{#include ../haskell/Week4/Free.hs:Reader}}
```

A run function provides an interpretation of the `ReadOp` effect:  

```Haskell
{{#include ../haskell/Week4/Free.hs:RunReader}}
```

We can now define `ask` to be the constructor `ReadOp` with `pure` as its continuation, as we have seen before:
```Haskell
{{#include ../haskell/Week4/Free.hs:ask}}
```

While it is perhaps not terribly interesting to define other
interpretations of the `Reader` monad, it is possible to do so; for
example by storing the environment in a global variable or in a
database, and defining an interpretation function that runs in `IO`
and fetches the environment from there. This is made possible by
decoupling the name of an effect from its interpretation.

Instantiating `e` to `StateOp s` recovers the type we built by hand at the
start of this section. The correspondence is constructor by constructor:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:FSM_iso}}
```

### The Recipe for Other Effects

The two steps above — pass the continuation, then abstract the recursive
occurrence — work for any operation, not just `get` and `put`. Given a monadic operation

```Haskell
op  :: T1 -> ... -> Tn -> M R
```

its continuation-passing version has type

```Haskell
opk :: T1 -> ... -> Tn -> (R -> M a) -> M a
```

Abstracting `M a` to a parameter gives the constructor

```Haskell
Op  :: T1 -> ... -> Tn -> (R -> a) -> Ops a
```

When `R` is `()`, the continuation `() -> a` collapses to a plain `a`. That
is how `Putk` lost its unit argument, and why `StatePut` carries a plain `a`
while `StateGet` carries a function.

~~~admonish tip
The `Functor` instance is then forced: `a` occurs only in the continuation,
so `fmap` can only compose onto it. This is why the `Functor` instances in
this chapter are so mechanical — they are not making a choice, they are the
only lawful thing to write.
~~~

Note what the recipe does *not* cover: an operation whose argument is itself
a computation, such as `catch` below. We return to that in the section on error
handling below.  It is the shape that most of the interesting effects
turn out to have.

### Example: The Free State Monad

We already have `StateOp` from the development at the start of this section.
All that remains is to make it a `Functor` — which, by the recipe above, is
forced — and to apply `Free` to it. We call the result `FreeState`, to keep
it apart from the `State` of chapter 2. 

```Haskell
{{#include ../haskell/Week4/Free.hs:Functor_StateOp}}
```

Now we can define the monad `FreeState` simply as `Free` applied to the
state effects, and make it an instance of the `StateMonad` class from the
previous section:

```Haskell
{{#include ../haskell/Week4/Free.hs:FreeState}}
```

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:FreeState_instance}}
```

Evaluation of a `FreeState` computation is similar to the case
for `Reader`, and takes the form of a recursive function that
interprets the `StateOp` effects. When we encounter a `StatePut`, we
discard the current state and use the provided one.

```Haskell
{{#include ../haskell/Week4/Free.hs:runState}}
```

Finally, we can define the usual `put` and `get` operations as the
corresponding constructors with `pure` as the continuation argument.

```Haskell
{{#include ../haskell/Week4/Free.hs:put_get}}
```

On top of these, we can define the usual helper functions, such as
`modify`, in the way discussed in chapter 2 — or simply reuse the `modify`
we already wrote against the `StateMonad` interface, since `FreeState` is
an instance of it.

### Interpreting into a Monad

`runState` above interprets a `FreeState` computation directly into a Haskell
function.  Notice, however, how little of it was about *state*: the `Pure` case and
the recursion are the same in every interpreter we have written. Only the
treatment of the named primitive operations differs.

We can make that precise. An *interpretation* of an effect functor `e` in a
monad `m` is a function

```Haskell
h :: forall x. e x -> m x
```

It says what a single operation means, and says nothing whatsoever about
sequencing using bind. The `forall x` matters: `h` must work for every result type, so
it cannot inspect the continuation — it can only hand it back. (Such a
uniform family of functions is called a *natural transformation*.)

From any such `h` we get an interpreter for whole computations mechanically:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:interpret}}
```

The `Free` case is where the `join` presentation of a monad is illustrated.
We have `g :: e (Free e a)`. Applying `h` gives `m (Free e a)`: the
operation, now performed in `m`, yielding the rest of the computation.
Interpreting that rest with `fmap` gives `m (m a)`, and `join` flattens it.

For state, the interpretation of a single operation is three lines:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:stateOps}}
```

and instantiating `m` picks the implementation:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:runFreeState_targets}}
```

```
> runFreeStateF stackExampleFree []
(Just 8,[])
> newIORef [] >>= runFreeStateI stackExampleFree >>= print
Just 8
```

Going through `interpret` and then through `State` builds an intermediate
closure for every operation. Nothing stops us from writing the composite
directly — and in fact we already did, several pages ago. Compare

```Haskell
{{#include ../haskell/Week4/Free.hs:runState}}
```

with `runFreeStateF`: up to the order of the arguments they are the same
function, the second being the first with `interpret` and the `State`
instance inlined and the intermediate closures fused away. The hand-written
interpreter was never a different technique; it was the general one,
specialised.

Being able to write either — the compositional one for clarity, the fused
one for speed, and to check them against each other — is one of the
practical benefits of having computations as data.

### Computations as Data

Interpreting is not the only thing we can do with an element of `Free e a`. It is an
ordinary value, so we can also *inspect* it and *rewrite* it.

Inspecting means walking the structure without running it. How far we get
depends on the effects. `StatePut s m` carries its continuation as a computation, so
we can walk past it; `StateGet k` carries a *function*, and to get past it we
would have to choose an `s` and apply `k` to it — and different choices give
different computations. So a static count of the writes a computation performs is
available only until the first read:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:countPuts}}
```

This is the general limit on inspecting free-monad computations. 
What you can see depends on how much of the
computation is *first-order*. A monadic operation that returns a value of type `T`
is represented by a constructor with a continuation of type `T -> M a` as an argument.  
Inspecting the computation `M a` requires choosing a value of type `T`.  

Rewriting means transforming the operations while leaving the sequencing by bind
alone:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:modifyEffects}}
```

~~~admonish warning
`modifyEffects` walks the *spine* of the computation. If an operation's argument
is itself a `Free` value — as `ErrorCatch` will be in the next section, and
as most of the interesting effects in assignment 4 are — then the rewriting
function must rebuild that argument explicitly. Leave it out and the rewrite 
does nothing to it. 
~~~

### What Makes `Free` Free

We can now say what "free" means.

`Free e` is the monad generated by `e` in such a way that only those equalities 
between its elements hold that are forced by the monad laws. Specifically, for
every monad `m` and every interpretation `h :: forall x. e x -> m x`, there
is exactly one function `interpret h :: Free e a -> m a` that respects
`pure` and `(>>=)`. Existence means you never have to invent sequencing
behaviour — writing the interpreter for the primitive operations is the *whole* job.
Uniqueness means that once you have
fixed what the primitive operations do, the meaning of every computation built from them
is determined.

We noted that a state monad 
must satisfy `put s >> put s' = put s'`.  The free state monad does not satisfy it because
the monad laws do not force the two terms to be equal. The equation only becomes true after
interpretation. So `Free (StateOp s)` is *not* a state monad.  Keeping the names of 
primitive operations is what lets us interpret them in multiple ways.

~~~admonish warning title="The cost of freedom"
The representation we use for free monads has a performance trap. In `m >>= f` with `m
= Free g`, the bind is pushed *through* the whole of `m` by `fmap`. So a
left-nested chain `((m1 >>= f1) >>= f2) >>= ...` re-traverses its prefix at
every step, and building an `n`-operation computation that way takes time quadratic in
`n`. Right-nested chains — what `do` notation produces from
straight-line code — are fine, which is why this rarely bites in practice.

Fixes are
known: continuation-passing (Church) encodings, as in
[`Control.Monad.Free.Church`](https://hackage.haskell.org/package/free-5.2/docs/Control-Monad-Free-Church.html),
make `>>=` constant-time but destroy the very inspectability we get from using constructors;
representing the continuation as a type-aligned queue keeps both. Neither is
needed for this course.
~~~

### Implementing an Error Monad

As another example, consider a free monad with error handling, similar to that
provided by `Either`. The main thing we will find is that it is easy to define a
free effect that is law-abiding and intuitively appears correct, but exhibits
unexpected behaviour. We will need to introduce a new language construct to fix
it.

We define the `ErrorOp` effect as follows.

```Haskell
-- Beware: subtly wrong!
data ErrorOp e a
  = ErrorThrow e
  | ErrorCatch a (e -> a)

instance Functor (ErrorOp e) where
  fmap _ (ErrorThrow e) = ErrorThrow e
  fmap f (ErrorCatch a c) = ErrorCatch (f a) $ \e -> f (c e)

type Error e a = Free (ErrorOp e) a
```

The interpretation function `runError` is a little more sophisticated
than the ones we saw before, as some control flow is now required to
handle the error cases. However, it is fundamentally very similar to
the bind method we have seen previously for the `Either` monad.

```Haskell
runError :: Error e a -> Either e a
runError (Pure x) = Right x
runError (Free (ErrorThrow e)) = Left e
runError (Free (ErrorCatch x c)) =
  case runError x of
    Left e -> runError $ c e
    Right x' -> Right x'
```

Finally, we can define the usual boilerplate accessor functions for
using the effects:

```Haskell
throw :: e -> Error e a
throw e = Free $ ErrorThrow e

catch :: Error e a -> (e -> Error e a) -> Error e a
catch x c = Free $ ErrorCatch x c
```

Although these definitions look right, and the `Functor` instance for `ErrorOp`
is law-abiding, our definition of `catch` behaves unexpectedly. The problem is
that for `Free`, the `>>=` operator distributes the continuation into every
branch of the continuation type, leading to the following equality:

```Haskell
(m `catch` h) >>= k   ==   (m >>= k) `catch` (\e -> h e >>= k)
```
Essentially, the continuation `k` ends up running *inside* the handler. If the `catch` succeeds and `k`
afterwards throws, the handler catches an exception raised after the `catch`
had already finished. With a concrete example:

```Haskell
k 1 = throw "E2"
k v = pure v

> runError ((pure 1) `catch` (\_ -> pure 99) >>= k)
Right 99
```

We could expect this to always throw an error, since that is what `k` does, but
the exception handler `(\_ -> pure 99)` is also applied to the continuation `k`.
The behaviour we have here is not wrong from a type-class perspective, but it is
certainly not like most `catch` constructs in real programming languages. It
behaves more like installing an exception handler that is dynamically active
*for the entire rest of the computation*, which is certainly a feature that may
be meaningful, but is not really what we intended.

To fix this, we need to define an `ErrorOp e a` where the "action to perform in
case of error" is *not* a continuation as far as `Free` is concerned. This means
it must have a different type than `a`, or else we cannot define the `Functor`
instance, as that must apply the given function to *every* `a` in order to be
lawful.

The solution is to use *existential quantification* to bind a type parameter `x`
that is known *only* inside the `ErrorCatch` constructor, and then express
the `ErrorCatch` constructor as containing three values:

* The immediate action to perform, of type `ErrorM e x`.

* The action to perform in case of error, of type `e -> ErrorM e x`.

* The continuation, of type `x -> a`.

In Haskell it is written in this way:

```Haskell
{{#include ../haskell/Week4/Free.hs:Error}}
```

The type variable `x` occurs only in the arguments of the constructor, not in
its result, so it is existentially quantified.

Despite the added complexity of the type, the run function is fairly
straightforward:

```Haskell
{{#include ../haskell/Week4/Free.hs:runError}}
```

And similarly, the accessor functions are also almost identical:

```Haskell
{{#include ../haskell/Week4/Free.hs:throw_catch}}
```

As a rule of thumb, it is generally a mistake to have more than a single real
continuation (of type `a`) in an effect definition. At least we must give
careful thought to the consequences when it occurs. This issue occurs for many
interesting effects: transactions, retries, timeouts, resource scoping, loops
you can break out of. Using existential quantification is a common solution to
this problem, although it can also be done by requiring the "intermediate
result" `x` to have a *specific* type, rather than be existentially
quantified. 

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
all ultimately expressed in terms of non-monadic code.

There is one exception, however: the `IO` monad is truly built into
the language, and cannot be expressed using normal Haskell. It is the
ultimate mechanism by which Haskell programs interact with the
surrounding world. This is evident in the type of `main`, the
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
monad. This interpretation is useful to an extent, but breaks down
when considering concurrency, which we will look at later in the
course. Ultimately, it is most useful to simply consider `>>=` for the
IO monad as straight-up impure and executing side effects.

### Programming with IO

Programming with the `IO` monad in Haskell is very similar to
programming in a conventional imperative language, and the same as
with programming with any other monad. However, the fact that it is so
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
program's `main` function:

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
(even when using monads to propagate the error situation).

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
much of what it provides. The main thing we will need is the `catch`
function:

```Haskell
catch :: Exception e => IO a -> (e -> IO a) -> IO a
```

The Haskell exception handling machinery is fairly similar to that
which you might be familiar with from other languages. The `catch`
function takes two arguments. It tries to run the provided `IO`
action, and if an exception is thrown during that action, it calls the
provided handler function with the exception. The wrinkle is that
`catch` is *polymorphic*: since an exception is any value that
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
Haskell to know which one we mean? We need to put in a type annotation
to specify the one we are interested in. We will mainly use the
type `SomeException`, which acts as a "root type" for all other kinds
of exceptions. In general, we will not discriminate between
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
result of the computation (after `catch` is done) will the division
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
list, it will only be up to the first cons cell. However, this is
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
order to interact with `IORef`s.

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

### Example: Imperative State Monad

We promised earlier that we would provide another implementation of the `StateMonad` interface.
We keep the state in a mutable cell of type `IORef ...` and
let a computation be a function from that cell to an `IO` action.

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:IState}}
```

The instance definitions are unremarkable:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:IState_instances}}
```

This is the implementation an imperative programmer would have written
first, and it is genuinely different from `State`: it mutates one cell
rather than threading a value. Yet it satisfies the same laws, and

```Haskell
tickI :: IState Int Int
tickI = tick
```

is the same `tick` we ran with `State`, at a different type.

~~~admonish note
`IState` is not more efficient than `State` in any deep sense — GHC is
perfectly good at compiling state threading — but it is the shape you are
forced into when the state lives outside the program: a file, a database, a
device. 
~~~

### Example: Object-based State Monad

`IState` passes the reference holding the state to every operation. It is worth asking whether
the state could be kept as the private field of an object that has `get` and `put` as its methods. It can, and the result is a fourth
implementation of the `StateMonad` interface.

An object is a record of operations closed over a cell that nothing else can
reach:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:StateObj}}
```

`newStateObj` creates the cell and captures it in the two closures. After
that, nothing passes the cell to `objGet` or `objPut` — they already have it.

A computation is then a function from such an object to an `IO` action, and
running one allocates a fresh object:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:OState}}
```

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:OState_instances}}
```

Reading the three definitions in order, `pure a` *ignores* the object, which
is exactly why it performs no side effects; `m >>= f` hands the *same* object
to both halves, which is why the second sees what the first did. `get` and
`put` are the object's own methods, lifted into the monad. As always,
`fmap` and `<*>` follow from `>>=`.

`OState` satisfies the same three laws as the others, and the same `tick`
runs over it:

```
> runOState tick 0
0
> runOState (tick >> tick >> tick) 0
2
```

~~~admonish note title="This is `IState`, with the reference abstracted"
Put the two side by side:

```Haskell
newtype IState s a = IState (IORef s -> IO a)
newtype OState s a = OState (StateObj s -> IO a)
```

They have the same shape, and the reference is still threaded in both — it
has merely moved from an explicit argument into the object you must be
holding in order to call a method at all. That is not a defect of the
encoding. It is what makes two runs independent: `runOState` allocates a
fresh object each time, so the only thing distinguishing two runs is *which*
object they were given.

Making the reference genuinely global — a top-level `IORef` created with
`unsafePerformIO` — does remove the argument, and removes that independence
with it: one cell for the whole program, no way to scope or reset it, and, by
the functional dependency on `StateMonad`, exactly one state type for the
monad `IO` forever after.
~~~

The gain over `IState` is that `IORef s` is a concrete representation while
`StateObj s` is an interface, so `OState` accepts implementations that
`IState` cannot express. Any function on objects gives one; here is a wrapper
that reports every operation:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:tracing}}
```

```
> o <- newStateObj (0 :: Int)
> runOStateOn (tick >> tick) (tracing "counter" o)
counter: get -> 0
counter: put 1
counter: get -> 1
counter: put 2
1
```

The same `tick`, the same monad, a different object — and state in a file, in
an `MVar`, or behind a network call would be three more, none of them
requiring a change to `tick` or to `OState`. This is the same separation the
free monad buys, obtained by abstracting the *representation* rather than the
*program*, and it has the same limit as the rest of this section: you can
vary what `get` and `put` do, but a computation is still a function, so there
is nothing to inspect, rewrite or suspend.

## Free Monads with IO

The examples of free monads we saw above are perhaps a bit contrived,
as they merely involved replicating existing monads. In practice, we
often use free monads to abstract over complicated effects, typically
those in IO. Let us look at some use cases. Given the constraints of
these notes, they will still be somewhat contrived (we can't fit an
actual production system here), but they will be more interesting than
spelling `State` in a new way.

### One Computation, Four Implementations

Before moving on, it is worth collecting what we now have. The single
definition

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:tick}}
```
mentions no implementation of state at all, and we can run it five ways:

```
> runState 0 tickF                 -- pure state threading
(0,1)
> newIORef 0 >>= runIState tickI   -- a mutable IORef
0
> runOState tickO 0                -- an object holding the reference
0
> runFreeStateF tickFree 0         -- free monad, interpreted purely
(0,1)
> newIORef 0 >>= runFreeStateI tickFree   -- free, interpreted in IO
0
```

The first three use the type-class method: `tick` is a polymorphic value, and
choosing its type chooses the implementation. The last two use the free
monad: `tickFree` is a concrete *data structure*, and choosing the
interpreter chooses the implementation.

The difference between the two methods only becomes visible when we want to
do something with the computation other than run it. `tickF` is a function:
the only thing to be done with it is to pass it to `runState`, and the pair
that comes back is everything observable — which operations ran, and in what
order, cannot be recovered from it. `tickFree` is a value you can pattern match
on, store in a list, count the operations of, interpret twice and compare,
or — as we are about to do — interpret in a way that *no* implementation of
the `StateMonad` interface could have provided, because it does something
the interface never mentions.

### An Uncontrived Real World Example

To start with, let us consider the recursive Fibonacci function:

```Haskell
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

One common requirement when writing software is *logging*, yet we do
not wish every function to depend on some specific implementation of a
logging system. In particular, logging typically requires IO, and we
don't want every single function to live in the `IO` monad. Free
monads are a handy way to abstract out the notion of logging. Let us
define a type `FibOp` that encapsulates the effects that we need in
our `fib` function, currently restricted to merely logging.

```Haskell
data FibOp a = FibLog String a

instance Functor FibOp where
  fmap f (FibLog s c) = FibLog s $ f c
```

Now we can define a `FibM` monad that supports `FibOp` effects, with
a function `fibLog` that performs a single logging step:

```Haskell
type FibM a = Free FibOp a

fibLog :: String -> FibM ()
fibLog s = Free $ FibLog s $ Pure ()
```

Finally we use it in a monadic definition of `fib`:

```Haskell
fib :: Int -> FibM Int
fib 0 = pure 1
fib 1 = pure 1
fib n = do
  fibLog ("fib(" ++ show n ++ ")")
  x <- fib (n - 1)
  y <- fib (n - 2)
  pure $ x + y
```

One of the interesting parts of the `FibM` monad is that there are
many legitimate and interesting ways to interpret it (in contrast to
`Reader` or `State`, which have only one sensible
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
   in (x', s : msgs)
```

```
> logFibM $ fib 5
(8,["fib(5)","fib(4)","fib(3)","fib(2)","fib(2)","fib(3)","fib(2)"])
```

Note that the list is in the same order as the lines `ioFibM` printed. That
is not an accident we should be relaxed about: two interpreters of the same
computation disagreeing about the order of the effects they both report would be
a bug in one of them, and it is precisely the kind of bug that having two
interpreters makes easy to catch.

It is easy to imagine how this could be useful for testing the pure
logic for logging, without actually interacting with a complicated
logging infrastructure.

~~~admonish warning
Be careful to prepend with `s : msgs` rather than append with `msgs ++ [s]`.
Appending would both reverse the order and make logging cost quadratic time
in the number of messages. If you do need to build a list from the far end,
either accumulate in reverse and `reverse` once at the end, or use a data
structure with cheap appending. This is orthogonal to the issue of free
monads, but it is an easy mistake to make in an interpreter.
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
last time.

Memoisation is notoriously inconvenient to implement in pure
languages, because of the need to maintain a state. The idea behind
memoisation is that the effect of the cache is not *observable*, but
merely speeds up the computation, but Haskell does not know that.
Instead, we have to manually manage the cache of previous results,
which raises additional questions, such as when to expire cache
entries in order to avoid space leaks. This is a rather complex
design space, and intermingling memoisation logic with algorithmic logic is
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
   computing `fib n`.

2. A `FibM Int` computation that computes `fib(n)` if executed.

3. A continuation `Int -> a` that should be invoked with the result of
   the computation stored in the `FibM Int` - or a memoised version if
   available.

The instance definition for `FibOp` as a functor is straightforward, resembling those we have seen before.

```Haskell
instance Functor FibOp where
  fmap f (FibLog s c)    = FibLog s $ f c
  fmap f (FibMemo n m c) = FibMemo n m $ f . c

fibMemo :: Int -> FibM Int -> FibM Int
fibMemo n m = Free $ FibMemo n m Pure
```

The idea behind `fibMemo` is that we use it to wrap a computation. 

```Haskell
fib :: Int -> FibM Int
fib 0 = pure 1
fib 1 = pure 1
fib n = fibMemo n $ do
  fibLog ("fib(" ++ show n ++ ")")
  x <- fib (n - 1)
  y <- fib (n - 2)
  pure $ x + y
```

The operational idea is that whenever the `fib n` case is reached, we
want to check whether an existing result for `n` has already been computed.
If so, we return it. If not, we compute the result using the provided
computation. Of course, whether that is *actually* what happens
depends on how we write our interpreter function for `FibM`. For
example, we can add support for `FibMemo` to `pureFibM` in a way that
does not actually perform any memoisation:

```Haskell
{{#include ../haskell/Week4/Free.hs:pureFibM}}
```

But of course, that rather defeats the purpose of `FibMemo`. Instead,
we can write an interpretation function `memoFibM` that carries around
a cache of type `[(Int,Int)]`, in which memoisation results are
stored. We implement this by using a local helper function that
accepts and returns the state, and then we discard the final state at
the end.

```Haskell
{{#include ../haskell/Week4/Free.hs:memoFibM}}
```

In the `FibMemo` case, we check whether a result for `n` is already known, and
if so, we return it. Otherwise we compute it by executing `fn`. Note that `run`
could also be implemented using the `State` monad, as the way we handle the
cache is identical to how `State` handles state.

Now even large applications of `fib` finish almost instantaneously, as
memoisation transforms the exponential number of recursive calls into a linear
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

Another useful change is to allow memoisation of arguments and results
that are not exclusively of type `Int` (or some other fixed type), or to use a
more efficient data structure than a list to store the cache. This is not
particularly difficult, although somewhat more verbose, and so we have left it
out of the exposition here.

### Asynchronous Programming with Free Monads

Asynchronous programming styles have become common in languages targeted towards
network programming, often supported by language features such as async/await
facilities. The purpose of such language features is to *hide* the somewhat
contorted control flow otherwise required by asynchronous programming. For
example, a program often needs to *wait* for an event to happen. Operationally,
this happens by suspending the current computation by recording its state in a
data structure somewhere. Whenever an event comes in, it is checked whether any
suspended computations depend on it, and if so, they are resumed until the next
time they need to be suspended. However, exposing this directly leads to a very
awkward programming experience. Instead, we would like to have a seemingly
normal function that *blocks* until the requested event arrives - but of course
without suspending the entire system. We shall now see how this can be
accomplished using a design based on free monads.

We start by defining a *very* simple model of events. An event is a
name paired with a value, and the value is always an integer.

```Haskell
{{#include ../haskell/Week4/FreeAsync.hs:Event}}
```

Events come from the outside world, in unpredictable order and with
uncertain timing, and are in practice often the result of
communication with other programs. For testing purposes, we can model
sequences of events as Haskell lists, but baking such an assumption
into our systems would make them useless in practice. At any given
time, many computations may be suspended waiting for events to happen;
some of them perhaps waiting for the same events.

To support this style of programming, we define an effect type for our
asynchronous programming model, with support for two effects:

1. Waiting for an event of a given name.

2. Logging a message. This is solely so we can observe execution of
   our asynchronous programs through their side effects (printing to
   the console), and is in principle unrelated to events.

The definition, along with its `Functor` instance, is as follows:

```Haskell
{{#include ../haskell/Week4/FreeAsync.hs:EventOp}}
```

And it is all packed together under the name `EventM` with two
functions `waitFor` and `logMsg`:

```Haskell
{{#include ../haskell/Week4/FreeAsync.hs:waitFor_logMsg}}
```

Here are three examples of how to use the monad. All of these
functions listen for events and do something fairly simple with the
result. The purpose of the `divider` example is solely to illustrate
that control flow and looping are possible.

```Haskell
{{#include ../haskell/Week4/FreeAsync.hs:processes}}
```

A definition such as `adder` represents a *process*. It runs as
far as possible until the value of an event is needed, at which point
it is suspended. We can write an interpretation function that does
just that; evaluating as many of the effects as possible until
reaching a `WaitFor`:

```Haskell
{{#include ../haskell/Week4/FreeAsync.hs:stepUntilWait}}
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
not an instance of `Show`, but we can be sure it is 
stuck on a `WaitFor` effect.

At some point, an event may arrive. We can then check whether the
event name matches what the suspended execution is waiting for, and if
so, call the continuation with the value. If the event name does not
match, we do nothing. We can encapsulate this in a function:

```Haskell
{{#include ../haskell/Week4/FreeAsync.hs:deliver}}
```

After invoking the continuation, we use `stepUntilWait` to evaluate
any subsequent non-`WaitFor` effects, but we do *not* recursively call
`deliver`. This is because events are distinguishable: we want
the `adder` process to process two *distinct* events, not have the
same event with name `"add"` provide values for both `waitFor`s.

Here is how we can use `deliver`, continuing execution of the
`a` above:

```
> b <- deliver ("add", 1) a
> c <- deliver ("add", 2) b
1 + 2 = 3
```

Note how execution returns to our control after every invocation of
`deliver`. This allows us to use arbitrary logic to retrieve
events (such as reading them from the network or a file), without the
process definitions (`adder`, `multiplier`, `divider`) having to care
about the details.

This is often called an *event pump*, by analogy to old-fashioned
water pumps. We continue cranking the handle (calling
`deliver`), which lets the process continue through its
execution. Note that the suspended computations,
the `a`, `b`, and `c` values above, are ordinary Haskell values, which
we can manipulate like any other Haskell values. One potentially dubious
thing we can do is to keep reusing the same suspended computation
multiple times:

```
> deliver ("add", 2) b
1 + 2 = 3
> deliver ("add", 2) b
1 + 2 = 3
> deliver ("add", 2) b
1 + 2 = 3
```

A useful extension is keeping *multiple* suspended
processes in a list. Whenever an event arrives, we deliver 
it to each of them, where every process that is *not*
waiting for this particular event just ignores it.  So the whole
pump is a recursive function over the list of events:

```Haskell
{{#include ../haskell/Week4/FreeAsync.hs:runEventM}}
```

The first `mapM stepUntilWait` gets every process started, up to its
first `WaitFor`; after that, `deliver` does the stepping itself.

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
interactively from the console, through the `readLn` function,
which reads a value in Haskell syntax.

```Haskell
{{#include ../haskell/Week4/FreeAsync.hs:interactivelyRunEventM}}
```

Here is an example of using it, where the output from the processes is
intermixed with typed input:

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
