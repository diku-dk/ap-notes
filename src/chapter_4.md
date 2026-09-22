# More Monads

## Monads Revisited

Before we build new monads, it is worth being precise about what a monad
*is*, because the rest of this chapter is about constructing one
mechanically.

### Monads as Abstract Data Types

A *computation* is a value that wraps up code which, **if and only if** it
is executed, performs some side effects and finally returns a value. This is
the key idea behind monadic programming, and it is worth dwelling on: `IO
()` is not an action that happens, it is a *description* of an action, which
is an ordinary Haskell value until something runs it. The same is true of
`State s a` from chapter 2, and of everything we build below.

A *monad* is then an abstract data type `M a` of computations, with an
interface

```Haskell
fmap   :: (a -> b) -> M a -> M b
return :: a -> M a                   -- Applicative spells it pure
(>>=)  :: M a -> (a -> M b) -> M b
```

subject to equational laws. The point of the laws is that `return` should
have no effects of its own, and that `>>=` should be a genuine sequencing
operator — associative, with `return` as its unit:

```Haskell
return x >>= f    ==  f x
m >>= return      ==  m
(m >>= f) >>= g   ==  m >>= (\x -> f x >>= g)
```

An equivalent presentation, and the one used in category theory, replaces
`>>=` by `join`, which flattens a computation that produces a computation:

```Haskell
class Functor m => Monad m where
  return :: a -> m a
  join   :: m (m a) -> m a
```

with the laws

```Haskell
join (return c)      ==  c
join (fmap return c) ==  c
join (join c)        ==  join (fmap join c)
```

The two presentations define each other: `join c = c >>= id` and `m >>= f =
join (fmap f m)`. Categorically, a monad on a category (for us: `Hask`,
whose objects are types and whose arrows are functions) is a functor
together with two natural transformations `return` and `join` satisfying
exactly these equations. We will use the `join` form once, when we say what
an interpretation of a free monad is.

A third presentation composes functions `a -> m b` — *Kleisli arrows* —
directly:

```Haskell
(>=>)  :: (a -> m b) -> (b -> m c) -> (a -> m c)
(f >=> g) x = f x >>= g
```

With `return` as the identity and `>=>` as composition, the three laws above
say precisely that these arrows form a category. We use `>=>` occasionally
below, where it is shorter than writing the lambda out.

~~~admonish note
Haskell splits this interface across three classes — `Functor`,
`Applicative` and `Monad` — for historical reasons. `Applicative` sits
between the two: it can sequence computations whose *shape* does not depend
on earlier results. Every monad is an applicative functor via `(<*>) = ap`,
which is why the instances below are so often one-liners.
~~~

A *concrete* monad almost always has operations beyond this interface, with
laws of its own. The state monad has

```Haskell
get      :: State s s
put      :: s -> State s ()
runState :: s -> State s a -> (a, s)
```

and satisfies, for example,

```Haskell
put s >> get      ==  put s >> return s
put s >> put s'   ==  put s'
get >>= put       ==  return ()
```

~~~admonish warning
It is tempting to write the first law as `put s >> get == return s`, but
that is false: both sides return `s`, yet the left-hand side also *changes
the state* and the right-hand side does not. Two computations are equal when
they agree on their return value **and** on their effects. `runState 99 (put
7 >> get)` is `(7, 7)`, while `runState 99 (return 7)` is `(7, 99)`.
~~~

This is the useful way to think about the sections that follow. A monad is
an interface plus laws; the interesting question is how many different
*implementations* of that interface we can give, and how cheaply we can
switch between them.

### Abstracting Over the Implementation

Suppose we want to write a program that uses state, without committing to
how the state is represented. One way is to put the operations in a type
class and quantify over the monad:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:StateMonad}}
```

The functional dependency `m -> s` is the part worth dwelling on. Without it,
`StateMonad m s` declares a *relation* between two types, and nothing says a
given `m` has only one `s`. That is not a pedantic worry: without the
dependency, GHC cheerfully accepts

```Haskell
instance StateMonad (FState s) s        -- the intended one
instance StateMonad (FState Int) Bool   -- also fine!
```

side by side, so `FState Int` genuinely has two state types and the question
"what state does `m` carry?" has no answer.

The dependency is a *promise* that the relation is a partial function: at
most one `s` per `m`. GHC uses it in both directions. It uses it to *infer* —
given a wanted `StateMonad m s1` and a known `StateMonad m s2`, it may
conclude `s1 ~ s2` and unify, and this fires even while `m` is still a type
variable, which ordinary instance lookup cannot do. And it *checks* the
promise: with the dependency in force, the second instance above is rejected
with `Functional dependencies conflict between instance declarations`.

Here is what goes wrong without it:

```Haskell
copyState :: (StateMonad m Bool) => m ()
copyState = get >>= put
```

```
* Could not deduce (StateMonad m a0) arising from a use of 'get'
  from the context: StateMonad m Bool
  The type variable 'a0' is ambiguous
```

Note carefully what is *not* the problem. `>>=` does tie the state type of
`get` to that of `put` — both are `a0`. What is missing is any link between
`a0` and the `Bool` in the signature's constraint, and without the promise
there need not be one.

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:modify}}
```

~~~admonish note
`modify` as written above in fact compiles *without* the dependency, and so
do `push` and `pop` below: the argument `f :: s -> s` carries the signature's
`s` into the body, so there is nothing left to infer. The trouble starts as
soon as nothing does. In `stackExample` below, `push 3` gives the literal `3`
a type variable of its own, which defaults to `Integer` and can then never be
reconciled with the `Int` in the signature:

```
* Could not deduce (StateMonad m [Integer])
  from the context: StateMonad m [Int]
```

The rule of thumb: without the dependency, the state type must be pinned
*syntactically*, by an argument's type or by the result type. A type it has
only by virtue of the class constraint is out of reach. An associated type
family, `type S m`, makes the same promise in a different notation.
~~~

Now we can write programs that mention no implementation at all. Here is a
counter, and a small stack machine:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:tick}}
```

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:stack}}
```

The purely functional implementation from chapter 2 is one instance. We call
it `FState` here, to keep it distinct from the free construction we build
later in this chapter:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:FState}}
```

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:FState_instances}}
```

We will meet two more implementations once we have seen `IORef` — one
mutating a cell, one holding that cell inside an object — and a fourth, the
free one, in the next section. All of them run the *same* `tick` and
`stackExample`, with no change to their source text:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:four_targets}}
```

```
> runFState tickF 0
(0,1)
> runFreeStateF tickFree 0
(0,1)
```

(Both return the pair of the value `tick` returned and the final state.)

This technique — write against an interface, pick the implementation at the
call site — is the first of the two ways of decoupling the use of an effect
from its interpretation. It is simple and it is what most Haskell libraries
do.

It is worth being careful about what it does and does not give us, because
the obvious complaint about it is wrong. Once we pick `m`, the only way of
observing an element of `m a` is by *running* it — that is, by passing it to
a `run...` function. This is the same point the section on computations made:
nothing happens until something kicks off execution, and execution is the
only window onto what happened.

Consider `tickF` above. Its type is `FState Int Int`, a perfectly definite
monad, and the value is a `newtype` around a function `Int -> (Int, Int)`.
The `run...` function is `runFState`; apply its result to a state and you
have the pair `tickF` returns. That pair is everything about `tickF` you can
observe. Which operations it performed on the way, and in what order, is not
recoverable.

But that is a fact about `tickF`, not about the method. Left
*polymorphic*, the same source text can be handed to several instances, because an implementation
that counts `put`s is simply another instance of the class:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:Counting}}
```

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:Prog}}
```

```
> runIt threeTicks
((2,3),(0,3))
```

One value, interpreted at `FState` and then at a counting instance. This is
the *tagless-final* encoding, and for the purpose of interpretation it is as
expressive as the free monad we are about to build. Indeed the two are
interconvertible: `Free e` is itself an instance of the class, so reifying a
polymorphic computation produces the term, and `interpret` turns a term back into
any instance.

So the difference is not *whether* a computation admits several meanings. It
is what you have in hand. With the type class, every meaning has to be
arranged in advance as an instance, each one re-elaborates the source text,
and there is no value denoting "the part not yet run". With a free monad the
computation is *data* from the start — a tree of constructors, with functions
at the branches: analyses are ordinary functions on data needing no instance,
a half-run computation is a value that can be stored and resumed, rewriting a
computation yields another computation, and an operation can carry a
subcomputation as data. Those four are what the rest of this chapter is
about.

The second way keeps the structure.

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
sees fairly wide use among functional programmers. It is the second of
the two methods of abstracting over effects mentioned above: rather than
making the source text polymorphic in the monad, we make the computation a
piece of *data* that several interpreters can consume.

The actual *definition* of the general concept of a free monad is
rather abstract, and may be skimmed on a first reading. The *use* of a
free monad is somewhat more concrete, and is more important for the
course. We therefore build one free monad in full first — the free *state*
monad — and only afterwards abstract from it.

### The Free State Monad, Directly

Before giving the general construction, let us build one instance of it by
hand, for state. The construction is short, every step is forced, and the
general definition will then be a matter of noticing what the steps have in
common.

Start from *any* monad with `get` and `put` — that is, from any instance of
the class of the previous section. Give each operation its continuation as an
extra argument:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:putk_getk}}
```

Both directions relate the continuation-passing forms to the originals:

```Haskell
put s = putk s return       put s >>= m  =  putk s m
get   = getk return         get   >>= m  =  getk m
```

The left-hand column is the right unit law `m >>= return == m`, so this uses
no property of the implementation beyond the monad laws. The point of the exercise is the
right-hand column: in a computation built from `getk` and `putk` alone, every
`>>=` has a *primitive operation* on its left, and the continuation-passing
operation absorbs it. Sequencing is no longer needed as a separate operation;
it has been pushed into the operations themselves.

Now drop the constraint and make `getk` and `putk` *uninterpreted*: instead
of functions that mean something in some instance, take them as constructors
of a type `M` that merely record that the operation was requested. Their
types are

```Haskell
Getk :: (s -> M a) -> M a
Putk :: s -> M a -> M a
```

where `() -> M a` has been collapsed to `M a`, since a function from the unit
type carries no more information than its result. But `M` is no longer an
unknown monad: it is exactly the type these two constructors build. Name it,
and add the one further constructor the two above cannot supply:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:FSM}}
```

`Return` is forced. `Getk` and `Putk` can only extend a computation — each takes
an `FSM s a` and produces one — so neither can start one. `Return` is where
the `return` of the equations above went.

~~~admonish note
`FSM s a` occurs in the argument of `Getk` only to the *right* of the arrow,
in `s -> FSM s a`. A type occurring to the left of an arrow in its own
declaration would not be well founded; this one is fine.
~~~

Because every operation now carries its continuation, `>>=` is no longer a
primitive at all: it is an ordinary recursive function over the three
constructors, which walks to the `Return` leaves and grafts `f` on.

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:FSM_monad}}
```

The `Getk` case is Kleisli composition: `k >=> f` is `\s -> k s >>= f`, the
continuation `k` followed by `f`.

Note that `fmap` and `<*>` are defined *from* `>>=`, rather than
independently of it. That is the right dependency order here: `>>=` is the
operation the construction is about, and the other two are consequences.

Making `FSM s` a state monad is then three lines, and the interpreter is the
obvious recursion:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:FSM_state}}
```

We can check that the continuation-passing equations we started from come
back, by calculation:

```Haskell
get >>= f    ==  Getk Return >>= f
             ==  Getk (\s -> Return s >>= f)
             ==  Getk f

put s >>= f  ==  Putk s (Return ()) >>= f
             ==  Putk s (Return () >>= f)
             ==  Putk s (f ())
```

This is a third instance of `StateMonad`, so `tick`, `push`, `pop` and
`stackExample` run over it without change — but now they build a data
structure rather than a function:

```
> runFSM 0 (tick :: FSM Int Int)
(0,1)
> runFSM [] (stackExample :: FSM [Int] (Maybe Int))
(Just 8,[])
```

### Abstracting the Recursive Occurrences

`FSM` is the free state monad, and nothing above mentioned any other effect.
To get the general construction, look at what is specific to state and what
is not.

Write `X` for `FSM s a` and list the argument types of the three
constructors:

```Haskell
Return :: a         -> X
Getk   :: (s -> X)  -> X
Putk   :: s -> X    -> X
```

so that, as a type equation,

```Haskell
X  ==  a  +  (s -> X)  +  (s, X)
```

The first summand is the `Return` case and mentions no `X`. The other two are
the operations, and each mentions `X` in exactly one place: under `s ->` for
`Getk`, directly for `Putk`. Those are the positions where *the rest of the
computation* goes.

Abstract them. Replace that occurrence of `X` by a parameter, and collect the
two operation summands into a type of their own:

```Haskell
{{#include ../haskell/Week4/Free.hs:StateOp}}
```

Then the equation reads

```Haskell
X  ==  a  +  StateOp s X
```

`StateOp s r` describes *one layer* of the syntax: a single operation, with
`r` marking the holes that its subcomputations occupy. Only the *argument*
occurrences of `X` are abstracted; the result occurrence is what makes these
constructors of `X` in the first place, and it is restored by the recursion
in the next section.

~~~admonish warning title="Two different `a`s"
The parameter of `StateOp` is conventionally written `a`, which collides with
the `a` of `FSM s a`, and they are not the same thing. In `FSM s a` — and in
`Free e a` below — the `a` is the *return type of the computation*. In
`StateOp s a` it is the *continuation slot*. The two coincide only once the
recursion is tied, when the slot is filled with the recursive type itself.
Reading `StateGet (s -> a)` as `s -> `*`rest of computation`* is what makes the
definition stop looking arbitrary.

This also answers the obvious question about step one: what became of the `M`
in `getk :: (s -> M a) -> M a`? Twice over. As a constructor, `M a` became
the recursive type itself, `FSM s a`; abstracting its argument occurrence
then turned it into the parameter.
~~~

### Defining `Free`

Tying the recursion back up gives the general definition. We define a monad
`Free e a` that represents a computation producing a value of type `a` (like
in `IO a`), where `e` is the layer functor describing the possible effects —
`StateOp s` in the development above, and anything else of that shape
elsewhere. The definition is as follows[^church]:

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
with `ReadOp` above). Note that the two `fmap`s we use are on different
types: the outermost one uses the `Functor` instance for `e`, and the
innermost one uses the `Functor` instance for `Free e` (recursively).

~~~admonish note title="Where the `Functor` requirement comes from"
It is worth seeing that this requirement is not a technicality invented to
make `Free` typecheck. Look again at `FSM`'s `>>=`:

```Haskell
Getk k   >>= f = Getk (\s -> k s >>= f)
Putk s m >>= f = Putk s (m >>= f)
```

Both equations do the same thing: apply `(>>= f)` to every subcomputation sitting
inside the operation, and leave the operation's own data alone. Once the
operation has been abstracted into a layer, that is exactly what `fmap` for
that layer does. So `Functor e` is `FSM`'s bind, factored out and named — and
that is also why the `Functor` instances in this chapter are forced rather
than chosen.
~~~

We then move on to defining an `Applicative` instance for `Free`.
The unit is simply the `Pure` constructor. (Haskell requires the definition
to be given as `Applicative`'s `pure`; it is the same function as `return`,
which is what we write everywhere else.) For `<*>` we follow the same two
cases as `fmap`: a `Pure` function can be applied straight away, and a `Free`
layer is descended into with the layer's own `fmap`.

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
However, since we require `e` to be a `Functor`, we can apply a function to
each `Free e a` sitting inside the `e`. The function we need has type
`Free e a -> Free e b`, and it is the recursive invocation `(>>= f)` itself,
so the whole `Free` case is `Free (fmap (>>= f) g)`.

Instantiating `e` to `StateOp s` recovers the type we built by hand at the
start of this section. The correspondence is constructor by constructor, and
both directions are in the repository:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:FSM_iso}}
```

### The Recipe for Other Effects

The two steps above — pass the continuation, then abstract the recursive
occurrence — work for any operation, not just `get` and `put`. Given

```Haskell
op  :: T1 -> ... -> Tn -> M R
```

passing the continuation gives

```Haskell
opk :: T1 -> ... -> Tn -> (R -> M a) -> M a
```

and abstracting `M a` to a parameter gives the constructor

```Haskell
Op  :: T1 -> ... -> Tn -> (R -> a) -> F a
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
a computation, such as `catch`. We return to that in the section on error
handling below, and it is the shape that most of the interesting effects
turn out to have.

### Implementing `State` in Terms of `Free`

We already have `StateOp` from the development at the start of this section.
All that remains is to make it a `Functor` — which, by the recipe above, is
forced — and to apply `Free` to it. We call the result `FreeState`, to keep
it apart from the `State` of chapter 2 and the `FState` above; all three
implement the same interface.

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

Evaluation of a `FreeState` computation is also very similar to the case
for `Reader`, and takes the form of a recursive function that
interprets the `StateOp` effects. When we encounter a `StatePut`, we
discard the current state and use the provided one.

```Haskell
{{#include ../haskell/Week4/Free.hs:runState}}
```

Finally, we can define the usual `put`/`get` accessor functions.

```Haskell
{{#include ../haskell/Week4/Free.hs:put_get}}
```

On top of these, we can define the usual helper functions, such as
`modify`, in the way discussed in chapter 2 — or simply reuse the `modify`
we already wrote against the `StateMonad` interface, since `FreeState` is
an instance of it.

### Interpreting into Any Monad

`runState` above interprets a `FreeState` computation directly into a Haskell
function. But notice how little of it was about *state*: the `Pure` case and
the recursion are the same in every interpreter we have written. Only the
treatment of the individual operations differs.

We can make that precise. An *interpretation* of an effect functor `e` in a
monad `m` is a function

```Haskell
h :: forall x. e x -> m x
```

It says what a single operation means, and says nothing whatsoever about
sequencing. The `forall x` matters: `h` must work for every result type, so
it cannot inspect the continuation — it can only hand it back. (Such a
uniform family of functions is called a *natural transformation*.)

From any such `h` we get an interpreter for whole computations, mechanically:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:interpret}}
```

The `Free` case is where the `join` presentation of a monad earns its keep.
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

Going through `interpret` and then through `FState` builds an intermediate
closure for every operation. Nothing stops us from writing the composite
directly — and in fact we already did, several pages ago. Compare

```Haskell
{{#include ../haskell/Week4/Free.hs:runState}}
```

with `runFreeStateF`: up to the order of the arguments they are the same
function, the second being the first with `interpret` and the `FState`
instance inlined and the intermediate closures fused away. The hand-written
interpreter was never a different technique; it was the general one,
specialised.

Being able to write either — the compositional one for clarity, the fused
one for speed, and to check them against each other — is one of the
practical benefits of having the computation as data.

### Computations as Data

Interpreting is not the only thing we can do with a `Free e a`. It is an
ordinary value, so we can also *inspect* it and *rewrite* it.

Inspecting means walking the structure without running it. How far we get
depends on the effects. `StatePut s m` carries its continuation directly, so
we can walk past it; `StateGet k` carries a *function*, and to get past it we
would have to choose an `s` and apply `k` to it — and different choices give
different computations. So a static count of the writes a computation performs is
available only until the first read:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:countPuts}}
```

This is the general limit on inspecting free-monad computations, and it is
worth being precise about it: what you can see depends on how much of the
computation
was made *first-order*. Every operation that hands back a Haskell function
is a point past which analysis cannot go on its own.

Rewriting means transforming the operations while leaving the sequencing
alone:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:modifyEffects}}
```

This is the function you will use in the week 4 exercises to implement local
environments: rewrite every read of the environment so that it sees a
modified one, and leave every other operation as it is.

~~~admonish warning
`modifyEffects` walks the *spine* of the computation. If an operation's payload
is itself a `Free` value — as `ErrorCatch` will be in the next section, and
as most of the interesting effects in assignment 4 are — then the rewriting
function must rebuild that payload explicitly. Leave it out and the rewrite compiles, runs, and silently
does nothing to the part of the computation you cared about.
~~~

### What Makes `Free` Free

We can now say what "free" means, and it is worth saying, because it
explains why this construction keeps working.

`Free e` is the monad generated by `e` and *nothing else*. Concretely: for
every monad `m` and every interpretation `h :: forall x. e x -> m x`, there
is exactly one function `interpret h :: Free e a -> m a` that respects
`return` and `>>=`. Existence means you never have to invent sequencing
behaviour — writing the interpreter for the operations is the *whole* job.
Uniqueness means there is nothing else it could have been: once you have
fixed what `get` and `put` do, the meaning of every computation built from them
is determined.

That is also the precise sense in which `Free e` imposes no laws. `FState`
satisfies `put s >> put s' == put s'`; the free state monad does not, because
in it those two computations are visibly different values — a two-`StatePut`
chain and a one-`StatePut` chain. The equation only becomes true after
interpretation. So `Free (StateOp s)` is not *the* state monad; it is the
syntax of state operations, from which the state monad is one quotient among
many. Keeping the syntax around is exactly what lets us interpret it in
several ways, and what lets `memoFibM` below do something no lawful state
monad would.

~~~admonish warning title="The cost of freedom"
The representation we use here has a performance trap. In `m >>= f` with `m
= Free g`, the bind is pushed *through* the whole of `m` by `fmap`. So a
left-nested chain `((m1 >>= f1) >>= f2) >>= ...` re-traverses its prefix at
every step, and building an `n`-operation computation that way takes time quadratic in
`n`. Right-nested chains — what `do` notation produces from
straight-line code — are fine, which is why this rarely bites in practice.

Recursive functions that append are the usual way to hit it. Fixes are
known: continuation-passing (Church) encodings, as in
[`Control.Monad.Free.Church`](https://hackage.haskell.org/package/free-5.2/docs/Control-Monad-Free-Church.html),
make `>>=` constant-time but destroy the very inspectability we came for;
representing the continuation as a type-aligned queue keeps both. Neither is
needed for this course.
~~~

### Implementing an Error Monad

As another example, consider a free monad with error handling, similar to that
provided by `Either`. The main thing we will find is that it is easy to define a
free effect that is law-abiding and intuitively appears correct, but exhibits
unexpected behaviour. We will need to introduce a new language construct to fix
it.

We define the `ErrorOp` effect as follows, which is hopefully not hard to
understand:

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
k _ = throw "E2"

> runError ((return 1) `catch` (\_ -> pure 99) >>= k)
Right 99
```

We could expect this to always throw an error, since that is what `k` does, but
the exception handler `(\_ -> pure 99)` is also applied to the continuation `k`.
The behaviour we have here is not wrong from a type-class perspective, but it is
certainly not like most `catch` constructs in real programming language. It
behaves more like installing an exception handler that is dynamically active
*for the entire rest of the computation*, which is certainly a feature that may
be meaningful, but is not really what we intended.

To fix this, we need to define an `ErrorOp e a` where the "action to perform in
case of error" is *not* a continuation as far as `Free` is concerned. This means
it must have a different type than `a`, or else we cannot define the `Functor`
instance, as that must apply the given function to *every* `a` in order to be
lawful.

The solution is to use *existential quantification* to bind a type parameter `x`
that is known *only* inside the `ErrorCatch` constructor, and then have express
the `ErrorCatch` constructor as containing three values:

* The immediate action to perform, of type `ErrorM e x`.

* The action to perform in case of error, of type `e -> ErrorM e x`.

* The continuation, of type `x -> a`.

In Haskell it is written in this way:

```Haskell
{{#include ../haskell/Week4/Free.hs:Error}}
```

The type variable `x` occurs only in the arguments of the constructor, not in
its result, so it is existentially quantified; this is why `ErrorOp` uses
existential quantification.

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
careful thought to the consequences when it occures. This issue occurs for many
interesting effects: transactions, retries, timeouts, resource scoping, loops
you can break out of. Using existential quantification is a common solution to
this problem, although it can also be done by requiring the type of the
"intermediate result" (`x`) to have a *specific* type, rather than be
monomorphic. This is what you will eventually do in A4.

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
main = putStrLn "hello world" >>= \_ -> return ()

-- or equivalently

main :: IO ()
main = do putStrLn "hello world"
          return ()
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
        then return FileNotFound
        else return $ CouldNotRead $ show e
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
        return 42
  return (div 1 0) `catch` handler
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

An expression `evaluate x` is much like `return x`, but evaluates its
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
        return 42
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

### An Imperative State Monad

We promised earlier a further implementation of the `StateMonad` interface.
With `IORef` in hand we can give it: keep the state in a mutable cell, and
let a computation be a function from that cell to an `IO` action.

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:IState}}
```

The instances are unremarkable — all the work is done by `IO`:

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:IState_instances}}
```

This is the implementation an imperative programmer would have written
first, and it is genuinely different from `FState`: it mutates one cell
rather than threading a value. Yet it satisfies the same laws, and

```Haskell
tickI :: IState Int Int
tickI = tick
```

is the same `tick` we ran with `FState`, at a different type.

~~~admonish note
`IState` is not more efficient than `FState` in any deep sense — GHC is
perfectly good at compiling state threading — but it is the shape you are
forced into when the state lives outside the program: a file, a database, a
device. Assignment 4 has you do exactly that, with the key-value store held
in a file.
~~~

### A State Monad from an Object

`IState` passes the reference to every operation. It is worth asking whether
the reference could instead be *held*, as the private field of an object is
held, with `get` and `put` as its methods. It can, and the result is a fourth
implementation of the same interface.

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

Read the three definitions in order. `return a` *discards* the object, which
is exactly why it performs no side effects. `m >>= f` hands the *same* object
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

The example of free monads we saw above are perhaps a bit contrived,
as they merely involved replicating existing monads. In practice, we
often use free monads to abstract over complicated effects, typically
those in IO. Let us look at some use cases. By the constraints of
these notes, they will still be somewhat contrived (we can't fit an
actual production system here), but they will be more interesting than
spelling `State` in a new way.

### One Computation, Four Implementations

Before moving on, it is worth collecting what we now have. The single
definition

```Haskell
{{#include ../haskell/Week4/StateMonads.hs:tick}}
```

mentions no implementation of state at all, and we can run it four ways:

```
> runFState tickF 0                -- pure state threading
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

The difference between the two methods only becomes visible when you want to
do something with the computation other than run it. `tickF` is a function:
the only thing to be done with it is to pass it to `runFState`, and the pair
that comes back is everything observable — which operations ran, and in what
order, cannot be recovered from it. `tickFree` is a value you can pattern match
on, store in a list, count the operations of, interpret twice and compare,
or — as we are about to do — interpret in a way that *no* implementation of
the `StateMonad` interface could have provided, because it does something
the interface never mentions.

### An Uncontrived Real World Example

To start out with, let us consider one of the most interesting and
useful functions, the recursive Fibonacci function:

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
our `fib` function; currently restricted to merely logging.

```Haskell
data FibOp a = FibLog String a

instance Functor FibOp where
  fmap f (FibLog s c) = FibLog s (f c)
```

Now we can define a `FibM` monad that supports `FibOp` effects, with
an accessor function `fibLog`:

```Haskell
type FibM a = Free FibOp a

fibLog :: String -> FibM ()
fibLog s = Free (FibLog s (Pure ()))
```

And finally we can use it in our definition of `fib`:

```Haskell
fib :: Int -> FibM Int
fib 0 = return 1
fib 1 = return 1
fib n = do
  fibLog ("fib(" ++ show n ++ ")")
  x <- fib (n - 1)
  y <- fib (n - 2)
  return (x + y)
```

One of the interesting parts of the `FibM` monad is that there are
many legitimate and interesting ways to interpret it (in contrast to
`Reader` or `State`, which have only a single sensible
interpretation). One obvious one is to interpret it in the `IO` monad,
where the logging messages are printed as lines:

```Haskell
ioFibM :: FibM a -> IO a
ioFibM (Pure x) = return x
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
monads, but it is a very easy mistake to make in an interpreter.
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
  fmap f (FibLog s c)    = FibLog s (f c)
  fmap f (FibMemo n m c) = FibMemo n m (f . c)

fibMemo :: Int -> FibM Int -> FibM Int
fibMemo n m = Free (FibMemo n m Pure)
```

The idea behind `fibMemo` is that we use it to wrap a computation, for
example like so:

```Haskell
fib :: Int -> FibM Int
fib 0 = return 1
fib 1 = return 1
fib n = fibMemo n (do
  fibLog ("fib(" ++ show n ++ ")")
  x <- fib (n - 1)
  y <- fib (n - 2)
  return (x + y))
```

The operational idea is that whenever the `fib n` case is reached, we
want to look if an existing result for `n` has already been computed.
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

Another useful change would be to allow memoisation of arguments and results
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

To support his style of programming, we define an effect type for our
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
accessor functions `waitFor` and `logMsg`:

```Haskell
{{#include ../haskell/Week4/FreeAsync.hs:waitFor_logMsg}}
```

Here are three examples of how to use the monad. All of these
functions listen for events and do something (fairly trivial) with the
result. The purpose of the `divider` example is solely to illustrate
that control flow and looping is possible.

```Haskell
{{#include ../haskell/Week4/FreeAsync.hs:processes}}
```

Once you have finished with this section, and you have seen how the
sausage is made, we suggest returning to these definitions and note
how *normal* they look. The complexity of how they are actually
executed is completely hidden by the monad abstraction.

A definition such as `adder` represents a *process*. It runs for as
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
not an instance of `Show`, but we can be pretty sure it is currently
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
execution. The interesting thing is that the suspended computations,
the `a`, `b`, and `c` values above, are ordinary Haskell values, that
we can manipulate like any other Haskell value. One slightly dubious
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

Another more useful thing we can do is to keep *multiple* suspended
processes in a list. Whenever an event arrives, we crank the pump once
on each of them, and `deliver` leaves alone every process that is not
waiting for this particular event — including one that has already
finished, which is a `Pure` value that no event matches. So the whole
pump is one recursive function over the list of events:

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
interactively from the console, through the the `readLn` function
which reads a value in Haskell syntax.

```Haskell
{{#include ../haskell/Week4/FreeAsync.hs:interactivelyRunEventM}}
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
