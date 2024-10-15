# Structured Concurrent Programming

## Concurrency with Free Monads

In Chapter 4 we saw an example of [Asynchronous Programming with Free
Monads](chapter_4.html#asynchronous-programming-with-free-monads). The
principles underlying asynchronous programming are quite similar to
those of concurrent programming. We can actually express a concurrent
system using a free monad, and then interpret the system using either
true IO-based concurrency, or simulation. This raises the potential of
using the simulation to systematically (or probabilistically) explore
the potential execution paths possible in the system, in order to
discover race conditions.

In the following we will design a concurrency abstraction that is
rather similar to the one described in Chapter 5, based on threads,
channels, and messages. One concession we will make for simplicity is
that our channels will be *monomorphic*, and capable of sending only a
single type of messages, for which we pick `String`:

```Haskell
type Msg = String
```

This restriction can be lifted, but requires a significant amount of
Haskell type-level trickery, which is not the point of this chapter.

### An Initial Attempt

Let us now define a data type `CCOp` for representing concurrency
effects. We allow four effects: forking a thread, creating a channel,
sending a message on a channel, and receiving a message from a
channel. We also instantiate the `Free` monad with the `CCOp` effect
to produce `CC`, a monadic representation of concurrency:

```Haskell
data CCOp a
  = CCFork (CC ()) a
  | CCNewChan (Chan Msg -> a)
  | CCSend (Chan Msg) Msg a
  | CCReceive (Chan Msg) (Msg -> a)

type CC a = Free CCOp a
```

Note that we are using the `Control.Concurrent.Chan` type as our
channel representation - we will change that later. We must of course
also define the usual `Functor` instance for `CCOp`:

```Haskell
instance Functor CCOp where
  fmap f (CCFork m c) = CCFork m (f c)
  fmap f (CCNewChan c) = CCNewChan $ f . c
  fmap f (CCSend chan msg c) = CCSend chan msg $ f c
  fmap f (CCReceive chan c) = CCReceive chan $ f . c
```

And finally we define accessor functions for constructing monadic
operations with these effects:

```Haskell
ccNewChan :: CC (Chan Msg)
ccNewChan = Free $ CCNewChan pure

ccFork :: CC () -> CC ()
ccFork m = Free $ CCFork m $ pure ()

ccSend :: Chan Msg -> Msg -> CC ()
ccSend chan msg = Free $ CCSend chan msg $ pure ()

ccReceive :: Chan Msg -> CC Msg
ccReceive chan = Free $ CCReceive chan pure
```

Interpreting `CC` computations in `IO` is quite straightforward, due
to how closely our effects match the interface provided by
`Control.Concurrency`:

```Haskell
interpCCIO :: CC (Chan Msg) a -> IO a
interpCCIO (Pure x) =
  pure x
interpCCIO (Free (CCFork m c)) = do
  _ <- forkIO $ interpCCIO m
  interpCCIO c
interpCCIO (Free (CCNewChan c)) = do
  chan <- newChan
  interpCCIO $ c chan
interpCCIO (Free (CCSend chan msg c)) = do
  writeChan chan msg
  interpCCIO c
interpCCIO (Free (CCReceive chan c)) = do
  msg <- readChan chan
  interpCCIO $ c msg
```

And now we can write a contrived little program that passes a message
through a chain of threads, each adding a token to the message and
passing it to the next process:

```Haskell
pipeline :: CC String
pipeline = do
  chan_0 <- ccNewChan
  chan_1 <- ccNewChan
  chan_2 <- ccNewChan
  chan_3 <- ccNewChan
  chan_4 <- ccNewChan
  let passOn tok from to = do
        x <- ccReceive from
        ccSend to $ x ++ tok
  ccFork $ passOn "a" chan_0 chan_1
  ccFork $ passOn "b" chan_1 chan_2
  ccFork $ passOn "c" chan_2 chan_3
  ccFork $ passOn "d" chan_3 chan_4
  ccSend chan_0 ""
  ccReceive chan_4
```

Running it yields the expected results:

```Haskell
> interpCCIO pipeline
"abcd"
```

### Abstract Channels

Unfortunately, the definition of concurrent effects given above is not
suitable for simulation. The reason is that the we specified that a
channel is always of type `Chan Msg`, meaning it is intrinsically tied
to the interface provided by `Control.Concurrent`. In order to allow
multiple interpretations of concurrency, we need to make `CCOp`
*polymorphic* in its representation of channel. To this end, we add a
type parameter `chan`, which we use instead of `Chan Msg`:

```Haskell
data CCOp chan a
  = CCFork (CC chan ()) a
  | CCNewChan (chan -> a)
  | CCSend chan Msg a
  | CCReceive chan (Msg -> a)

instance Functor (CCOp chan) where
  fmap f (CCFork m c) = CCFork m (f c)
  fmap f (CCNewChan c) = CCNewChan $ f . c
  fmap f (CCSend chan msg c) = CCSend chan msg $ f c
  fmap f (CCReceive chan c) = CCReceive chan $ f . c
```

In our definition of the `CC` type alias, we also add `chan` as a type
parameter:

```Haskell
type CC chan a = Free (CCOp chan) a
```

Now a value of type `CC chan a` represents a concurrent execution that
produces a value of type `a`, and uses a channel representation
`chan`. When we actually write computations in `CC`, we will leave
`chan` polymorphic - only the interpretation functions will impose
constraints on it. This sounds a bit abstract (because it is), but
will become clear later. First, however, we have to redefine the
accessor functions, which will also have to use a `chan` type
parameter:

```Haskell
ccNewChan :: CC chan chan
ccNewChan = Free $ CCNewChan pure

ccFork :: CC chan () -> CC chan ()
ccFork m = Free $ CCFork m $ pure ()

ccSend :: chan -> Msg -> CC chan ()
ccSend chan msg = Free $ CCSend chan msg $ pure ()

ccReceive :: chan -> CC chan Msg
ccReceive chan = Free $ CCReceive chan pure
```

Now we can make the `pipeline` example work with our new definition.
This is quite straightforward - in fact, we only have to change the
type, and the definition can be unchanged:

```Haskell
pipeline :: CC chan String
pipeline = ...
```

A similar situation arises for the interpretation function
`interpCCIO`. Here we simply require that the channel representation
is `Chan Msg`, but otherwise the implementation is the same:

```Haskell
interpCCIO :: CC (Chan Msg) a -> IO a
interpCCIO = ...
```

Now consider what happens when we run the example:

```Haskell
> interpCCIO pipeline
"abcd"
```

This type checks because `pipeline` has the polymorphic type `CC chan
String` where `chan` can be instantiated with any type, and in
particular it can be instantiate with `Chan Msg` - which is what
`interpCCIO` requires. In this way we can write generic code that
delays the concrete choice of channel representation. Let us now
exploit this to actually write a pure interpreter for `CC`.

### A Pure Interpreter

The pure interpreter will more complicated than `interpCCIO`, because
we cannot piggyback on the existing Haskell runtime system for
concurrency. Our approach will essentially be that of a state monad,
where we maintain the following main bits of state:

* A collection of all channels and the messages they currently
  contain.

* A collection of all threads that can be executed further.

Since channels have a notion of identity, we need a way to uniquely
identify them, which we will do with the `ChanId` type:

```Haskell
type ChanId = Int
```

Each channel will be associated with a unique integer. This means we
also need to have a source of fresh integers, which we will accomplish
by maintaining a counter in our state.

Now we are ready to define a Haskell type encapsulating our
concurrency simulator state:

```Haskell
data CCState = CCState
  { ccCounter :: ChanId,
    ccChans :: [(ChanId, [Msg])],
    ccThreads :: [CC ChanId ()]
  }
```

The `ccChans` field stores all existing channels, keyed by a `ChanId`,
with each channel storing a list of messages with the oldest first.
The `ccThreads` stores suspended threads, which are represented as
monadic computations of type `CC ChanId ()`.

The actual monad we will use is the `State` monad from [Chapter
2](chapter_2.html) with `CCState` as the state. When programming with
state monads it is usually a good idea to define higher-level utility
functions rather than using `get`/`put` directly. First we define a
function `getChan` that retrieves the messages associated with a
specific channel:

```Haskell
getChan :: ChanId -> State CCState [Msg]
getChan chan_id = do
  state <- get
  pure $
    fromMaybe (error "unknown channel") $
      lookup chan_id $
        ccChans state
```

And its counterpart, `setChan`, that sets the messages associated with
a channel.

```Haskell
setChan :: ChanId -> [Msg] -> State CCState ()
setChan chan_id msgs = do
  state <- get
  put $
    state
      { ccChans =
          (chan_id, msgs)
            : filter ((/= chan_id) . fst) (ccChans state)
      }
```

It is not difficult to imagine how we will implement reading from a
channel: use `getChan` to fetch the inbox, remove the first message,
and use `setChan` to put back the remainder. The tricky part is how to
handle the situation when no messages are available, but we will come
back to that.

We also need to be able to add threads to the state, which is done by
`addThread`.

```Haskell
addThread :: CC ChanId () -> State CCState ()
addThread m = do
  state <- get
  put $ state {ccThreads = m : ccThreads state}
```

And finally, `incCounter` increments the counter in the state and
returns the old value. It serves as our mechanism for obtaining fresh
`ChanId`s.

```Haskell
incCounter :: State CCState ChanId
incCounter = do
  state <- get
  put $ state {ccCounter = ccCounter state + 1}
  pure $ ccCounter state
```

We can now define a function `step` that evaluates a `CC Int ()`
computation *as far as possible*, meaning until it blocks or
terminates. Remember that the only way a thread can block in our
system is by trying to read from an empty channel. The `step` function
is *not* a full interpretation function, but we will use it to build
one.

The `step` function has the following type:

```Haskell
step :: CC Int a -> State CCState (CC ChanId a)
```

Note that it returns a `CC ChanId a` because there is no guarantee that
it is able to run the computation to completion (which would produce
an `a`). The simplest case is the one for `Pure`, which represents a
finished computation for which there is nothing further to do:

```Haskell
step (Pure x) = pure $ Pure x
```

Now we need to handle the various effects. Creating a channel is done
by retrieving an unused `ChanId`, then adding a channel with an
initially empty message queue:

```Haskell
step (Free (CCNewChan c)) = do
  chan_id <- incCounter
  setChan chan_id []
  step $ c chan_id
```

Forking a thread simply adds the computation to the state with
`addThread`:

```Haskell
step (Free (CCFork m c)) = do
  addThread m
  step c
```

The `CCSend` effect is executed by appending the given message to the
specified channel, then executing the continuation:

```Haskell
step (Free (CCSend chan_id msg c)) = do
  msgs <- getChan chan_id
  setChan chan_id $ msgs ++ [msg]
  step c
```

Finally, the most interesting effect is `CCReceive`, because it can
block when the channel is empty. This is represented by simply
returning the monadic computation unchanged:

```Haskell
step (Free (CCReceive chan_id c)) = do
  msgs <- getChan chan_id
  case msgs of
    [] -> pure $ Free $ CCReceive chan_id c
    msg : msgs' -> do
      setChan chan_id msgs'
      step $ c msg
```

We now have a `step` function for running as much of a single
computation (i.e., thread!) as possible. But if `step` is stuck on a
`CCReceive`, then no amount of re-running `step` is going to make
progress - rather, some other thread must be given a chance to run,
which may end up putting a message in the queue of some channel that
the original thread was stuck on. To accomplish this, the function
`stepThreads` invokes `step` on every thread in the system. First we
write an incorrect implementation:

```Haskell
-- BEWARE: WRONG!
stepThreads :: State CCState ()
stepThreads = do
  state <- get
  threads <- mapM step $ ccThreads state
  put $ state {ccThreads = threads}
```

At first glance, this may look right: fetch all the threads, advance
them a step, then put them back in the state. But that final `put` is
a problem, because it effectively reverts any state modifications done
inside `step`. We can attempt a fix:


```Haskell
-- BEWARE: STILL WRONG!
stepThreads :: State CCState ()
stepThreads = do
  state <- get
  threads <- mapM step $ ccThreads state
  new_state <- get
  put $ new_state {ccThreads = threads}
```

This is still wrong. While we now maintain *some* state modifications
done in `step`, we completely overwrite the list of threads. This
means that if `step` creates any new threads, they are thrown away.
Let us try again:

```Haskell
-- BEWARE: STILL WRONG!
stepThreads :: State CCState ()
stepThreads = do
  state <- get
  threads <- mapM step $ ccThreads state
  new_state <- get
  put $ new_state {ccThreads = threads ++ ccThreads new_state}
```

Instead of overwriting the list of threads, we not simply prepend to
it. But now we end up *duplicating* the threads, since the original
threads from `state` (the ones we pass to `step`) are still present in
`new_state`. A correct solution requires is to *remove* the threads
from the state before we step them:

```Haskell
stepThreads :: State CCState ()
stepThreads = do
  state <- get
  put $ state {ccThreads = []}
  threads <- mapM step $ ccThreads state
  new_state <- get
  put $ new_state {ccThreads = threads ++ ccThreads new_state}
```

The point of this progression through incorrect implementations of
`stepThreads` was not to demonstrate how to write incorrect code, but
as an illustration of the subtleties of working with mutable state.

Now we can write a function that evaluates a "main thread" (the one
producing the ultimate execution result), but first uses `stepThreads`
to advance all "background threads".

```Haskell
interp :: CC ChanId a -> State CCState a
interp (Pure x) = pure x
interp (Free op) = do
  stepThreads
  op' <- step $ Free op
  interp op'
```

As soon as the "main thread" reaches the `Pure` constructor, the
system is considered terminated. This is different from
`Control.Concurrency`, which has no notion of a "main thread", and
where forked threads can continue to do arbitrary side effects
forever.

The final bit of machinery we need is is a bit of boilerplate for
running our state monad with an initial state, and projecting out the
result we care about:

```Haskell
interpCCPure :: CC ChanId a -> a
interpCCPure orig =
  fst $ runState initial_state $ interp orig
  where
    initial_state =
      CCState
        { ccCounter = 0,
          ccChans = [],
          ccThreads = []
        }
```

And we can see that things work:

```
> interpCCPure pipeline
"abcd"
```

One limitation of this approach, which is not present in `interpCCIO`,
is that we cannot handle infinite loops in *pure* code. We are only
able to switch between threads when `step` encounters an effect.
