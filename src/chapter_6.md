# Concurrent Programming

In this chapter we will discuss concurrent programming in Haskell.
There are many ways to write concurrent programs in Haskell; ranging
all the way from almost implicit concurrency, to classic stateful
concurrency with threads and locks (and deadlocks, and race
conditions).

The approach we take to concurrent programming in AP is based on the
paradigm of *message passing*, and is heavily inspired by languages
such as Erlang. The idea is to structure concurrent programs as
threads that communicate with each other by sending asynchronous
*messages*. Each thread maintains its own private state, and threads
cannot directly modify the state of other threads, except by sending
messages. A server that receives and send messages is often called a
*server*.

Although the concurrent systems we will construct in AP will run only
on a single machine (and within a single Haskell process), the
approach is perfectly suitable for distributed systems, as seen in for
example [Cloud Haskell](http://haskell-distributed.github.io/), or
languages such as Erlang and Elixir.

Concurrency is closely related (but not the same as) *parallelism*.
While multi-threading in Haskell is indeed one way to take advantage
of parallel multi-core computers, this is not an explicit aspect of
our study of concurrency. Instead, we focus on concurrency as a
programming model which happens to be convenient for expressing
certain forms of event-driven systems.

We use the Haskell modules
[Control.Concurrent](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Concurrent.html)
and
[Control.Concurrent.Chan](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Concurrent-Chan.html).
The latter is implicitly re-exported by the former. As always, we will
use only a fairly small subset of the facilities provided by the
Haskell standard library. You are welcome (and encouraged) to peruse
the documentation to enligteh yourself, but in this particular case
you should be careful not to be tempted by functions that subvert the
notion of message-passing.

## Threads and Channels

We create a thread by using the function `forkIO`, which accepts as
argument an `IO` action that will be executed in the new thread:

```Haskell
forkIO :: IO () -> IO ThreadId
```

The `ThreadId` that is returned is an opaque handle to the thread. We
can use it to perform a few low-level operations, such as killing the
thread or waiting for it to finish, but this will not be the main way
we interact with threads.

Instead, we will usually pass an `IO` action that is some kind of
potentially infinite loop that receives and handles messages. Messages
are not sent directly to threads, but instead over *channels*, which
are created with `newChan`.

```Haskell
newChan :: IO (Chan a)
```

The `newChan` action produces a channel that can be used for sending
and receiving messages of type `a`. The precise type of `a` will be
inferred by the compiler.

Whenever we create a thread, we will also create a channel through
which we can communicate with the thread, as shown in below (assuming
the existence of a function `threadLoop`):

```Haskell
do c <- newChan
   forkIO $ threadLoop c
   ...
```

Now, both the new thread and we have a reference to the channel (`c`).

Messages can be both read and written to a channel, corresponding to
receiving and sending messages, using the following two functions:

```Haskell
writeChan :: Chan a -> a -> IO ()

readChan :: Chan a -> IO a
```

Conceptually, a channel is an unbounded queue of messages. Writing to
a channel is an asynchronous operation - it immediately and always
succeeds. Reading from a channel retrieves the oldest message in the
channel. If the channel is empty, reading blocks until a message is
available.

**Single-reader principle:** we adopt the rule that a channel may have
only a *single* reader, meaning only a single thread is allowed to
call `readChan` on any given channel. This is typically the thread
that we created the channel for. This is not enforced by the Haskell
type system, and there are indeed forms of concurrent programming
that are more flexible, but they are outside the scope of this note.

It is perfectly acceptable (and often necessary) for a channel to have
multiple writers.

If we call `readChan` on a channel where we hold the only reference
(meaning we would in principle wait forever), the Haskell runtime
system will raise an exception that will cause the thread to be
terminated. This is a natural and safe way to shut down a thread that
is no longer necessary, assuming the thread does not hold resources
(e.g., open files) that must be manually closed. Handling such cases
is outside the scope of these notes.

## Remote procedure calls (RPC)

The Haskell message passing facility is asynchronous, meaning that
`writeChan` returns immediately with a unit value. This is suitable
when we are sending an order, or some information, to a remote server,
and have no interest in the result. However, in many cases we are in
fact interested in the response, either because we made a query for
information, or because we want to know whether the request failed.
Specifically, we want a way to perform *remote procedure calls*
(RPCs).

To implement RPC, we need to invent a bit of machinery on top of the
raw asynchronous message passing machinery. The way we make it work is
by creating a new channel that is used for transmitting the result.
This channel is then sent along as part of the message.

The first step is to make a type for the messages that will be send to
the server. Again, we use the pattern where we make constructor for
each kind of message, and the last argument for each constructor is a
channel for sending back the response:

```Haskell
data Msg = Incr     (Chan ())
         | Decr Int (Chan ())
         | GetValue (Chan Int)
```

Here we use the Haskell type `()` when no meaningful result is
expected.

Next, we declare a type alias `Counter` for representing a counter
server, here just the input channel, and a function for creating a new
counter server:

```Haskell
type Counter = Chan Msg

counter :: IO Counter
counter = do
  input <- newChan
  _ <- forkIO $ counterLoop input 0
  return input
```

We can define a function to abstract the communication pattern where
we send a message and then wait for an reply:

```haskell
requestReply :: Counter -> (Chan a -> Msg) -> IO a
requestReply cnt con = do
  reply_chan <- newChan
  writeChan cnt $ con reply_chan
  readChan reply_chan
```

Note that the second argument of the `requestReply` function is a
*function* that constructs a `Msg` value.

Now we can use the `requestReply` function to define the three API
functions `incr`, `decrWith` and `getValue` for a counter server:

```haskell
incr :: Counter -> IO ()
incr cnt = requestReply cnt Incr

decrWith :: Counter -> Int -> IO ()
decrWith cnt n = requestReply cnt $ Decr n

getValue :: Counter -> IO Int
getValue cnt = requestReply cnt GetValue
```

Finally, we define the internal server loop function:

```Haskell
counterLoop :: Chan Msg -> Int -> IO b
counterLoop input state = do
  msg <- readChan input
  case msg of
    Incr from -> do
      let (newState, res) = (state + 1, ())
      writeChan from res
      counterLoop input newState
    Decr n from -> do
      let (newState, res) = (state - n, ())
      writeChan from res
      counterLoop input newState
    GetValue from -> do
      let (newState, res) = (state, state)
      writeChan from res
      counterLoop input newState
```

## Timeouts

The channel abstraction does not directly support timeouts for RPC
calls. However, we can build our own support for timeouts. The
technique we employ is to allow the reply to be either the intended
value *or* a special timeout value. When we perform an RPC, we then
also launch a new thread that sleeps for some period of time, then
write the timeout value to the channel. If the non-timeout response is
the first to arrive, then the timeout value is ignored and harmless.

First we must import the `threadDelay` function.

```Haskell
import Control.Concurrent (threadDelay)
```

Then we define a type `Timeout` with a single value `Timeout`.

```Haskell
data Timeout = Timeout
```

Then we define a message type (in this case polymorphic in `a`) where
the reply channel accepts messages of type `Either Timeout a`.

```Haskell
data Msg a = MsgDoIt (Chan (Either Timeout a)) (IO a)
```

A `Msg a` denotes a request to perform some impure operation `IO a`
(perhaps a network request), then reply with the resulting value of
type `a`.

We can use this to build a facility for performing an action with a
timeout:

```Haskell
actionWithTimeout :: Int -> IO a -> IO (Either Timeout a)
actionWithTimeout seconds action = do
  reply_chan <- newChan
  _ <- forkIO $ do -- worker thread
    x <- action
    writeChan reply_chan $ Right x
  _ <- forkIO $ do -- timeout thread
    threadDelay (seconds * 1000000)
    writeChan reply_chan $ Left Timeout
  readChan reply_chan
```

You will note that this is not a server in the usual sense, as it does
not loop: it simply launches two threads.

One downside of this function is that the worker thread (the one that
runs `action`, and might take too long) is not terminated after the
timeout. This is a problem if it is, for example, stuck in an infinite
loop that consumes ever more memory. To fix this, we can have the
timeout thread explicitly kill the worker thread. First we have to
import the `killThread` function.

```Haskell
import Control.Concurrent (killThread)
```

Then we can use it as follows.

```Haskell
actionWithTimeout2 :: Int -> IO a -> IO (Either Timeout a)
actionWithTimeout2 seconds action = do
  reply_chan <- newChan
  worker_tid <- forkIO $ do
    -- worker thread
    x <- action
    writeChan reply_chan $ Right x
  _ <- forkIO $ do
    -- timeout thread
    threadDelay (seconds * 1000000)
    killThread worker_tid
    writeChan reply_chan $ Left Timeout
  readChan reply_chan
```

Note that killing a thread is a dangerous operation in general. It may
be the case that the worker thread is stuck in some loop or waiting
for a network request, in which case it is harmless, but killing it
may also leave some shared state in an unspecified state. We will
(hopefully) not encounter such cases in AP, but it is something to be
aware of in the future.
