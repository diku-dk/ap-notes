# Concurrent Programming

Haskell offers a wide range of facilities to support for different
styles of concurrent and parallel programming. In this chapter we focus
on a limited set of primitives for *concurrent programming* which can
often be found in similar form in other modern programming
languages. We use:

 * Lightweight independent **threads** of control.

   Lightweight means that it is not a big concern to keep the number
   of threads low (up to a couple of hundred thousand is usually
   fine).

 * **Channels** for communicating between threads.

The approach we take to concurrent programming in AP is based on the
paradigm of *message passing*, and is heavily inspired by languages
such as Erlang. The idea is to structure concurrent programs as
independent *servers* (a.k.a. *concurrent objects* or *actors* in other
languages and frameworks) that communicate with each other by sending
asynchronous *messages*. Each server maintains its own private state,
and servers cannot directly modify the state of other servers, except
by sending messages.

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

Concurrent programming in Haskell is done in the `IO` monad because
threads (can) have effects. Effects from multiple threads are
interleaved *nondeterministically* at runtime.

Concurrent programming allows programs that interact with multiple
external agents to be modular:

 * The interaction with each agent is programmed separately
 * Allows programs to be structured as a collection of interacting
   agents, sometimes called **actors** or (mini) **servers**.

This chapter is about a principled and systematic way of constructing
concurrent programs as a collection of interacting servers.


## Basics Primitives

We use the Haskell modules
[Control.Concurrent](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Concurrent.html)
and
[Control.Concurrent.Chan](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Concurrent-Chan.html).
The latter is implicitly re-exported by the former. As always, we will
use only a fairly small subset of the facilities provided by the
Haskell standard library. You are welcome (and encouraged) to peruse
the documentation to enlighten yourself, but in this particular case
you should be careful not to be tempted by functions that subvert the
notion of message-passing.

We will not use these primitives directly. Instead we wrap
these primitive in the module `Genserver`, and in the rest of this
note we use the `Genserver` module to write our servers.


## Implementation of the `Genserver` module

Assume that we have the following import and type alias:

```haskell
{{#include ../haskell/concurrency/genserver/src/Genserver.hs:Setup}}
```


### Servers

The basic requirement of concurrency is to be able to fork a new
thread of control. In Haskell we do this with the `forkIO` operation:

```Haskell
forkIO :: IO () -> IO ThreadId
```

The function `forkIO` takes a computation of type `IO ()` as its
argument; that is, a computation in the `IO` monad that eventually
returns a value of type `()`. The computation passed to `forkIO` is
executed in a new *thread* that runs concurrently with the other
threads in the system.

The `ThreadId` that is returned is an opaque handle to the thread. We
can use it to perform a few low-level operations, such as killing the
thread or waiting for it to finish, but this will not be the main way
we interact with threads.

However, we want a canonical way to communicate with our
servers. Thus, we introduce the notion of a *server*, we represent a
server as a pair: a `ThreadId` and an *input channel*:

```haskell
{{#include ../haskell/concurrency/genserver/src/Genserver.hs:Server}}
```

Here we use the type variable `message` to denote the type of messages
that a server can receive, which can be different for each kind of server.

```haskell
{{#include ../haskell/concurrency/genserver/src/Genserver.hs:Spawn}}
```


### Channels

Conceptually, a channel is an unbounded queue of messages. Writing to
a channel is an asynchronous operation - it immediately and always
succeeds. Reading from a channel retrieves the oldest message in the
channel. If the channel is empty, reading blocks until a message is
available.


~~~admonish warning
**TODO:** Write some words that explains the code
~~~

```haskell
{{#include ../haskell/concurrency/genserver/src/Genserver.hs:SendReceive}}
```


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


### Request-Reply Pattern

~~~admonish warning
**TODO:** Write some words that explains the code
~~~

```haskell
{{#include ../haskell/concurrency/genserver/src/Genserver.hs:RequestReply}}
```


## Method

~~~admonish warning
**TODO:** Elaborate on the method
~~~


1. Determine what data (the state) the server should keep track of,
   declare a type for this.

2. Determine the interface for the server, that is a set of functions:

   - The type of each function
   - If the function is *blocking* or *non-blocking*

3. Declare a message type

4. Implement a *server-loop* function

5. Implement API functions


## Worked Example

In this example we want to make a server that keeps track of a count,
a *counter server*. It should be possible to *get the value* of the
counter, to *increment* the counter by one, or to *decrement* the
counter by positive amount `n`. We will maintain the invariant that
the counter is always non-negative.

~~~admonish warning
**TODO:** Maybe explain why this is a useful server. For instance it
can be used to dynamically bound a resource, such a the number of
threads started when traversing a tree.
~~~

### Step 1: Internal state

The server should keep track of an integer as the internal state:

```haskell
type InternalData = Int
```

### Step 2: API functions

The API for the counter server is:

 * `newCounter initial` for creating a new counter server with the
   initial value `initial`. It is an error if `initial` is negative.

 * `getVale cnt` for getting the value of the counter server,
   `cnt`. This is a *blocking* function, because we need to wait for a
   result.

 * `incr cnt` for incrementing the value of the counter server, `cnt`,
   by one. This is a *non-blocking* function, because it cannot fail
   and we don't need to wait for a result.

 * `decr cnt n` for decrementing the value of the counter server,
   `cnt`, by `n`. This is a *blocking* function, because it can fail
   if `n` is larger than the current value of the counter
   server. Thus, we need to wait for a result that tells us if the
   function succeeded.

   Note, this is the interesting function of the example. Because it
   isn't clear what it should mean to decrement a counter with too
   large an amount. We have two options:

   1. The function blocks for a short amount of time, and reports if
      the decrement was successful or not (our choice for now). We
      sometimes call this *limited blocking*.

   2. The function blocks until it is possible to decrement the
      counter with the given amount. This means that the function
      might block forever (in principle). We call this *unlimited
      blocking*.

### Step 3: A type for messages

This step is to make an internal type for the messages that will be
send to the server. We use the pattern where we make constructor for
each kind of message and the argument to be send with that kind
message. If there is an expected reply to given kind of message, we
use the convention that the last argument for that constructor is a
channel for sending back the response.

For the counter server we have a message for each of the interface
functions and don't have any internal messages:

```haskell
{{#include ../haskell/concurrency/genserver/src-exe/Counter.hs:CounterMsg}}
```

Note how the messages for the blocking functions `getValue` and `decr`
have a channel as the last argument.

### Step 4: Implement the server-loop function

The server-loop function for a counter server is:

```Haskell
{{#include ../haskell/concurrency/genserver/src-exe/Counter.hs:CounterLoop}}
```


### Step 5: Implement API functions

The API functions for a counter server can now be implemented straight
forward by using the `spawn`, `sendTo` and `requestReply` functions
from the `Genserver` module:

```Haskell
{{#include ../haskell/concurrency/genserver/src-exe/Counter.hs:CounterAPI}}
```


### Example use of a counter server

```haskell
{{#include ../haskell/concurrency/genserver/src-exe/Counter.hs:CounterExample}}
```



## Timeouts

~~~admonish warning
**TODO:** This section is work in progress and is not using the right
terminology, yet.
~~~

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
