# Concurrent Programming

Haskell offers a wide range of facilities to support for different
styles of concurrent and parallel programming. In this chapter we focus
on a limited set of primitives for *concurrent programming* which can
often be found in similar form in other modern programming
languages. We use:

 * Lightweight independent **threads** of control.

   Lightweight means we can create many threads without worrying about
   overhead (up to a couple of hundred thousand is usually fine).

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

Concurrency is closely related to (but not the same as) *parallelism*.
While multi-threading in Haskell is indeed one way to take advantage
of parallel multi-core computers, this is not an explicit aspect of
our study of concurrency. Instead, we focus on concurrency as a
programming model which happens to be convenient for expressing
certain forms of event-driven systems.

Concurrent programming in Haskell is done in the `IO` monad because
threads are executed for their effects. Threads do not have a "return
value" as such, so the only way they can influence a computation is
through their side effects. Effects from multiple threads are
interleaved *nondeterministically* at runtime.

Concurrent programming allows programs that interact with multiple
external agents to be modular:

 * The interaction with each agent is programmed separately
 * Allows programs to be structured as a collection of interacting
   agents, sometimes called **actors** or (mini) **servers**.

This chapter is about a principled and systematic way of constructing
concurrent programs as a collection of interacting servers.


## Concurrency Primitives

We use the Haskell modules
[Control.Concurrent](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Concurrent.html)
and
[Control.Concurrent.Chan](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Concurrent-Chan.html).
The latter is implicitly re-exported by the former. As always, we will
use only a fairly small subset of the facilities provided by the
Haskell standard library. You are welcome (and encouraged) to peruse
the documentation to enlighten yourself, but in this particular case
you should be careful not to be tempted by functions that subvert the
notion of message-passing. In this section we will use the following
facilities from `Control.Concurrent`:

```Haskell
import Control.Concurrent
  ( Chan,
    ThreadId,
    forkIO,
    newChan,
    readChan,
    writeChan,
  )
```

To create a new thread in Haskell, we use the `forkIO` function. The
`forkIO` function has the following type:

```Haskell
forkIO :: IO () -> IO ThreadId
```

In other words, to create a thread we pass `forkIO` an action of type
`IO ()`, meaning a monadic computation in the `IO` monad. Typically,
this will be some kind of potentially infinite loop that receives and
handles messages, as we will see in a moment. The thread will continue
to run until this action terminates.

The `forkIO` function returns a `ThreadId` that can be used for
interacting with the thread in low level ways, although we will not
make much use of that in AP. Instead, we will communicate using
channel-based messaging.

~~~admonish example

```Haskell
runThread :: IO ()
runThread = do
  t <- forkIO $ putStrLn "Hello there."
  print t
```

```
> runThread
HellToh rtehaedrIed.
47
```

Note how the output of the new thread and the original computation is interleaved.

~~~


### Channels and Messages

In Haskell, communication is done via *channels*. A channel is created
using the `newChan` action:

```Haskell
newChan :: IO (Chan a)
```

The `newChan` action produces a channel that can be used for sending
and receiving messages of type `a`. The precise type of `a` will be
inferred by the compiler.

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

~~~admonish example

```Haskell
channelExample :: IO ()
channelExample = do
  c <- newChan
  _ <- forkIO $ do
    r <- readChan c
    putStrLn $ "Received message: " <> r
  writeChan c "Hello there."
```

```
> channelExample
Receive message: Hello there.
```

~~~

Whenever we create a thread, we will also create a channel through
which we can communicate with the thread. Typically the thread will
run a loop that repeatedly reads from the channel and responds to
message.

~~~admonish example

```Haskell
channelLoopExample :: IO ()
channelLoopExample = do
  c <- newChan
  let threadLoop = do
        r <- readChan c
        putStrLn $ "Received message: " <> r
        threadLoop
  _ <- forkIO threadLoop
  writeChan c "The first"
  writeChan c "The second"
  writeChan c "The third"
```

```
> channelLoopExample
Received message: The first
Received message: The second
Received message: The third
```

~~~

When two different threads have a reference to the same channel (`c`
in the example above), they can communicate. However, completely
arbitrary use of a shared channel will quickly lead to chaos, so we
step in to restore order.

**Single-reader principle:** we adopt the rule that a channel may have
only a *single* reader, meaning only a single thread is allowed to
call `readChan` on any given channel. This is typically the thread
that we created the channel for. This is not enforced by the Haskell
type system, and there are indeed forms of concurrent programming
that are more flexible, but they are outside the scope of AP.

It is perfectly acceptable (and often necessary) for a channel to have
multiple writers.

If we call `readChan` on a channel where we hold the only reference
(meaning we would in principle wait forever), the Haskell runtime
system will raise an exception that will cause the thread to be
terminated. This is a natural and safe way to shut down a thread that
is no longer necessary, assuming the thread does not hold resources
(e.g., open files) that must be manually closed. Handling such cases is
outside the scope of this note.

### Remote procedure calls (RPC)

The Haskell message passing facility is asynchronous, but quite often
we wish to send a message to a server and then wait for it to respond
with some kind of result, corresponding to a procedure call. To
implement synchronous *remote procedure calls* (RPC), we need to
invent a bit of machinery on top of the basic message passing
machinery. The way we make it work is by creating a new channel that
is used for transmitting the result. This channel is then sent along
as part of the message.

The starting point (and always good practice) is to define an explicit
type for the messages we would like to send.

```Haskell
data Msg = MsgInc Int (Chan Int)
```

We then define our thread loop as follows:

```Haskell
threadLoop :: Chan Msg -> IO ()
threadLoop c = do
  msg <- readChan c
  case msg of
    MsgInc x from ->
      writeChan (x + 1) from
  threadLoop c
```

Given a handle to a channel of type `Chan Msg`, we can then send a
message, and wait for a response, as follows:

```Haskell
performRPC :: Chan Msg -> Int -> IO Int
performRPC c x = do
  from <- newChan
  writeChan c $ MsgInc x from
  readChan from
```

And tying it all together:

```Haskell
ex2 :: IO ()
ex2 = do
  c <- newChan
  _ <- forkIO $ threadLoop c
  print =<< performRPC c 0
  print =<< performRPC c 1

```

## Implementation of the `GenServer` module

Using the concurrency primitives directly is somewhat error-prone,
particularly for the constrained form of concurrency we study in AP.
Therefore, we wrap these primitive in a module `GenServer` that
defines a canonical way of using the techniques discussed above, and
in the rest of this note we use the `GenServer` module to write our
servers. There may still be cases where we have to break out of the
`GenServer` abstraction, but we will largely try to work within it.

Assume that we have the following import and type alias:

```haskell
{{#include ../haskell/GenServer.hs:Setup}}
```

### Servers

The `forkIO` procedure provides a low-level way to create a new
thread. However, we want a canonical way to communicate with our
servers. Thus, we introduce the `Server` type. We represent a server
as a pair: a `ThreadId` and an *input channel*:

```haskell
{{#include ../haskell/GenServer.hs:Server}}
```

Here we use the type variable `message` to denote the type of messages
that a server can receive, which can be different for each kind of server.

```haskell
{{#include ../haskell/GenServer.hs:Spawn}}
```


### Channels

Channels are largely unchanged from their primitive form, except that
we define some more concise functions. Further, to users of a server
the channel is hidden away in the `Server` type, and so we provide a
dedicated `sendTo` function for sending a message to the server.


~~~admonish warning title='WIP: Text can be improved'
**TODO:** Write some words that explains the code
~~~

```haskell
{{#include ../haskell/GenServer.hs:SendReceive}}
```

### Request-Reply Pattern

~~~admonish warning title='WIP: Text can be improved'
Write some words that explains the code
~~~

```haskell
{{#include ../haskell/GenServer.hs:RequestReply}}
```


## Method

Following is a five step method to systematically designing and
implementing a server. The steps are presented as sequential phases,
but in practise there will be a bit of going back and forward between
steps.

1. Determine what data (the internal state) the server should keep
   track of, declare a type for this.

   If you lack imagination for a name for this type you can always go
   with `InternalData`.

2. Determine the interface for the server. That is, a set of functions
   where each function takes the server as the first argument and
   possibly other arguments as well. Furthermore, for each function we
   should determine if the function is *blocking* or *non-blocking*.

   In general a function is *blocking* if we need to wait for a
   result depending on the state of the server.

   The blocking behaviour can sometimes be refined into *limited
   blocking* if there is some upper limit on how long a function can
   be blocked. We might not know what exactly the upper limit is (as
   it might depend on various system specific constants and dynamic
   behaviours). Or *unlimited blocking* if the function might block
   forever.

   These functions is the external interface for the server.

3. Declare an internal type for the kind of messages (both external
   messages and internal messages) that a server can receive.

   We use the pattern where we make constructor for each kind of
   message and the argument to be send with that kind message. If
   there is an expected reply to given kind of message, we use the
   convention that the last argument for that constructor is a channel
   for sending back the response. This convention is to make sure that
   we can use the `requestReply` function without too much bother.

4. Implement a *server-loop* function.

   We use the convention that a server-loop function should take the
   input channel for the server as the first argument and the initial
   value for the internal data as the second argument. This convention
   makes it convenient to use the `spawn` function.

   The server loop will usually start by receiving a message on the
   input channel and then use a `case`-expression to pattern match on
   each kind of message and determine what action to do. Each action
   will usually compute a (potentially unchanged) internal state, and
   potentially send some messages, for instance replies to requests.

5. Implement API functions.

   When we have declared the type for messages and implemented the
   server loop, the last step is to implement the API functions. Where
   we use `requestReply` for each blocking function and `sendTo` for
   each non-blocking function.

It is best practise to declare each server in a separate module. Thus
when we talk about *internal* types and functions, it is types and
functions not exported from the module.



## Worked Example: Counter Server

In this example we want to make a server that keeps track of a count,
a *counter server*. It should be possible to *get the value* of the
counter, to *increment* the counter by one, or to *decrement* the
counter by positive amount `n`. We will maintain the invariant that
the counter is always non-negative.

~~~admonish warning title='WIP: Text can be improved'
Maybe explain why a counter server is a useful server. For instance it
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
      call this *limited blocking*.

   2. The function blocks until it is possible to decrement the
      counter with the given amount. This means that the function
      might block forever (in principle). We call this *unlimited
      blocking*.

### Step 3: A type for messages

This step is to make an internal type for the messages that will be
send to the server.

For the counter server we have a message for each of the interface
functions and don't have any internal messages:

```haskell
{{#include ../haskell/Week6/Counter.hs:CounterMsg}}
```

Note how the messages for the blocking functions `getValue` and `decr`
have a channel as the last argument.

### Step 4: Implement the server-loop function

The server-loop function for a counter server is:

```Haskell
{{#include ../haskell/Week6/Counter.hs:CounterLoop}}
```


### Step 5: Implement API functions

The API functions for a counter server can now be implemented straight
forward by using the `spawn`, `sendTo` and `requestReply` functions
from the `GenServer` module:

```Haskell
{{#include ../haskell/Week6/Counter.hs:CounterAPI}}
```


### Example use of a counter server

```haskell
{{#include ../haskell/Week6/Counter.hs:CounterExample}}
```



## Extending `Genserver` with support for timeouts

The server abstraction does not directly support timeouts for blocking
calls. However, we can build our own support for timeouts. The
technique we employ is based on three ingredients: 

1. we have a channel where we allow the reply to be either the
   intended value *or* a special timeout value.
2. we start a worker thread to evaluating an action, and send the
   result back to us.
3. we launch a extra thread that sleeps for some period of time, then
   sends the timeout value to us.

If the non-timeout response is the first to arrive, then the timeout
value is ignored and harmless.

Then we define a type `Timeout` with a single value `Timeout`.

```Haskell
data Timeout = Timeout
```


We can use this to build a function for performing an action with a
timeout:

```Haskell
actionWithTimeout :: Int -> IO a -> IO (Either Timeout a)
```

That is, `actionWithTimeout s act` will perform the action `act`
within a time limit of `s` seconds; it returns an action of type
`IO(Either Timeout a)` where the special value `Timeout` is returned
if `act` did not complete within the time limit:

```Haskell
{{#include ../haskell/concurrency/genserver/src/Genserver.hs:ActionWithTimeout}}
```

You will note that this is not a server in the `Genserver` sense, as it does
not loop: it is simply a utility function that launches two threads.

One downside of this function is that the worker thread (the one that
runs `action`, and might take too long) is not terminated after the
timeout. This is a problem if `action` is, for example, stuck in an infinite
loop that consumes ever more memory. To fix this, we can have the
timeout thread explicitly kill the worker thread:

```Haskell
{{#include ../haskell/concurrency/genserver/src/Genserver.hs:ActionWithTimeoutKill}}
```

Note that killing a thread is a dangerous operation in general. It may
be the case that the worker thread is stuck in some loop or waiting
for a network request, in which case it is harmless, but killing it
may also leave some shared state in an unspecified state. We will
(hopefully) not encounter such cases in AP, but it is something to be
aware of in the future.
