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

We will make use of the following imports:

```haskell
{{#include ../haskell/GenServer.hs:Setup}}
```

### Servers

The `forkIO` procedure provides a low-level way to create a new
thread. However, we want a canonical way to communicate with our
servers. Thus, we introduce the `Server` abstract type. We represent a
server as a pair: a `ThreadId` and an *input channel*:

```haskell
{{#include ../haskell/GenServer.hs:Server}}
```

Here we use the type variable `message` to denote the type of messages
that a server can receive, which can be different for each kind of
server.

```haskell
{{#include ../haskell/GenServer.hs:Spawn}}
```

Note that `Server` will be an abstract type - users cannot directly
access its components, except through the interface we define below.

### Channels

In the `GenServer` abstraction, channels are unchanged from their
primitive form, except that we define some more concise function
names.

```haskell
{{#include ../haskell/GenServer.hs:SendReceive}}
```

However, to users of a server the channel is hidden away in the
`Server` type, and so we provide a dedicated `sendTo` function for
sending a message to the server.

```haskell
{{#include ../haskell/GenServer.hs:SendTo}}
```

### Request-Reply Pattern

We saw above how to implement RPC on top of asynchronous messages. To
cut down on the boilerplate and avoid incorrect usage, we provide a
convenience API in the `GenServer` API for performing RPCs.

```haskell
{{#include ../haskell/GenServer.hs:RequestReply}}
```


## Method

The following is a five step method to systematically designing and
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
   and internal messages) a server can receive.

   We use the pattern where we make a constructor for each kind of
   message and the argument(s) to be send with that kind of message. If
   there is an expected reply to the given kind of message, we use the
   convention that the last argument for that constructor is a channel
   for sending back the response. This convention is to make sure that
   we can use the `requestReply` function without too much bother.

4. Implement a *server-loop* function.

   We use the convention that a server-loop function should take the
   input channel for the server as the first argument. This convention
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

It is usually best practise to declare each server in a separate
module. Thus when we talk about *internal* types and functions, it is
types and functions not exported from the module. However, there are
cases where a server may define its own internal servers for utility
purposes, that would be awkward to define in a separate module.



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
2. we start a worker thread, which will evaluate an action, and send the
   result back to us.
3. we also launch an extra thread that sleeps for some period of time, then
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
{{#include ../haskell/GenServer.hs:ActionWithTimeout}}
```

You will note that this is not a server in the `Genserver` sense, as it does
not loop: it is simply a utility function that launches two threads. Note also
that `threadDelay` accepts an argument in microseconds, so wehave to multiply
the provided timeout by one million.

One downside of this function is that the worker thread (the one that
runs `action`, and might take too long) is not terminated after the
timeout. This is a problem if `action` is, for example, stuck in an infinite
loop that consumes ever more memory. To fix this, we can have the
timeout thread explicitly kill the worker thread:

```Haskell
{{#include ../haskell/GenServer.hs:ActionWithTimeoutKill}}
```

Note that killing a thread is a dangerous operation in general. It may
be the case that the worker thread is stuck in some loop or waiting
for a network request, in which case it is harmless, but killing it
may also leave some shared state in an unspecified state. We will
(hopefully) not encounter such cases in AP, but it is something to be
aware of in the future.

## A Larger Example: A Module for Asynchronous Computation

In this section we will look at the design of a GenServer-based module
for executing pure Haskell functions in an asynchronous manner. It
demonstrates several important programming techniques, including the
use of sub-threads to ensure reponsivity and robustness. We are
concerned here only with pure functions. The API of the system we will
implement is as follows:

```Haskell
data Async a

type Seconds = Int

async :: Seconds -> (a -> b) -> a -> IO (Async b)

data Result a
  = Timeout
  | Exception String
  | Value a
  deriving (Eq, Ord, Show)

poll :: Async a -> IO (Maybe (Result a))

wait :: Async a -> IO (Result a)

```

A value of type `Async a` represents an asynchronous computation that
produce a value of type `a`. They are created using the function
`async`, which takes as argument a maximum allowed runtime, a function
`a -> b`, and an argument value `a`, returning an `Async b`. The
`async` function itself must return immediately.

The status of an `Async a` value can be inspected using the functions
`poll` and `wait`. The `poll` function immediately returns the state
of the asynchronous computation, returning `Nothing` if the
computation is still ongoing, and otherwise a result of type `Result
a`:

* `Timeout` is returned if the execution of the function exceeded the
  runtime specified by the original `async` invocation.

* `Exception`, along with the exception error message, is returned if
  the computation resulted in an exception being thrown.

* `Value` is returned if the computation finished without exceptions
  and within the allotted runtime.

The `wait` function is similar to `poll`, except that it *blocks*
until the computation finishes.

We will use the following definition of `fib` in our examples:

```Haskell
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n =
  if n < 0
    then error "negative n"
    else fib (n - 1) + fib (n - 2)
```

Because it returns `Int`, it is easy to use the `evaluate` function
from `Control.Exception` to ensure that the result is fully evaluated.

~~~admonish example

```
simpleDemo :: IO ()
simpleDemo = do
  putStrLn "a"
  a <- async 1 fib 10
  print =<< poll a
  print =<< wait a
```

~~~

### Initial Design

Our server thread can be seen as a state machine with two main states:

1. Before we know the result of the computation. At this point we must
   also maintain a list of those clients that have called `wait` and
   must be informed when the computation finishes.

2. After the result of the computation is known, which requires
   storing the result.

Further, when we transition from stage 1 to 2, we must inform all of
the waiting clients of the result.

For simplicity, we will initially not worry about timeouts and
exceptions. It will turn out to be quite simple to add support for
these features.

Our design will be to launch a separate *worker thread* that computes
the value. This allows the main server thread to immediately answer
`poll` requests even through the worker thread is engaged in a
long-running computation. Once the worker thread has computed the
desired value, it will be sent to the server thread in a message.
Further, we need messages for the `poll` and `wait` functions. Our
resulting message type is the following:

```Haskell
data Msg a
  = MsgPutVal a
  | MsgPoll (ReplyChan (Maybe (Result a)))
  | MsgWait (ReplyChan (Result a))
```

And the `poll` and `wait` functions are merely wrappers around sending
the corresponding messages:

```Haskell
poll :: Async a -> IO (Maybe (Result a))
poll (Async s) =
  requestReply s MsgPoll

wait :: Async a -> IO (Result a)
wait (Async s) =
  requestReply s MsgWait
```

To represent the two main states of the server, we use two recursive
functions:

```Haskell
noValueLoop :: Chan (Msg a) -> [ReplyChan (Result a)] -> IO ()
noValueLoop c waiters = do
  msg <- receive c
  case msg of
    MsgPutVal v' -> do
      forM_ waiters $ \from ->
        reply from $ Value v'
      valueLoop c (Value v')
    MsgPoll from -> do
      reply from Nothing
      noValueLoop c waiters
    MsgWait from ->
      noValueLoop c (from : waiters)

valueLoop :: Chan (Msg a) -> Result a -> IO ()
valueLoop c v = do
  msg <- receive c
  case msg of
    MsgPutVal _ ->
      valueLoop c v
    MsgPoll from -> do
      reply from $ Just v
      valueLoop c v
    MsgWait from -> do
      reply from v
      valueLoop c v
```

Note how the `MsgWait` case in `noValueLoop` simply adds the
`ReplyChan` to a list. Then, once we receive a `MsgPutVal`, we reply
to all of these pending calls. In contrast, when we receive a
`MsgWait` in `valueLoop`, we immediately respond with the result. If
we are in `valueLoop` and receive another `MsgPutVal` message, we
simply ignore it. In practice this will never occur (at least not
until we add timeouts and exceptions).

Instead of using two function to represent two states, we could also
have used a single function and stored the server state as a sum type
with two constructors. I prefer using mutually recursive functions to
represent the high level states of a server (if such a distinction
makes sense), but this is ultimately a matter of personal taste.

An `Async a` is now just a type that wraps a `Server (Msg a)`:

```Haskell
data Async a = Async (Server (Msg a))
```

The `async` function uses `spawn` to create a server. Before entering
the server loop, we use `forkIO` to create the worker thread, which
has a reference to a channel (`c`) from which the server reads
messages:

```Haskell
async :: Seconds -> (a -> b) -> a -> IO (Async b)
async timeout f x = do
  s <- spawn $ \c -> do
    void $ forkIO $ do
      x' <- evaluate $ f x
      send c $ MsgPutVal x'
    noValueLoop c []
  pure $ Async s
```

Initially the `noValueLoop` has no waiters.

This completes the basic functionality of the `Async` server.

### Handling Timeouts

Timeouts are (almost) always handled by creating a thread that waits
for some period of time, then sends a message or takes some other
action. Here, we will add a new message that indicates that the
timeout has passed:

```Haskell
data Msg a
  = ...
  | MsgTimeout

```

We modify `async` such that after creating the worker thread, we also
create a thread that sends this message after the timeout has passed.
Remember that `threadDelay` expects its argument in microseconds, therefore we
multiply the provided timeout by one million.

```Haskell
async :: Seconds -> (a -> b) -> a -> IO (Async b)
async timeout f x = do
  s <- spawn $ \c -> do
    void $ forkIO $ do
      x' <- evaluate $ f x
      send c $ MsgPutVal x'
    void $ forkIO $ do
      threadDelay $ timeout * 1000000
      send c MsgTimeout
    noValueLoop c []
  pure $ Async s
```

Finally, we modify `noValueLoop` and `valueLoop` to handle the new
`MsgTimeout`. In the former, reception of such a message denotes a
timeout, and so we switch to the `valueLoop` state, with `Timeout` as
our value:

```Haskell
noValueLoop :: Chan (Msg a) -> [ReplyChan (Result a)] -> IO ()
noValueLoop c waiters = do
  msg <- receive c
  case msg of
    ...
    MsgTimeout -> do
      forM_ waiters $ \from ->
        reply from Timeout
      valueLoop c Timeout
```

In `valueLoop`, `MsgTimeout` is simply ignored, as all it indicates is
that the original timeout has expired - which is unimportant once the
value has been received.

```Haskell
valueLoop :: Chan (Msg a) -> Result a -> IO ()
valueLoop c v = do
  msg <- receive c
  case msg of
    ...
    MsgTimeout ->
      valueLoop c v

```

~~~admonish example

```Haskell
timeoutDemo :: IO ()
timeoutDemo = do
  a <- async 1 fib 100
  print =<< poll a
  print =<< wait a
```

```
> timeoutDemo
Nothing
Timeout
```

~~~

Arguably, if the timeout is reached, we should use `killThread` to
liquidate the worker thread, in case it is stuck in an infinite loop.
This would require us to augment the state with a reference to the
`ThreadId` of the worker thread.

### Handling Exceptions

At the protocol level, the handling of exceptions is very similar to
the handling of values. We add a new message type:

```Haskell
data Msg a
  = ...
  | MsgPutException String

```

The handling of `MsgPutException` is then essentially identical to the
handling of `MsgPutVal`:

```Haskell
noValueLoop :: Chan (Msg a) -> [ReplyChan (Result a)] -> IO ()
noValueLoop c waiters = do
  msg <- receive c
  case msg of
    ...
    MsgPutException err -> do
      forM_ waiters $ \from ->
        reply from $ Exception err
      valueLoop c (Exception err)

```

```Haskell
valueLoop :: Chan (Msg a) -> Result a -> IO ()
valueLoop c v = do
  msg <- receive c
  case msg of
    ...
    MsgPutException _ ->
      valueLoop c v
```

Exceptions are caught in the worker thread, using the technique
discussed in Chapter 4 of these notes:

```Haskell
async :: Seconds -> (a -> b) -> a -> IO (Async b)
async timeout f x = do
  s <- spawn $ \c -> do
    void $ forkIO $ do
      let computeValue = do
            x' <- evaluate $ f x
            send c $ MsgPutVal x'
          onException :: SomeException -> IO ()
          onException e = do
            send c $ MsgPutException $ show e
      catch computeValue onException
    void $ forkIO $ do
      threadDelay $ timeout * 1000000
      send c MsgTimeout
    noValueLoop c []
  pure $ Async s

```

~~~admonish example

```Haskell
exceptionDemo :: IO ()
exceptionDemo = do
  a <- async 1 fib (-1)
  print =<< poll a
  print =<< wait a
```

```
> exceptionDemo
Nothing
Exception "negative n"
```

~~~
