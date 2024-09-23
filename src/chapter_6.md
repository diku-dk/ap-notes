# Concurrent Programming

Haskell offers a wide range of facilities to support for different
styles of concurrent and parallel programming. In this note we focus
on a limited set of primitives for *concurrent programming* which can
often be found in similar form in other modern programming
languages. We use:

 * Lightweight independent **threads** of control.

   Lightweight means that it is not a big concern to keep the number
   of threads low (up to a couple of hundred thousand is usually
   fine).

 * **Channels** for communicating between threads.

Concurrent programming is done in the `IO` monad because threads (can)
have effects. Effects from multiple threads are interleaved
*nondeterministically* at runtime.

Concurrent programming allows programs that interact with multiple
external agents to be modular:

 * The interaction with each agent is programmed separately
 * Allows programs to be structured as a collection of interacting
   agents, sometimes called **actors** or (mini) **servers**.

This chapter is about a principled and systematic way of constructing
concurrent programs as a collection of interacting servers.


## Basics

We use the Haskell modules
[Control.Concurrent](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Concurrent.html)
and
[Control.Concurrent.Chan](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Concurrent-Chan.html).

The latter is implicitly re-exported by the former. The following
import should give you everything you need in most cases:

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

We will not use these primitives directly. Instead we wrap
these primitive in the module `Genserver`, and in the rest of this
note we use the `Genserver` module to write our servers.

## Implementation of the `Genserver` module

We write our concurrent programs as interacting servers.

Assume that we have the following import and type alias:

```Haskell
import qualified Control.Concurrent as CC

type Chan a = CC.Chan a
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

However, we want a canonical way to communicate with our
servers. Thus, we introduce the notion of a *server*, we represent a
server as a pair: a `ThreadId` (for killing a server) and an *input channel*:

```Haskell
type Server message = (CC.ThreadId, Chan message)
```

Here we use the type variable `message` to denote the type of messages
that a server can receive, which can be different for each kind of server.


```Haskell
spawn :: (Chan a -> t -> IO ()) -> t -> IO (Server a)
spawn server initial = do
  input <- CC.newChan
  tid <- CC.forkIO $ server input initial
  return (tid, input)
```


### Channels

~~~admonish warning
**TODO:** Write some words that explains the code
~~~

```Haskell
send :: Chan a -> a -> IO ()
send chan msg =
  CC.writeChan chan msg

sendTo :: Server a -> a -> IO ()
sendTo (_tid, input) msg =
  send input msg

receive :: Chan a -> IO a
receive = CC.readChan
```


**Single-reader principle:** we adopt the rule that a channel may have
only a *single* reader, meaning only a single thread is allowed to
call `readChan` on any given channel. This is typically the thread
that we created the channel for. This is not enforced by the Haskell
type system, and there are indeed forms of concurrent programming
that are more flexible, but they are outside the scope of this note.

It is perfectly acceptable (and often necessary) for a channel to have
multiple writers.


### Request-Reply Pattern

~~~admonish warning
**TODO:** Write some words that explains the code
~~~

```Haskell
requestReply :: Server a -> (Chan b -> a) -> IO b
requestReply serv con = do
  reply_chan <- CC.newChan
  sendTo serv $ con reply_chan
  receive reply_chan
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
