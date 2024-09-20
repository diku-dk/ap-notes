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
(e.g., open files) that must be manually closed. Handling such cases is
outside the scope of this note.
