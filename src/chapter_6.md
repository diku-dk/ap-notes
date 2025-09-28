# Property-Based Testing

In the following we elaborate on the idea of testing functions by stating
properties that are expected to hold for them, then verify the function
implementations through randomly generated test cases.

## Properties

When dealing with type classes we discussed the concept of *laws*, which are
properties that should hold for instances of a given type class. For instance,
if a type `T` is an instance of `Eq` we expect `x == x` to evaluate to `True`
for every possible value `x :: T`.

Properties are not, however, intrinsically linked to type classes. An example
of a property not related to any type class is the interaction between `length`
and `(++)`. This holds in general, but for concreteness consider lists of
integers. For any two lists `xs :: [Integer]` and `ys :: [Integer]` we have

```Haskell
length (xs ++ ys) = length xs + length ys
```

Another way of stating the same thing is that the function

```Haskell
{{#include ../haskell/Week6/Properties.hs:Prop_LengthAppend}}
```

returns `True` for all possible arguments. We could come up with a number of test cases, e.g.

```Haskell
{{#include ../haskell/Week6/Properties.hs:TediousTest}}
```

and test with something like `all (\(xs, ys) -> prop_lengthAppend xs ys)
tediousTestCases`, but this is quite tedious. QuickCheck automates this tedium
away by generating (somewhat) random inputs. The workhorse is `quickCheck`
which accepts something `Testable` (explained later) runs it with a number of
different inputs. Simply running `quickCheck prop_lengthAppend` covers more
cases than any unit test suite we would realistically have the patience to
maintain. The default is 100 tests, but if we want more we can run e.g.

```Haskell
quickCheck $ withMaxSuccess 10000 prop_lengthAppend
```

Of course, no amount of test cases is enough to argue total correctness, but
tuning the amount of tests allows us to trade time for certainty.

### Counterexamples

Consider another property, stating that `(++)` is commutative:

```Haskell
{{#include ../haskell/Week6/Properties.hs:Prop_AppendCommutative}}
```

Running `quickCheck prop_appendCommutative` quickly falsifies this theory and
prints out a counterexample such `[0]` and `[1]`. QuickCheck is very useful
when we are genuinely unsure whether a property holds, since in practice false
properties often have easy-to-find counterexamples.

### The Value of Properties

Why do we care whether a property like `prop_lengthAppend` holds? It does,
after all, not directly say anything about the correctness of `length`, `(+)`
or `(++)`. For instance, given the obviously wrong definitions

```Haskell
_ + _ = 0
_ ++ _ = []
```

the property would still hold. The crucial observation is that in practice
code is seldom wrong in ways that happen to not violate any properties.
Therefore observing that a number of non-trivial properties involving some
function are true is a good proxy for correctness of the function.

But if properties are merely good proxies for correctness, why is that better
than just testing correctness directly? The reason is that many properties are
like the ones we have seen so far: they can be expressed as some boolean
condition with variables that should hold for all choices of those variables.
This is easy to test using QuickCheck or similar systems. Direct testing is
harder to automate. That would require producing many test cases like `[] ++ []
== []` and `[1, 2] ++ [3, 4] == [1, 2, 3, 4]` and so on, which is manual (and
error-prone) work.

## The `Testable` Type Class

The `quickCheck` function in fact works on any type which is an instance of
`Testable`. The primary instances and their semantics for testing are worth
going over:

* `()` is testable and succeeds if it returns `()` (the only possible value of the type `()`) and fails if an exception occurs.
* `Bool` is testable where `True` means success and `False` or an exception means failure.
* `Maybe a` is testable if `a` is. `Nothing` means that the test should be
  *discarded* and counted neither as a success nor as a failure. `Just result` has the meaning of `result`.
* `a -> b` is testable if `a` is `Arbitrary` (meaning that we have a way of
  generating values of that type; see the next section) and `b` is `Testable`.
  The semantics is that `f :: a -> b` succeeds if `f x :: b` succeeds for all
  `x :: a`. In practice this is tested by generating random values of `a`. Note
  that this instance applies recursively so e.g. `Integer -> Integer -> Bool`
  is `Testable` because `Integer -> Bool` is `Testable`; and `Integer -> Bool` is `Testable` because `Bool` is
  `Testable`.

This works great. We can write down properties using familiar Haskell types
without even depending on the QuickCheck library. However, what if we want to
collect all our properties into a list for test organisation purposes? If we
have `p :: Integer -> Bool` and `q :: String -> Bool` then `[p, q]` is not
well-typed. The `Testable` type class has a method `property :: a -> Property`
which converts any `Testable` value into a `Property`. Think of `Property` as a
`Testable` value of unknown type. A list of properties should be of type
`[Property]` and constructed like `[property p, property q]`.

## Arbitrary

So far we have relied on QuickCheck automatically coming up with values for
our properties. The mechanism behind this is the `Arbitrary` type class alluded
to above, which defines a method `arbitrary :: Gen a` that is supposed to
define the "canonical" generator for the type. Hence, we first need to
understand generators.

### Generator Basics

As a first approximation the type `Gen a` represents a probability distribution
over elements of `a`. It can also be thought of as a computation that can
depend on random choices. Generators are defined using a combination of
primitives and monad operations.

The simplest generator is `pure x` which produces the value `x` with
probability 1. Given a list of generators `gs :: [Gen a]` the generator `oneof
gs` chooses one of the generators with equal probability. For instance `oneof
[pure "heads", pure "tails"]` produces `"heads"` and `"tails"` with equal
probability, but `oneof [pure "heads", pure "tails", pure "tails"]` is biased
in favour of `"tails"`.

It is also possible to explicitly control the bias using `frequency`, which is
like `oneof` but allows specifying the weight of each option. The biased
example using `oneof` would written more idiomatically as `frequency [(1, pure
"heads"), (2, pure "tails")]`.

QuickCheck has a function called `sample` which takes a generator and prints 10
example values. This is quite useful to get a rough sense of what a generator
produces and is often sufficient to spot simple biases like in the previous
example.

### Recursive Generators

QuickCheck has a combinator called `listOf` which generates `[a]` given a
generator for `a`. Let us generate a list of integers using the standard
integer generator given by its `Arbitrary` instance. An example output is:

```
> sample $ listOf (arbitrary :: Gen Integer)
[]
[-1]
[2,1]
[-6,1,-6,-1]
[6]
[7,-10,8,2,-7,0,-7]
[-5,-4,-6,-5,-3,-2,-5,6,-7,-5,-6]
[]
[-11,2,7,-16,-11,11,-14,5,-12,13,12]
[18,-13,17,-9,-16]
[20,8,-12,-4,16,8,7,4,-20,-1,-6,8,6,16,14,-8,14,-6,-1]
```

Note that there is a decent spread both in the length of the list and the
individual integer values. How would we go about implementing a combinator like
`listOf`? A first attempt might be:

```Haskell
{{#include ../haskell/Week6/Properties.hs:List1}}
```

This is a choice between an empty list and a list consisting of an element
generated by `g` followed by a recursively generated list. Each choice has
equal chance, so 50% of lists will be empty, another 25% will have just a
single element and so on.

Alas, the distribution leaves something to be desired:

```
> sample $ list1 (arbitrary :: Gen Integer)
[]
[]
[0,1]
[4]
[-7,-4]
[]
[5,-8]
[1]
[]
[]
[8]
```

Every other sample is an empty list and long lists are exceedingly unlikely,
which makes this generator inefficient for exploring the search space. A second
attempt might be to use `frequency` to introduce a bias towards longer lists:

```Haskell
{{#include ../haskell/Week6/Properties.hs:List2}}
```

The resulting distribution is better, but still heavily favours short lists
with an occasional longer list.

```
> sample $ list2 (arbitrary :: Gen Integer)
[0]
[2,0]
[-2]
[1,5,4]
[0,6,-4]
[5,-2,-6,-3,-5]
[-4]
[-2]
[-11]
[-11]
[16]
```

We could try adjusting the bias, but no matter what value we use the length of
the list will follow an *exponential* distribution, which is not really what we
want.

For our third attempt, we exploit the fact that `Gen` is a monad. First
generate a non-negative integer `n`, and then generate a list of length `n`:

```Haskell
{{#include ../haskell/Week6/Properties.hs:List3}}
```

Now the distribution is similar to QuickCheck's `listOf`.

```
> sample $ list3 (arbitrary :: Gen Integer)
[]
[-2,-1]
[]
[-5,0,6,-2]
[-8,-1,-6,5,5,8,4,8]
[2,1,-8,-2,7,-6,-7,-5,-8,-10]
[2,4,2,4,5,-12,5,-5,12,-4,-2,-1]
[2,11,-10,6,-3,3,-8,11,9,9,7]
[6,13,9,6,3,3,6,13,-10,-4,2,-16,-9,-16,0]
[6,-10,12,-5,-12,14]
[-14,0]
```

### Size Dependent Generators

When testing a property it is often a good idea to start with small values and
then gradually increase the complexity of the test cases. QuickCheck uses this
approach by giving generators access to a `size` parameter (similar to a
`Reader` monad) which under default settings starts at 0 and increases by 1
every test up to a maximum of 99.

The size can be accessed directly using `getSize :: Gen Int` but usually a
neater approach is to use the combinator `sized :: (Int -> Gen a) -> Gen` which
turns a size-dependent generator into an ordinary one. A good generator
respects the `size` parameter, so our list generator is more idiomatically
written as:

```Haskell
{{#include ../haskell/Week6/Properties.hs:List4}}
```

This is essentially the definition of `listOf` in QuickCheck (where `go` is
known as `vectorOf`).

## Shrinking

Suppose we define our own type of pairs with an instance of `Arbitrary`:

```Haskell
{{#include ../haskell/Week6/Properties.hs:Pair}}
```

We can now define a version of commutativity for `(++)` that takes the input as
a `Pair`:

```Haskell
{{#include ../haskell/Week6/Properties.hs:Prop_AppendCommutativePair}}
```

QuickCheck still finds a counterexample. A possible output is:

```
> quickCheck prop_appendCommutative'
*** Failed! Falsified (after 5 tests):
Pair [-2,1,3,-2] [-4,4,-1,2]
```

However, the counterexample is not as simple as the counterexample to our
original property. Running `quickCheck` multiple times will reveal that
`prop_appendCommutative` consistently produces small counterexamples while
`prop_appendCommutative'` produces counterexamples of various sizes.

The secret ingredient is *shrinking*. The `Arbitrary` type class also defines a
member `shrink :: a -> [a]` which takes a value and produces a list of
*shrinks*, i.e. "slightly smaller" values. The idea is that if `x` is a
counterexample to some property then any of the elements in the list `shrink x`
could also be counterexamples.

When QuickCheck finds a counterexample `x` it tests the property for each
shrink of `x`. If that result in another, by definition simpler, counterexample
the process repeats recursively until the (locally) simplest counterexample is
reached.

For a value `Pair x y` a natural notion of "slighty smaller" is a pair which is
slightly smaller in *either* the first component *or* the second component. The
complete `Arbitrary` instance is thus:

```Haskell
{{#include ../haskell/Week6/Properties.hs:PairShrink}}
```

Why not shrink both components simultaneously? Well, suppose `x'` is slighty
smaller than `x` and `y'` is slightly smaller than `y`. Then `Pair x' y'` is
slightly smaller than `Pair x y'` (or `Pair x' y`) which in turn is slightly
smaller than `Pair x y` so assuming that either `Pair x y'` or `Pair x' y` is
also a counterexample the process would reach `Pair x' y'` in two steps. In
general, there is a trade-off between efficiency (i.e. not producing too many
shrinks) and likelihood of finding the very simplest counterexample.

## Tasty QuickCheck

The Tasty testing framework has support for QuickCheck via the package
`tasty-quickcheck`. For example, `testProperties "properties" props` is a
simple test tree, given `props :: [(String, Property)]`.

This test tree exposes options for Tasty on the command line. For example, to
control the number of tests we can run

```
$ cabal test --test-option --quickcheck-tests=10000
```

## Testing stateful systems

The examples above assume that we are testing a pure function where properties
are involve a relationship between the inputs and outputs of the function.
Regrettably however, many computer systems are *stateful* - they encapsulate
some internal data, which is modified in response to commands, and divulges
information in response to requests. A network service a clean example of a
stateful system, but even a conventional mutable data structure, such as a
resisable array, fits the definition.

Large stateful systems are often complicated and error-prone, and hence it is
very desirable to be able to test them effectively. It turns out that
property-based testing is also an effective approach for such systems (and in
industry, this may even be the most impactful use case), although it requires us
to build some additional infrastructure.

The basic idea is that we take the so-called *software under test* (SUT) and
construct a *model* that captures the most important properties of the SUT. In
many cases a stateful system has implementation details that are important to
its operation (such as caching, optimisations, persistency in a database,
integration with other systems, etc), but which are not part of its external
interface. A model is a program that imitates some subset of the behaviour of
the SUT, and is typically much simpler than the SUT. We then randomly generate
commands that interact with the SUT and the model, and test that the observable
behaviour is the same. Essentially, the model is an executable specification
that we compare against the SUT.

One very important detail is that there is no requirement that the SUT is
implemented in the same language as the property-based testing framework (e.g.,
Haskell in our case). Indeed, it is a common approach to construct a model in
Haskell and use QuickCheck to verify that some other network-based system
matches the behaviour of the model. This is particularly useful when the choice
of technology for the SUT is constrained due to efficiency or integration
concerns.

### A sample stateful system

The SUT we will test in the following does happen to be implemented in Haskell
for simplicity. Specifically, we will define a datatype `DynamicArray a` for
mutable arrays of elements of type `a` with efficient support for appending
elements. Since the array is mutable, it will live in `IO`. The implementation
details of the array are not important, and so will be covered somewhat briefly.

First we will need to import some library functions.

```Haskell
{{#include ../haskell/Week6/Stateful.hs:Stateful_Imports}}
```

Our representation of `DynamicArray` will be quite similar to what you may have
seen in systems programming classes, or implemented yourself. The idea is to
have an underlying array with room for more elements than have actually been
inserted yet. We call the size of this array the *capacity*. When the number of
elements inserted by the user exceeds the capacity, then we bump the capacity by
some factor, allocate a new array of that size, then copy the old elements to
the new array. If we always double the capacity, then it can be shown that
appending an element can be done in constant amortised time.

```Haskell
{{#include ../haskell/Week6/Stateful.hs:Stateful_DynamicArray}}
```

Because we need to modify both the element count and the capacity, we put them
in a mutable `IORef`. The underlying array is an `IOArray Int a`, which is a
mutable (but non-resizable) provided by Haskell. The `Int` type argument is the
index type, which can be used to represent multidimensional arrays, but we will
not make use of this.

~~~admonish note

The interface to `IOArray` is through various class-polymorphic functions
defined by `Data.Array.MArray`. For simplicity of exposition, the types in the
following have been monomorphised to be specific to `IOArray`.

~~~

An `IOArray` is produced by the function `newArray_` of the following type:

```Haskell
newArray_ :: (Int,Int) -> IO (IOArray Int a)
```

The `(Int,Int)` pair is the smallest and largest valid index (these arrays do
not have to start at zero, although ours do). Initially, all elements of the
array will be undefined. Reading an undefined element will cause an IO
exception.

We arbitrarily decide that the initial capacity of our dynamic arrays will be
10.

```Haskell
{{#include ../haskell/Week6/Stateful.hs:Stateful_newDynamicArray}}
```

When indexing an array we check whether the index is in-bounds, returning
`Nothing` if not.

```Haskell
{{#include ../haskell/Week6/Stateful.hs:Stateful_index}}
```

Inserting always succeeds and produces no result beyond modifying the array, but
we have to resize the underlying array if the new element causes the capacity to
be exceeded.

```Haskell
{{#include ../haskell/Week6/Stateful.hs:Stateful_insert}}
```

Overwriting an existing element is quite similar to indexing. We return
`Nothing` on out-of-bounds, and `Just ()` on success.

```Haskell
{{#include ../haskell/Week6/Stateful.hs:Stateful_write}}
```

Our final operation allows the deletion of an element anywhere in the array.
This is a somewhat costly operation, as we have to shift all elements after the
deleted one left. Further, to avoid using too much memory, if the capacity is
too large after the deletion, we shrink the array. back down to a smaller size.
As before, we return `Nothing` in case the index is out of bounds.

```Haskell
{{#include ../haskell/Week6/Stateful.hs:Stateful_delete}}
```

### Defining a model

There is a lot of intricate index-fiddling code in the above. Can we really be
sure it is correct? At a semantic level, what `DynArray` is doing is not
complicated - it is merely maintaining a sequence of elements. The complexity
comes from our desire for certain performance characteristics. Let us define a
model that captures the central behaviour of `DynArray`, but without caring
about operational matters such as performance.

```Haskell
{{#include ../haskell/Week6/Stateful.hs:Stateful_Model}}
```

A model is described by its internal state and by which *commands* can be sent
to it, and which *responses* may be produced in return. These commands usually
resemble the API of the SUT, but perhaps simplified in various ways. Due to the
simplicity of the system we are testing here, the commands are however fairly
simple.

```Haskell
{{#include ../haskell/Week6/Stateful.hs:Stateful_Command}}
```

We then define functions corresponding to each of the commands. Each function
returns a new model state, as well as a response. For example, the function
corresponding to an insertion is defined as follows.

```Haskell
{{#include ../haskell/Week6/Stateful.hs:Stateful_cmdInsert}}
```

We define functions for the remaining commands, and finally a `step` function
for simulating the effect of running a certain `Command` on a `Model`:

```Haskell
{{#include ../haskell/Week6/Stateful.hs:Stateful_step}}
```

Similarly, we also define a function for executing a `Command` on our SUT. This
involves mapping the SUT API to the `Command`/`Response` types, which in our
case is quite simple, but can sometimes a bit laborious.

```Haskell
{{#include ../haskell/Week6/Stateful.hs:Stateful_exec}}
```

### Specifying a property

We denote a sequence of commands as a *program*. The testing problem is now to
ensure that the SUT and the model behave identically for all programs.

```Haskell
{{#include ../haskell/Week6/Stateful.hs:Stateful_Program}}
```

First of course, we have to define appropriate `Arbitrary` instances. We are
going to define these in quite a simple way - the previous remarks about good
generators still apply, but it turns out we can explore a lot of the test space
even without putting any particular thought into it.

```Haskell
{{#include ../haskell/Week6/Stateful.hs:Stateful_Arbitrary_Command}}
```

Let us look at what programs end up being generated.

```Haskell
> sample (arbitrary :: Gen (Program Int))
Program []
Program [Insert (-2),Insert (-2)]
Program []
Program [Index 6,Insert 1]
Program [Delete 0]
Program []
Program [Write 4 4,Write (-2) (-12),Index 9,Write (-8) 12,Insert (-5),Delete 5,Index 6]
Program [Write 9 (-12),Index (-7),Delete 10,Write (-2) (-5),Insert (-6),Write 13 13,Insert 12]
Program []
Program [Write (-18) 12,Insert (-8),Insert 9,Insert (-8),Insert (-10),Delete (-6),Index (-12),Index (-4),Delete (-6),Index (-16),Index (-10),Index (-14),Delete 17]
Program [Write 15 13,Delete (-11),Insert (-20)]
```

It is clear that once again, we generate a good number of empty programs, but we
also generate a good bit of *invalid* ones. Look at how many of them perform
`Index` or `Delete` commands without first inserting some elements. It is good
and necessary to test these cases, but perhaps they are generated too often.
Generating good sequences of commands for complicated stateful systems is an
interesting topic in itself, but the above is sufficient for our needs.

Next we generate a procedure for executing a `Program a` on both a `DynamicArray
a` and a `Model a`, producing a boolean that indicates whether the same
responses were observed. Recall that we cannot directly inspect the internal
state of a `DynamicArray`, but we *can* chcek its observable behaviour.

```Haskell
{{#include ../haskell/Week6/Stateful.hs:Stateful_runProgram}}
```

Finally, we just need to construct a property that uses `runProgram`. The
crucial building block is the function `ioProperty`, which allows us to turn an
arbitrary IO operation into a `Property`:

```
ioProperty :: Testable prop => IO prop -> Property
```

We must of course be very careful when using this function, as it is easy to use
it to construct a property that behaves nondeterministically, which can make it
difficult for QuickCheck to shrink test cases properly. In our case we are
merely using it to construct the initial dynamic array and to execute
`runProgram`:

```Haskell
{{#include ../haskell/Week6/Stateful.hs:Stateful_prop_array}}
```

Finally we can verify the property.

```Haskell
> quickCheck prop_array
+++ OK, passed 100 tests.
```

Of course, to demonstrate that things really work, let us introduce a bug. For
example, let us remove the line `writeArray arr' used x` from the `else`-branch
in the definition of `insert` and rerun the tests:

```
> quickCheck prop_array
*** Failed! (after 56 tests and 13 shrinks):
Exception:
  MArray: undefined array element
  CallStack (from HasCallStack):
    error, called at libraries/array/Data/Array/Base.hs:914:16 in array-0.5.8.0-74ab:Data.Array.Base
Program [Insert 21,Insert (-33),Insert (-55),Insert 46,Insert (-48),Insert 47,Insert 37,Insert 19,Insert (-35),Insert (-37),Insert 55,Delete 0,Index 9]
```

We are not told exactly what is wrong (this is an exception thrown from the
underlying array library), but we are given a sequence of commands that reliably
lead to the problem, and which we can use to debug the problem.
