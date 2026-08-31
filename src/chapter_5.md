# Property-Based Testing

This chapter is about how we convince ourselves - and others - that a program
does what it is supposed to do. The technique we focus on is *property-based
testing*: we state properties that the code is expected to satisfy, and then let
a machine try very hard to find inputs that violate them. In Haskell the
standard tool for this is [QuickCheck](https://hackage.haskell.org/package/QuickCheck),
which is also the origin of the idea; property-based testing libraries in other
languages (Hypothesis for Python, PropEr for Erlang, jqwik for Java, proptest
for Rust, ...) are all descendants of it.

Before we get to the mechanics we spend some time on the vocabulary, because
much confusion about testing is really confusion about what a test *is*. We then
work through properties, generators, shrinking, and test data design, and close
with the most valuable industrial application of the technique: testing
*stateful* systems against a *model*.

## Specification, implementation, verification

Programming consists of three activities that are worth keeping apart:

1. **Specification**: *What* is computed? Which functions exist, and which
   properties do they have?
2. **Implementation** (or *coding*): *How* is it computed? The source code.
3. **Verification**: Which explicit *evidence* is there that the code meets its
   specification?

Verification always requires *both* code and specification. The same code may be
correct with respect to one specification and incorrect with respect to another.
This has an immediate practical consequence:

~~~admonish warning title="Avoid saying: my code works"
With respect to *which specification*? Code is trivially correct with respect to
whatever it happens to do - there is nothing to test, because it does what it
does. The interesting question is what it does that a *user of the code* is
entitled to rely on, and whether you have evidence that it always does so, for
all valid inputs, and not merely on the handful of inputs you happened to try.
~~~

### Specifications

A *specification* describes what a collection of functions does, not how it is
implemented. The litmus test is that it contains *everything* a user of a module
needs to know, and *nothing else*.

Specifications are not automatically perfect. They may be

* *inconsistent*: they contain a contradiction, so no implementation can satisfy
  them; or
* *incomplete*: they leave room for several implementations with observably
  different behaviour. This is very common, and often deliberate.

It is useful to distinguish a specification from the *requirements*, which are an
informal description of the desired functionality. Requirements are typically
vague and open to interpretation; a specification *resolves and documents* that
ambiguity. In practice a full specification consists of a formal part (properties
that must hold) plus an informal natural-language part.

### An interface is a contract

A module interface specification is a *contract* between the programmer who
*uses* a module and the programmer who *implements* it:

* User to implementor: "Everything I need to know must be in the module
  specification. How I use your implementation is none of your business. Don't
  make me read your code."
* Implementor to user: "Everything you need to know is in the module
  specification. How I implement it is none of your business. Don't read my code
   - I may change it at any time."

Note that *user* and *implementor* are roles; they are frequently the same
person on different days.

What happens if specification and implementation are inconsistent, as shown by a
failing test? There are three possible repairs, with very different costs:

* Change the code to meet the specification. Requires no negotiation with users.
* Change the specification to meet the code. Requires negotiation, because the
  change may propagate into code that users have already written.
* Change both. This is a natural part of *exploratory* programming, while the
  requirements are still being worked out.

The most common problem in practice is that modules are *underspecified*: a user
experiments with the implementation and comes to rely on behaviour that was never
promised; the implementor later changes that behaviour; and the resulting errors
are both hard to detect and hard to localise.

### What testing is - and is not

*Testing* is partial verification by

* executing code on inputs and checking whether the result is evidence of a
  specification violation; and
* *systematically* constructing those inputs so as to *maximise the likelihood
  of demonstrating a violation*.

It is worth separating three activities that are often all called "testing":

* **Formal verification**: a mathematical proof that there exists *no* valid
  input leading to a specification violation.
* **Testing**: the systematic discipline of finding nasty inputs that maximise
  the chance of finding a violation. Failing to break the code is taken as
  *evidence* - not proof - that the code may be correct with respect to the given
  specification.
* **Trying out**, *illustrating*, *exemplifying*: running the code on some inputs
  to show that it does something useful. Perfectly respectable, but it is not
  testing.

~~~admonish note
Do not use the word "test" unless you have *both* a specification and code, and
you are *systematically* looking for violations of the former by the latter. Say
"illustrate", "exemplify" or "try out" instead.
~~~

Finally, a note on terminology for the different scopes of testing. *Unit
testing* is testing a module implementation against its module specification; a
*unit* here corresponds to a module, and its bespoke dependencies are stubbed
out. *Integration testing* tests a composition of modules against the
specification of the resulting component. *System testing* tests a deployable
system against its specification, and *acceptance testing* tests it against
business and user requirements. Everything in this chapter is about unit testing,
although the model-based technique at the end scales to much larger systems.

## Properties

A *logical statement* is a statement that is either true or false. A *property*
of one or more functions is a logical statement involving those functions that is
supposed to be true. Most properties we care about are *universally quantified*:

\\[ \forall x, y, \ldots\, .\, Q(x, y, \ldots) \\]

or, when the property only applies to some inputs,

\\[ \forall x, y, \ldots\, .\, P(x, y, \ldots) \Rightarrow Q(x, y, \ldots). \\]

A *partial formal specification* is typically a conjunction of such properties.

When we discussed type classes in [Chapter 1](chapter_1.html#type-class-laws) we
met the concept of *laws*, which are exactly properties that must hold for
instances of a given type class. For instance, if a type `T` is an instance of
`Eq` we expect `x == x` to evaluate to `True` for every value `x :: T`.

Properties are not, however, intrinsically linked to type classes. An example of
a property not related to any type class is the interaction between `length` and
`(++)`. This holds in general, but for concreteness consider lists of integers.
For any two lists `xs :: [Integer]` and `ys :: [Integer]` we have

```Haskell
length (xs ++ ys) = length xs + length ys
```

Another way of stating the same thing is that the function

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_LengthAppend}}
```

returns `True` for all possible arguments. We could come up with a number of test
cases, e.g.

```Haskell
{{#include ../haskell/Week5/Properties.hs:TediousTest}}
```

and test with something like `all (\(xs, ys) -> prop_lengthAppend xs ys)
tediousTestCases`, but this is quite tedious. QuickCheck automates the tedium away
by generating (somewhat) random inputs. The workhorse is `quickCheck`, which
accepts something `Testable` (explained below) and runs it with a number of
different inputs. Simply running `quickCheck prop_lengthAppend` covers more cases
than any unit test suite we would realistically have the patience to maintain.
The default is 100 tests, but if we want more we can run e.g.

```Haskell
quickCheck $ withMaxSuccess 10000 prop_lengthAppend
```

Of course, no amount of test cases is enough to argue total correctness, but
tuning the number of tests allows us to trade time for certainty.

### Counterexamples

Consider another property, stating that `(++)` is commutative:

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_AppendCommutative}}
```

Running `quickCheck prop_appendCommutative` quickly falsifies this theory and
prints out a counterexample such as `[0]` and `[1]`. QuickCheck is very useful
when we are genuinely unsure whether a property holds, since in practice false
properties often have easy-to-find counterexamples.

### The value of properties

Why do we care whether a property like `prop_lengthAppend` holds? It does, after
all, not directly say anything about the correctness of `length`, `(+)` or
`(++)`. For instance, given the obviously wrong definitions

```Haskell
_ + _ = 0
_ ++ _ = []
```

the property would still hold. The crucial observation is that in practice code
is seldom wrong in ways that happen to not violate any properties. Therefore
observing that a number of non-trivial properties involving some function are
true is a good proxy for correctness of the function.

But if properties are merely good proxies for correctness, why is that better
than testing correctness directly? The reason is that many properties are like
the ones we have seen so far: they can be expressed as a boolean condition with
variables that should hold for all choices of those variables. This is easy to
test using QuickCheck or similar systems. Direct testing is harder to automate.
That would require producing many test cases like `[] ++ [] == []` and `[1, 2] ++
[3, 4] == [1, 2, 3, 4]` and so on, which is manual (and error-prone) work.

### Where properties come from

Coming up with properties is the genuinely difficult part of property-based
testing, and it is a skill that improves with practice. The following catalogue
of recurring patterns is a good place to start when staring at a fresh module.

**Algebraic laws.** The type class laws of [Chapter 1](chapter_1.html#type-class-laws)
and the monad laws of [Chapter 2](chapter_2.html) are properties, and so are
associativity, commutativity, idempotence, distributivity, and the presence of
neutral or absorbing elements for whichever operations your module provides.

**Invariants.** Many data structures maintain an invariant that every operation
must preserve. Suppose we have a function that inserts an element into a sorted
list:

```Haskell
{{#include ../haskell/Week5/Properties.hs:Sorted}}
```

The invariant is that the result is sorted whenever the input is:

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_InsertSorted}}
```

The `(==>)` operator constructs a *conditional property*: test cases that do not
satisfy the precondition are *discarded* rather than counted as successes. We
shall see below that this innocent-looking property has a serious problem.

**Round-trip (inverse) properties.** Whenever a module offers two functions that
are supposed to undo each other - encode and decode, serialise and deserialise,
print and parse - their composition should be the identity:

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_ShowRead}}
```

This pattern is extremely productive. The printer and parser from
[Chapter 3](chapter_3.html) are a case in point, and you will be asked to
exploit it in the assignment.

**Comparison against a reference implementation.** If a simple, obviously correct
but perhaps hopelessly inefficient implementation is available, it can serve as
an *executable specification* for a clever one. Here is a merge sort:

```Haskell
{{#include ../haskell/Week5/Properties.hs:Msort}}
```

and here is its specification, in terms of the standard library's `sort`:

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_Msort}}
```

This is the single most useful pattern in industrial practice, and the whole of
the last part of this chapter is devoted to a generalisation of it.

**Metamorphic properties.** Sometimes we do not know what the right answer *is*,
but we do know how the answer must change when we change the input. Such
properties relate two runs of the same function:

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_MsortMetamorphic}}
```

They are invaluable when there is no reference implementation to compare against.

**Postconditions.** A weaker but still useful pattern: state what must be true of
the output, without pinning it down completely.

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_MsortPost}}
```

Note that `prop_msortSorted` alone is satisfied by the function `const []`, which
is why a good specification usually combines several properties. A conjunction of
weak properties can pin down behaviour surprisingly tightly: a sorting function
that produces a sorted list *and* preserves the multiset of elements is fully
specified.

## The `Testable` type class

The `quickCheck` function works on any type which is an instance of `Testable`.
The primary instances and their semantics for testing are worth going over:

* `()` is testable and succeeds if it returns `()` (the only possible value of
  the type `()`) and fails if an exception occurs.
* `Bool` is testable where `True` means success and `False` or an exception means
  failure.
* `Maybe a` is testable if `a` is. `Nothing` means that the test should be
  *discarded* and counted neither as a success nor as a failure. `Just result`
  has the meaning of `result`.
* `a -> b` is testable if `a` is `Arbitrary` (meaning that we have a way of
  generating values of that type; see the next section) and `b` is `Testable`.
  The semantics is that `f :: a -> b` succeeds if `f x :: b` succeeds for all
  `x :: a`. In practice this is tested by generating random values of `a`. Note
  that this instance applies recursively, so e.g. `Integer -> Integer -> Bool` is
  `Testable` because `Integer -> Bool` is `Testable`; and `Integer -> Bool` is
  `Testable` because `Bool` is `Testable`.
* `Property` is testable, and is the type of properties that have been enriched
  with extra information - preconditions, labels, expected coverage, and so on.

This works great. We can write down properties using familiar Haskell types
without even depending on the QuickCheck library. However, what if we want to
collect all our properties into a list for test organisation purposes? If we have
`p :: Integer -> Bool` and `q :: String -> Bool` then `[p, q]` is not well-typed.
The `Testable` type class has a method `property :: a -> Property` which converts
any `Testable` value into a `Property`. Think of `Property` as a `Testable` value
of unknown type. A list of properties should be of type `[Property]` and
constructed like `[property p, property q]`.

### Combinators on properties

A handful of combinators turn ordinary boolean functions into more informative
properties. The most important are:

```Haskell
(==>)       :: Testable prop => Bool -> prop -> Property
(===)       :: (Eq a, Show a) => a -> a -> Property
(=/=)       :: (Eq a, Show a) => a -> a -> Property
forAll      :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
counterexample :: Testable prop => String -> prop -> Property
ioProperty  :: Testable prop => IO prop -> Property
withMaxSuccess :: Testable prop => Int -> prop -> Property
```

`(===)` deserves special mention. It behaves like `(==)`, but when the test fails
it prints *both* values, which is almost always what you want:

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_LengthAppendEq}}
```

`(==>)` introduces a precondition, as we saw above. A property whose precondition
is false is *discarded*: QuickCheck generates a fresh test case instead. By
default QuickCheck gives up after discarding 1000 times as many cases as it needs
successes, and reports how many it threw away. As an example of a genuine
precondition, division:

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_DivMod}}
```

Discarding is wasteful when the precondition is unlikely to hold, and - as we
shall see - it silently biases the test data. Where possible it is better to
generate only values that satisfy the precondition in the first place. For simple
side conditions QuickCheck offers *modifier* newtypes that do exactly that:

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_DivModNonZero}}
```

Useful modifiers include `NonZero`, `NonNegative`, `Positive`, `NonEmptyList`,
`OrderedList`, `InfiniteList`, and `Fun` (which we return to below). They are all
ordinary `newtype`s with a tailored `Arbitrary` instance, so you can easily write
your own.

## Generators and `Arbitrary`

So far we have relied on QuickCheck automatically coming up with values for our
properties. The mechanism behind this is the `Arbitrary` type class alluded to
above, which defines a method `arbitrary :: Gen a` that is supposed to define the
"canonical" generator for the type. Hence, we first need to understand
generators.

### Generator basics

As a first approximation the type `Gen a` represents a probability distribution
over elements of `a`. It can also be thought of as a computation that can depend
on random choices. Concretely it is essentially

```Haskell
newtype Gen a = MkGen { unGen :: QCGen -> Int -> a }
```

that is, a function from a pseudo-random seed and a *size* parameter to a value.
The seed is threaded through in the style of a state monad, and the size is
available in the style of a reader monad; both are managed by QuickCheck, and
generators are built from primitives and monad operations rather than by
manipulating them directly.

The simplest generator is `pure x` which produces the value `x` with probability
1. Given a list of generators `gs :: [Gen a]` the generator `oneof gs` chooses one
of the generators with equal probability. For instance `oneof [pure "heads", pure
"tails"]` produces `"heads"` and `"tails"` with equal probability, but `oneof
[pure "heads", pure "tails", pure "tails"]` is biased in favour of `"tails"`.

It is also possible to explicitly control the bias using `frequency`, which is
like `oneof` but allows specifying the weight of each option. The biased example
using `oneof` would be written more idiomatically as `frequency [(1, pure
"heads"), (2, pure "tails")]`.

The other core primitives are `elements :: [a] -> Gen a`, which picks uniformly
from a list of *values* (rather than generators), and `choose :: Random a => (a,
a) -> Gen a`, which picks uniformly from an inclusive range. For integers,
`chooseInt :: (Int, Int) -> Gen Int` avoids a class constraint.

QuickCheck has a function called `sample` which takes a generator and prints 10
example values. This is quite useful to get a rough sense of what a generator
produces and is often sufficient to spot simple biases like in the previous
example. Its sibling `sample' :: Gen a -> IO [a]` returns the values instead of
printing them, and `generate :: Gen a -> IO a` produces a single one.

~~~admonish tip
Whenever you write a non-trivial generator, run `sample` on it. It takes five
seconds and it will save you hours. Later in this chapter we shall see how to
automate the same sanity check so that it runs as part of your test suite.
~~~

### Recursive generators

QuickCheck has a combinator called `listOf` which generates `[a]` given a
generator for `a`. Let us generate a list of integers using the standard integer
generator given by its `Arbitrary` instance. An example output is:

```
> sample $ listOf (arbitrary :: Gen Integer)
[]
[2]
[1,0]
[1,6,0,-4]
[-6,6,6]
[-7,5,2]
[8,-8,-1,12,-4,11,-2,9,5]
[13,-14,2,5,9,8,-7,3,-9,-8,-13,-7,-1,1]
[8,-10,14,15,-9,-3,-5,-8,9,-2,-5,8,-16,-4,-15]
[18,-8,-3,15,13,1,6,-2]
[-12,20,18,10]
```

Note that there is a decent spread both in the length of the list and the
individual integer values. How would we go about implementing a combinator like
`listOf`? A first attempt might be:

```Haskell
{{#include ../haskell/Week5/Properties.hs:List1}}
```

This is a choice between an empty list and a list consisting of an element
generated by `g` followed by a recursively generated list. Each choice has equal
chance, so 50% of lists will be empty, another 25% will have just a single
element and so on.

Alas, the distribution leaves something to be desired:

```
> sample $ list1 (arbitrary :: Gen Integer)
[0]
[]
[]
[]
[1,8,7]
[]
[1,-12]
[]
[-16,7,0]
[4,3]
[]
```

Every other sample is an empty list and long lists are exceedingly unlikely,
which makes this generator inefficient for exploring the search space. A second
attempt might be to use `frequency` to introduce a bias towards longer lists:

```Haskell
{{#include ../haskell/Week5/Properties.hs:List2}}
```

The resulting distribution is better, but the length of the list now follows a
*geometric* distribution with mean 9 - the generator has no idea how large a test
case is currently wanted, so it produces lists of roughly the same length whether
QuickCheck is looking for a small first test case or a large hundredth one.

```
> sample $ list2 (arbitrary :: Gen Integer)
[]
[1,1,1,0,-2,-1,-1,1,2,-2,-1,-1,2,-1,-1,2,-1,1,1]
[4]
[]
[-4,-7,-4]
[-4,10,-6,4,5,-7,9]
[-11,-3,-7,-5,-2,2,-3,6,-1,12,9,-9]
[-7,-8,-8,10,1,-9,-9,8,-9,2,4]
[15,1]
[-13,13,5,14,4,5,-8,2,-13,9,4,0,-15]
[-5,17,6,14,3,4,-18,-1]
```

We could try adjusting the bias, but no matter what value we use the length will
follow the same shape of distribution, which is not really what we want.

For our third attempt, we exploit the fact that `Gen` is a monad. First generate
a non-negative integer `n`, and then generate a list of length `n`:

```Haskell
{{#include ../haskell/Week5/Properties.hs:List3}}
```

Now the distribution is similar to QuickCheck's `listOf`.

```
> sample $ list3 (arbitrary :: Gen Integer)
[]
[0,-2]
[0,2,-1,2]
[]
[-8]
[8,-6,0,-10]
[-8,5,10,3]
[-1,-1,-12]
[-16,-1,-11,6,-1,-13,14,-1,8,-1]
[-4,14,-7,-9,9,-13,1,6,-18,10]
[12,-16,11,-3,20]
```

### Size-dependent generators

Why did `list3` work? Because `arbitrary :: Gen Int` respects the *size*
parameter, and `list3` inherits that behaviour by accident. It is better to be
explicit about it.

When testing a property it is often a good idea to start with small values and
then gradually increase the complexity of the test cases. QuickCheck uses this
approach by giving generators access to a size parameter which under default
settings starts at 0 and increases by 1 every test up to a maximum of 99.

The size can be accessed directly using `getSize :: Gen Int` but usually a neater
approach is to use the combinator `sized :: (Int -> Gen a) -> Gen a` which turns a
size-dependent generator into an ordinary one. A good generator respects the size
parameter, so our list generator is more idiomatically written as:

```Haskell
{{#include ../haskell/Week5/Properties.hs:List4}}
```

This is essentially the definition of `listOf` in QuickCheck (where `go` is known
as `vectorOf`). The related combinators `resize :: Int -> Gen a -> Gen a` and
`scale :: (Int -> Int) -> Gen a -> Gen a` let you locally override the size,
which is the standard trick for making a recursive generator terminate: recurse
at a strictly smaller size, and produce a leaf when the size reaches zero.

~~~admonish warning
A recursive generator that does *not* shrink the size on recursive calls may
diverge, or produce test cases so enormous that the test suite appears to hang.
If your property-based test suite suddenly stops terminating, an unbounded
generator is the first thing to suspect.
~~~

### Generating functions

Some properties quantify over *functions*, not just over data. The functor law
`fmap (f . g) == fmap f . fmap g` is an example. QuickCheck can generate
functions, print them, and even shrink them, via the `Fun` modifier:

```Haskell
applyFun :: Fun a b -> (a -> b)
```

A property taking a `Fun a b` argument receives a random function that QuickCheck
knows how to display as a finite table of input/output pairs, which makes
counterexamples readable. The type `a` must be an instance of `CoArbitrary` and
`Function`, both of which can be derived for most types. We use this facility in
the next section.

## Testing an abstract data type against a reference implementation

We now have enough machinery to attack a realistic specification problem. Recall
from [Chapter 2](chapter_2.html#the-state-monad) the state monad. An *abstract
data type* (ADT) is an abstract type together with operations on it and a precise
description of what those operations do. The state monad is a good example,
because it has (at least) two quite different implementations: a purely
functional one that threads the state explicitly, and an imperative one built on
an `IORef` as introduced in [Chapter 4](chapter_4.html#io-references).

```Haskell
{{#include ../haskell/Week5/Properties.hs:FState}}
```

```Haskell
{{#include ../haskell/Week5/Properties.hs:IState}}
```

There are two standard ways of specifying such an ADT, and both are testable.

### Method 1: equational laws

The first is to state *universal equational properties* that the operations must
satisfy. Beyond the monad laws, a state monad should satisfy the following
*get/put laws*, for all `s` and `t`:

```Haskell
put s >> get   =  put s >> return s
put s >> put t =  put t
get >> get     =  get
```

To test these we need to say what the `=` means for two state computations. Two
computations are equivalent exactly when they cannot be told apart by any
observation, and the only observation we can make of an `FState s a` is to run it
on an initial state and look at the resulting value and final state. So
*observational equivalence* is:

```Haskell
{{#include ../haskell/Week5/Properties.hs:StateEquiv}}
```

Note what has happened here: the universal quantification over initial states has
turned into a QuickCheck property, because `property` applied to a function
generates the argument. We can now write the laws down directly:

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_StateLaws}}
```

~~~admonish note
`(~=)` is an *approximation* of observational equivalence: it only tries finitely
many initial states, and it fixes the state type to `Int`. This is typical.
Deciding equivalence of programs is undecidable in general; the whole point of
testing is that a good approximation is cheap and a proof is expensive.
~~~

### Method 2: a reference implementation

The second way to specify an ADT is to give a *reference implementation* (also
called an executable specification or a model implementation) and require that
the real implementation be observationally indistinguishable from it. Here
`FState` plays the role of reference and `IState` the role of the implementation
under test.

The two have different types, so we cannot compare them directly. We instead
provide a conversion that runs an `IState` computation in the same way we run an
`FState` computation, namely from an initial state to a value and a final state:

```Haskell
{{#include ../haskell/Week5/Properties.hs:RunIState}}
```

Now "the implementation simulates the reference" is expressible:

```Haskell
{{#include ../haskell/Week5/Properties.hs:StateSimulates}}
```

and the specification of the implementation is a handful of one-line properties:

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_StateSimulates}}
```

The last of these is where `Fun` earns its keep: `bindI` takes a *function* as its
second argument, so to test it at all we must generate one. `applyFun` turns the
generated `Fun Int Int` into an ordinary function, and if the property fails
QuickCheck prints the function as a readable table rather than as `<function>`.

~~~admonish note
`prop_simBind` only tests `bindI` at continuations of the very restricted shape
`putI . f`. Testing `bind` for *arbitrary* continuations requires generating
random *computations*, not just random functions - which means generating values
of a datatype describing computations, and interpreting that datatype in both
implementations. That is exactly the technique we develop in the last part of
this chapter.
~~~

## Test data design: partitioning and coverage

A property-based test is only as good as the data it is run on. Random generation
is excellent at producing large numbers of *typical* values, but it is poor at
hitting the rare *boundary* values where bugs actually live. The classical
technique for finding those is *input partitioning*, and the two approaches
combine rather than compete.

### Input partitioning

*Input partitioning* (also called *equivalence partitioning*) is a design method
for constructing test data by inspecting the property to be checked. Given a
universally quantified property, partition its valid inputs into a *finite* set of
pairwise disjoint subsets whose union is the whole input space. From each
partition, choose

* a *typical* element, and
* one or more *boundary* elements;

and determine the expected output for each according to the *specification* -
not the code. The boundary elements are where the falsification power lies.

For example, a generally useful partitioning of numeric types is:

* For 32-bit two's-complement integers: negative numbers, `{0}`, positive
  numbers, with values such as `-2147483648`, `-137`, `-1`, `0`, `1`, `2377`,
  `2147483647`.
* For 64-bit IEEE 754 floating-point numbers: negative infinity, negative
  numbers, `{0}`, positive numbers, positive infinity, and NaN - remembering that
  `0.0` has two representations, and that NaN is not equal to itself.

Note how most of the interesting values are boundary values, and note how
unlikely a uniform random generator is to produce any of them.

~~~admonish warning title="Test design is not the test suite"
Specification-driven testing is the systematic, documented *process* of analysing
a specification to arrive at test data with high falsification power. The test
suite is the *output* of that process. By itself a test suite says nothing about
its own falsification power; only the reasoning that produced it does. This is
why your reports are asked to describe *how* you designed your tests, not merely
to list them.
~~~

### Seeing what you are actually testing

The bridge between the two approaches is that QuickCheck can *report* the
distribution of the test data it generated, so that we can check whether our
partitions are being hit. The relevant combinators are:

```Haskell
classify :: Testable prop => Bool -> String -> prop -> Property
label    :: Testable prop => String -> prop -> Property
collect  :: (Show a, Testable prop) => a -> prop -> Property
tabulate :: Testable prop => String -> [String] -> prop -> Property
```

`classify` reports the percentage of tests for which a condition held, `label`
and `collect` group test cases by a string, and `tabulate` maintains a named
table of frequencies. Let us apply them to the sorted-insertion property from
earlier, which used `(==>)`:

```Haskell
{{#include ../haskell/Week5/Properties.hs:LengthBucket}}
```

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_InsertSortedTabulate}}
```

The result is sobering:

```
> quickCheck prop_insertSorted'
*** Gave up! Passed only 77 tests; 1000 discarded tests (64% trivial).

length of xs (77 in total):
74% 1-4
26% 0
```

The property passes, but it has tested almost nothing. Random lists are almost
never sorted, so 1000 test cases were thrown away, and the ones that survived
were sorted *by accident* - which is to say, they were nearly all of length 0, 1
or 2. A precondition that is rarely satisfied does not merely waste time: it
quietly restricts the test data to a tiny and unrepresentative corner of the
input space.

The fix is to generate values that satisfy the precondition by construction, and
to feed them to the property with `forAll`:

```Haskell
{{#include ../haskell/Week5/Properties.hs:GenSorted}}
```

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_InsertSortedForAll}}
```

```
> quickCheck prop_insertSorted''
+++ OK, passed 100 tests.

length of xs (100 in total):
48% >=20
35% 5-19
11% 1-4
 6% 0
```

Much better - no discards, and a healthy spread of list lengths.

### Demanding coverage

Inspecting a table by eye is fine while developing a generator, but it does not
help six months later when someone changes the generator and silently destroys
the distribution. QuickCheck can therefore be asked to *require* a given level of
coverage, and to fail the test if it is not met:

```Haskell
cover         :: Testable prop => Double -> Bool -> String -> prop -> Property
checkCoverage :: Testable prop => prop -> Property
```

`cover p b s` asserts that at least `p` percent of test cases satisfy `b`, and
labels them `s`. On its own this only prints a warning; wrapping the property in
`checkCoverage` turns insufficient coverage into a *test failure*. Note that
`checkCoverage` does something statistically non-trivial: rather than testing a
fixed number of times and comparing percentages, it keeps running more tests
until it can conclude, with high confidence, either that the requirement is met
or that it is not. This is why the number of tests reported below is not 100.

Let us apply the partitioning idea to insertion into a sorted list. The
interesting partitions are: the empty list, insertion before all elements,
insertion after all elements, and insertion of a value already present.

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_InsertSortedCover}}
```

Our carefully improved generator turns out to be badly inadequate:

```
> quickCheck prop_insertSortedCover
*** Failed! Insufficient coverage (after 400 tests):
17.5% insert a duplicate
 7.8% insert at back
 5.8% empty list
 5.8% insert at front

Only 5.8% insert at front, but expected 20.0%
```

Of course: `x` is generated completely independently of `xs`, so in a list of
thirty elements it will almost always land somewhere in the middle. The
boundaries - which is where an off-by-one error in `insertSorted` would show up -
are hardly ever exercised.

The remedy is to let the design of the test data drive the generator: generate
`x` *in relation to* `xs`, deliberately producing boundary cases a good fraction
of the time.

```Haskell
{{#include ../haskell/Week5/Properties.hs:GenInsertion}}
```

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_InsertSortedCover2}}
```

```
> quickCheck prop_insertSortedCover'
+++ OK, passed 800 tests:
30.0% insert a duplicate
27.8% insert at back
26.8% insert at front
 5.4% empty list
```

This is the practical synthesis of the two techniques: *input partitioning
determines what the distribution ought to look like, `cover` states that intention
as a machine-checkable requirement, and the generator is written to satisfy it.*
The random generator then fills in the typical values within each partition for
free.

~~~admonish note title="Where implementation knowledge is allowed"
Purists will object that peeking at the implementation is not specification-driven
testing. The distinction that matters is this: the *expected output* must always
come from the specification, never from the code - otherwise the test merely
asserts that the code does what it does. But knowledge of the implementation may
legitimately inform *which inputs we generate*, since a test case that never
reaches a suspicious branch cannot possibly falsify anything. Deriving test data
from the code in this way is called *structural* (or white-box) testing, and it
complements rather than replaces specification-driven testing.
~~~

## Shrinking

Suppose we define our own type of pairs with an instance of `Arbitrary`:

```Haskell
{{#include ../haskell/Week5/Properties.hs:Pair}}
```

We can now define a version of commutativity for `(++)` that takes the input as a
`Pair`:

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_AppendCommutativePair}}
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
member `shrink :: a -> [a]` which takes a value and produces a list of *shrinks*,
i.e. "slightly smaller" values. The idea is that if `x` is a counterexample to
some property then any of the elements in the list `shrink x` could also be
counterexamples.

When QuickCheck finds a counterexample `x` it tests the property for each shrink
of `x`. If that results in another, by definition simpler, counterexample the
process repeats recursively until the (locally) simplest counterexample is
reached.

For a value `Pair x y` a natural notion of "slightly smaller" is a pair which is
slightly smaller in *either* the first component *or* the second component. The
complete `Arbitrary` instance is thus:

```Haskell
{{#include ../haskell/Week5/Properties.hs:PairShrink}}
```

Why not shrink both components simultaneously? Well, suppose `x'` is slightly
smaller than `x` and `y'` is slightly smaller than `y`. Then `Pair x' y'` is
slightly smaller than `Pair x y'` (or `Pair x' y`) which in turn is slightly
smaller than `Pair x y`, so assuming that either `Pair x y'` or `Pair x' y` is
also a counterexample the process would reach `Pair x' y'` in two steps. In
general, there is a trade-off between efficiency (i.e. not producing too many
shrinks) and likelihood of finding the very simplest counterexample.

~~~admonish warning title="The rule every shrink function must obey"
Every value in `shrink x` must be *strictly smaller* than `x` under some
well-founded ordering. If a shrink can ever produce a value that is not smaller,
shrinking may loop forever, and your test suite will appear to hang after finding
a bug - which is the worst possible moment for it to do so. `shrink _ = []` is
always a legal (if useless) definition, and is a perfectly reasonable starting
point.
~~~

There is a second pitfall, which we shall meet in earnest in the next section:
shrinking is not aware of any preconditions. If your generator produces values
satisfying an invariant that `shrink` does not preserve, QuickCheck will happily
"simplify" your counterexample into a value that was never a legal input in the
first place, and you will spend an afternoon debugging a non-bug. When using
`forAll` with a custom generator, use `forAllShrink` to supply a matching shrink
function - and make sure it preserves the invariant.

## Running properties with Tasty

The Tasty testing framework we have used since [Chapter 1](chapter_1.html#testing)
has support for QuickCheck via the package `tasty-quickcheck`. For example,
`testProperties "properties" props` is a simple test tree, given `props ::
[(String, Property)]`, and `testProperty name prop` produces a single one.

This test tree exposes options for Tasty on the command line. The most useful are:

```
$ cabal test --test-option --quickcheck-tests=10000
$ cabal test --test-option --quickcheck-replay=42
$ cabal test --test-option --quickcheck-verbose
```

The first controls the number of tests, and is the standard way of trading time
for confidence. The second fixes the random seed, which makes a failing run
*reproducible* - indispensable when you are chasing a bug that only shows up once
in a thousand runs. The same effects can be had programmatically with
`withMaxSuccess` and `quickCheckWith stdArgs { replay = ... }`.

~~~admonish tip
A property that fails intermittently in CI and passes on your machine is a
property whose seed you have not recorded. Note the seed from the failing run and
replay it.
~~~

## Testing stateful systems

The examples above assume that we are testing a pure function where properties
involve a relationship between the inputs and outputs of the function.
Regrettably however, many computer systems are *stateful*: they encapsulate some
internal data which is modified in response to commands, and partially revealed in
response to requests. A conventional mutable data structure, such as a resizable
array, is a simple example of a stateful system, and is the one we shall study
below. The definition covers much larger systems too - a network service, or the
concurrent servers of [Chapter 6](chapter_6.html).

Large stateful systems are often complicated and error-prone, and hence it is very
desirable to be able to test them effectively. It turns out that property-based
testing is also an effective approach for such systems (and in industry, this may
even be the most impactful use case), although it requires us to build some
additional infrastructure.

The basic idea is a generalisation of the reference-implementation technique we
applied to the state monad above. We take the so-called *software under test*
(SUT) and construct a *model* that captures the most important properties of the
SUT. In many cases a stateful system has implementation details that are important
to its operation (such as caching, optimisations, persistency in a database,
integration with other systems, etc), but which are not part of its external
interface. A model is a program that imitates some subset of the behaviour of the
SUT, and is typically much simpler than the SUT. We then randomly generate
*commands* that interact with the SUT and the model, and test that the observable
behaviour is the same. Essentially, the model is an executable specification that
we use to validate the behaviour of the SUT.

One very important detail is that there is no requirement that the SUT is
implemented in the same language as the property-based testing framework (e.g.,
Haskell in our case). Indeed, it is a common approach to construct a model in
Haskell and use QuickCheck to verify that some other network-based system matches
the behaviour of the model. This is particularly useful when the choice of
technology for the SUT is constrained due to efficiency or integration concerns.

### A sample stateful system

The SUT we will test in the following does happen to be implemented in Haskell for
simplicity, although we stress that this is not required for the approach to work.
Specifically, our SUT is a datatype `DynamicArray a` that implements mutable
arrays of elements of type `a` with efficient support for appending elements.
Since the array is mutable, all operations on it will be in `IO`. The
implementation details of the array are not important, and so will be covered
somewhat briefly.

First we will need to import some library functions.

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_Imports}}
```

Our representation of `DynamicArray` will be quite similar to what you may have
seen in systems programming classes, or implemented yourself. The idea is to have
an underlying array with room for more elements than have actually been inserted
yet, with the inserted elements all being at the front of the array, such that
there is room to grow at the end. We call the size of this array the *capacity*.
When the number of elements inserted by the user exceeds the capacity, then we
bump the capacity by some factor, allocate a new array of that size, then copy the
old elements to the new array. If we always double the capacity, then it can be
shown that appending an element can be done in amortised constant time.

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_DynamicArray}}
```

Because we need to modify both the number of used elements and the capacity, we
represent these as mutable `IORef`s. The underlying array is an `IOArray Int a`,
which is a mutable (but non-resizable) type provided by Haskell. The `Int` type
argument is the index type, which can be used to represent multidimensional
arrays, but we will not make use of this.

~~~admonish note

The interface to `IOArray` is through various class-polymorphic functions defined
by `Data.Array.MArray`. For simplicity of exposition, the types in the following
have been monomorphised to be specific to `IOArray`.

~~~

An `IOArray` is produced by the function `newArray_` of the following type:

```Haskell
newArray_ :: (Int,Int) -> IO (IOArray Int a)
```

The `(Int,Int)` pair is the smallest and largest valid index. These arrays do not
have to start at zero, although ours do. Initially, all elements of the array will
be undefined. Reading an undefined element will cause an IO exception.

We arbitrarily decide that the initial capacity of our dynamic arrays will be 10.

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_newDynamicArray}}
```

When indexing an array we check whether the index is in-bounds, returning
`Nothing` if not.

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_index}}
```

Inserting always succeeds and produces no result beyond modifying the array, but
we have to resize the underlying array if the new element causes the capacity to
be exceeded. This is done by allocating a new array and copying the contents of
the old one.

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_insert}}
```

Overwriting an existing element is quite similar to indexing. We return `Nothing`
on out-of-bounds, and `Just ()` on success.

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_write}}
```

Our final operation allows the deletion of an element anywhere in the array. This
is a somewhat costly operation, as we have to shift all elements after the deleted
one left. Further, to avoid using too much memory, if the capacity is too large
after the deletion, we shrink the array back down to a smaller size. As before, we
return `Nothing` in case the index is out of bounds.

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_delete}}
```

### Defining a model

There is a lot of intricate index-fiddling code in the above. Can we really be
sure it is correct? At a semantic level, what `DynamicArray` is doing is not
complicated - it is merely maintaining a sequence of elements. The complexity
comes from our desire for certain performance characteristics. Let us define a
model that captures the central behaviour of `DynamicArray`, but without caring
about operational matters such as performance.

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_Model}}
```

A model is described by its internal state and by which *commands* can be sent to
it, and which *responses* may be produced in return. These commands usually
resemble the API of the SUT, but perhaps simplified in various ways. Due to the
simplicity of the system we are testing here, the commands are however very
similar to the API of the SUT.

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_Command}}
```

~~~admonish tip title="Command-query separation"
Note that our commands fall into two groups: those that change the state
(`Insert`, `Write`, `Delete`) and those that merely observe it (`Index`). A design
discipline in which every operation is either a *command* that changes state and
returns nothing, or a *query* that returns something and changes nothing, is known
as *command-query separation*. It is not a rule you must follow, but systems that
respect it are markedly easier to test, because the observations are cleanly
separated from the state transitions. If you find that a stateful system is
painful to model, an operation that both mutates and reports is often the reason.
~~~

We then define functions corresponding to executing each of the commands. Each
function accepts the command parameters and the current model state, and returns a
new model state, as well as a response. For example, the function corresponding to
an insertion is defined as follows.

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_cmdInsert}}
```

We define functions for the remaining commands, and finally a `step` function for
simulating the effect of running a certain `Command` on a `Model`:

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_step}}
```

Similarly, we also define a function for executing a `Command` on our SUT. This
involves mapping the SUT API to the `Command`/`Response` types, which in our case
is quite simple, but can sometimes be a bit laborious.

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_exec}}
```

### Specifying a property

We denote a sequence of commands as a *program*. The testing problem is now to
ensure that the SUT and the model behave identically for all programs.

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_Program}}
```

First of course, we have to define appropriate `Arbitrary` instances. We are going
to define these in the simplest way imaginable, and then - as in the pure case
above - find out how bad they are.

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_Arbitrary_Command}}
```

Let us look at what programs end up being generated.

```
> sample (arbitrary :: Gen (Program Int))
Program []
Program []
Program [Index (-3),Delete (-3),Delete 2]
Program []
Program [Insert (-8),Delete 6,Delete 7,Insert 2,Insert (-2)]
Program [Write (-1) (-10),Insert 9,Index 5]
Program [Delete (-9),Write (-6) 3,Index (-8),Write 10 0,Index 8,Insert (-10)]
Program [Index (-9),Write (-4) 0,Delete (-11),Write 2 (-9),Index 4,Index 2,Insert (-1),Insert 2,Delete 10,Delete (-14)]
Program [Write 14 (-12),Delete (-3),Delete (-1),Delete (-15),Index (-2),Write (-5) (-15),Index (-9),Delete 4,Write (-7) (-2),Index 0,Index 10,Insert (-11),Delete (-6),Index 16,Delete (-13)]
Program [Index 11]
Program [Write (-14) (-11),Write (-11) (-13),Insert (-11),Index (-9),Insert (-17),Index (-19),Delete (-18),Delete 6]
```

It is clear that once again, we generate a good number of empty programs, but we
also generate a good many programs that barely touch the array at all. Look at how
many of them perform `Index` or `Delete` commands - very often at negative indices
- without first inserting any elements. It is good and necessary to test these
cases, but they are clearly generated too often.

Next we define a procedure for executing a `Program a` on both a `DynamicArray a`
and a `Model a`, producing a boolean that indicates whether the same responses
were observed. Recall that we cannot directly inspect the internal state of a
`DynamicArray`, but we *can* check its observable behaviour.

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_runProgram}}
```

Finally, we just need to construct a property that uses `runProgram`. The crucial
building block is the function `ioProperty`, which allows us to turn an arbitrary
IO operation into a `Property`:

```Haskell
ioProperty :: Testable prop => IO prop -> Property
```

We must of course be very careful when using this function, as it is easy to use
it to construct a property that behaves nondeterministically, which can make it
difficult for QuickCheck to shrink test cases properly. In our case we are merely
using it to construct the initial dynamic array and to execute `runProgram`:

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_prop_array}}
```

Finally we can verify the property.

```
> quickCheck prop_array
+++ OK, passed 100 tests.
```

### Making failures informative

Passing tests are gratifying but uninformative. Let us introduce a bug: remove the
line `writeArray arr' used x` from the `else`-branch in the definition of
`insert`, so that a resize loses the element that triggered it. Rerunning the
tests gives, some of the time:

```
> quickCheck prop_array
*** Failed! (after 99 tests and 14 shrinks):
Exception:
  MArray: undefined array element
  CallStack (from HasCallStack):
    error, called at libraries/array/Data/Array/Base.hs:812:16 in array-0.5.8.0:Data.Array.Base
Program [Insert (-60),Insert (-12),Insert (-92),Insert 78,Insert (-4),Insert (-67),Insert 66,Insert (-82),Insert (-65),Insert (-38),Insert 38,Delete 6,Index 9]
```

We are not told exactly what is wrong (this is an exception thrown from the
underlying array library), but we are given a sequence of commands that reliably
leads to the problem, and which we can use to debug it.

When the SUT does not crash but merely disagrees with the model, `runProgram`'s
`Bool` result is much less helpful: we are told *that* some program failed, but
not *which command* diverged, nor what the two sides said. Since we have the
information at hand, we may as well report it, using `counterexample` to attach a
message to a failing property:

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_checkProgram}}
```

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_prop_array2}}
```

This costs four lines and saves a great deal of debugging. As a rule, when a
property is more complicated than a single equation, spend the effort to make its
failure output say what actually went wrong.

### Generating programs that make sense

The naive generator produces far too many commands that are rejected out of hand.
The fix is the same one as for sorted lists: use the *model* - which we have
right there - to generate commands that make sense in the state reached so far.

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_genCommand}}
```

Two things are going on here. First, whenever the array is non-empty we generate
in-bounds indices a good fraction of the time, by choosing them from the range the
model says is valid. Second, the weight of `Insert` *depends on the current model
state*: it is high when the array is small and drops as the array fills up, so
that programs tend to drive the array to an interesting size and then keep it
there rather than growing without bound. This kind of state-dependent weighting is
the bread and butter of writing generators for stateful systems.

Generating a program is then a fold over model states: generate a command for the
current state, step the model, and continue.

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_genProgram}}
```

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_prop_array3}}
```

Note the use of `forAllShrink`, which supplies a shrink function alongside the
custom generator; plain `forAll` does not shrink at all, and a stateful
counterexample that has not been shrunk is nearly unreadable.

### Preconditions and shrinking

We got away with reusing the ordinary list `shrink` for `Program` because *every*
command is meaningful in *every* state: an out-of-bounds `Index` is not a
programmer error, it is a legitimate request with a `Failure` response. Many
stateful systems are not like that. If your SUT would crash, or has undefined
behaviour, when a command is issued in the wrong state, then that command has a
*precondition*, and both generation and shrinking must respect it - shrinking
especially, since dropping an earlier command can easily invalidate a later one.

The standard recipe is to make the precondition explicit and filter the shrinks
through it:

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_precondition}}
```

Here `shrinkList (const []) cmds` produces the sub-lists of `cmds` obtained by
deleting commands (but does not attempt to shrink the individual commands), and
`validProgram` re-runs the precondition check along the model trace. For our
`DynamicArray` this particular precondition would be too strong - we *want* to
test out-of-bounds accesses - but the pattern is exactly what you need for a
system where illegal commands must not be issued.

### Partitioning the state space

For a pure function we partitioned the space of *inputs*. For a stateful system we
must partition the space of *states*, and design commands that reach the
interesting ones. For our dynamic array, the interesting states are those around
the capacity boundaries: just below the initial capacity, just above it (so that a
resize has happened), and having deleted back down again (so that a shrink has
happened).

Since the model records the sequence of states a program passes through, we can
compute these observations directly from the program:

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_trace}}
```

and then state our coverage requirements:

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_coverage}}
```

```Haskell
{{#include ../haskell/Week5/Stateful.hs:Stateful_prop_coverage}}
```

Applying this to the naive generator is illuminating:

```
> quickCheck prop_arrayNaiveCoverage
*** Failed! Insufficient coverage (after 800 tests):
92.5% some command out of bounds
16.6% grows past initial capacity
14.9% deletes while large

maximum number of elements (799 in total):
63.8% 1-10
19.5% 0
14.5% 11-20
 2.1% >20

Only 16.6% grows past initial capacity, but expected 30.0%
```

Over 92% of programs contain an out-of-bounds command, but fewer than one in five
ever grows the array past its initial capacity - which is to say that the
resizing code, by far the most intricate part of the implementation, is barely
tested at all. The state-aware generator does much better:

```
> quickCheck prop_arrayCoverage
+++ OK, passed 400 tests:
74.0% some command out of bounds
47.5% grows past initial capacity
43.0% deletes while large

maximum number of elements (400 in total):
47.5% 11-20
44.5% 1-10
 8.0% 0
```

This is not a cosmetic difference. Reintroduce the bug in `insert` from above and
run each version twenty times, at the default 100 tests per run:

| generator      | runs that caught the bug |
|----------------|--------------------------|
| naive          | 7 out of 20              |
| state-aware    | 20 out of 20             |

A test suite that reports success two times out of three on code that is
definitely broken is worse than no test suite at all, because it is believed. The
coverage check is what tells you - *before* you rely on it - that your test data
never reaches the code you care about.

~~~admonish tip
The general lesson is worth stating on its own: for a stateful system, the hard
part is almost never writing the model, and almost always generating command
sequences that reach interesting states. Budget your effort accordingly.
~~~

### Perspective

You may rightly object that the amount of infrastructure needed to perform testing
of this kind is rather large - our test harness is more lines of code than the
`DynamicArray` implementation itself. This is a fair objection, but real systems
tend to grow in complexity much faster than their models, so this is a technique
that scales well as the SUT grows complicated, although it is sometimes
impractical for testing small systems.

It is also worth realising how little of the above depends on the SUT being
Haskell. The technique requires only that there is some handle representing the
SUT (in our case, `DynamicArray`) and some operation (which may be in `IO`) that
performs state changes based on `Command`s. There is nothing that fundamentally
prevents us from testing a remote network service this way, or the control system
for a robot, or a C library invoked through a foreign-function interface. We might
in those cases need to write more complicated `exec` functions, of course.

Because the pattern is so stereotyped, it has been packaged into libraries -
`quickcheck-state-machine` and `quickcheck-dynamic` are the well-known Haskell
ones - which supply the generation, shrinking and reporting machinery so that you
need only provide the model, the commands, and the `exec` function. We have built
it by hand here because the machinery is worth understanding, and because in
practice you will frequently need to adapt it.

Finally, a warning about the limits of the technique. Everything above assumes the
commands are executed *sequentially*. Testing *concurrent* code is substantially
harder, because a property must hold for *all* possible schedules - all
interleavings of all concurrently executing threads - not merely for the schedule
that your machine happened to choose today. The standard specification for a
concurrent stateful system is *linearisability*: the observed behaviour of a set
of concurrent operations must be explainable by *some* sequential ordering of them
that respects the real-time order of non-overlapping operations. Checking this
requires both a way to explore many schedules and a search for a valid
linearisation, and is beyond the scope of AP - but it is built on exactly the
model-based foundation developed here.
