# Property-Based Testing

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
