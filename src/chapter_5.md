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
length (xs ++ ys) == length xs + length ys
```

Another way of stating the same thing is that the function

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_LengthAppend}}
```

returns `True` for all possible arguments. We could come up with a number of test cases, e.g.

```Haskell
{{#include ../haskell/Week5/Properties.hs:TediousTest}}
```

and test with something like `all (\(xs, ys) -> prop_lengthAppend xs ys)
tediousTestCases`, but this is quite tedious. QuickCheck automates this tedium
away by generating (somewhat) random inputs. Simply running `quickCheck
prop_lengthAppend` covers more cases than any unit test suite that we would
realistically have the patience to maintain. The default is 100 tests, but if we want more we can run e.g.

```Haskell
quickCheck $ withMaxSuccess 10000 prop_lengthAppend
```

### Counterexamples

Consider another property, stating that `(++)` is commutative:

```Haskell
{{#include ../haskell/Week5/Properties.hs:Prop_AppendCommutative}}
```

Running `quickCheck prop_appendCommutative` quickly falsifies this theory and
prints out a counterexample such `[0]` and `[1]`. QuickCheck is very useful
when we are genuinely unsure whether a property holds, since in practice false
properties usually have easy-to-find counterexamples.

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
  `x :: a`. In practice this is tested by generated random values of `a`. Note
  that this instance applies recursively so e.g. `Integer -> Integer -> Bool`
  is `Testable` because `Integer -> Bool` is `Testable` because `Bool` is
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
primitives and the fact that `Gen` is a monad.

The simplest generator is `pure x` which produces the value `x` with
probability 1. Given a list of generators `gs :: [Gen a]` the generator `oneof
gs` chooses one of the generators with equal probability. For instance `oneof
[pure "heads", pure "tails"]` produces `"heads"` and `"tails"` with equal
probability, but `oneof [pure "heads", pure "tails", pure "tails"]` is biased
in favour of `"tails"`.

QuickCheck has a function called `sample` which takes a generator and prints 10
example values. This is quite useful to get a rough sense of what a generator
produces and is often sufficient to spot simple biases like in the previous
example.

### Size Dependent Generators

## Tasty QuickCheck
