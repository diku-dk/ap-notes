# Haskell and Type Classes

## Testing

In the assignments you will be required to write tests. You must use
the Haskell package [Tasty](https://hackage.haskell.org/package/tasty)
to write our tests. Tasty is a meta-framework that allows different
testing frameworks to be combined - in particular, `hunit` for unit
tests and `QuickCheck` for property-based testing. Although you will
initially only be writing unit tests, later in the course you will be
taught property-based testing. In order to avoid having to switch
testing framework halfway through the course, we use Tasty from the
start.

Generally the code handout will already contain dependencies on the
relevant packages. The ones we will use in the following are `tasty`
and `tasty-hunit`, which you can add to the `build-depends` field of
your `.cabal` file.

The Tasty documentation is fairly good and you are encouraged to read
it. However, this section will list everything you need to know for
the first week of AP.

### Structuring tests

There are many ways of structuring test suites. Because the programs
you will write in AP are so small, we will use a particularly simple
scheme. For any Haskell module `Foo`, the tests will be in a
corresponding *test module* called `Foo_Tests`. Each test module
defines a test suite named `test`, which in Tasty is of type
`TestTree`. We will see below how to define these.

To run the tests for an entire project, we write *test runner*, which
we will normally call `runtests.hs`. This test runner will import the
various `TestTrees`, combine them if necessary, and pass them to the
`defaultMain` function provided by Tasty. When the program is run,
Tasty will run the test suite and report any errors. If the test
runner is registered as a test suite in the `.cabal` file, we can use
the `cabal test` command to run the tests. This will be the case for
all code handouts that come with a `.cabal` file.

A test runner can look like this, where we assume the tests are
defined in a module `Foo_Tests`:

```Haskell
import qualified Foo_Tests
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain Foo_Tests.tests
```

If we load this module into `ghci`, we can also simply execute `main`
to run the test suite. This makes interactive development easy.

### Writing tests

To write a unit test, we import the module
[`Test.Tasty.HUnit`](https://hackage.haskell.org/package/tasty-hunit-0.10.2/docs/Test-Tasty-HUnit.html).
This gives us access to a variety of functions that produce
`TestTree`s. For example, `testCase`:

```Haskell
testCase :: TestName -> Assertion -> TestTree
```

The `TestName` type is a synonym for `String` and is used to label
failures. The `Assertion` type is a synonym for `IO ()`, but in
practice it is constructed using a variety of constructor functions.
One of the simplest is `assertBool`:

```Haskell
assertBool :: String -> Bool -> Assertion
```

We give it a `String` that is shown when the test fails, and then a
`Bool` that is `True` when the test succeeds. This is how we can write
a test that fails:

```Haskell
failingTest :: TestTree
failingTest = TestCase "should not work" $ assertBool "1 is not 2" $ 1==2
```

And here is one that succeeds:

```Haskell
successfulTest :: TestTree
successfulTest = TestCase "should work" $ assertBool "1 is not 1" $ 1==1
```

We can combine multiple `TestTree`s with `testGroup`:

```Haskell
tests :: TestTree
tests =
  testGroup
    "unit test suite"
    [ successfulTest,
      failingTest
    ]
```

The first argument is a descriptive string, and the second is a list
of `TestTree`s. We can use this to define test suites with arbitrarily
complicated nesting (and support for running only parts of the entire
test suite), but this is not needed for the comparatively simple test
suites we write in AP.

The `Test.Tasty.HUnit` module also provides other handy operators for
certain common cases. For example, `@?=` can be used for testing
equality in a more concise way than using `assertBool`:

```Haskell
failingTest2 :: TestTree
failingTest2 = testCase "should work" $ 1 @?= 2
```

When run, this will produce output such as the following:

```
unit test suite
  should work:     OK
  should not work: FAIL
    .../Week1Tests.hs:10:
    1 is not 2
    Use -p '/should not work/' to rerun this test only.
  should work:     FAIL
    .../Week1Tests.hs:13:
    expected: 2
     but got: 1
    Use -p '$0=="unit test suite.should work"' to rerun this test only.
```

## Useful types

This section discusses various useful Haskell types that are available
in the base library, as well as common functions on those types. We
will make use of some of these types in the later chapters.

### `Maybe`

The `Maybe` type is available in the Prelude (the module that is
implicitly imported in every Haskell program), but has the following
definition:

```Haskell
data Maybe a = Nothing
             | Just a
```

A value of type `Maybe a` is either `Nothing`, representing the
absence of a value, or `Just x` for some value `x` of type `a`. It is
often called the *option type*, and serves roughly the same role as
the `null` value in poorly designed languages, but in contrast to
`null` is visible in the type system.

It is often used to represent an operation that can fail. For example,
we can imagine a function of type

```Haskell
integerFromString :: String -> Maybe Int
```

that tries to turn a `String` into an `Integer`, and either returns
`Nothing`, indicating that the `String` is malformed, or `Just x`,
where `x` is the corresponding integer. We will return to this in
[Week 3](./chapter_3.md).

One useful function for operating on `Maybe` values is the `maybe`
function:

```Haskell
maybe :: b -> (a -> b) -> Maybe a -> b
```

It accepts a value that is returned in the `Nothing` case, and
otherwise a function that is applied to the value in the `Just` case.
It is equivalent to pattern matching, but is more concise.

Another function, which we must import from the
[Data.Maybe](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Maybe.html)
module, is `fromMaybe`:

```Haskell
fromMaybe :: a -> Maybe a -> a
```

It turns a `Maybe a` into an `a` by providing a default value:

```Haskell
> fromMaybe 0 Nothing
0
> fromMaybe 0 (Just 1)
1
```

Again this is nothing we could not write ourselves using `case`, but
using these functions can result in neater code.

### `Either`

The `Either` type is available in the Prelude and has this definition:

```Haskell
data Either a b = Left a
                | Right b
```

It is used to represent two different possibilities, with different
types. In practice, it is often used to represent functions that can
fail, with the `Left` constructor used to represent information about
the failure, and the `Right` constructor used to represent success.

A useful function for manipulating `Either` values is `either`:

```Haskell
either :: (a -> c) -> (b -> c) -> Either a b -> c
```

### `Void`

The `Void` type must be imported from `Data.Void` and has the
following definition:

```Haskell
data Void
```

This odd-looking type has *no constructors*, meaning there are no
values of type `Void`. This is admittedly somewhat exotic, but it has
some uses. For example, if we have a function of type `Int -> Void`,
we know that this function cannot possibly return, as no value of type
`Void` can be constructed. This is not really useful for a pure
function, but if we have an impure function with side-effects, such as
the infinite loops that are used in servers for reading incoming
requests (later in the course), then it may be sensible to use a
`Void` return type to clarify that the function will never terminate.

Another use of `Void` is to eliminate cases in polymorphic types. For
example, if we have a type `Either Void a`, then we know that the
`Left` case can never occur. This means we do not need to handle it
when pattern matching the `Either` type.

<div class="warning">This is strictly not true. Haskell is a lazy
language, so every value is inhabited by the special value ⊥
("bottom"), which represents a diverging computation.  Example:

```
> Left undefined :: Either Void Integer
Left *** Exception: Prelude.undefined
```

We will return to laziness later in the course, but it is standard to
reason about the type-level guarantees of Haskell code as if it were
eager.</div>
