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
we will normally call `tests.hs`. This test runner will import the
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
