module Week1.Tests where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

successfulTest :: TestTree
successfulTest = testCase "should work" $ assertBool "1 is not 1" $ 1 == 1

failingTest :: TestTree
failingTest = testCase "should not work" $ assertBool "1 is not 2" $ 1 == 2

failingTest2 :: TestTree
failingTest2 = testCase "should work" $ 1 @?= 2

tests :: TestTree
tests =
  testGroup
    "unit test suite"
    [ successfulTest,
      failingTest,
      failingTest2
    ]

main :: IO ()
main = defaultMain tests
