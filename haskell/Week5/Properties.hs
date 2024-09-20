module Week5.Properties where

import Test.QuickCheck

-- ANCHOR: Prop_LengthAppend
prop_lengthAppend :: [Integer] -> [Integer] -> Bool
prop_lengthAppend xs ys = length (xs ++ ys) == length xs + length ys

-- ANCHOR_END: Prop_LengthAppend

-- ANCHOR: TediousTest
tediousTestCases :: [([Integer], [Integer])]
tediousTestCases = [([], []), ([0], [1, 2]), ([3, 4, 5], [])] -- etc.

-- ANCHOR_END: TediousTest

-- ANCHOR: Prop_AppendCommutative
prop_appendCommutative :: [Integer] -> [Integer] -> Bool
prop_appendCommutative xs ys = xs ++ ys == ys ++ xs

-- ANCHOR_END: Prop_AppendCommutative

