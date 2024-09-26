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

-- ANCHOR: List1
list1 :: Gen a -> Gen [a]
list1 g = oneof [pure [], (:) <$> g <*> list1 g]

-- ANCHOR_END: List1

-- ANCHOR: List2
list2 :: Gen a -> Gen [a]
list2 g = frequency [(1, pure []), (9, (:) <$> g <*> list1 g)]

-- ANCHOR_END: List2

-- ANCHOR: List3
list3 :: Gen a -> Gen [a]
list3 g = abs <$> (arbitrary :: Gen Int) >>= go
    where
        go 0 = pure []
        go n = (:) <$> g <*> go (n - 1)

-- ANCHOR_END: List3

-- ANCHOR: List4
list4 :: Gen a -> Gen [a]
list4 g = sized $ \n -> chooseInt (0, n) >>= go
    where
        go 0 = pure []
        go n = (:) <$> g <*> go (n - 1)

-- ANCHOR_END: List4

-- ANCHOR: Pair
data Pair a b = Pair a b
    deriving Show

-- ANCHOR: PairShrink
instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

-- ANCHOR_END: Pair
    shrink (Pair x y) = [Pair x' y | x' <- shrink x] ++ [Pair x y' | y' <- shrink y]

-- ANCHOR_END: PairShrink

-- ANCHOR: Prop_AppendCommutativePair
prop_appendCommutative' :: Pair [Integer] [Integer] -> Bool
prop_appendCommutative' (Pair xs ys) = xs ++ ys == ys ++ xs

-- ANCHOR_END: Prop_AppendCommutativePair
