module Week5.Properties where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sort)
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

-- ANCHOR: Prop_LengthAppendEq
prop_lengthAppend' :: [Integer] -> [Integer] -> Property
prop_lengthAppend' xs ys = length (xs ++ ys) === length xs + length ys

-- ANCHOR_END: Prop_LengthAppendEq

-- ANCHOR: Prop_DivMod
prop_divMod :: Integer -> Integer -> Property
prop_divMod x y = y /= 0 ==> (x `div` y) * y + (x `mod` y) === x

-- ANCHOR_END: Prop_DivMod

-- ANCHOR: Prop_DivModNonZero
prop_divMod' :: Integer -> NonZero Integer -> Property
prop_divMod' x (NonZero y) = (x `div` y) * y + (x `mod` y) === x

-- ANCHOR_END: Prop_DivModNonZero

-------------------------------------------------------------------------
-- Where properties come from: a catalogue.
-------------------------------------------------------------------------

-- ANCHOR: Sorted
sorted :: (Ord a) => [a] -> Bool
sorted xs = and $ zipWith (<=) xs (drop 1 xs)

insertSorted :: (Ord a) => a -> [a] -> [a]
insertSorted x [] = [x]
insertSorted x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insertSorted x ys

-- ANCHOR_END: Sorted

-- ANCHOR: Prop_InsertSorted
prop_insertSorted :: Integer -> [Integer] -> Property
prop_insertSorted x xs =
  sorted xs ==> sorted (insertSorted x xs)

-- ANCHOR_END: Prop_InsertSorted

-- ANCHOR: Prop_ShowRead
prop_showRead :: [Integer] -> Property
prop_showRead xs = read (show xs) === xs

-- ANCHOR_END: Prop_ShowRead

-- ANCHOR: Msort
msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where
    (ys, zs) = splitAt (length xs `div` 2) xs
    merge [] bs = bs
    merge as [] = as
    merge (a : as) (b : bs)
      | a <= b = a : merge as (b : bs)
      | otherwise = b : merge (a : as) bs

-- ANCHOR_END: Msort

-- ANCHOR: Prop_Msort
prop_msort :: [Integer] -> Property
prop_msort xs = msort xs === sort xs

-- ANCHOR_END: Prop_Msort

-- ANCHOR: Prop_MsortMetamorphic
prop_msortAppend :: [Integer] -> [Integer] -> Property
prop_msortAppend xs ys = msort (xs ++ ys) === msort (ys ++ xs)

-- ANCHOR_END: Prop_MsortMetamorphic

-- ANCHOR: Prop_MsortPost
prop_msortSorted :: [Integer] -> Bool
prop_msortSorted xs = sorted (msort xs)

-- ANCHOR_END: Prop_MsortPost

-------------------------------------------------------------------------
-- Generators.
-------------------------------------------------------------------------

-- ANCHOR: List1
list1 :: Gen a -> Gen [a]
list1 g = oneof [pure [], (:) <$> g <*> list1 g]

-- ANCHOR_END: List1

-- ANCHOR: List2
list2 :: Gen a -> Gen [a]
list2 g = frequency [(1, pure []), (9, (:) <$> g <*> list2 g)]

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

-------------------------------------------------------------------------
-- Test data design: partitioning and coverage.
-------------------------------------------------------------------------

-- ANCHOR: LengthBucket
lengthBucket :: [a] -> String
lengthBucket xs
  | n == 0 = "0"
  | n < 5 = "1-4"
  | n < 20 = "5-19"
  | otherwise = ">=20"
  where
    n = length xs

-- ANCHOR_END: LengthBucket

-- ANCHOR: Prop_InsertSortedTabulate
prop_insertSorted' :: Integer -> [Integer] -> Property
prop_insertSorted' x xs =
  sorted xs ==>
    classify (length xs < 2) "trivial" $
      tabulate "length of xs" [lengthBucket xs] $
        sorted (insertSorted x xs)

-- ANCHOR_END: Prop_InsertSortedTabulate

-- ANCHOR: GenSorted
genSorted :: Gen [Integer]
genSorted = sort <$> arbitrary

-- ANCHOR_END: GenSorted

-- ANCHOR: Prop_InsertSortedForAll
prop_insertSorted'' :: Integer -> Property
prop_insertSorted'' x =
  forAll genSorted $ \xs ->
    tabulate "length of xs" [lengthBucket xs] $
      sorted (insertSorted x xs)

-- ANCHOR_END: Prop_InsertSortedForAll

-- ANCHOR: Prop_InsertSortedCover
prop_insertSortedCover :: Integer -> Property
prop_insertSortedCover x =
  forAll genSorted $ \xs ->
    checkCoverage
      . cover 2 (null xs) "empty list"
      . cover 20 (not (null xs) && x < head xs) "insert at front"
      . cover 20 (not (null xs) && x > last xs) "insert at back"
      . cover 20 (x `elem` xs) "insert a duplicate"
      $ sorted (insertSorted x xs)

-- ANCHOR_END: Prop_InsertSortedCover

-- ANCHOR: GenInsertion
boundaries :: [Integer] -> [(Int, Gen Integer)]
boundaries xs =
  [ (3, pure $ head xs - 1),
    (3, pure $ last xs + 1),
    (3, elements xs)
  ]

genInsertion :: Gen (Integer, [Integer])
genInsertion = do
  xs <- genSorted
  x <-
    frequency $
      (2, arbitrary) : if null xs then [] else boundaries xs
  pure (x, xs)

-- ANCHOR_END: GenInsertion

-- ANCHOR: Prop_InsertSortedCover2
prop_insertSortedCover' :: Property
prop_insertSortedCover' =
  forAll genInsertion $ \(x, xs) ->
    checkCoverage
      . cover 2 (null xs) "empty list"
      . cover 20 (not (null xs) && x < head xs) "insert at front"
      . cover 20 (not (null xs) && x > last xs) "insert at back"
      . cover 20 (x `elem` xs) "insert a duplicate"
      $ sorted (insertSorted x xs)

-- ANCHOR_END: Prop_InsertSortedCover2

-------------------------------------------------------------------------
-- Shrinking.
-------------------------------------------------------------------------

-- ANCHOR: Pair
data Pair a b = Pair a b
  deriving (Show)

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

-------------------------------------------------------------------------
-- Testing an abstract data type against a reference implementation.
-------------------------------------------------------------------------

-- ANCHOR: FState
newtype FState s a = FState {runFState :: s -> (a, s)}

getF :: FState s s
getF = FState $ \s -> (s, s)

putF :: s -> FState s ()
putF s = FState $ \_ -> ((), s)

returnF :: a -> FState s a
returnF x = FState $ \s -> (x, s)

bindF :: FState s a -> (a -> FState s b) -> FState s b
bindF (FState m) f = FState $ \s ->
  let (x, s') = m s in runFState (f x) s'

-- ANCHOR_END: FState

-- ANCHOR: IState
type IState s a = IORef s -> IO a

getI :: IState s s
getI = readIORef

putI :: s -> IState s ()
putI s ref = writeIORef ref s

returnI :: a -> IState s a
returnI x _ = pure x

bindI :: IState s a -> (a -> IState s b) -> IState s b
bindI m f ref = do
  x <- m ref
  f x ref

-- ANCHOR_END: IState

-- ANCHOR: RunIState
runIState :: IState s a -> s -> IO (a, s)
runIState m s = do
  ref <- newIORef s
  x <- m ref
  s' <- readIORef ref
  pure (x, s')

-- ANCHOR_END: RunIState

-- ANCHOR: StateEquiv
infix 4 ~=

(~=) :: (Eq a, Show a) => FState Int a -> FState Int a -> Property
m1 ~= m2 = property $ \s -> runFState m1 s === runFState m2 s

-- ANCHOR_END: StateEquiv

-- ANCHOR: Prop_StateLaws
prop_putGet :: Int -> Property
prop_putGet s = (putF s `bindF` \_ -> getF) ~= (putF s `bindF` \_ -> returnF s)

prop_putPut :: Int -> Int -> Property
prop_putPut s t = (putF s `bindF` \_ -> putF t) ~= putF t

prop_getGet :: Property
prop_getGet = (getF `bindF` \_ -> getF) ~= getF

-- ANCHOR_END: Prop_StateLaws

-- ANCHOR: StateSimulates
simulates :: (Eq a, Show a) => IState Int a -> FState Int a -> Property
mi `simulates` mf = property $ \s ->
  ioProperty $ do
    r <- runIState mi s
    pure $ r === runFState mf s

-- ANCHOR_END: StateSimulates

-- ANCHOR: Prop_StateSimulates
prop_simGet :: Property
prop_simGet = getI `simulates` getF

prop_simPut :: Int -> Property
prop_simPut s = putI s `simulates` putF s

prop_simReturn :: Int -> Property
prop_simReturn x = returnI x `simulates` returnF x

prop_simBind :: Fun Int Int -> Property
prop_simBind fun =
  (getI `bindI` (putI . f)) `simulates` (getF `bindF` (putF . f))
  where
    f = applyFun fun

-- ANCHOR_END: Prop_StateSimulates
