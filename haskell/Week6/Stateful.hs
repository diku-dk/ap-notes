module Week6.Stateful where

-- ANCHOR: Stateful_Imports
import Control.Monad (forM_)
import Data.Array.IO (IOArray)
import Data.Array.MArray (newArray_, readArray, writeArray)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
-- ANCHOR_END: Stateful_Imports

-- ANCHOR: Stateful_QuickCheckImports
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    ioProperty,
    listOf,
    oneof,
    quickCheck,
    sample,
  )

-- ANCHOR_END: Stateful_QuickCheckImports

-- ANCHOR: Stateful_DynamicArray
data DynamicArray a
  = DynamicArray
  { -- Number of elements inserted.
    daUsed :: IORef Int,
    -- Capacity.
    daCap :: IORef Int,
    -- Underlying array.
    daArr :: IORef (IOArray Int a)
  }

-- ANCHOR_END: Stateful_DynamicArray

-- ANCHOR: Stateful_newDynamicArray
newDynamicArray :: IO (DynamicArray a)
newDynamicArray = do
  let capacity = 10
  arr <- newArray_ (0, capacity - 1)
  used_ref <- newIORef 0
  capacity_ref <- newIORef capacity
  arr_ref <- newIORef arr
  pure $
    DynamicArray
      { daUsed = used_ref,
        daCap = capacity_ref,
        daArr = arr_ref
      }

-- ANCHOR_END: Stateful_newDynamicArray

-- ANCHOR: Stateful_index
index :: Int -> DynamicArray a -> IO (Maybe a)
index i (DynamicArray used_ref _cap_ref arr_ref) = do
  used <- readIORef used_ref
  if i >= 0 && i < used
    then do
      arr <- readIORef arr_ref
      Just <$> readArray arr i
    else pure Nothing

-- ANCHOR_END: Stateful_index

-- ANCHOR: Stateful_insert
insert :: a -> DynamicArray a -> IO ()
insert x (DynamicArray used_ref cap_ref arr_ref) = do
  used <- readIORef used_ref
  cap <- readIORef cap_ref
  arr <- readIORef arr_ref
  if used < cap
    then do
      writeArray arr used x
      writeIORef used_ref $ used + 1
    else do
      let cap' = cap * 2
      arr' <- newArray_ (0, cap')
      forM_ [0 .. used - 1] $ \i ->
        writeArray arr' i =<< readArray arr i
      writeArray arr' used x
      writeIORef arr_ref arr'
      writeIORef used_ref $ used + 1
      writeIORef cap_ref cap'

-- ANCHOR_END: Stateful_insert

-- ANCHOR: Stateful_write
write :: Int -> a -> DynamicArray a -> IO (Maybe ())
write i x (DynamicArray used_ref _cap_ref arr_ref) = do
  used <- readIORef used_ref
  if i >= 0 && i < used
    then do
      arr <- readIORef arr_ref
      writeArray arr i x
      pure $ Just ()
    else pure Nothing

-- ANCHOR_END: Stateful_write

-- ANCHOR: Stateful_delete
delete :: Int -> DynamicArray a -> IO (Maybe ())
delete i (DynamicArray used_ref cap_ref arr_ref) = do
  used <- readIORef used_ref
  if i >= 0 && i < used
    then do
      cap <- readIORef cap_ref
      arr <- readIORef arr_ref
      forM_ [i + 1 .. used - 1] $ \j ->
        writeArray arr (j - 1) =<< readArray arr j
      writeIORef used_ref $ used - 1
      if used < cap `div` 2
        then do
          let cap' = cap `div` 2
          arr' <- newArray_ (0, cap')
          forM_ [0 .. used - 1] $ \j ->
            writeArray arr' j =<< readArray arr j
          writeIORef used_ref $ used - 1
          writeIORef arr_ref arr'
          writeIORef cap_ref cap'
        else pure ()
      pure $ Just ()
    else pure Nothing

-- ANCHOR_END: Stateful_delete

-- ANCHOR: Stateful_Model
data Model a = Model [a]
  deriving (Show)

initModel :: Model a
initModel = Model []

-- ANCHOR_END: Stateful_Model

-- ANCHOR: Stateful_Command
data Command a
  = Insert a
  | Index Int
  | Write Int a
  | Delete Int
  deriving (Eq, Show)

data Response a = Success | Failure | Elem a
  deriving (Eq, Show)

-- ANCHOR_END: Stateful_Command

-- ANCHOR: Stateful_cmdInsert
cmdInsert :: a -> Model a -> (Model a, Response a)
cmdInsert x (Model xs) = (Model $ xs ++ [x], Success)

-- ANCHOR_END: Stateful_cmdInsert

-- ANCHOR: Stateful_step
cmdIndex :: Int -> Model a -> (Model a, Response a)
cmdIndex i (Model xs) =
  if i >= 0 && i < length xs
    then (Model xs, Elem (xs !! i))
    else (Model xs, Failure)

cmdWrite :: Int -> a -> Model a -> (Model a, Response a)
cmdWrite i x (Model xs) =
  if i >= 0 && i < length xs
    then (Model $ take i xs ++ [x] ++ drop (i + 1) xs, Success)
    else (Model xs, Failure)

cmdDelete :: Int -> Model a -> (Model a, Response a)
cmdDelete i (Model xs) =
  if i >= 0 && i < length xs
    then (Model $ take i xs ++ drop (i + 1) xs, Success)
    else (Model xs, Failure)

step :: Model a -> Command a -> (Model a, Response a)
step m (Insert x) = cmdInsert x m
step m (Index i) = cmdIndex i m
step m (Write i x) = cmdWrite i x m
step m (Delete i) = cmdDelete i m

-- ANCHOR_END: Stateful_step

-- ANCHOR: Stateful_exec
exec :: DynamicArray a -> Command a -> IO (Response a)
exec a (Insert x) = do
  insert x a
  pure Success
exec a (Index i) = do
  r <- index i a
  case r of
    Just x -> pure $ Elem x
    Nothing -> pure Failure
exec a (Write i x) = do
  r <- write i x a
  case r of
    Just () -> pure Success
    Nothing -> pure Failure
exec a (Delete i) = do
  r <- delete i a
  case r of
    Just () -> pure Success
    Nothing -> pure Failure

-- ANCHOR_END: Stateful_exec

-- ANCHOR: Stateful_Program
newtype Program a = Program [Command a]
  deriving (Show)

-- ANCHOR_END: Stateful_Program

-- ANCHOR: Stateful_Arbitrary_Command
instance (Arbitrary a) => Arbitrary (Command a) where
  arbitrary =
    oneof
      [ Insert <$> arbitrary,
        Index <$> arbitrary,
        Write <$> arbitrary <*> arbitrary,
        Delete <$> arbitrary
      ]

instance (Arbitrary a) => Arbitrary (Program a) where
  arbitrary = Program <$> listOf arbitrary
  shrink (Program l) = map Program (shrink l)

-- ANCHOR_END: Stateful_Arbitrary_Command

samplePrograms :: IO ()
samplePrograms = sample (arbitrary :: Gen (Program Int))

-- ANCHOR: Stateful_runProgram
runProgram :: (Eq a) => DynamicArray a -> Model a -> Program a -> IO Bool
runProgram c0 m0 (Program cmds0) = go c0 m0 cmds0
  where
    go _c _m [] = pure True
    go c m (cmd : cmds) = do
      sut_resp <- exec c cmd
      let (m', model_resp) = step m cmd
      if sut_resp == model_resp
        then go c m' cmds
        else pure False

-- ANCHOR_END: Stateful_runProgram

-- ANCHOR: Stateful_prop_array
prop_array :: Program Int -> Property
prop_array prog = ioProperty $ do
  c <- newDynamicArray
  runProgram c initModel prog

-- ANCHOR_END: Stateful_prop_array

test_prop_array :: IO ()
test_prop_array = quickCheck prop_array
