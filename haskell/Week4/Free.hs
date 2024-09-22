module Week4.Free where

import Control.Monad (ap)

-- ANCHOR: Free
data Free e a
  = Pure a
  | Free (e (Free e a))

-- ANCHOR_END: Free

-- ANCHOR: Functor_Free
instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free g) = Free $ fmap (fmap f) g

-- ANCHOR_END: Functor_Free

-- ANCHOR: Applicative_Free
instance (Functor e) => Applicative (Free e) where
  pure = Pure
  (<*>) = ap

-- ANCHOR_END: Applicative_Free

-- ANCHOR: Monad_Free
instance (Functor e) => Monad (Free e) where
  Pure x >>= f = f x
  Free g >>= f = Free $ h <$> g
    where
      h x = x >>= f

-- ANCHOR_END: Monad_Free

-- ANCHOR: ReadOp
data ReadOp r a = ReadOp (r -> a)

-- ANCHOR_END: ReadOp

-- ANCHOR: Functor_ReadOp
instance Functor (ReadOp r) where
  fmap f (ReadOp g) = ReadOp $ \x -> f (g x)

-- ANCHOR_END: Functor_ReadOp

-- ANCHOR: Reader
type Reader r a = Free (ReadOp r) a

-- ANCHOR_END: Reader

-- ANCHOR: RunReader
runReader :: r -> Reader r a -> a
-- ANCHOR_END: RunReader
-- ANCHOR: RunReader_Pure
runReader _ (Pure x) = x
-- ANCHOR_END: RunReader_Pure
-- ANCHOR: RunReader_Free
runReader r (Free (ReadOp g)) = runReader r (g r)

-- ANCHOR_END: RunReader_Free

-- ANCHOR: ask
ask :: Reader r r
ask = Free $ ReadOp $ \x -> Pure x

-- ANCHOR_END: ask

-- ANCHOR: StateOp
data StateOp s a
  = StatePut s a
  | StateGet (s -> a)

-- ANCHOR_END: StateOp

-- ANCHOR: Functor_StateOp
instance Functor (StateOp s) where
  fmap f (StatePut s x) = StatePut s $ f x
  fmap f (StateGet g) = StateGet $ \s -> f (g s)

-- ANCHOR_END: Functor_StateOp

-- ANCHOR: State
type State s a = Free (StateOp s) a

-- ANCHOR_END: State

-- ANCHOR: runState
runState :: s -> State s a -> a
runState _ (Pure x) = x
runState s (Free (StateGet f)) = runState s (f s)
runState _ (Free (StatePut s x)) = runState s x

-- ANCHOR_END: runState

-- ANCHOR: put_get
put :: s -> State s ()
put s = Free $ StatePut s $ Pure ()

get :: State s s
get = Free $ StateGet $ \s -> Pure s

-- ANCHOR_END: put_get

-- ANCHOR: Error
data ErrorOp e a
  = ErrorThrow e
  | ErrorCatch a (e -> a)

instance Functor (ErrorOp e) where
  fmap _ (ErrorThrow e) = ErrorThrow e
  fmap f (ErrorCatch a c) = ErrorCatch (f a) $ \e -> f (c e)

type Error e a = Free (ErrorOp e) a

-- ANCHOR_END: Error

-- ANCHOR: runError
runError :: Error e a -> Either e a
runError (Pure x) = Right x
runError (Free (ErrorThrow e)) = Left e
runError (Free (ErrorCatch x c)) =
  case runError x of
    Left e -> runError $ c e
    Right x' -> Right x'

-- ANCHOR_END: runError

-- ANCHOR: throw_catch
throw :: e -> Error e a
throw e = Free $ ErrorThrow e

catch :: Error e a -> (e -> Error e a) -> Error e a
catch x c = Free $ ErrorCatch x c

-- ANCHOR_END: throw_catch

data FibOp a
  = FibLog String a
  | FibMemo Int (FibM Int) (Int -> a)

instance Functor FibOp where
  fmap f (FibLog s c) = FibLog s $ f c
  fmap f (FibMemo n fn c) = FibMemo n fn $ \y -> f (c y)

type FibM a = Free FibOp a

ioFibM :: FibM a -> IO a
ioFibM (Pure x) = pure x
ioFibM (Free (FibMemo _ fn c)) = do
  fn' <- ioFibM fn
  ioFibM $ c fn'
ioFibM (Free (FibLog s c)) = do
  putStrLn s
  ioFibM c

fibLog :: String -> FibM ()
fibLog s = Free $ FibLog s $ pure ()

fibMemo :: Int -> FibM Int -> FibM Int
fibMemo k m = Free $ FibMemo k m pure

fib :: Int -> FibM Int
fib 0 = pure 1
fib 1 = pure 1
fib n = fibMemo n $ do
  fibLog $ "fib(" ++ show n ++ ")"
  x <- fib (n - 1)
  y <- fib (n - 2)
  pure $ x + y

pureFibM :: FibM a -> a
pureFibM (Pure x) = x
pureFibM (Free (FibMemo _ x c)) = pureFibM $ c $ pureFibM x
pureFibM (Free (FibLog _ c)) = pureFibM c

logFibM :: FibM a -> (a, [String])
logFibM (Pure x) = (x, [])
logFibM (Free (FibMemo _ x c)) =
  let (x', x_msgs) = logFibM x
      (c', c_msgs) = logFibM $ c $ x'
   in (c', x_msgs ++ c_msgs)
logFibM (Free (FibLog s x)) =
  let (x', msgs) = logFibM x
   in (x', msgs ++ [s])

memoFibM :: FibM a -> a
memoFibM m = fst $ memo [] m
  where
    memo :: [(Int, Int)] -> FibM a -> (a, [(Int, Int)])
    memo cache (Pure x) = (x, cache)
    memo cache (Free (FibMemo n fn c)) =
      case lookup n cache of
        Just res -> memo cache $ c res
        Nothing ->
          let (fn', cache') = memo cache fn
           in memo ((n, fn') : cache') (c fn')
    memo cache (Free (FibLog _ x)) =
      memo cache x
