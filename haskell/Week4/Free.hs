{-# LANGUAGE ExistentialQuantification #-}
-- | The free monad over a functor, and the effects built on it in chapter 4.
-- The definitions here are the ones shown in the week 4 slides
-- (ap-e2026-private/lectures/monads/Monads.hs), verbatim.
module Week4.Free where

-- ANCHOR: Free
data Free e a =
  Pure a
  | Free (e (Free e a))

-- ANCHOR_END: Free

-- ANCHOR: Functor_Free
instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Free g) = Free (fmap (fmap f) g)

-- ANCHOR_END: Functor_Free

-- ANCHOR: Applicative_Free
instance (Functor e) => Applicative (Free e) where
  pure = Pure
  Pure f <*> m = fmap f m
  Free g <*> m = Free (fmap (<*> m) g)

-- ANCHOR_END: Applicative_Free

-- ANCHOR: Monad_Free
instance (Functor e) => Monad (Free e) where
  Pure x >>= f = f x
  Free g >>= f = Free (fmap (>>= f) g)

-- ANCHOR_END: Monad_Free

-- ANCHOR: ReadOp
data ReadOp r a = ReadOp (r -> a)

-- ANCHOR_END: ReadOp

-- ANCHOR: Functor_ReadOp
instance Functor (ReadOp r) where
  fmap f (ReadOp g) = ReadOp (f . g)

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
ask = Free (ReadOp Pure)

-- ANCHOR_END: ask

-- ANCHOR: StateOp
data StateOp s r =
    StateGet (s -> r)
  | StatePut s r

-- ANCHOR_END: StateOp

-- ANCHOR: Functor_StateOp
instance Functor (StateOp s) where
  fmap h (StateGet k) = StateGet (h . k)
  fmap h (StatePut s a) = StatePut s (h a)

-- ANCHOR_END: Functor_StateOp

-- ANCHOR: FreeState
type FreeState s a = Free (StateOp s) a

-- ANCHOR_END: FreeState

-- ANCHOR: runState
runState :: s -> FreeState s a -> (a, s)
runState s (Pure x) = (x, s)
runState s (Free (StateGet k)) = runState s (k s)
runState _ (Free (StatePut s' m)) = runState s' m

-- ANCHOR_END: runState

-- ANCHOR: put_get
put :: s -> FreeState s ()
put s = Free (StatePut s (Pure ()))

get :: FreeState s s
get = Free (StateGet Pure)

-- ANCHOR_END: put_get

-- ANCHOR: Error
data ErrorOp e a =
  ErrorThrow e
  | forall x . ErrorCatch (ErrorM e x) (e -> ErrorM e x) (x -> a)

instance Functor (ErrorOp e) where
  fmap _ (ErrorThrow e) = ErrorThrow e
  fmap f (ErrorCatch m h c) = ErrorCatch m h (f . c)

type ErrorM e a = Free (ErrorOp e) a

-- ANCHOR_END: Error

-- ANCHOR: runError
runError :: ErrorM e a -> Either e a
runError (Pure x) = Right x
runError (Free (ErrorThrow e)) = Left e
runError (Free (ErrorCatch m h c)) =
  case runError m of
    Right x -> runError (c x)
    Left err -> runError (h err >>= c)

-- ANCHOR_END: runError

-- ANCHOR: throw_catch
throw :: e -> ErrorM e a
throw e = Free (ErrorThrow e)

catch :: ErrorM e a -> (e -> ErrorM e a) -> ErrorM e a
catch m h = Free (ErrorCatch m h Pure)

-- ANCHOR_END: throw_catch

-- ANCHOR: FibOp
data FibOp a = FibLog String a
             | FibMemo Int (FibM Int) (Int -> a)
type FibM a = Free FibOp a

instance Functor FibOp where
  fmap f (FibLog s c)    = FibLog s (f c)
  fmap f (FibMemo n m c) = FibMemo n m (f . c)

-- ANCHOR_END: FibOp

-- ANCHOR: fibLog_fibMemo
fibMemo :: Int -> FibM Int -> FibM Int
fibMemo n m = Free (FibMemo n m Pure)

fibLog :: String -> FibM ()
fibLog s = Free (FibLog s (Pure ()))

-- ANCHOR_END: fibLog_fibMemo

-- ANCHOR: fib
fib :: Int -> FibM Int
fib 0 = return 1
fib 1 = return 1
fib n = fibMemo n (do
  fibLog ("fib(" ++ show n ++ ")")
  x <- fib (n - 1)
  y <- fib (n - 2)
  return (x + y))

-- ANCHOR_END: fib

-- ANCHOR: pureFibM
pureFibM :: FibM a -> a
pureFibM (Pure x) = x
pureFibM (Free (FibLog _ c)) = pureFibM c
pureFibM (Free (FibMemo _ fn c)) = pureFibM (c (pureFibM fn))

-- ANCHOR_END: pureFibM

-- ANCHOR: ioFibM
ioFibM :: FibM a -> IO a
ioFibM (Pure x) = return x
ioFibM (Free (FibLog s c)) = do
  putStrLn s
  ioFibM c
ioFibM (Free (FibMemo _ fn c)) = do
  x <- ioFibM fn
  ioFibM (c x)

-- ANCHOR_END: ioFibM

-- ANCHOR: logFibM
logFibM :: FibM a -> (a, [String])
logFibM (Pure x) = (x, [])
logFibM (Free (FibLog s c)) =
  let (x, msgs) = logFibM c
   in (x, s : msgs)
logFibM (Free (FibMemo _ fn c)) =
  let (x, msgs) = logFibM fn
      (y, msgs') = logFibM (c x)
   in (y, msgs ++ msgs')

-- ANCHOR_END: logFibM

-- ANCHOR: memoFibM
memoFibM :: FibM a -> a
memoFibM m = fst (run [] m)
  where
    run :: [(Int, Int)] -> FibM b -> (b, [(Int, Int)])
    run cache (Pure x)              = (x, cache)
    run cache (Free (FibLog _ c))   = run cache c
    run cache (Free (FibMemo n fn c)) =
      case lookup n cache of
        Just x  -> run cache (c x)
        Nothing -> let (x, cache') = run cache fn
                   in  run ((n, x) : cache') (c x)

-- ANCHOR_END: memoFibM
