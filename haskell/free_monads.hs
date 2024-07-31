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

-- ANCHOR_END: Monad_free

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
runReader r (Free (ReadOp f)) = runReader r (f r)

-- ANCHOR_END: RunReader_Free

-- ANCHOR: ask
ask :: Reader r r
ask = Free $ ReadOp $ \x -> Pure x

-- ANCHOR_END: ask

data StateOp s a
  = StatePut s a
  | StateGet (s -> a)

instance Functor (StateOp s) where
  fmap f (StatePut s x) = StatePut s $ f x
  fmap f (StateGet g) = StateGet $ \s -> f (g s)

type State s = Free (StateOp s)

runState :: s -> State s a -> a
runState _ (Pure x) = x
runState s (Free (StateGet f)) = runState s (f s)
runState _ (Free (StatePut s x)) = runState s x
