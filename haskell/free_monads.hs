data Free f a
  = Pure a
  | Free (f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Free g) = Free (fmap f <$> g)

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  f <*> x = do
    f' <- f
    x' <- x
    pure $ f' x'

instance (Functor f) => Monad (Free f) where
  Pure x >>= f = f x
  Free g >>= f = Free ((>>= f) <$> g)

data ReadOp a b = ReadOp (a -> b)

instance Functor (ReadOp a) where
  fmap f (ReadOp g) = ReadOp $ \x -> f (g x)

type Reader r = Free (ReadOp r)

runReader :: r -> Reader r a -> a
runReader _ (Pure x) = x
runReader r (Free (ReadOp f)) = runReader r (f r)

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
