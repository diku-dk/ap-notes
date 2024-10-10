module Week2.ReaderState where

import Control.Monad (ap, liftM)

-- ANCHOR: Reader
newtype Reader env a = Reader (env -> a)

-- ANCHOR_END: Reader

-- ANCHOR: Functor_Reader
instance Functor (Reader env) where
  fmap = liftM

-- ANCHOR_END: Functor_Reader

-- ANCHOR: Applicative_Reader
instance Applicative (Reader env) where
  pure x = Reader $ \_env -> x
  (<*>) = ap

-- ANCHOR_END: Applicative_Reader

-- ANCHOR: Monad_Reader
instance Monad (Reader env) where
  Reader m >>= f = Reader $ \env ->
    let x = m env
        Reader f' = f x
     in f' env

-- ANCHOR_END: Monad_Reader

runReader :: env -> Reader env a -> a
runReader env (Reader f) = f env

ask :: Reader env env
ask = Reader $ \env -> env

local :: (env -> env) -> Reader env a -> Reader env a
local f (Reader g) = Reader $ \env -> g (f env)

data Tree
  = Leaf Int
  | Inner Tree Tree
  deriving (Show)

incLeaves :: Tree -> Reader Int Tree
incLeaves (Leaf x) = do
  depth <- ask
  pure $ Leaf $ x + depth
incLeaves (Inner l r) = do
  l' <- local (+ 1) $ incLeaves l
  r' <- local (+ 1) $ incLeaves r
  pure $ Inner l' r'

-- ANCHOR: State
newtype State s a = State (s -> (a, s))

-- ANCHOR_END: State

-- ANCHOR: Functor_State
instance Functor (State s) where
  fmap = liftM

-- ANCHOR_END: Functor_State

-- ANCHOR: Applicative_State
instance Applicative (State s) where
  pure x = State $ \state -> (x, state)
  (<*>) = ap

-- ANCHOR_END: Applicative_State

-- ANCHOR: Monad_State
instance Monad (State s) where
  State m >>= f = State $ \state ->
    let (x, state') = m state
        State f' = f x
     in f' state'

-- ANCHOR_END: Monad_State

runState :: s -> State s a -> (a, s)
runState s (State f) = f s

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

numberLeaves :: Tree -> State Int Tree
numberLeaves (Leaf _) = do
  i <- get
  put (i + 1)
  pure $ Leaf i
numberLeaves (Inner l r) = do
  l' <- numberLeaves l
  r' <- numberLeaves r
  pure $ Inner l' r'

-- ANCHOR: RS
newtype RS env s a = RS (env -> s -> (a, s))

-- ANCHOR_END: RS

-- ANCHOR: Functor_RS
instance Functor (RS env s) where
  fmap = liftM

-- ANCHOR_END: Functor_RS

-- ANCHOR: Applicative_RS
instance Applicative (RS env s) where
  pure x = RS $ \_env state -> (x, state)
  (<*>) = ap

-- ANCHOR_END: Applicative_RS

-- ANCHOR: Monad_RS
instance Monad (RS env s) where
  RS m >>= f = RS $ \env state ->
    let (x, state') = m env state
        RS f' = f x
     in f' env state'

-- ANCHOR_END: Monad_RS

runRS :: env -> s -> RS env s a -> (a, s)
runRS env state (RS f) = f env state

rsGet :: RS env s s
rsGet = RS $ \_env state -> (state, state)

rsPut :: s -> RS env s ()
rsPut state = RS $ \_env _ -> ((), state)

rsAsk :: RS env s env
rsAsk = RS $ \env state -> (env, state)

rsLocal :: (env -> env) -> RS env s env -> RS env s env
rsLocal f (RS g) = RS $ \env state -> g (f env) state
