module ReaderState where

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

-- ANCHOR_END: Monad_State

-- ANCHOR: State
newtype State s a = State (s -> (a, s))

-- ANCHOR_END: State

-- ANCHOR: Functor_State
instance Functor (State env) where
  fmap = liftM

-- ANCHOR_END: Functor_State

-- ANCHOR: Applicative_State
instance Applicative (State env) where
  pure x = State $ \state -> (x, state)
  (<*>) = ap

-- ANCHOR_END: Applicative_State

-- ANCHOR: Monad_State
instance Monad (State env) where
  State m >>= f = State $ \state ->
    let (x, state') = m state
        State f' = f x
     in f' state'

-- ANCHOR_END: Monad_State

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
