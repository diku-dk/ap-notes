{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

-- | Three implementations of one state-monad interface, and the two ways of
-- abstracting over them: parameterising over the monad (the 'StateMonad'
-- class) and the free monad over the state operations ('FreeState').
module Week4.StateMonads where

import Control.Monad (join, (>=>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Week4.Free (Free (..), FreeState, StateOp (..))

-- ANCHOR: StateMonad
class (Monad m) => StateMonad m s | m -> s where
  get :: m s
  put :: s -> m ()

-- ANCHOR_END: StateMonad

-- ANCHOR: modify
modify :: (StateMonad m s) => (s -> s) -> m ()
modify f = get >>= (put . f)

-- ANCHOR_END: modify

-- ANCHOR: tick
tick :: (StateMonad m Int) => m Int
tick = do n <- get; put (n + 1); pure n

-- ANCHOR_END: tick

-- ANCHOR: stack
push :: (StateMonad m [a]) => a -> m ()
push x = modify (x :)

pop :: (StateMonad m [a]) => m (Maybe a)
pop = do
  xs <- get
  case xs of
    [] -> pure Nothing
    (y : ys) -> do put ys; pure (Just y)

stackExample :: (StateMonad m [Int]) => m (Maybe Int)
stackExample = do
  push 3
  push 5
  a <- pop
  b <- pop
  pure (addMaybe a b)
  where
    addMaybe mx my = do
      x <- mx
      y <- my
      pure (x + y)

-- ANCHOR_END: stack

-- ANCHOR: FState
-- A newtype is an isomorphism; record syntax names both directions of it,
-- the constructor one way and the selector the other.
newtype FState s a = FState {runFState :: s -> (a, s)}

-- ANCHOR_END: FState

-- ANCHOR: FState_instances
instance Monad (FState s) where
  c >>= f =
    FState
      ( \s ->
          let (a, s') = runFState c s
           in runFState (f a) s'
      )

-- Haskell requires Functor and Applicative to be DECLARED, but for a monad
-- they need not be INVENTED: the two definitions below say nothing about
-- FState, and recur verbatim for every monad here.  pure is the one
-- operation (>>=) cannot supply.
instance Applicative (FState s) where
  pure a = FState (\s -> (a, s))
  mf <*> ma = mf >>= (\f -> fmap f ma)

instance Functor (FState s) where
  fmap f m = m >>= (pure . f)

instance StateMonad (FState s) s where
  get = FState (\s -> (s, s))
  put s = FState (\_ -> ((), s))

-- ANCHOR_END: FState_instances

-- ANCHOR: IState
-- Like FState, a newtype whose selector is the true inverse of the
-- constructor: the caller supplies the reference.
newtype IState s a = IState {runIState :: IORef s -> IO a}

-- Convenience: create the reference from an initial state, then run.
runIStateFrom :: s -> IState s a -> IO a
runIStateFrom s c = do
  ref <- newIORef s
  runIState c ref

-- ANCHOR_END: IState

-- ANCHOR: IState_instances
instance Monad (IState s) where
  c >>= f =
    IState
      ( \ref -> do
          a <- runIState c ref
          runIState (f a) ref
      )

instance Applicative (IState s) where
  pure a = IState (\_ -> pure a)
  mf <*> ma = mf >>= (\f -> fmap f ma)

instance Functor (IState s) where
  fmap f m = m >>= (pure . f)

instance StateMonad (IState s) s where
  get = IState readIORef
  put s = IState (\ref -> writeIORef ref s)

-- ANCHOR_END: IState_instances

-- ANCHOR: Counting
newtype Counting a = Counting {runCounting :: Int -> (a, Int)}

instance Monad Counting where
  Counting g >>= f =
    Counting (\n -> let (a, n') = g n in runCounting (f a) n')

instance Functor Counting where
  fmap f m = m >>= (pure . f)

instance Applicative Counting where
  pure a = Counting (\n -> (a, n))
  mf <*> ma = mf >>= (\f -> fmap f ma)

-- Any state value will do; the point is to count the writes.
instance StateMonad Counting Int where
  get = Counting (\n -> (0, n))
  put _ = Counting (\n -> ((), n + 1))

-- ANCHOR_END: Counting

-- ANCHOR: Prog
newtype Prog = Prog (forall m. (StateMonad m Int) => m Int)

threeTicks :: Prog
threeTicks = Prog (do _ <- tick; _ <- tick; tick)

runIt :: Prog -> ((Int, Int), (Int, Int))
runIt (Prog p) = (runFState p 0, runCounting p 0)

-- ANCHOR_END: Prog

-- ANCHOR: StateObj
data StateObj s = StateObj
  { objGet :: IO s,
    objPut :: s -> IO ()
  }

newStateObj :: s -> IO (StateObj s)
newStateObj s0 = do
  ref <- newIORef s0
  pure (StateObj {objGet = readIORef ref, objPut = writeIORef ref})

-- ANCHOR_END: StateObj

-- ANCHOR: OState
newtype OState s a = OState {runOStateOn :: StateObj s -> IO a}

runOState :: OState s a -> s -> IO a
runOState m s0 = do
  o <- newStateObj s0
  runOStateOn m o

-- ANCHOR_END: OState

-- ANCHOR: OState_instances
instance Monad (OState s) where
  OState g >>= f =
    OState
      ( \o -> do
          a <- g o
          runOStateOn (f a) o
      )

instance Functor (OState s) where
  fmap f m = m >>= (\x -> pure (f x))

instance Applicative (OState s) where
  -- The object is discarded: pure performs no side effects.
  pure a = OState (\_ -> pure a)
  mf <*> ma = mf >>= (\f -> fmap f ma)

instance StateMonad (OState s) s where
  get = OState objGet
  put s = OState (\o -> objPut o s)

-- ANCHOR_END: OState_instances

-- ANCHOR: tracing
tracing :: (Show s) => String -> StateObj s -> StateObj s
tracing name o =
  StateObj
    { objGet = do
        s <- objGet o
        putStrLn (name ++ ": get -> " ++ show s)
        pure s,
      objPut = \s -> do
        putStrLn (name ++ ": put " ++ show s)
        objPut o s
    }

-- ANCHOR_END: tracing

-- ANCHOR: putk_getk
putk :: (StateMonad m s) => s -> (() -> m a) -> m a
putk s = (put s >>=)

getk :: (StateMonad m s) => (s -> m a) -> m a
getk = (get >>=)

-- ANCHOR_END: putk_getk

-- ANCHOR: FSM
data FSM s a
  = Return a
  | Getk (s -> FSM s a)
  | Putk s (FSM s a)

-- ANCHOR_END: FSM

-- ANCHOR: FSM_monad
instance Monad (FSM s) where
  Return x >>= f = f x
  Getk k >>= f = Getk (k >=> f)
  Putk s m >>= f = Putk s (m >>= f)

instance Functor (FSM s) where
  fmap f m = m >>= (pure . f)

instance Applicative (FSM s) where
  pure = Return
  mf <*> ma = mf >>= (\f -> fmap f ma)

-- ANCHOR_END: FSM_monad

-- ANCHOR: FSM_state
instance StateMonad (FSM s) s where
  get = Getk Return
  put s = Putk s (Return ())

runFSM :: s -> FSM s a -> (a, s)
runFSM s (Return x) = (x, s)
runFSM s (Getk k) = runFSM s (k s)
runFSM _ (Putk s m) = runFSM s m

-- ANCHOR_END: FSM_state

-- ANCHOR: FSM_iso
toFree :: FSM s a -> FreeState s a
toFree (Return x) = Pure x
toFree (Getk k) = Free (StateGet (toFree . k))
toFree (Putk s m) = Free (StatePut s (toFree m))

fromFree :: FreeState s a -> FSM s a
fromFree (Pure x) = Return x
fromFree (Free (StateGet k)) = Getk (fromFree . k)
fromFree (Free (StatePut s m)) = Putk s (fromFree m)

-- ANCHOR_END: FSM_iso

-- ANCHOR: FreeState_instance
instance StateMonad (Free (StateOp s)) s where
  get = Free (StateGet Pure)
  put s = Free (StatePut s (Pure ()))

-- ANCHOR_END: FreeState_instance

-- ANCHOR: interpret
interpret ::
  (Monad m, Functor e) =>
  (forall x. e x -> m x) ->
  Free e a ->
  m a
interpret _ (Pure x) = pure x
interpret h (Free g) = join (fmap (interpret h) (h g))

-- ANCHOR_END: interpret

-- ANCHOR: stateOps
stateOps :: (StateMonad m s) => StateOp s x -> m x
stateOps (StateGet k) = fmap k get
stateOps (StatePut s x) = put s >> pure x

runFreeState :: (StateMonad m s) => FreeState s a -> m a
runFreeState = interpret stateOps

-- ANCHOR_END: stateOps

-- ANCHOR: modifyEffects
modifyEffects ::
  (Functor e, Functor h) =>
  (e (Free e a) -> h (Free e a)) ->
  Free e a ->
  Free h a
modifyEffects _ (Pure x) = Pure x
modifyEffects g (Free e) = Free (fmap (modifyEffects g) (g e))

-- ANCHOR_END: modifyEffects

-- ANCHOR: countPuts
countPuts :: FreeState s a -> Maybe Int
countPuts (Pure _) = Just 0
countPuts (Free (StatePut _ m)) = fmap (1 +) (countPuts m)
countPuts (Free (StateGet _)) = Nothing

-- ANCHOR_END: countPuts

-- ANCHOR: runFreeState_targets
runFreeStateF :: FreeState s a -> s -> (a, s)
runFreeStateF m = runFState (runFreeState m)

runFreeStateI :: FreeState s a -> IORef s -> IO a
runFreeStateI m = runIState (runFreeState m)

-- ANCHOR_END: runFreeState_targets

-- ANCHOR: four_targets
tickF :: FState Int Int
tickF = tick

tickI :: IState Int Int
tickI = tick

tickO :: OState Int Int
tickO = tick

tickFree :: FreeState Int Int
tickFree = tick

-- ANCHOR_END: four_targets

stackExampleF :: FState [Int] (Maybe Int)
stackExampleF = stackExample

stackExampleI :: IState [Int] (Maybe Int)
stackExampleI = stackExample

stackExampleFree :: FreeState [Int] (Maybe Int)
stackExampleFree = stackExample
