{-# LANGUAGE ExistentialQuantification #-}

module Week6.FreeConcurrency where

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Control.Monad (ap, liftM)
import Data.Maybe (fromMaybe)
import Week2.ReaderState (State, get, put, runState)

data Free e a
  = Pure a
  | Free (e (Free e a))

instance (Functor e) => Functor (Free e) where
  fmap = liftM

instance (Functor e) => Applicative (Free e) where
  pure = Pure
  (<*>) = ap

instance (Functor e) => Monad (Free e) where
  Pure x >>= f = f x
  Free g >>= f = Free $ h <$> g
    where
      h x = x >>= f

type Msg = String

type CC chan a = Free (CCOp chan) a

data CCOp chan a
  = CCFork (CC chan ()) a
  | CCNewChan (chan -> a)
  | CCSend chan Msg a
  | CCReceive chan (Msg -> a)

instance Functor (CCOp chan) where
  fmap f (CCFork m c) = CCFork m (f c)
  fmap f (CCNewChan c) = CCNewChan $ f . c
  fmap f (CCSend chan msg c) = CCSend chan msg $ f c
  fmap f (CCReceive chan c) = CCReceive chan $ f . c

ccNewChan :: CC chan chan
ccNewChan = Free $ CCNewChan pure

ccFork :: CC chan () -> CC chan ()
ccFork m = Free $ CCFork m $ pure ()

ccSend :: chan -> Msg -> CC chan ()
ccSend chan msg = Free $ CCSend chan msg $ pure ()

ccReceive :: chan -> CC chan Msg
ccReceive chan = Free $ CCReceive chan pure

interpCCIO :: CC (Chan Msg) a -> IO a
interpCCIO (Pure x) =
  pure x
interpCCIO (Free (CCFork m c)) = do
  _ <- forkIO $ interpCCIO m
  interpCCIO c
interpCCIO (Free (CCNewChan c)) = do
  chan <- newChan
  interpCCIO $ c chan
interpCCIO (Free (CCSend chan msg c)) = do
  writeChan chan msg
  interpCCIO c
interpCCIO (Free (CCReceive chan c)) = do
  msg <- readChan chan
  interpCCIO $ c msg

type ChanId = Int

data CCState = CCState
  { ccCounter :: Int,
    ccChans :: [(ChanId, [Msg])],
    ccThreads :: [CC Int ()]
  }

getChan :: ChanId -> State CCState [Msg]
getChan chan_id = do
  state <- get
  pure $
    fromMaybe (error "unknown channel") $
      lookup chan_id $
        ccChans state

setChan :: ChanId -> [Msg] -> State CCState ()
setChan chan_id msgs = do
  state <- get
  put $
    state
      { ccChans =
          (chan_id, msgs)
            : filter ((/= chan_id) . fst) (ccChans state)
      }

addThread :: CC Int () -> State CCState ()
addThread m = do
  state <- get
  put $ state {ccThreads = m : ccThreads state}

incCounter :: State CCState Int
incCounter = do
  state <- get
  put $ state {ccCounter = ccCounter state + 1}
  pure $ ccCounter state

interp :: CC Int a -> State CCState (CC Int a)
interp (Pure x) = pure $ Pure x
interp (Free (CCSend chan_id msg c)) = do
  msgs <- getChan chan_id
  setChan chan_id $ msgs ++ [msg]
  interp c
interp (Free (CCReceive chan_id c)) = do
  msgs <- getChan chan_id
  case msgs of
    [] -> pure $ Free $ CCReceive chan_id c
    msg : msgs' -> do
      setChan chan_id msgs'
      interp $ c msg
interp (Free (CCFork (Pure _) c)) =
  interp c
interp (Free (CCFork m c)) = do
  addThread m
  interp c
interp (Free (CCNewChan c)) = do
  chan_id <- incCounter
  setChan chan_id []
  interp $ c chan_id

stepThreads :: State CCState ()
stepThreads = do
  state <- get
  put $ state {ccThreads = []}
  threads <- mapM interp $ ccThreads state
  new_state <- get
  put $ new_state {ccThreads = threads ++ ccThreads new_state}

interpFully :: CC Int a -> State CCState a
interpFully (Pure x) = pure x
interpFully (Free op) = do
  stepThreads
  op' <- interp $ Free op
  interpFully op'

interpCCPure :: CC Int a -> a
interpCCPure orig =
  fst $ runState initial_state $ interpFully orig
  where
    initial_state =
      CCState
        { ccCounter = 0,
          ccChans = [],
          ccThreads = []
        }

carousel :: CC chan String
carousel = do
  chan_0 <- ccNewChan
  chan_1 <- ccNewChan
  chan_2 <- ccNewChan
  chan_3 <- ccNewChan
  chan_4 <- ccNewChan
  let passOn from to = do
        x <- ccReceive from
        ccSend to $ x ++ "x"
  ccFork $ passOn chan_0 chan_1
  ccFork $ passOn chan_1 chan_2
  ccFork $ passOn chan_2 chan_3
  ccFork $ passOn chan_3 chan_4
  ccSend chan_0 ""
  ccReceive chan_4

demoIO :: IO ()
demoIO = print =<< interpCCIO carousel

demoPure :: IO ()
demoPure = print $ interpCCPure carousel
