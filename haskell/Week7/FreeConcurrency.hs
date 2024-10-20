module Week6.FreeConcurrency where

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import Week2.ReaderState (State, get, put, runState)
import Week4.Free (Free (..))

type Msg = String

data CCOp chan a
  = CCFork (CC chan ()) a
  | CCNewChan (chan -> a)
  | CCSend chan Msg a
  | CCReceive chan (Msg -> a)

type CC chan a = Free (CCOp chan) a

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
  { ccCounter :: ChanId,
    ccChans :: [(ChanId, [Msg])],
    ccThreads :: [CC ChanId ()]
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

addThread :: CC ChanId () -> State CCState ()
addThread m = do
  state <- get
  put $ state {ccThreads = m : ccThreads state}

incCounter :: State CCState ChanId
incCounter = do
  state <- get
  put $ state {ccCounter = ccCounter state + 1}
  pure $ ccCounter state

step :: CC ChanId a -> State CCState (CC ChanId a)
step (Pure x) = pure $ Pure x
step (Free (CCNewChan c)) = do
  chan_id <- incCounter
  setChan chan_id []
  step $ c chan_id
step (Free (CCFork m c)) = do
  addThread m
  step c
step (Free (CCSend chan_id msg c)) = do
  msgs <- getChan chan_id
  setChan chan_id $ msgs ++ [msg]
  pure c
step (Free (CCReceive chan_id c)) = do
  msgs <- getChan chan_id
  case msgs of
    [] -> pure $ Free $ CCReceive chan_id c
    msg : msgs' -> do
      setChan chan_id msgs'
      step $ c msg

stepThreads :: State CCState ()
stepThreads = do
  state <- get
  put $ state {ccThreads = []}
  threads <- mapM step $ ccThreads state
  new_state <- get
  put $ new_state {ccThreads = threads ++ ccThreads new_state}

interp :: CC ChanId a -> State CCState a
interp (Pure x) = pure x
interp (Free op) = do
  stepThreads
  op' <- step $ Free op
  interp op'

interpCCPure :: CC ChanId a -> a
interpCCPure orig =
  fst $ runState initial_state $ interp orig
  where
    initial_state =
      CCState
        { ccCounter = 0,
          ccChans = [],
          ccThreads = []
        }

pipeline :: CC chan String
pipeline = do
  chan_0 <- ccNewChan
  chan_1 <- ccNewChan
  chan_2 <- ccNewChan
  chan_3 <- ccNewChan
  chan_4 <- ccNewChan
  let passOn tok from to = do
        x <- ccReceive from
        ccSend to $ x ++ tok
  ccFork $ passOn "a" chan_0 chan_1
  ccFork $ passOn "b" chan_1 chan_2
  ccFork $ passOn "c" chan_2 chan_3
  ccFork $ passOn "d" chan_3 chan_4
  ccSend chan_0 ""
  ccReceive chan_4

demoIO :: IO ()
demoIO = print =<< interpCCIO pipeline

demoPure :: IO ()
demoPure = print $ interpCCPure pipeline

infiniteWrite :: CC chan String
infiniteWrite = do
  chan <- ccNewChan
  ccFork $ forever $ ccSend chan "x"
  a <- ccReceive chan
  b <- ccReceive chan
  pure $ a ++ b

infiniteLoop :: CC chan String
infiniteLoop = do
  chan <- ccNewChan
  ccFork $ forever $ pure ()
  ccFork $ ccSend chan "x"
  ccReceive chan
