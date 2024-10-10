{-# LANGUAGE ExistentialQuantification #-}

module Week6.FreeConcurrency where

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Control.Monad (ap, liftM)
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

type CC chan a = Free (CCOp chan) a

data CCOp chan a
  = CCPrint String a
  | CCFork (CC chan ()) a
  | forall msg. CCNewChan (chan msg -> a)
  | forall msg. CCSend (chan msg) msg a
  | forall msg. CCReceive (chan msg) (msg -> a)

instance Functor (CCOp chan) where
  fmap f (CCPrint s c) = CCPrint s (f c)
  fmap f (CCFork m c) = CCFork m (f c)
  fmap f (CCNewChan c) = CCNewChan $ f . c
  fmap f (CCSend chan msg c) = CCSend chan msg $ f c
  fmap f (CCReceive chan c) = CCReceive chan $ f . c

ccNewChan :: CC chan (chan msg)
ccNewChan = Free $ CCNewChan pure

ccFork :: CC chan () -> CC chan ()
ccFork m = Free $ CCFork m $ pure ()

ccPrint :: String -> CC chan ()
ccPrint s = Free $ CCPrint s $ pure ()

ccSend :: chan msg -> msg -> CC chan ()
ccSend chan msg = Free $ CCSend chan msg $ pure ()

ccReceive :: chan msg -> CC chan msg
ccReceive chan = Free $ CCReceive chan pure

interpCCIO :: CC Chan a -> IO a
interpCCIO (Pure x) =
  pure x
interpCCIO (Free (CCPrint s c)) = do
  putStrLn s
  interpCCIO c
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

data CCState = CCState {ccCounter :: Int}

interpCCPure :: CC Chan a -> a
interpCCPure orig = fst $ runState initial_state (interp orig)
  where
    initial_state = CCState {ccCounter = 0}
    interp = undefined

carousel :: CC chan Int
carousel = do
  chan_0 <- ccNewChan
  chan_1 <- ccNewChan
  chan_2 <- ccNewChan
  chan_3 <- ccNewChan
  chan_4 <- ccNewChan
  let passOn from to = do
        y <- ccReceive from
        ccSend to $ y + 1
  ccFork $ passOn chan_0 chan_1
  ccFork $ passOn chan_1 chan_2
  ccFork $ passOn chan_2 chan_3
  ccFork $ passOn chan_3 chan_4
  ccSend chan_0 0
  ccReceive chan_4

try :: IO ()
try = print =<< interpCCIO carousel
