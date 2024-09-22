module Week6.Counter where

import Control.Concurrent

data Msg
  = Incr (Chan ())
  | Decr Int (Chan ())
  | GetValue (Chan Int)

type Counter = Chan Msg

counter :: IO Counter
counter = do
  input <- newChan
  _ <- forkIO $ counterLoop input 0
  return input

requestReply :: Counter -> (Chan a -> Msg) -> IO a
requestReply cnt con = do
  reply_chan <- newChan
  writeChan cnt $ con reply_chan
  readChan reply_chan

incr :: Counter -> IO ()
incr cnt = requestReply cnt Incr

decrWith :: Counter -> Int -> IO ()
decrWith cnt n = requestReply cnt $ Decr n

getValue :: Counter -> IO Int
getValue cnt = requestReply cnt GetValue

counterLoop :: Chan Msg -> Int -> IO b
counterLoop input state = do
  msg <- readChan input
  case msg of
    Incr from -> do
      let (newState, res) = (state + 1, ())
      writeChan from res
      counterLoop input newState
    Decr n from -> do
      let (newState, res) = (state - n, ())
      writeChan from res
      counterLoop input newState
    GetValue from -> do
      let (newState, res) = (state, state)
      writeChan from res
      counterLoop input newState
