module Week5.Basics where

import Control.Concurrent
  ( Chan,
    ThreadId,
    forkIO,
    newChan,
    readChan,
    writeChan,
  )

runThread :: IO ()
runThread = do
  t <- forkIO $ putStrLn "Hello there."
  print t

channelExample :: IO ()
channelExample = do
  c <- newChan
  _ <- forkIO $ do
    r <- readChan c
    putStrLn $ "Received message: " <> r
  writeChan c "Hello there."

channelLoopExample :: IO ()
channelLoopExample = do
  c <- newChan
  let threadLoop = do
        r <- readChan c
        putStrLn $ "Received message: " <> r
        threadLoop
  _ <- forkIO threadLoop
  writeChan c "The first"
  writeChan c "The second"
  writeChan c "The third"

data Msg = MsgInc Int (Chan Int)

threadLoop :: Chan Msg -> IO ()
threadLoop c = do
  msg <- readChan c
  case msg of
    MsgInc x from ->
      writeChan from (x + 1)
  threadLoop c

performRPC :: Chan Msg -> Int -> IO Int
performRPC c x = do
  from <- newChan
  writeChan c $ MsgInc x from
  readChan from

ex2 :: IO ()
ex2 = do
  c <- newChan
  _ <- forkIO $ threadLoop c
  print =<< performRPC c 0
  print =<< performRPC c 1
