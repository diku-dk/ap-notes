module Week6.Basics where

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
