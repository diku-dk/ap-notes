module GenServer
  ( Chan,
    Server,
    receive,
    send,
    sendTo,
    spawn,
    ReplyChan,
    requestReply,
    reply,
    actionWithTimeout,
    actionWithTimeoutKill,
  )
where

-- ANCHOR: Setup
import Control.Concurrent
  ( Chan,
    ThreadId,
    forkIO,
    killThread,
    newChan,
    readChan,
    threadDelay,
    writeChan,
  )
-- ANCHOR_END: Setup

-- ANCHOR: Server
data Server msg = Server ThreadId (Chan msg)
-- ANCHOR_END: Server

-- ANCHOR: SendReceive
send :: Chan a -> a -> IO ()
send = writeChan

receive :: Chan a -> IO a
receive = readChan
-- ANCHOR_END: SendReceive

-- ANCHOR: SendTo
sendTo :: Server a -> a -> IO ()
sendTo (Server _tid input) msg =
  send input msg
-- ANCHOR_END: SendTo

-- ANCHOR: Spawn
spawn :: (Chan a -> IO ()) -> IO (Server a)
spawn serverLoop = do
  input <- newChan
  tid <- forkIO $ serverLoop input
  return $ Server tid input
-- ANCHOR_END: Spawn

-- ANCHOR: ReplyChan
newtype ReplyChan a = ReplyChan (Chan a)
-- ANCHOR_END: ReplyChan

-- ANCHOR: reply
reply :: ReplyChan a -> a -> IO ()
reply (ReplyChan chan) x = send chan x
-- ANCHOR_END: reply

-- ANCHOR: RequestReply
requestReply :: Server a -> (ReplyChan b -> a) -> IO b
requestReply serv con = do
  reply_chan <- newChan
  sendTo serv $ con $ ReplyChan reply_chan
  receive reply_chan
-- ANCHOR_END: RequestReply

data Timeout = Timeout

-- ANCHOR: ActionWithTimeout
actionWithTimeout :: Int -> IO a -> IO (Either Timeout a)
actionWithTimeout seconds action = do
  chan <- newChan
  _ <- forkIO $ do
    -- worker thread
    x <- action
    send chan $ Right x
  _ <- forkIO $ do
    -- timeout thread
    threadDelay (seconds * 1000000)
    send chan $ Left Timeout
  receive chan
-- ANCHOR_END: ActionWithTimeout

-- ANCHOR: ActionWithTimeoutKill
actionWithTimeoutKill :: Int -> IO a -> IO (Either Timeout a)
actionWithTimeoutKill seconds action = do
  chan <- newChan
  worker_tid <- forkIO $ do
    -- worker thread
    x <- action
    send chan $ Right x
  _ <- forkIO $ do
    -- timeout thread
    threadDelay (seconds * 1000000)
    killThread worker_tid
    send chan $ Left Timeout
  receive chan
-- ANCHOR_END: ActionWithTimeoutKill
