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
type Server msg = (ThreadId, Chan msg)
-- ANCHOR_END: Server

-- ANCHOR: SendReceive
send :: Chan a -> a -> IO ()
send chan msg =
  writeChan chan msg

sendTo :: Server a -> a -> IO ()
sendTo (_tid, input) msg =
  send input msg

receive :: Chan a -> IO a
receive = readChan
-- ANCHOR_END: SendReceive

-- ANCHOR: Spawn
spawn :: (Chan a -> IO ()) -> IO (Server a)
spawn serverLoop = do
  input <- newChan
  tid <- forkIO $ serverLoop input
  return (tid, input)
-- ANCHOR_END: Spawn

-- ANCHOR: RequestReply
newtype ReplyChan a = ReplyChan (Chan a)

requestReply :: Server a -> (ReplyChan b -> a) -> IO b
requestReply serv con = do
  reply_chan <- ReplyChan <$> newChan
  sendTo serv $ con reply_chan
  receiveReply reply_chan

reply :: ReplyChan a -> a -> IO ()
reply (ReplyChan chan) x = send chan x

receiveReply :: ReplyChan a -> IO a
receiveReply (ReplyChan chan) = receive chan
-- ANCHOR_END: RequestReply

data Timeout = Timeout

-- ANCHOR: ActionWithTimeout
actionWithTimeout :: Int -> IO a -> IO (Either Timeout a)
actionWithTimeout seconds action = do
  chan <- ReplyChan <$> newChan
  _ <- forkIO $ do
    -- worker thread
    x <- action
    reply chan $ Right x
  _ <- forkIO $ do
    -- timeout thread
    threadDelay (seconds * 1000000)
    reply chan $ Left Timeout
  receiveReply chan
-- ANCHOR_END: ActionWithTimeout

-- ANCHOR: ActionWithTimeoutKill
actionWithTimeoutKill :: Int -> IO a -> IO (Either Timeout a)
actionWithTimeoutKill seconds action = do
  chan <- ReplyChan <$> newChan
  worker_tid <- forkIO $ do
    -- worker thread
    x <- action
    reply chan $ Right x
  _ <- forkIO $ do
    -- timeout thread
    threadDelay (seconds * 1000000)
    killThread worker_tid
    reply chan $ Left Timeout
  receiveReply chan
-- ANCHOR_END: ActionWithTimeoutKill
