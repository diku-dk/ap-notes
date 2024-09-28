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
  )
where

-- ANCHOR: Setup
import Control.Concurrent (Chan, ThreadId, forkIO, newChan, readChan, writeChan)
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
  reply_chan <- newChan
  sendTo serv $ con $ ReplyChan reply_chan
  receive reply_chan

reply :: ReplyChan a -> a -> IO ()
reply (ReplyChan chan) x = send chan x
-- ANCHOR_END: RequestReply
