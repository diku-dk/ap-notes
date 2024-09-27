module Genserver
  ( Chan
  , Server
  , receive
  , send
  , sendTo
  , spawn
  , requestReply
  )
where

-- ANCHOR: Setup
import qualified Control.Concurrent as CC
import Control.Exception (evaluate)

type Chan a = CC.Chan a
-- ANCHOR_END: Setup


-- ANCHOR: Server
type Server message = (CC.ThreadId, Chan message)
-- ANCHOR_END: Server

-- ANCHOR: SendReceive
send :: Chan a -> a -> IO ()
send chan msg =
  CC.writeChan chan msg

sendTo :: Server a -> a -> IO ()
sendTo (_tid, input) msg =
  send input msg

receive :: Chan a -> IO a
receive = CC.readChan
-- ANCHOR_END: SendReceive

-- ANCHOR: Spawn
spawn :: (Chan a -> IO ()) -> IO (Server a)
spawn serverLoop = do
  input <- CC.newChan
  tid <- CC.forkIO $ serverLoop input
  return (tid, input)
-- ANCHOR_END: Spawn


-- ANCHOR: RequestReply
requestReply :: Server a -> (Chan b -> a) -> IO b
requestReply serv con = do
  reply_chan <- CC.newChan
  sendTo serv $ con reply_chan
  receive reply_chan
-- ANCHOR_END: RequestReply
