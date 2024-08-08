module Genserver
  ( Chan
  , receive
  , send
  , sendTo
  , spawn
  , requestReply
  , MsgKind (..)
  )
where

import qualified Control.Concurrent as CC

type Chan a = CC.Chan a


data MsgKind req rep tell = Call req rep | Cast tell

type Server a = (CC.ThreadId, Chan a)


send :: Chan a -> a -> IO ()
send chan msg =
  CC.writeChan chan msg

sendTo :: Server a -> a -> IO ()
sendTo (_tid, input) msg =
  send input msg

receive :: Chan a -> IO a
receive = CC.readChan

spawn :: (Chan a -> t -> IO ()) -> t -> IO (Server a)
spawn server initial = do
  input <- CC.newChan
  tid <- CC.forkIO $ server input initial
  return (tid, input)

requestReply :: Server a -> (Chan b -> a) -> IO b
requestReply serv con = do
  reply_chan <- CC.newChan
  sendTo serv $ con reply_chan
  receive reply_chan
