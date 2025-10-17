module Week7.Tricks where

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Control.Monad (forM_, forever)
import GenServer

data Peek a
  = Peek (ReplyChan (Maybe a))
  | Put a

type PeekChan a = Server (Peek a)

peek :: PeekChan a -> IO (Maybe a)
peek s = requestReply s Peek

peekChan :: Chan a -> IO (PeekChan a)
peekChan c = do
  s <- spawn $ loop []
  _ <- forkIO $ forever $ sendTo s . Put =<< receive c
  pure s
  where
    loop buffer sc = do
      msg <- receive sc
      case msg of
        Put x -> loop (buffer ++ [x]) sc
        Peek rc ->
          case buffer of
            [] -> do
              reply rc Nothing
              loop buffer sc
            x : xs -> do
              reply rc $ Just x
              loop xs sc

joinChans :: Chan a -> Chan b -> IO (Chan (Either a b))
joinChans chan_a chan_b = do
  chan_c <- newChan
  _ <- forkIO $ forever $ send chan_c . Left =<< receive chan_a
  _ <- forkIO $ forever $ send chan_c . Right =<< receive chan_b
  pure chan_c

joinChansAny :: [Chan a] -> IO (Chan a)
joinChansAny ins = do
  outc <- newChan
  forM_ ins $ \inc ->
    forkIO $ forever $ send outc =<< receive inc
  pure outc

joinChansAnyTagged :: [(tag, Chan a)] -> IO (Chan (tag, a))
joinChansAnyTagged ins = do
  outc <- newChan
  forM_ ins $ \(tag, inc) ->
    forkIO $ forever $ do
      msg <- receive inc
      send outc (tag, msg)
  pure outc

broadcast :: [Chan a] -> IO (Chan a)
broadcast [] = error "broadcast: empty list"
broadcast [x] = pure x
broadcast cs = do
  let (as, bs) = splitAt (length cs `div` 2) cs
  as_c <- broadcast as
  bs_c <- broadcast bs
  c <- newChan
  _ <- forkIO $ forever $ do
    msg <- receive c
    send as_c msg
    send bs_c msg
  pure c
