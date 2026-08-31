module Week6.Async
  ( Async,
    Result (..),
    async,
    poll,
    wait,

    -- * Demonstrations
    simpleDemo,
    timeoutDemo,
    exceptionDemo,
  )
where

import Control.Concurrent
  ( forkIO,
    threadDelay,
  )
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (forM_, void)
import GenServer
import Prelude hiding (lookup)

data Result a
  = Timeout
  | Exception String
  | Value a
  deriving (Eq, Ord, Show)

data Msg a
  = MsgPutVal a
  | MsgPoll (ReplyChan (Maybe (Result a)))
  | MsgWait (ReplyChan (Result a))
  | MsgTimeout
  | MsgPutException String

data Async a = Async (Server (Msg a))

type Seconds = Int

noValueLoop :: Chan (Msg a) -> [ReplyChan (Result a)] -> IO ()
noValueLoop c waiters = do
  msg <- receive c
  case msg of
    MsgPutVal v' -> do
      forM_ waiters $ \from ->
        reply from $ Value v'
      valueLoop c (Value v')
    MsgPoll from -> do
      reply from Nothing
      noValueLoop c waiters
    MsgWait from ->
      noValueLoop c (from : waiters)
    MsgTimeout -> do
      forM_ waiters $ \from ->
        reply from Timeout
      valueLoop c Timeout
    MsgPutException err -> do
      forM_ waiters $ \from ->
        reply from $ Exception err
      valueLoop c (Exception err)

valueLoop :: Chan (Msg a) -> Result a -> IO ()
valueLoop c v = do
  msg <- receive c
  case msg of
    MsgPutVal _ ->
      valueLoop c v
    MsgPoll from -> do
      reply from $ Just v
      valueLoop c v
    MsgWait from -> do
      reply from v
      valueLoop c v
    MsgTimeout ->
      valueLoop c v
    MsgPutException _ ->
      valueLoop c v

async :: Seconds -> (a -> b) -> a -> IO (Async b)
async timeout f x = do
  s <- spawn $ \c -> do
    void $ forkIO $ do
      let computeValue = do
            x' <- evaluate $ f x
            send c $ MsgPutVal x'
          onException :: SomeException -> IO ()
          onException e = do
            send c $ MsgPutException $ show e
      catch computeValue onException
    void $ forkIO $ do
      threadDelay $ timeout * 1000000
      send c MsgTimeout
    noValueLoop c []
  pure $ Async s

poll :: Async a -> IO (Maybe (Result a))
poll (Async s) =
  requestReply s MsgPoll

wait :: Async a -> IO (Result a)
wait (Async s) =
  requestReply s MsgWait

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n =
  if n < 0
    then error "negative n"
    else fib (n - 1) + fib (n - 2)

simpleDemo :: IO ()
simpleDemo = do
  putStrLn "a"
  a <- async 1 fib 10
  print =<< poll a
  print =<< wait a

timeoutDemo :: IO ()
timeoutDemo = do
  a <- async 1 fib 100
  print =<< poll a
  print =<< wait a

exceptionDemo :: IO ()
exceptionDemo = do
  a <- async 1 fib (-1)
  print =<< poll a
  print =<< wait a
