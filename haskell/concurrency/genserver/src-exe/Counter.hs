module Main
  ( CounterServer
  , newCounter
  , getValue
  , incr
  , decr

  , main
  )
where

import qualified Genserver as GS
import Control.Monad (replicateM_)

type InternalData = Int

-- ANCHOR: CounterMsg
data Msg = GetValue (GS.Chan Int)
         | Incr
         | Decr Int (GS.Chan Bool)
-- ANCHOR_END: CounterMsg

-- ANCHOR: CounterAPI
type CounterServer = GS.Server Msg

newCounter :: Int -> IO CounterServer
newCounter initial | initial >= 0 = GS.spawn counterLoop initial
newCounter _ = error "Initial value should be non-negative"

getValue :: CounterServer -> IO Int
getValue cnt = GS.requestReply cnt GetValue

incr :: CounterServer -> IO ()
incr cnt = GS.sendTo cnt Incr

decr :: CounterServer -> Int -> IO Bool
decr cnt n | n >= 0 = GS.requestReply cnt $ Decr n
decr _ _ = error "Cannot decrement with negative amount"
-- ANCHOR_END: CounterAPI

-- ANCHOR: CounterLoop
counterLoop :: GS.Chan Msg -> InternalData -> IO ()
counterLoop input state = do
  msg <- GS.receive input
  case msg of
    GetValue from -> do
      let (newState, res) = (state, state)
      GS.send from res
      counterLoop input newState
    Incr -> do
      let newState = state + 1
      counterLoop input newState
    Decr n from -> do
      let (newState, res) =
            case state of
              value | value > n -> (value - n, True)
              _                 -> (state, False)
      GS.send from res
      counterLoop input newState
-- ANCHOR_END: CounterLoop


main :: IO ()
-- ANCHOR: CounterExample
main = do
  c <- newCounter 0
  incr c
  replicateM_ 5 $ incr c
  _ <- decr c 1
  v <- getValue c
  putStrLn $ "The counter should now be 5, and it is " ++ show v
-- ANCHOR_END: CounterExample
