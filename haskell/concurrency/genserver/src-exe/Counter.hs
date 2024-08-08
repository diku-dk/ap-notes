module Main where

import qualified Genserver as GS


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"


counter = GS.spawn counterLoop

data Msg = Incr     (GS.Chan ())
         | NonblockIncr
         | Decr Int (GS.Chan ())
         | GetValue (GS.Chan Int)

incr cnt = GS.requestReply cnt Incr
decrWith cnt n = GS.requestReply cnt $ Decr n
getValue cnt = GS.requestReply cnt GetValue
nb_incr cnt = GS.sendTo cnt NonblockIncr

counterLoop input state = do
  msg <- GS.receive input
  case msg of
    Incr from -> do
      let (newState, res) = (state + 1, ())
      GS.send from res
      counterLoop input newState
    Decr n from -> do
      let (newState, res) = (state - n, ())
      GS.send from res
      counterLoop input newState
    GetValue from -> do
      let (newState, res) = (state, state)
      GS.send from res
      counterLoop input newState

    NonblockIncr -> do
      let newState = state + 1
      counterLoop input newState
