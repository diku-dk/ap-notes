{-# LANGUAGE GADTs #-}
module Main where

import qualified Genserver as GS


main :: IO ()
main = do
  c <- counter 0
  incr c
  v <- getvalue c

  putStrLn $ "The counter should now be 5, and it is" ++ show v


counter = GS.spawn $ counterLoop

-- TODO: MKind and serverloop belongs in Genserver

data MKind req from tell = MCall req from | MCast tell

serverloop handlecast handlecall input state = loop state
  where loop state = do
          msg <- GS.receive input
          case msg of
            MCast t -> do
              let newState = handlecast state t
              loop newState
            MCall r from -> do
              let (newState, res) = handlecall state r
              GS.send from res
              loop newState

-- TODO: Maybe mkReq should have a better name and be in Genserver
mkReq inp from = MCall inp from




data NBMsg = NonblockIncr

data BMsg reply where
  Incr     ::        BMsg ()
  Decr     :: Int -> BMsg ()
  GetValue ::        BMsg Int

type State = Int

incr cnt = GS.requestReply cnt $ mkReq Incr
decrWith cnt n = GS.requestReply cnt $ mkReq $ Decr n
getValue cnt = GS.requestReply cnt $ mkReq GetValue
nb_incr cnt = GS.sendTo cnt $ MCast NonblockIncr

counterLoop input state  = serverloop handlecast handlecall input state
  where
    handlecast state NonblockIncr = state + 1

    handlecall :: State -> BMsg reply -> (State, reply)
    handlecall state msg =
      case msg of
        Incr     -> (state, ())
        Decr n   -> (state - n, ())
        GetValue -> (state, state)
