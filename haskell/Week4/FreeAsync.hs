-- | An event pump built on the free monad.  The definitions shared with the
-- week 4 slides (ap-e2026-private/lectures/monads/Monads.hs) are verbatim the
-- ones shown there; 'divider' and 'interactivelyRunEventM' are extra.
module Week4.FreeAsync where

import Week4.Free (Free (..))

-- ANCHOR: Event
type EventName = String

type EventValue = Int

type Event = (EventName, EventValue)

-- ANCHOR_END: Event

-- ANCHOR: EventOp
data EventOp a
  = WaitFor EventName (EventValue -> a)
  | LogMsg String a

instance Functor EventOp where
  fmap f (WaitFor s c) = WaitFor s $ f . c
  fmap f (LogMsg s c)  = LogMsg s $ f c

type EventM a = Free EventOp a

-- ANCHOR_END: EventOp

-- ANCHOR: waitFor_logMsg
waitFor :: EventName -> EventM EventValue
waitFor s = Free (WaitFor s Pure)

logMsg :: String -> EventM ()
logMsg s = Free $ LogMsg s $ Pure ()

-- ANCHOR_END: waitFor_logMsg

-- ANCHOR: processes
adder, multiplier :: EventM ()
adder = do
  logMsg "starting adder"
  x <- waitFor "add"; y <- waitFor "add"
  logMsg $ unwords [show x, "+", show y, "=", show $ x+y]
multiplier = do
  logMsg "starting multiplier"
  x <- waitFor "mul"; y <- waitFor "mul"
  logMsg $ unwords [show x, "*", show y, "=", show $ x*y]

divider :: EventM ()
divider = do
  logMsg "starting divider"
  x <- waitFor "div"
  y <- waitForDivisor
  logMsg $ unwords [show x, "/", show y, "=", show $ div x y]
  where
    waitForDivisor = do
      y <- waitFor "div"
      if y == 0
        then do
          logMsg "Cannot divide by zero"
          waitForDivisor
        else pure y

-- ANCHOR_END: processes

-- ANCHOR: stepUntilWait
stepUntilWait :: EventM a -> IO (EventM a)
stepUntilWait (Pure x) = pure $ Pure x
stepUntilWait (Free (LogMsg s c)) = do
  putStrLn s
  stepUntilWait c
stepUntilWait w@(Free (WaitFor _ _)) = pure w

-- ANCHOR_END: stepUntilWait

-- ANCHOR: deliver
deliver :: Event -> EventM () -> IO (EventM ())
deliver (name, val) (Free (WaitFor wanted c))
  | wanted == name = stepUntilWait (c val)
deliver _ p = pure p

-- ANCHOR_END: deliver

-- ANCHOR: runEventM
runEventM :: [EventM ()] -> [Event] -> IO [EventM ()]
runEventM ps [] = mapM stepUntilWait ps
runEventM ps (e : es) = do
  ps' <- mapM stepUntilWait ps
  ps'' <- mapM (deliver e) ps'
  runEventM ps'' es

-- ANCHOR_END: runEventM

-- ANCHOR: interactivelyRunEventM
interactivelyRunEventM :: [EventM ()] -> IO ()
interactivelyRunEventM ps = do
  ps' <- mapM stepUntilWait ps
  case filter running ps' of
    [] -> pure ()
    qs -> do
      event <- readLn
      qs' <- mapM (deliver event) qs
      interactivelyRunEventM qs'
  where
    running (Pure ()) = False
    running _ = True

-- ANCHOR_END: interactivelyRunEventM
