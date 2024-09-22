module Week4.FreeAsync where

import Week4.Free (Free (..))

type EventName = String

type EventValue = Int

type Event = (String, EventValue)

data EventOp a
  = WaitFor EventName (EventValue -> a)
  | LogMsg String a

instance Functor EventOp where
  fmap f (WaitFor s c) = WaitFor s $ \x -> f (c x)
  fmap f (LogMsg s c) = LogMsg s $ f c

type EventM a = Free EventOp a

waitFor :: String -> EventM EventValue
waitFor s = Free (WaitFor s pure)

logMsg :: String -> EventM ()
logMsg s = Free $ LogMsg s $ pure ()

adder :: EventM ()
adder = do
  logMsg "starting adder"
  x <- waitFor "add"
  y <- waitFor "add"
  logMsg $ unwords [show x, "+", show y, "=", show $ x + y]

multiplier :: EventM ()
multiplier = do
  logMsg "starting multiplier"
  x <- waitFor "mul"
  y <- waitFor "mul"
  logMsg $ unwords [show x, "*", show y, "=", show $ x * y]

divider :: EventM ()
divider = do
  logMsg "starting divider"
  x <- waitFor "div"
  y <- waitForDivisor
  logMsg $ unwords [show x, "/", show y, "=", show $ x `div` y]
  where
    waitForDivisor = do
      y <- waitFor "div"
      if y == 0
        then do
          logMsg $ "Cannot divide by zero"
          waitForDivisor
        else pure y

stepUntilWait :: EventM a -> IO (EventM a)
stepUntilWait (Pure x) = pure $ Pure x
stepUntilWait (Free (LogMsg s c)) = do
  putStrLn $ s
  stepUntilWait c
stepUntilWait (Free (WaitFor s c)) =
  pure $ Free $ WaitFor s c

stepSingleEvent :: EventM () -> Event -> IO (EventM ())
stepSingleEvent (Free (WaitFor waiting_for c)) (event_name, event_val) =
  if waiting_for == event_name
    then stepUntilWait $ c event_val
    else pure $ Free $ WaitFor waiting_for c
stepSingleEvent p _ = pure p

stepEventM :: [EventM ()] -> Event -> IO [EventM ()]
stepEventM [] _ = pure []
stepEventM (p : ps) event = do
  p' <- stepUntilWait p
  case p' of
    Pure () -> stepEventM ps event
    _ -> do
      p'' <- stepSingleEvent p' event
      ps' <- stepEventM ps event
      pure $ p'' : ps'

runEventM :: [EventM ()] -> [Event] -> IO [EventM ()]
runEventM ps [] = do
  pure ps
runEventM ps (e : es) = do
  ps' <- stepEventM ps e
  runEventM ps' es

interactivelyRunEventM :: [EventM ()] -> IO ()
interactivelyRunEventM [] = pure ()
interactivelyRunEventM ps = do
  ps' <- mapM stepUntilWait ps
  event <- readLn
  ps'' <- stepEventM ps' event
  interactivelyRunEventM ps''
