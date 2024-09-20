module Week4.Exceptions where

import Control.Exception (SomeException, catch, evaluate)
import System.IO.Error (isDoesNotExistError)

handleDivByZero :: IO ()
handleDivByZero = do
  let handler :: SomeException -> IO ()
      handler e = putStrLn $ "It went wrong: " ++ show e
  print (div 1 0) `catch` handler

doesNotWork :: IO Int
doesNotWork = do
  let handler :: SomeException -> IO Int
      handler e = do
        putStrLn $ "It went wrong: " ++ show e
        pure 42
  pure (div 1 0) `catch` handler

doesWork :: IO Int
doesWork = do
  let handler :: SomeException -> IO Int
      handler e = do
        putStrLn $ "It went wrong: " ++ show e
        pure 42
  evaluate (div 1 0) `catch` handler

data FileContents
  = FileNotFound
  | CouldNotRead String
  | FileContents String
  deriving (Show)

readFileSafely :: FilePath -> IO FileContents
readFileSafely f = (FileContents <$> readFile f) `catch` onException
  where
    onException :: IOError -> IO FileContents
    onException e =
      if isDoesNotExistError e
        then pure FileNotFound
        else pure $ CouldNotRead $ show e
