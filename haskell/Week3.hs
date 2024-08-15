-- Random code related to Week 3

import Control.Monad (ap)
import Data.Char (isDigit, ord)

charInteger :: Char -> Integer
charInteger c = toInteger $ ord c - ord '0'

readInteger :: String -> Integer
readInteger s = loop 1 $ reverse s
  where
    loop _ [] = 0
    loop w (c : cs) = charInteger c * w + loop (w * 10) cs

readIntegerMaybe :: String -> Maybe Integer
readIntegerMaybe s = loop 1 $ reverse s
  where
    loop _ [] = Just 0
    loop w (c : cs)
      | isDigit c = do
          x <- loop (w * 10) cs
          pure $ charInteger c * w + x
      | otherwise =
          Nothing

readLeadingInteger :: String -> Maybe (Integer, String)
readLeadingInteger s =
  case span isDigit s of
    ("", _) -> Nothing
    (digits, s') -> Just (loop 1 $ reverse digits, s')
  where
    loop _ [] = 0
    loop w (c : cs) =
      charInteger c * w + loop (w * 10) cs

readSpace :: String -> Maybe (Char, String)
readSpace (' ' : s) = Just (' ', s)
readSpace _ = Nothing

readEOF :: String -> Maybe ((), String)
readEOF "" = Just ((), "")
readEOF _ = Nothing

readTwoIntegers :: String -> Maybe ((Integer, Integer), String)
readTwoIntegers s =
  case readLeadingInteger s of
    Nothing -> Nothing
    Just (x, s') -> case readSpace s' of
      Nothing -> Nothing
      Just (_, s'') ->
        case readLeadingInteger s'' of
          Nothing -> Nothing
          Just (y, s''') -> pure ((x, y), s''')

readTwoIntegers2 :: String -> Maybe ((Integer, Integer), String)
readTwoIntegers2 s = do
  (x, s') <- readLeadingInteger s
  (_, s'') <- readSpace s'
  (y, s''') <- readLeadingInteger s''
  Just ((x, y), s''')

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f x = do
    x' <- x
    pure $ f x'

instance Applicative Parser where
  (<*>) = ap
  pure x = Parser $ \s -> Just (x, s)

instance Monad Parser where
  f >>= g = Parser $ \s ->
    case runParser f s of
      Nothing -> Nothing
      Just (x, s') -> runParser (g x) s'

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  c : cs ->
    if p c
      then Just (c, cs)
      else Nothing
  _ -> Nothing

eof :: Parser ()
eof = Parser $ \s ->
  case s of
    "" -> Just ((), "")
    _ -> Nothing

choice :: [Parser a] -> Parser a
choice [] = Parser $ \_ -> Nothing
choice (p : ps) = Parser $ \s ->
  case runParser p s of
    Nothing -> runParser (choice ps) s
    Just (x, s') -> Just (x, s')

parseDigit :: Parser Integer
parseDigit = do
  c <- satisfy isDigit
  pure $ toInteger $ ord c - ord '0'

parseTwoDigits :: Parser (Integer, Integer)
parseTwoDigits = (,) <$> parseDigit <*> parseDigit

many :: Parser a -> Parser [a]
many p =
  choice
    [ (:) <$> p <*> many p,
      pure []
    ]

some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p

parseInteger :: Parser Integer
parseInteger = loop 1 . reverse <$> some parseDigit
  where
    loop _ [] = 0
    loop w (d : ds) =
      d * w + loop (w * 10) ds

parseTwoIntegers :: Parser (Integer, Integer)
parseTwoIntegers = do
  x <- parseInteger
  _ <- satisfy (== ' ')
  y <- parseInteger
  pure (x, y)
