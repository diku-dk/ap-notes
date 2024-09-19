module Week3.Parser where

import Control.Monad (ap, void)
import Data.Char (isAlpha, isDigit, isSpace, ord)

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

instance MonadFail Parser where
  fail _ = Parser $ \_ -> Nothing

notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser f) = Parser $ \s ->
  case f s of
    Nothing -> Just ((), s)
    _ -> Nothing

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

chunk :: String -> Parser String
chunk [] = pure ""
chunk (c : cs) = do
  void $ satisfy (== c)
  void $ chunk cs
  pure $ c : cs

parseDigit :: Parser Integer
parseDigit = do
  c <- satisfy isDigit
  pure $ toInteger $ ord c - ord '0'

parseTwoDigits :: Parser (Integer, Integer)
parseTwoDigits = (,) <$> parseDigit <*> parseDigit

many :: Parser a -> Parser [a]
many p =
  choice
    [ do
        x <- p
        xs <- many p
        pure $ x : xs,
      pure []
    ]

some :: Parser a -> Parser [a]
some p = do
  x <- p
  xs <- many p
  pure $ x : xs

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

space :: Parser ()
space = do
  _ <- many $ satisfy isSpace
  pure ()

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  space
  pure x

lDecimal :: Parser Integer
lDecimal = lexeme $ loop 1 . reverse <$> some parseDigit
  where
    loop _ [] = 0
    loop w (d : ds) =
      d * w + loop (w * 10) ds

pDecimals :: Parser [Integer]
pDecimals = many lDecimal

data BExp
  = Lit Bool
  | Not BExp
  | And BExp BExp
  | Or BExp BExp
  | Var String
  deriving (Eq, Show)

keywords :: [String]
keywords = ["not", "true", "false", "and", "or"]

lVar :: Parser String
lVar = lexeme $ do
  v <- some $ satisfy isAlpha
  if False && v `elem` keywords
    then fail "keyword"
    else pure v

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ do
  void $ chunk s
  notFollowedBy $ satisfy isAlpha

pBool :: Parser Bool
pBool =
  choice
    [ do
        lKeyword "true"
        pure True,
      do
        lKeyword "false"
        pure False
    ]

pAtom :: Parser BExp
pAtom =
  choice
    [ Lit <$> pBool,
      Var <$> lVar,
      do
        lKeyword "not"
        Not <$> pBExp
    ]

pBExp1 :: Parser BExp
pBExp1 = do
  x <- pAtom
  chain x
  where
    chain x =
      choice
        [ do
            lKeyword "and"
            y <- pAtom
            chain $ And x y,
          pure x
        ]

pBExp0 :: Parser BExp
pBExp0 = do
  x <- pBExp1
  chain x
  where
    chain x =
      choice
        [ do
            lKeyword "or"
            y <- pBExp1
            chain $ Or x y,
          pure x
        ]

pBExp :: Parser BExp
pBExp = pBExp0

parseBExp :: String -> Maybe BExp
parseBExp s = fst <$> runParser p s
  where
    p = do
      space
      x <- pBExp
      eof
      pure x
