-- A simple parsing library that imitates the naming of the Megaparsec
-- API, but is implemented differently. In particular, this is a
-- backtracking-based parser, because this is easier to explain.
module QuasiParsec
  ( -- Parser interface
    Parser,
    parse,
    -- Primitive parsers
    satisfy,
    chunk,
    notFollowedBy,
    choice,
    eof,
    -- Parser combinators
    many,
    some,
  )
where

import Control.Monad (ap)

-- A parser of things
-- is a function from strings
-- to a list of pairs
-- of things and strings
newtype Parser a = Parser {runParser :: String -> [(a, String)]}

parse :: Parser a -> String -> [(a, String)]
parse (Parser f) s = f s

instance Functor Parser where
  fmap f x = do
    x' <- x
    pure $ f x'

instance Applicative Parser where
  (<*>) = ap
  pure x = Parser $ \s -> [(x, s)]

instance Monad Parser where
  Parser f >>= g = Parser $ \s ->
    concatMap
      ( \(x, s') ->
          let Parser g' = g x
           in g' s'
      )
      (f s)

instance MonadFail Parser where
  fail _ = Parser $ \_ -> []

-- Primitive parsers

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = []
    f (c : cs) =
      if p c
        then [(c, cs)]
        else []

notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser f) = Parser $ \s ->
  case f s of
    [] -> [((), s)]
    _ -> []

choice :: [Parser a] -> Parser a
choice ps = Parser $ \s -> concatMap (\p -> runParser p s) ps

chunk :: String -> Parser String
chunk = sequence . map (satisfy . (==))

eof :: Parser ()
eof = Parser $ \s ->
  case s of
    "" -> [((), "")]
    _ -> []

-- Parser combinators

many :: Parser a -> Parser [a]
many p =
  choice
    [ (:) <$> p <*> many p,
      pure []
    ]

some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p
