import Control.Monad (void)
import Data.Char (isAlpha, isSpace)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    runParser,
    satisfy,
    some,
    try,
  )

type Parser = Parsec Void String

space :: Parser ()
space = do
  _ <- many $ satisfy isSpace
  pure ()

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  space
  pure x

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
lVar = lexeme $ try $ do
  v <- some $ satisfy isAlpha
  if False && v `elem` keywords
    then fail "keyword"
    else pure v

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ try $ do
  void $ chunk s
  notFollowedBy $ satisfy isAlpha

pBool :: Parser Bool
pBool =
  choice
    [ lKeyword "true" >> pure True,
      lKeyword "false" >> pure False
    ]

pBExp2 :: Parser BExp
pBExp2 =
  choice
    [ Lit <$> pBool,
      Var <$> lVar,
      do
        lKeyword "not"
        Not <$> pBExp
    ]

pBExp :: Parser BExp
pBExp = do
  x <- pBExp2
  chain x
  where
    chain x =
      choice
        [ do
            lKeyword "and"
            y <- pBExp2
            chain $ And x y,
          do
            lKeyword "or"
            y <- pBExp2
            chain $ Or x y,
          pure x
        ]

parseBExp :: FilePath -> String -> Either String BExp
parseBExp fname s = case runParser p fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
  where
    p = do
      space
      x <- pBExp
      eof
      pure x
