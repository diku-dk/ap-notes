# Monadic and Applicative Parsing

It is sometimes the case that we have to operate on data that is not
already in the form of a richly typed Haskell value, but is instead
stored in a file or transmitted across the network in some serialised
format - usually in the form of a sequence of bytes or characters.
Although such situations are always regrettable, in this chapter we
shall see a flexible technique for making sense out of unstructured
data: *parser combinators*.

## Parsing Integers Robustly

To start with, consider turning a `String` of digit characters into
the corresponding `Integer`. That is, we wish to construct the
following function:

```Haskell
readInteger :: String -> Integer
```

The function `ord :: Char -> Int` from `Data.Char` can convert a
character into its corresponding numeric code. Exploiting the fact
that the integers have consecutive codes, we can write a function for
converting a digit character into its corresponding `Integer`. Note
that we have to convert the `Int` produce by `ord` into an `Integer`:

```Haskell
import Data.Char (ord)

charInteger :: Char -> Integer
charInteger c = toInteger $ ord c - ord '0'
```

Exploiting the property that the numeric characters are consecutively
encoded, we can implement `readInt` with a straightforward recursive
loop over the characters of the string, from right to left:

```Haskell
readInteger :: String -> Integer
readInteger s = loop 1 $ reverse s
  where
    loop _ [] = 0
    loop w (c : cs) = charInteger c * w + loop (w * 10) cs
```

Example use:

```
> readInteger "123"
123
```

However, see what happens if we pass in invalid input:

```Haskell
λ> readInteger "xyz"
8004
```

Silently producing garbage on invalid input is usually considered poor
engineering. Instead, our function should return a `Maybe` type,
indicating invalid input by returning `Nothing`. This can be done by
using `isDigit` from `Data.Char` to check whether each character is a
proper digit:

```Haskell
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
```

Note how we are using the fact that `Maybe` is a monad to avoid
explicitly checking whether the recursive call to `loop` fails.

We now obtain the results we would expect:

```
> readIntegerMaybe "123"
Just 123
> readIntegerMaybe "xyz"
Nothing
```

## Composing Parsers

Now suppose we extend the problem: we must now parse an integer,
followed by a space, followed by another integer. We can of course
write a function that does this from scratch, but it would be better
if we could reuse our function that parses a single integer.
Unfortunately, this is not possible with `readIntegerMaybe`, as it
requires that the input string consists solely of digits. We could
split the string by spaces in advance, but this is rather ad-hoc.
Instead, let us construct a function that reads a *leading* integer
from a string, and then returns a *remainder string*.

```Haskell
readLeadingInteger :: String -> Maybe (Integer, String)
readLeadingInteger s =
  case span isDigit s of
    ("", _) -> Nothing
    (digits, s') -> Just (loop 1 $ reverse digits, s')
  where
    loop _ [] = 0
    loop w (c : cs) =
      charInteger c * w + loop (w * 10) cs
```

The `span` function breaks a string into two parts: the prefix of
characters that satisfy the predicate (here `isDigit`), and a
*remainder string* with the prefix removed. If the first part is
empty, we return `Nothing`, as an integer requires at least a single
digit. Otherwise we convert the digits into an `Integer` and return it
along with the remaining string.

```
> readLeadingInteger "123"
Just (321,"")
> readLeadingInteger "123 456"
Just (123," 456")
```

Let us also write two more helper functions: one that reads a single
leading space from a string (and returns the remainder), and one that
asserts that the string is empty.

```Haskell
readSpace :: String -> Maybe (Char, String)
readSpace (' ' : s) = Just (' ', s)
readSpace _ = Nothing

readEOF :: String -> Maybe ((), String)
readEOF "" = Just ((), "")
readEOF _ = Nothing
```

Note `readLeadingInteger`, `readSpace`, and `readEOF` all have types
of the same form: `String -> Maybe (a, String)` for some `a`. This
strongly suggests that there is some kind of commonality that we can
exploit to construct ways of composing them. But first, let us try to
compose them manually, to solve the problem of reading two
space-separated integers:

```Haskell
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
```

```
> readTwoIntegers "123 456"
Just ((123,456),"")
```

While it works, it is quite verbose with all that explicit
pattern-matching of `Nothing`/`Just`. We can exploit the fact that
`Maybe` is a monad to write it a bit more concisely:

```Haskell
readTwoIntegers2 :: String -> Maybe ((Integer, Integer), String)
readTwoIntegers2 s = do
  (x, s') <- readLeadingInteger s
  (_, s'') <- readSpace s'
  (y, s''') <- readLeadingInteger s''
  Just ((x, y), s''')
```

However, it is still quite annoying that we manually have to thread
the `String` around. It also means we can screw up, and use the wrong
one, since they all have the same type. It would be better if this
kind of book-keeping was done automatically behind the scenes. And
indeed that is possible, by creating a *parser monad*, whose structure
is essentially the same as the functions above, and which hides away
the book-keeping behind the monadic interface.

## A simple parser monad

As stated above, all our parser functions are of the form `String ->
Maybe (a, String)`. We will make that the definition of our parser
type:

```Haskell
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
```

Now `runParser :: Parser a -> String -> Maybe (a, String)` is the
function for running a parser on some input.

This type can be made a monad. Its definition looks like this:

```Haskell
instance Monad Parser where
  f >>= g = Parser $ \s ->
    case runParser f s of
      Nothing -> Nothing
      Just (x, s') -> runParser (g x) s'
```

The idea is the following. We have `f :: Parser a` and `g :: a ->
Parser b`. We can run the parser `f` to get either a parse error or a
value `x :: a` and a remainder string `s'`. We can then pass `x` to
`g` in order to obtain a `Parser b`, which we can then run on `s'.` It
is quite similar to a state monad combined the `Maybe` monad.

We also need to define `Applicative` and `Functor` instances. As
always, the only case that is not purely mechanical is `pure`, which
is used for a "parser" that consumes no input and always succeeds.

```Haskell
import Control.Monad (ap)

instance Functor Parser where
  fmap f x = do
    x' <- x
    pure $ f x'

instance Applicative Parser where
  (<*>) = ap
  pure x = Parser $ \s -> Just (x, s)
```

Now that we have defined the fundamental machinery, we can define some
very simple primitive parsers. We start with one that parses a single
character that satisfies some predicate:

```Haskell
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  c : cs ->
    if p c
      then Just (c, cs)
      else Nothing
  _ -> Nothing
```

```
> runParser (satisfy isDigit) "123"
Just ('1',"23")
```

And a parser that succeeds only if there is no more input left:

```Haskell
eof :: Parser ()
eof = Parser $ \s ->
  case s of
    "" -> Just ((), "")
    _ -> Nothing
```

While `eof` may seem a bit odd, we often use it as the very last step
of parsing a complete file or data format, as usually we do not want
to allow trailing garbage.

A parser *combinator* is a function on parsers. As our first parser
combinator, we will construct one that accepts a list of parsers, and
tries them all in turn. This is useful in the very common case where
multiple inputs can be valid.

```Haskell
choice :: [Parser a] -> Parser a
choice [] = Parser $ \_ -> Nothing
choice (p : ps) = Parser $ \s ->
  case runParser p s of
    Nothing -> runParser (choice ps) s
    Just (x, s') -> Just (x, s')
```

The parsers and combinators above directly manipulate the parser state
and use the `Parser` constructor. We say they are *primitive parsers*.
However, the vast majority of parsers we will write will not be
primitive, but will instead be built in terms of the primitives, using
the monadic interface.

## Using Parser Combinators

Let us try to rewrite our integer parser using the `Parser` type.
First, we define a function for parsing a single digit, including
decoding.

```Haskell
parseDigit :: Parser Integer
parseDigit = do
  c <- satisfy isDigit
  pure $ toInteger $ ord c - ord '0'
```

Here is how we would use it to write a function that parses two
digits:

```Haskell
parseTwoDigits :: Parser (Integer, Integer)
parseTwoDigits = do
  x <- parseDigit
  y <- parseDigit
  pure (x, y)
```

Or more concisely, making use of the `Functor` and `Applicative`
operators, as well as the operator section `(,)`:

```Haskell
parseTwoDigits :: Parser (Integer, Integer)
parseTwoDigits = (,) <$> parseDigit <*> parseDigit
```

We also construct a combinator that repeatedly applies a given parser
until it fails:

```Haskell
many :: Parser a -> Parser [a]
many p =
  choice
    [ (:) <$> p <*> many p,
      pure []
    ]
```

```
> runParser (many parseDigit) "123"
Just ([1,2,3],"")
```

(Ponder what happens if we flipped the elements in the list we pass to
`choice` in the definition of `many`.)

We have to be careful when using `many`: if it is given a parser that
can succeed without consuming any input (such as `eof`), it will loop
forever.

Also, `many` is not quite what we need, as it will succeed even if the
given parser succeeds zero times. The variant `some` requires that the
given parser succeeds at least once:

```Haskell
some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p
```

Now we can write our function for parsing integers:

```Haskell
parseInteger = do
  digits <- some parseDigit
  pure $ loop 1 $ reverse digits
  where
    loop _ [] = 0
    loop w (d : ds) =
      d * w + loop (w * 10) ds
```

Or more concisely:

```Haskell
parseInteger :: Parser Integer
parseInteger = loop 1 . reverse <$> some parseDigit
  where
    loop _ [] = 0
    loop w (d : ds) =
      d * w + loop (w * 10) ds
```

And finally we can use it to build a parser for two space-separated integers:

```Haskell
parseTwoIntegers :: Parser (Integer, Integer)
parseTwoIntegers = do
  x <- parseInteger
  _ <- satisfy (== ' ')
  y <- parseInteger
  pure (x, y)
```

See how easy it is to understand what input it parses (once you
understand parser combinators, mind you), compared to the original
`readTwoIntegers` function, which was littered with book-keeping.

```
> runParser parseTwoIntegers "123 456"
Just ((123,456),"")
```
