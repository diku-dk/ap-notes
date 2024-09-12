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
the monadic interface. For example, we can define a derived parser
that parses an expected string and returns it:

```Haskell
import Control.Monad (void)

chunk :: String -> Parser String
chunk [] = pure ""
chunk (c : cs) = do
  void $ satisfy (== c)
  void $ chunk cs
  pure $ c : cs
```

```
> runParser (choice [chunk "foo", chunk "bar"]) "foo"
Just ("foo","")
```

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

We would likely also want to use the `eof` parser to assert that no
garbage follows the second integer.

## Tokenisation

Syntaxes for programming languages and data formats are usually
structured in the form of rules for how to combine *tokens* (sometimes
also called *lexemes*), along with rules for how the tokens are
formed.

Following the example above, we could consider integers (in the form
of a nonzero number of decimal digits) to be a token. In many
languages, tokens can be separated by any number of whitespace
characters. In the traditional view of parsing, the syntactic analysis
is preceded by a *lexical analysis* that splits the input into tokens,
using for example regular expressions. With parser combinators,
lexical and syntactic analysis is done simultaneously. Yet to
correctly handle the lexical rules of a language, we must exercise
significant discipline.

~~~admonish info
Not all languages have straightforward lexical
rules. Some (such as Python or Haskell) use indentation to structure
their syntactical rules, others have context-sensitive tokenisation,
and others allow extensible or user-defined syntaxes. Parser
combinators can cope with all of these, but for simplicity we stick to
more well-behaved syntaxes in these notes.
~~~

### Whitespace

One core concern is how to handle whitespace. The most common
convention is that each parser must consume any *trailing* whitespace,
but can assume that there is no *leading* whitespace. If we
systematically follow this rule, then we can ensure that we never fail
to handle whitespace. The exception is the top parser, where we cannot
assume that the initial input does not have leading whitespace. Still,
systematically doing anything without mistakes is difficult for
humans. In the following, we define some simple (but rigid) rules that
are easy to follow in our code.

First we write a parser that skips any number of whitespace
characters.

```Haskell
import Data.Char (isSpace)

space :: Parser ()
space = do
  _ <- many $ satisfy isSpace
  pure ()
```

Then we write a parser combinator that takes any parser and consumes
subsequent whitespace:

```Haskell
lexeme :: Parser a -> Parser a
lexeme p = do x <- p
              space
              pure x
```

Now we institute a rule: whenever we write a parser that operates at
the lexical level, it must be of the form

```
lFoo = lexeme $ ...
```

and *no other parser than those of this form* is allowed to use
`lexeme` or `space` directly. The `l` prefix is a mnemonic for
*lexical* - similarly we will begin prefixing our syntax-level parsers
with `l`.

For example, this would now be a parser for decimal integers that
consumes trailing whitespace:

```
lDecimal :: Integer
lDecimal = lexeme $ loop 1 . reverse <$> some parseDigit
  where
    loop _ [] = 0
    loop w (d : ds) =
      d * w + loop (w * 10) ds
```

Note that we will use `parseDigit` (which does not handle whitespace)
as a low level building block. We must only use these from within
`l`-prefixed functions.

Now we can easily a function that parses any number of
whitespace-separated decimal numbers:

```Haskell
pDecimals :: Parser [Integer]
pDecimals = many lDecimal
```

```
> runParser pDecimals "123  456   789   "
Just ([123,456,789],"")
```

However, we will fail to parse a string with leading whitespace:


```
> runParser pDecimals " 123"
Just ([]," 123")
```

The solution is to explicitly skip leading whitespace:

```
> runParser (space >> pDecimals) " 123"
Just ([123],"")
```

We do this *only* in the top level parser - usually in the one that is
passed immediately to the function that executed the `Parser` monad
itself.

### Longest match

When lexing the string `"123"`, we see it as a single token `123`
rather than three tokens `1`, `2`, `3`? The reason for this is that
most grammars follow the *longest match* (or *maximum munch*) rule:
each tokens extend as far as possible. This principle has actually
been baked into the definition of the `many` combinator above, as it
tries the recursive case before the terminal case. If we instead
defined `many` like this:

```Haskell
many :: Parser a -> Parser [a]
many p =
  choice
    [ pure [],
      (:) <$> p <*> many p
    ]
```

Then we would observe the following behaviour:

```
> runParser lDecimal "123"
Just (1,"23")
```

Thus, simply by defining `many` the way we originally did, we obtain
conventional behaviour.

## A larger example

Let us study a larger example of parsing, including proper
tokenisation, handling of keywords, and transforming a given grammar
to make it amenable to parsing.

Consider parsing a language of Boolean expressions, represented by the
following Haskell datatype.

```Haskell
data BExp
  = Lit Bool
  | Var String
  | And BExp BExp
  | Or BExp BExp
  deriving (Eq, Show)
```

The concrete syntax will be strings such as `"x"`, `"not x"`, `"x and
true"`, `"true and y or z`". We write down a grammar in
[EBNF](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form):

```
var ::= ? one or more alphabetic characters ? ;
BExp ::= "true"
       | "false"
       | "not" BExp
       | BExp "and" BExp
       | BExp "or" BExp ;
```

The words enclosed in double quotes are *terminals* (tokens). The
lowercase `var` is also a token, but is defined informally using an
EBNF comment. We adopt the convention that tokens can be separated by
whitespace, and that we follow the longest match rule. This is
strictly speaking an abuse of convention, as the handling of
whitespace ought also be explicit in the grammar, but it is common to
leave it out for simplicity.

Note that the grammar does not *exactly* match the Haskell abstract
syntax tree (AST) definition - in particular, the `"true"` and
`"false"` cases are combined into a single `Lit` constructor. This is
quite common, and we will see many cases where superfluous details of
the form of the concrete syntax are simplified away in the AST.

Our first attempt at parsing Boolean expressions looks like this:

```Haskell
import Data.Char (isAlpha)

lVar :: Parser String
lVar = lexeme $ some $ satisfy isAlpha

lTrue :: Parser ()
lTrue = lexeme $ void $ chunk "true"

lFalse :: Parser ()
lFalse = lexeme $ void $ chunk "false"

lAnd :: Parser ()
lAnd = lexeme $ void $ chunk "and"

lOr :: Parser ()
lOr = lexeme $ void $ chunk "or"

pBool :: Parser Bool
pBool =
  choice
    [ lTrue >> pure True,
      lFalse >> pure False
    ]

pBExp :: Parser BExp
pBExp =
  choice
    [ Lit <$> pBool,
      Var <$> lVar,
      do
        x <- pBExp
        lAnd
        y <- pBExp
        pure $ And x y,
      do
        x <- pBExp
        lOr
        y <- pBExp
        pure $ Or x y
    ]
```

Note now the structure of the code fairly closely matches the
structure of the grammar. This is is always something we seek to
aspire to.

### Keywords

Unfortunately, despite looking pretty, it fails to work properly for
any but trivial cases:

```
> runParser pBExp "x"
Just (Var "x","")
> runParser pBExp "true"
Just (Lit True,"")
> runParser pBExp "not x"
Just (Var "not","x")
```

The third case doesn't work. How can that be? The reason it goes wrong
can be seen by trying to parse a variable name:

```
> runParser lVar "not"
Just ("not","")
```

Words such as `not`, `and`, `or`, `true`, and `false` are also valid
variable names according to our parser. While we forgot to state it
explicitly in the grammar, our intention is for these words to be
*keywords* (or *reserved words*), which are not valid as variables. So
now we add another side condition to the grammar: a `var` must not be
one of `not`, `and`, `or`, `true`, and `false`. How do we implement
this in the parser? After all, in `lVar` we cannot know whether we
will end up reading a keyword until after we are done. We actually
need to add a new primitive operation to our parser: *explicit
failure*. We do this by implementing the `MonadFail` type class, which
requires a single method, `fail :: String -> Parser a`:

```Haskell
instance MonadFail Parser where
  fail _ = Parser $ \_ -> Nothing
```

The argument to `fail` is for an error message, which is not supported
by our parser definition, so we just throw it away. The result of
`fail` is a parser that always fails. We can use this to fix our
definition of `lVar` to explicitly not allow keywords:

```Haskell
keywords :: [String]
keywords = ["not", "true", "false", "and", "or"]

lVar :: Parser String
lVar = lexeme $ do
  v <- some $ satisfy isAlpha
  if v `elem` keywords
    then fail "keyword"
    else pure v
```

This shows some of the strength (and danger) of monadic parsing: we
can intermingle arbitrary Haskell-level decision making with the
purely syntactical analysis. This allows parser combinators to support
heinously context-sensitive grammars when necessary, but as mentioned
above, we will stick to more well-behaved ones in this course.

Now we get the desired behaviour:

```
> runParser pBExp "not x"
Just (Var "not","x")
```

But another case still behaves oddly:

```
> runParser pBExp "truexx"
Just (Lit True,"xx")
```

We don't really want to parse `"truexx"` as the Boolean literal `true`
followed by some garbage - this is in violation of the longest match
rule. We can fix this by requiring that a keyword is *not* followed by
an alphabetic character. This does require us to add a new primitive
parser to our parsing library (but this is the last one):

```Haskell
notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser f) = Parser $ \s ->
  case f s of
    Nothing -> Just ((), s)
    _ -> Nothing
```

The `notFollowedBy` combinator succeeds *only* if the provided parser
fails (and if so, consumes no input). We can then use this to define a
combinator for parsing keywords:

```Haskell
lKeyword :: String -> Parser ()
lKeyword s = lexeme $ do
  void $ chunk s
  notFollowedBy $ satisfy isAlpha
```

Using `lKeyword`, there is no need for dedicated functions for parsing
the individual keywords, although you can still use them if you like.
I prefer using `lKeyword` directly:

```Haskell
lKeyword :: String -> Parser ()
lKeyword s = lexeme $ do
  void $ chunk s
  notFollowedBy $ satisfy isAlpha

pBool :: Parser Bool
pBool =
  choice
    [ lKeyword "true" >> pure True,
      lKeyword "false" >> pure False
    ]

pBExp :: Parser BExp
pBExp =
  choice
    [ Lit <$> pBool,
      Var <$> lVar,
      do
        lKeyword "not"
        Not <$> pBExp,
      do
        x <- pBExp
        lKeyword "and"
        y <- pBExp
        pure $ And x y,
      do
        x <- pBExp
        lKeyword "or"
        y <- pBExp
        pure $ Or x y
    ]
```

```Haskell
> runParser pBExp "truexx"
Just (Var "truexx","")
```

### Left recursion

We have now implemented tokenisation properly. Unfortunately, our
parser still does not work:

```
> runParser pBExp "x and y"
Just (Var "x","and y")
```

The reason is the `choice` in `pBExp`. Our definition of `choice`
takes the *first* parser that succeeds, which is the one that produces
`Var`, and so it never proceeds to the one for `And`.

~~~admonish info
There are ways of implementing parser combinators
such that the ordering does not matter, which is largely by using a
list instead of a `Maybe` in the definition of `Parser`. However, this
will not solve the nontermination problem discussed below.
~~~

We can try to fix our parser by changing the order of parsers provided
to `choice`:

```Haskell
pBExp :: Parser BExp
pBExp =
  choice
    [ do
        x <- pBExp
        lKeyword "and"
        y <- pBExp
        pure $ And x y,
      do
        x <- pBExp
        lKeyword "or"
        y <- pBExp
        pure $ Or x y,
      Lit <$> pBool,
      Var <$> lVar,
      do
        lKeyword "not"
        Not <$> pBExp
    ]
```

Unfortunately, now the parser goes into an infinite loop:

```
> runParser pBExp "x"
... waiting for a long time ...
```

The operational reason is that underneath all the monadic syntax
sugar, our parsers are just recursive Haskell functions. Looking at
`pBExp`, we see that the very first thing it does is recursively
invoke `pBExp`. If we look at the EBNF grammar for Boolean
expressions, we also see that some of the production rules for `BExp`
start with `BExp`. In the nomenclature of parser theory, this is
called *left recursion*. The style of Parser combinator library we are
studying here is equivalent to so-called *recursive descent parsers
with arbitrary lookahead*, which are known to not support left
recursion. The solution to this problem is to rewrite the grammar to
eliminate left-recursion. If you need a refresher on how to do this,
see [Grammars and parsing with Haskell using parser
combinators](https://github.com/diku-dk/ap-e2024-pub/blob/main/week3/parsernotes.pdf).

Transforming the grammar (note that we do not modify the Haskell AST
definition) provides us with the following:

```
var ::= ? one or more alphabetic characters ? ;
BExp2 ::= "true"
        | "false"
        | var
        | "not" BExp ;
BExp' ::= "and" BExp2 BExp'
        | "or" BExp2 BExp'
        | ;
BExp ::= BExp2 BExp' ;
```

Note that we have decided that the `and` operator is
left-associative - meaning that `"x and y and z"` is parsed as `"(x
and y) and x"` (or would be if our syntax supported parentheses).

A grammar without left-recursion can be implemented fairly
straightforwardly. The idea is that parsing a `BExp` consists of
initially parsing a `BExp2`, followed by a chain of zero or more
`and`/`or` clauses.

```Haskell
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
            y <- pBExp
            chain $ And x y,
          do
            lKeyword "or"
            y <- pBExp
            chain $ Or x y,
          pure x
        ]
```

```
> runParser pBExp "x and y"
Just (And (Var "x") (Var "y"),"")
> runParser pBExp "x and y and z"
Just (And (And (Var "x") (Var "y")) (Var "z"),"")
```

Usually when constructing a parser, we do not expose the raw parser
functions (such as `pBExp`), but instead define a convenient wrapper
function, such as the following:

```Haskell
parseBExp :: String -> Maybe BExp
parseBExp s = fst <$> runParser p s
  where
    p = do
      space
      x <- pBExp
      eof
      pure x
```

### Operator precedence

## Megaparsec

While the parser library implemented above is fully operational, it
has serious flaws that leave it unsuitable for production use:

1. It is rather inefficient. This is partially because of the use of
   `String` as the fundamental type, but mostly because of how
   `choice` is implemented, which has to keep track of the original
   input essentially forever, even if backtracking will never become
   relevant.

2. It produces no error messages, instead simply returning `Nothing`.

While point 1 does not matter much for AP, point 2 makes it very
difficult to debug your parsers - which is certainly going to have an
impact. For the exercises and assignments, you will therefore be using
a state-of-the-art industrial-strength parser combinator library:
[Megaparsec](https://hackage.haskell.org/package/megaparsec).

The downside of using an industrial parser library such as Megaparsec
is that is is complicated. It has a rich programming interface and
more complicated types than what we need in AP. However, a subset of
Megaparsec's interface is identical to the interface presented above
(this is not a coincidence), and this is what we will be using in AP.

The facilities we will need are from the
[`Text.Megaparsec`](https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec.html)
module. Megaparsec is quite well documented, so it may be worth your
time to skim the documentation, although the information provided here
is intended to be sufficient for our needs. In Megaparsec, parsers are
monadic functions in the `Parsec e s` monad, which is itself a
specialisation of the `ParsecT` monad *transformer* - a concept that
lies outside of the AP curriculum. The point of this flexibilty is to
be generic in the underlying *stream type* (e.g. the kind of "string"
we are parsing), the form that errors can take, and so on. We do not
need such flexibility, and the first thing we need to do when using
Megaparsec is to define the following type synonym:

```Haskell
import Data.Void (Void)

type Parser = Parsec Void String
```

This states that our `Parser`s will have no special error component,
and the input will be a `String`.

To run such a `Parser`, we make use of the `runParser` function, which
in simplified form has this type:

```Haskell
runParser :: Parser a
          -> String
          -> String
          -> Either (ParseErrorBundle String Void) a
```

The first `String` is the *filename* of the input, which is used for
error messages. The second `String` is the actual input. The result is
either a special error value, or the result of parsing. Note that in
contrast to our `runParser`, no remainder string is returned. The
error value can be turned into a human-readable string with the
function `errorBundlePretty`.

For example, this is how we would define `parseBExp` using Megaparsec.
The rest of the parser code is (for now) completely unchanged:

```Haskell
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
```

We are using the `FilePath = String` type synonym in the function type
to make it clearer which is the filename and which is the input
string.

It mostly works just as before:

```
> parseBExp "input" "x and y and z"
Right (And (And (Var "x") (Var "y")) (Var "z"))
```

But it can now also produce quite nice error messages:

```
> either putStrLn print $ parseBExp "input" "x and y and"
input:1:12:
  |
1 | x and y and
  |            ^
unexpected end of input
expecting "false", "not", or "true"
> either putStrLn print $ parseBExp "input" "x true"
input:1:3:
  |
1 | x true
  |   ^
unexpected 't'
expecting "and", "or", or end of input
```

However, some inputs now produce a rather unexpected error:

```
> either putStrLn print $ parseBExp "input" "truex"
input:1:5:
  |
1 | truex
  |     ^
unexpected 'x'
```

The reason for this is that Megaparsec, for efficiency reasons, does
not automatically backtrack when a parser fails. Due to the way we
have ordered our `choice` in `pBExp2`, we will initially try to parse
the literal `true` with `lKeyword` in `pBool`, which will read the
input `true`, and then fail due to `notFollowedBy`. However, *the
input remains read*, which means Megaparsec's implementation of
`choice` won't even try to the other possibilities in `choice`. The
way to fix this is to use the `try` combinator, which has this type
(specialising to our `Parser` monad):

```Haskell
try :: Parser a -> Parser a
```

A parser `try p` behaves like `p`, but ensures the the parser state is
unmodified if `p` fails. We must use it whenever a parser can fail
*after* successfully consuming input. In this case, we must use it in
`lVar` and `lKeyword`:

```Haskell
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
```

When to use `try` is certainly rather un-intuitive at first, and
remains fairly subtle for ever. One possibility is to *always* use it
in the cases we pass to `choice` - this will work, but is inefficient,
as it makes every `choice` a potential backtracking point. Most
grammars are designed such that backtracking is only needed for the
lexical functions.
