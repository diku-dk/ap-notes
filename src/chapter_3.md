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
