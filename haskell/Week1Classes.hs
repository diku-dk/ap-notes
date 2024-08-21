import Prelude hiding (elem)

contains :: (Eq a) => a -> [a] -> Bool
contains _ [] = False
contains x (y : ys) = x == y || contains x ys

data PL
  = Haskell
  | FSharp
  | Futhark
  | SML
  | OCaml

instance Eq PL where
  Haskell == Haskell = True
  FSharp == FSharp = True
  Futhark == Futhark = True
  SML == SML = True
  OCaml == OCaml = True
  _ == _ = False
  x /= y = not $ x == y

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldr _ acc Nil = acc
  foldr f acc (Cons x xs) = f x (foldr f acc xs)

toList :: (Foldable f) => f a -> [a]
toList = foldr op []
  where
    op x acc = x : acc

elem :: (Eq a, Foldable f) => a -> f a -> Bool
elem needle = foldr op False
  where
    op x acc =
      acc || x == needle
