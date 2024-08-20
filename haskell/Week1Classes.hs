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
