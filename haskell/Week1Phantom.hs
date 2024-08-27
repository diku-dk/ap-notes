module Week1Phantom where

data Joule

data Kilogram

data MetrePerSecond

data Q unit = Q Double
  deriving (Eq, Ord, Show)

weightOfUnladenSwallow :: Q Kilogram
weightOfUnladenSwallow = Q 0.020

speedOfUnladenSwallow :: Q Joule
speedOfUnladenSwallow = Q 9

energy :: Q Kilogram -> Q MetrePerSecond -> Q Joule
energy (Q m) (Q v) = Q (0.5 * m * (v ** 2))
