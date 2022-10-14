module ExRat
  ( rat
  , (//)
  , denominator
  , numerator
  ) where

-- define Rat:
data Rat = Rat Integer Integer

instance Show Rat where
  show (Rat i1 i2) = show i1 ++ " / " ++ show i2

instance Eq Rat where
  Rat x y == Rat n m = x * m == y * n

instance Num Rat where
  (+) (Rat x y) (Rat n m) = Rat (x * m + n * y) (y * m)
  (*) (Rat x y) (Rat n m) = Rat (x * n) (y * m)
  negate (Rat x y) = Rat (-x) y
  abs (Rat x y) | x < 0     = Rat (-x) y
                | otherwise = Rat x y
  signum (Rat x _) | x == 0    = Rat 0 1
                   | x < 0     = Rat (-1) 1
                   | otherwise = Rat 1 1
  fromInteger x = Rat x 1

instance Ord Rat where
  compare r1 r2 | r1 == r2  = EQ
                | r1 < r2   = LT
                | otherwise = GT

rat :: Integer -> Integer -> Rat
rat _ 0 = error "denominator is zero"
rat x y = Rat x y

(//) :: Rat -> Rat -> Rat
(//) r (Rat n m) = r * Rat m n

denominator :: Rat -> Integer
denominator (Rat _ m) = m

numerator :: Rat -> Integer
numerator (Rat n _) = n

