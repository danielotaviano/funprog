module ExRat
  ( rat
  , (//)
  , denominator
  , numerator
  ) where

-- define Rat:
data Rat = Rat Integer Integer
  deriving Show

-- instance Show Rat where
--     show = undefined

instance Eq Rat where
  Rat x y == Rat n m = x * m == y * n

instance Num Rat where
  (+)         = undefined
  (*)         = undefined
  negate      = undefined
  abs         = undefined
  signum      = undefined
  fromInteger = undefined

instance Ord Rat where
  compare = undefined

rat :: Integer -> Integer -> Rat
rat _ 0 = error "denominator is zero"
rat x y = Rat x y

(//) :: Rat -> Rat -> Rat
(//) = undefined

denominator :: Rat -> Integer
denominator = undefined

numerator :: Rat -> Integer
numerator = undefined

