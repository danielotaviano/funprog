{-# LANGUAGE InstanceSigs #-}
module ExMatrix2x2
  ( matrix
  , zero
  , identity
  , rows
  , cols
  , getElem
  , transpose
  , det
  , isDiagonal
  , isTriangular
  , isLowerTriangular
  , isUpperTriangular
  , singular
  , invertible
  , inverse
  ) where

type Number = Double
type Row = [Number]
type Col = [Number]

data Matrix2x2 = Matrix2x2 Number Number Number Number

instance Show Matrix2x2 where
  show (Matrix2x2 x1 x2 x3 x4) =
    " "
      ++ "("
      ++ show x1
      ++ " "
      ++ show x3
      ++ ")"
      ++ "\n"
      ++ " "
      ++ "("
      ++ show x2
      ++ " "
      ++ show x4
      ++ ")"
      ++ "\n"

instance Eq Matrix2x2 where
  (==) (Matrix2x2 x1 x2 x3 x4) (Matrix2x2 y1 y2 y3 y4) =
    x1 == y1 && x2 == y2 && x3 == y3 && x4 == y4



instance Num Matrix2x2 where
  (+) :: Matrix2x2 -> Matrix2x2 -> Matrix2x2
  (+) (Matrix2x2 x1 x2 x3 x4) (Matrix2x2 y1 y2 y3 y4) =
    Matrix2x2 (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4)
  (*) (Matrix2x2 x1 x2 x3 x4) (Matrix2x2 y1 y2 y3 y4) = Matrix2x2
    ((x1 * y1) + x3 * y2)
    ((x2 * y1) + (x4 * y2))
    ((x1 * y3) + (x3 * y4))
    ((x2 * y3) + (x4 * y4))
  negate (Matrix2x2 x1 x2 x3 x4) =
    Matrix2x2 ((-1) * x1) ((-1) * x2) ((-1) * x3) ((-1) * x4)
  abs         = undefined
  signum      = undefined
  fromInteger = undefined

-- matrix a b c d should create the matrix
-- ( a c )
-- ( b d )
matrix :: Number -> Number -> Number -> Number -> Matrix2x2
matrix = undefined

zero :: Matrix2x2
zero = undefined

identity :: Matrix2x2
identity = undefined

rows :: Matrix2x2 -> [Row]
rows = undefined

cols :: Matrix2x2 -> [Col]
cols = undefined

getElem :: (Int, Int) -> Matrix2x2 -> Number
getElem = undefined

transpose :: Matrix2x2 -> Matrix2x2
transpose = undefined

det :: Matrix2x2 -> Number
det = undefined

isDiagonal :: Matrix2x2 -> Bool
isDiagonal = undefined

isTriangular :: Matrix2x2 -> Bool
isTriangular = undefined

isLowerTriangular :: Matrix2x2 -> Bool
isLowerTriangular = undefined

isUpperTriangular :: Matrix2x2 -> Bool
isUpperTriangular = undefined

singular :: Matrix2x2 -> Bool
singular = undefined

invertible :: Matrix2x2 -> Bool
invertible = not . singular

inverse :: Matrix2x2 -> Matrix2x2
inverse = undefined

