module Shapes where


data Color = Red | Green | Blue deriving (Show, Eq)
data RGBColor = RGBColor Double Double Double
  deriving (Show, Eq)

data Shape = Circle Double
           | Square Double
           | Rectangle Double Double
           deriving (Show, Eq)

area :: Shape -> Double
area (Circle r     ) = pi * r * r
area (Square s     ) = s * s
area (Rectangle w h) = w * h

rot90 :: Shape -> Shape
rot90 (Rectangle w h) = Rectangle h w
rot90 x               = x

height :: Shape -> Double
height (Circle r     ) = r * 2
height (Square s     ) = s
height (Rectangle _ h) = h
