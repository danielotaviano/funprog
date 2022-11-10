module Freefun where

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f, g) (x, y) = (f x, g y)

pair :: (a -> b, a -> c) -> (a -> (b, c))
pair (f, g) x = (f x, g x)

dup :: a -> (a, a)
dup = pair (id, id)

f :: Int -> Int
f = uncurry (+) . dup . uncurry (*) . dup
