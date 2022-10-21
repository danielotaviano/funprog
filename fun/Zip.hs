module Zip where




zip' :: [a] -> [b] -> [(a, b)]
zip' []       _        = []
zip' _        []       = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []       _        = []
zipWith' _ _        []       = []
zipWith' p (x : xs) (y : ys) = p x y : zipWith' p xs ys
