module ExFromWhile where

-- 2 even [1,2,4,6,8,7,6,5] -> [2,4,6]
fromWhile :: Int -> (a -> Bool) -> [a] -> [a]
fromWhile n p xs = takeWhile p (drop n xs)

fromFor :: Int -> Int -> [a] -> [a]
fromFor x y xs = take y (drop x xs)

fromTo :: Int -> Int -> [a] -> [a]
fromTo x y xs = take (y-x+1) (drop x xs)

fromToThat :: Int -> Int -> (a -> Bool) -> [a] -> [a]
fromToThat x y p xs = filter p (take (y-x+1) (drop x xs))
