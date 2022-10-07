module ExFromWhile where

fromWhile :: Int -> (a -> Bool) -> [a] -> [a]
fromWhile n p = takeWhile p . drop n

fromFor :: Int -> Int -> [a] -> [a]
fromFor x y = take y . drop x

fromTo :: Int -> Int -> [a] -> [a]
fromTo x y  = take (y-x+1) . drop x

fromToThat :: Int -> Int -> (a -> Bool) -> [a] -> [a]
fromToThat x y p  = filter p . take (y-x+1) . drop x
