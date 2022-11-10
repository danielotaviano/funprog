module Wakeup where

insertAt :: c -> Int -> [c] -> [c]
insertAt c _ []     = [c]
insertAt x 0 xs     = x : xs
insertAt x n (y:ys) = y : insertAt x (n - 1) ys

inserts :: c -> [c] -> [[c]]
inserts c []     = [[c]]
inserts c (x:xs) = (c : x : xs) : map (x :) (inserts c xs)

inserts' c xs = [insertAt c i xs | i <- [0 .. length xs]]

cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [x : ys | x <- xs, ys <- cp xss]
