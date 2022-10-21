module Towards where

import           Data.Char

mapp :: (a -> a) -> [a] -> [a]
mapp _ []       = []
mapp f (x : xs) = f x : mapp f xs

numbers :: [Int]
numbers = [1, 2, 3, 4, 5]

phrase :: [Char]
phrase = "Hello there"

scream :: [Char] -> [Char]
scream = mapp toUpper

squareAll :: [Int] -> [Int]
squareAll = mapp (^ 2)
