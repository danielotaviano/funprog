module Pref where

import           Prelude hiding (all, and, any, concat, filter, foldr, length,
                          map, or, product, sum)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op base []     = base
foldr op base (x:xs) = op x (foldr op base xs)

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

length :: Integral i => [a] -> i
length []     = 0
length (_:xs) = 1 + length xs

concat :: [[a]] -> [a]
concat = foldr (++) []

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x = x : filter p xs
  | otherwise = filter p xs

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x:xs)
  | p x = True
  | otherwise = any p xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x:xs)
  | (not . p) x = False
  | otherwise = all p xs

and :: [Bool] -> Bool
and [] = True
and (x:xs)
  | not x = False
  | otherwise = and xs

or :: [Bool] -> Bool
or [] = False
or (x:xs)
  | x = True
  | otherwise = or xs
