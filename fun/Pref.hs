module Pref where

import           Prelude                 hiding ( all
                                                , and
                                                , any
                                                , concat
                                                , filter
                                                , length
                                                , map
                                                , or
                                                , product
                                                , sum
                                                )



sum :: Num a => [a] -> a
sum []       = 0
sum (x : xs) = x + sum xs

product :: Num a => [a] -> a
product []       = 0
product (x : xs) = x * product xs


length :: Integral i => [a] -> i
length []       = 0
length (_ : xs) = 1 + length xs

concat :: [[a]] -> [a]
concat []       = []
concat (x : xs) = x ++ concat xs


filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs) | p x       = x : filter p xs
                  | otherwise = filter p xs


map :: (a -> b) -> [a] -> [b]
map _ []       = []
map f (x : xs) = f x : map f xs

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x : xs) | p x       = True
               | otherwise = any p xs


all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x : xs) | (not . p) x = False
               | otherwise   = all p xs

and :: [Bool] -> Bool
and [] = True
and (x : xs) | not x     = False
             | otherwise = and xs


or :: [Bool] -> Bool
or [] = False
or (x : xs) | x         = True
            | otherwise = or xs
