module HO where

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs) | f x       = x : tail
                   | otherwise = tail
  where tail = filter' f xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x : xs) | f x       = x : takeWhile' f xs
                      | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x : xs) | f x       = dropWhile' f xs
                      | otherwise = x : xs
