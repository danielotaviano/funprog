module Talvez where

firstThat :: (a -> Bool) -> [a] -> Maybe a
firstThat _ [] = Nothing
firstThat p (x:xs)
    | p x = Just x
    | otherwise = firstThat p xs


isGoodFirstThat :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe [Char]
isGoodFirstThat _ _ [] = Nothing
isGoodFirstThat g f (x:xs)
    | f x && g x = Just "Sim!"
    | f x = Just "Nao!"
    | otherwise = isGoodFirstThat g f xs
