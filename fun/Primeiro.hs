module Primeiro where

errado :: [Char]
errado = "not good"

addFive :: Integer -> Integer
addFive 0    = 100
addFive 1    = 10 + five
addFive five = five + 5

five :: Integer
five = 5

f :: Integer -> Integer
f x = addFive x + addFive x

sempreSix :: Integer -> Integer
sempreSix _ = 6

av :: Fractional a => a -> a -> a
av num1 num2 = (num1 + num2) / 2
