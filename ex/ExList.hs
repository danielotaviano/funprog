{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
module ExList where

import qualified Data.Char as C
import qualified Data.List as L
import Prelude
  ( Bool (..),
    Char,
    Double,
    Enum (..),
    Eq (..),
    Float,
    Int,
    Integer,
    Integral (..),
    Num (..),
    Ord (..),
    String,
    curry,
    error,
    flip,
    not,
    otherwise,
    uncurry,
    undefined,
    ($),
    (&&),
    (.),
    (||),
  )
import qualified Prelude as P

-- to use a function from a qualified import
-- you need to prefix its name with its alias
-- and a dot:
-- P.head   C.toUpper   etc.
-- I import these for you to test the original functions on ghci:
-- ghci> :t C.toUpper
-- C.toUpper :: Char -> Char
-- You MUST NOT use ANY of these in your code

head :: [a] -> a
head [] = error "head: empty list"
head (r : rs) = r

tail :: [a] -> [a]
tail [] = error "tail: empty list"
tail (r : rs) = rs

null :: [a] -> Bool
null [] = True
null _ = False

length :: Integral i => [a] -> i
length [] = 0
length (_ : rs) = 1 + length rs

sum :: Num a => [a] -> a
sum [] = 0
sum (r : rs) = r + sum rs

product :: Num a => [a] -> a
product [] = 1
product (r : rs) = r * product rs

reverse :: [a] -> [a]
reverse [] = []
reverse (r : rs) = reverse rs ++ [r]

(++) :: [a] -> [a] -> [a]
(++) [] rs = rs
(++) (r : rs) rs' = r : (rs ++ rs')

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc r rs = rs ++ [r]

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ [] = xs
xs +++ [y] = xs <: y
xs +++ (y : ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?)
infixl 5 +++

-- minimum :: Ord a => [a] -> a
-- minimum [] = error "minimum: empty list"
-- minimum [a] = a
-- minimum (r : rs)
--   | r <= minimum rs = r
--   | otherwise = minimum rs

-- maximum :: Ord a => [a] -> a

-- take
-- drop

-- takeWhile
-- dropWhile

-- tails
-- init
-- inits

-- subsequences

-- any
-- all

-- and
-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
-- map

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}
