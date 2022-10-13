{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
module ExList where

import qualified Data.Char                     as C
import qualified Data.List                     as L
import           Prelude                        ( ($)
                                                , (&&)
                                                , (.)
                                                , Bool(..)
                                                , Char
                                                , Double
                                                , Enum(..)
                                                , Eq(..)
                                                , Float
                                                , Int
                                                , Integer
                                                , Integral(..)
                                                , Num(..)
                                                , Ord(..)
                                                , String
                                                , curry
                                                , error
                                                , flip
                                                , not
                                                , otherwise
                                                , uncurry
                                                , undefined
                                                , (||)
                                                )
import qualified Prelude                       as P

-- to use a function from a qualified import
-- you need to prefix its name with its alias
-- and a dot:
-- P.head   C.toUpper   etc.
-- I import these for you to test the original functions on ghci:
-- ghci> :t C.toUpper
-- C.toUpper :: Char -> Char
-- You MUST NOT use ANY of these in your code

head :: [a] -> a
head []      = error "head: empty list"
head (r : _) = r

tail :: [a] -> [a]
tail []       = error "tail: empty list"
tail (_ : rs) = rs

null :: [a] -> Bool
null [] = True
null _  = False

length :: Integral i => [a] -> i
length []       = 0
length (_ : rs) = 1 + length rs

sum :: Num a => [a] -> a
sum []       = 0
sum (r : rs) = r + sum rs

product :: Num a => [a] -> a
product []       = 1
product (r : rs) = r * product rs

reverse :: [a] -> [a]
reverse []       = []
reverse (r : rs) = reverse rs ++ [r]

(++) :: [a] -> [a] -> [a]
(++) []       rs  = rs
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
xs +++ []       = xs
xs +++ [y     ] = xs <: y
xs +++ (y : ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum []  = error "minimum: empty list"
minimum [a] = a
minimum (r : rs) | r <= minimum rs = r
                 | otherwise       = minimum rs

maximum :: Ord a => [a] -> a
maximum []  = error "maximum: empty list"
maximum [a] = a
maximum (r : rs) | r >= maximum rs = r
                 | otherwise       = maximum rs


take :: Integral a => a -> [b] -> [b]
take 0 _        = []
take _ []       = []
take n (y : ys) = y : take (n - 1) ys

drop :: Integral i => i -> [a] -> [a]
drop 0 xs       = xs
drop _ []       = []
drop n (_ : xs) = drop (n - 1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs) | p x       = x : takeWhile p xs
                     | otherwise = []
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x : xs) | p x       = dropWhile p xs
                     | otherwise = x : dropWhile p xs

-- tails :: 
init :: [a] -> [a]
init []       = []
init [a     ] = []
init (x : xs) = x : init xs

inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = xs : inits (init xs)

subsequences :: [a] -> [[a]]
subsequences []       = [[]]
subsequences (x : xs) = map (x :) (subsequences xs) ++ subsequences xs

any :: (a -> Bool) -> [a] -> Bool
any p (x : xs) | p x       = True
               | otherwise = any p xs
any _ _ = False

-- and
-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
map :: (a -> b) -> [a] -> [b]
map _ []       = []
map p (x : xs) = p x : map p xs

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

zip :: [a] -> [b] -> [(a, b)]
zip []       _        = []
zip _        []       = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys


zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ []       _        = []
zipWith _ _        []       = []
zipWith p (x : xs) (y : ys) = p x y : zipWith p xs ys

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
