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
dropWhile p (x : xs) | not $ p x = x : xs
                     | otherwise = dropWhile p xs

tails :: [a] -> [[a]]
tails []           = [[]]
tails xs'@(x : xs) = xs' : tails xs

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

and :: [Bool] -> Bool
and [] = True
and (x : xs) | not x     = False
             | otherwise = and xs


and' :: [Bool] -> Bool
and' = not . any not


or :: [Bool] -> Bool
or [] = False
or (x : xs) | x         = True
            | otherwise = or xs

or' :: [Bool] -> Bool
or' = any (\x -> x)

concat :: [[a]] -> [a]
concat []       = []
concat (x : xs) = x ++ concat xs

elem :: Eq a => a -> [a] -> Bool
elem x = any (== x)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x : xs) | a == x    = True
                 | otherwise = elem' a xs

(!!) :: [a] -> Int -> a
(!!) [] _ = error "too large index"
(!!) (x : xs) i | i == 0    = x
                | otherwise = (!!) xs (i - 1)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs) | p x       = x : filter p xs
                  | otherwise = filter p xs


map :: (a -> b) -> [a] -> [b]
map _ []       = []
map p (x : xs) = p x : map p xs

cycle :: [a] -> [a]
cycle xs = xs ++ cycle xs

repeat :: a -> [a]
repeat a = a : repeat a

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate i x = x : replicate (i - 1) x


isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _  = True
isPrefixOf _  [] = False
isPrefixOf (x : xs) (y : ys) | x == y    = isPrefixOf xs ys
                             | otherwise = False



isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _  = True
isInfixOf _  [] = False
isInfixOf xs'@(x : xs) ys'@(y : ys) | x == y    = xs' `isPrefixOf` ys'
                                    | otherwise = isInfixOf xs' ys

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = reverse xs `isPrefixOf` reverse ys

zip :: [a] -> [b] -> [(a, b)]
zip []       _        = []
zip _        []       = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys


zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ []       _        = []
zipWith _ _        []       = []
zipWith p (x : xs) (y : ys) = p x y : zipWith p xs ys

intercalate :: [a] -> [[a]] -> [a]
intercalate _  []       = []
intercalate _  [a     ] = a
intercalate xs (y : ys) = y ++ xs ++ intercalate xs ys


nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) | x `elem` right = nub xs
             | otherwise      = x : nub xs
  where right = nub xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt 0 xs       = ([], xs)
splitAt _ []       = ([], [])
splitAt i (x : xs) = (x : h, t) where (h, t) = splitAt (i - 1) xs


-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break _ [] = ([], [])
break p (x : xs) | p x       = ([], x : xs)
                 | otherwise = (x : h, t)
  where (h, t) = break p xs


lines :: String -> [String]
lines ""   = []
lines "\n" = [""]
lines s | null t    = [h]
        | otherwise = h : lines (tail t)
  where (h, t) = break (== '\n') s

words :: String -> [String]
words "" = []
words s | null t    = [h]
        | otherwise = h : words (tail t)
  where (h, t) = break (\x -> x == ' ' || x == '\n') s

unlines :: [String] -> String
unlines []       = ""
unlines (s : ss) = s ++ "\n" ++ unlines ss

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace _   = False

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

unwords :: [String] -> String
unwords []       = ""
unwords (s : ss) = s ++ " " ++ trim (unwords ss)

transpose :: [[a]] -> [[a]]
transpose []         = []
transpose ([] : xss) = transpose xss
transpose xss        = h : transpose t
 where
  h = map head xss
  t = map tail xss


toLower :: Char -> Char
toLower c | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32) :: Char
          | otherwise            = c

sanitaze :: String -> [Char] -> String
sanitaze s cs = map toLower $ filter (not . (`elem` cs)) s

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome s = satinazedString == f satinazedString
 where
  satinazedString = sanitaze s ['.', '?', ',', '\'', '!', ' ']
  f               = reverse
{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}
