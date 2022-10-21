module MyList where

import           Prelude                 hiding ( init
                                                , inits
                                                , subsequences
                                                , tails
                                                )

import qualified Data.List                     as L
import qualified Prelude                       as P

data List a = Empty | Cons a (List a) deriving (Show, Eq)


len :: [a] -> Int
len []      = 0
len (_ : n) = 1 + len n

rev :: [a] -> [a]
rev []       = []
rev (x : xs) = xs' ++ [x] where xs' = rev xs


atLeastTwo :: [a] -> Bool
atLeastTwo []  = False
atLeastTwo [x] = False
atLeastTwo _   = True


tails :: [a] -> [[a]]
tails []           = [[]]
tails xs'@(_ : xs) = xs' : tails xs


inits :: [a] -> [[a]]
inits []           = [[]]
inits xs'@(x : xs) = [] : map (x :) (inits xs)

