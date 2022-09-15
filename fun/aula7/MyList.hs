module MyList where

data List a = Empty | Cons a (List a) deriving (Show, Eq)


len :: [a] -> Int
len [] = 0
len (_ : n) = 1 + (len n)

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = xs' ++ [x] where xs' = rev xs


atLeastTwo :: [a] -> Bool
atLeastTwo [] = False
atLeastTwo [x] = False 
atLeastTwo _ = True

