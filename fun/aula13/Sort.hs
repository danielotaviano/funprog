module Sort
  ( sort
  , msort
  , qsort
  , isort
  ) where

-- xs and ys are sorted
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) | x <= y    = x : merge xs (y : ys)
                        | otherwise = y : merge (x : xs) ys


halve :: [a] -> ([a], [a])
halve []             = ([], [])
halve [x           ] = ([x], [])
halve (x1 : x2 : xs) = (x1 : zs, x2 : ys) where (zs, ys) = halve xs

msort :: Ord a => [a] -> [a]
msort []  = []
msort [z] = [z]
msort zs  = merge (msort xs) (msort ys) where (xs, ys) = halve zs



insert :: Ord a => a -> [a] -> [a]
insert y [] = [y]
insert y (x : xs) | y <= x    = y : x : xs
                  | otherwise = x : insert y xs

isort :: Ord a => [a] -> [a]
isort []       = []
isort [x     ] = [x]
isort (x : xs) = insert x (isort xs)




small :: Ord a => a -> [a] -> [a]
small _ [] = []
small y (z : zs) | z < y     = z : small y zs
                 | otherwise = small y zs

large :: Ord a => a -> [a] -> [a]
large _ [] = []
large y (z : zs) | z >= y    = z : large y zs
                 | otherwise = large y zs

qsort :: Ord a => [a] -> [a]
qsort []       = []
qsort (x : xs) = qsort (small x xs) ++ [x] ++ qsort (large x xs)


qsort' :: Ord a => [a] -> [a]
qsort' []       = []
qsort' (w : xs) = qsort small ++ [w] ++ qsort large
 where
  small = [ x | x <- xs, x <= w ]
  large = [ x | x <- xs, x > w ]

qsort'' :: Ord a => [a] -> [a]
qsort'' []       = []
qsort'' (w : xs) = qsort small ++ [w] ++ qsort large
 where
  small = filter (<= w) xs
  large = filter (> w) xs

sort :: Ord a => [a] -> [a]
sort = msort


sorted :: Ord a => [a] -> Bool
sorted (x : x' : xs) = x <= x' && sorted (x' : xs)
sorted _             = True

-- {{{ tests

prop_qsortLength xs = length xs == length (qsort xs)

prop_qsortSorts xs = sorted (qsort xs)

prop_qsortQsort xs = qsort xs == qsort (qsort xs)



-- }}}
