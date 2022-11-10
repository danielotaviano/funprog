{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant where" #-}
module ExDrunk
  ( atIndices
  , everyOther
  , disjoint
  , stretch
  , drunk
  , atIndex
  ) where

atIndex :: Integral i => i -> [a] -> a
atIndex 0 (x:_)  = x
atIndex _ []     = error "out of index"
atIndex i (x:xs) = atIndex (i - 1) xs

-- example:
-- atIndices [1,4,5] "Tchauzinho"
-- = "cuz"
atIndices :: Integral i => [i] -> [a] -> [a]
atIndices [] _      = []
atIndices (i:is) xs = atIndex i xs : atIndices is xs

-- example:
-- everyOther 2 "Hello There"
-- = "HloTee"
everyOther :: Int -> [a] -> [a]
everyOther i xs = removeIndex (i - 1) xs
  where
    removeIndex :: Int -> [a] -> [a]
    removeIndex _ []     = []
    removeIndex 0 (x:xs) = removeIndex (i - 1) xs
    removeIndex w (x:xs) = x : removeIndex (w - 1) xs

-- examples:
-- disjoint [1,5,9] [2 .. 6]
-- = False
-- disjoint [1,5,9] [2,4 ..]
-- = True
-- ASSUMPTIONS FOR disjoint xs ys:
--   xs and ys are sorted
disjoint :: Ord a => [a] -> [a] -> Bool
disjoint [] ys = True
disjoint (x:xs) ys
  | findI x ys = False
  | otherwise = disjoint xs ys
  where
    findI :: Ord a => a -> [a] -> Bool
    findI x [] = False
    findI x (y:ys)
      | x == y = True
      | x < y = False
      | otherwise = findI x ys

repeatX :: Integral i => i -> a -> [a]
repeatX 0 _ = []
repeatX i x = x : repeatX (i - 1) x

-- example:
-- stretch 3 "Gustavo"
-- = "GGGuuussstttaaavvvooo"
stretch :: Integral i => i -> [a] -> [a]
stretch _ []     = []
stretch i (x:xs) = repeatX i x ++ stretch i xs

takeX :: Integral i => i -> [a] -> [a]
takeX 0 _      = []
takeX i (x:xs) = x : takeX (i - 1) xs

atIndex' :: Integral i => i -> [a] -> [a]
atIndex' 0 (x:_)  = [x]
atIndex' _ []     = []
atIndex' i (x:xs) = atIndex' (i - 1) xs

-- Gustavo
-- Gtuasvto
crazy :: Integral i => i -> [a] -> [a]
crazy _ []     = []
crazy i (x:xs) = x : atIndex' (i - 1) xs ++ crazy i xs

-- example:
-- drunk 3 "Gustavo"
-- = "GusGtuasvtoavo"
-- drunk 5 "Gustavo"
-- = "GustaGvuostavo"
-- To understand these string, either get drunk or look at the markings:
--       , , , , ,,,
--   "GusGtuasvtoavo"
--    ''' ' ' ' '
--         , , ,,,,,
--   "GustaGvuostavo"
--    ''''' ' '
drunk :: Integral i => i -> [a] -> [a]
drunk i xs = takeX i xs ++ crazy i xs
