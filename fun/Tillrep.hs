module Tillrep where

import           Prelude hiding (last)

tillRep :: Eq a => [a] -> [a]
tillRep [] = []
tillRep (x:xs) = reverse $ tillRep' [x] xs
  where
    tillRep' :: Eq a => [a] -> [a] -> [a]
    tillRep' ys [] = ys
    tillRep' ys'@(y:ys) (x1:xs)
      | x1 == y = ys'
      | otherwise = tillRep' (x1 : ys') xs
