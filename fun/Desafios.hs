module Desafios where


sorted :: Ord a => [a] -> Bool
sorted []           = True
sorted xs'@(_ : xs) = and $ zipWith (<=) xs' xs


