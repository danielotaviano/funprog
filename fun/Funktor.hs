module Funktor where

import           Prelude hiding (fmap, map, (<$))

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

class Functor f where
  fmap :: (a -> b) -> f a -> f b
