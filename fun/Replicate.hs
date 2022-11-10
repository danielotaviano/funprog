module Replicate where

import           Prelude hiding (replicate)

replicate :: Int -> a -> [a]
replicate 0 c = []
replicate n c = c : replicate (n - 1) c
