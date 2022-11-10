module ReverseProof where

import           Prelude hiding (reverse)

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]
