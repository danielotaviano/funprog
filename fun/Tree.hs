module Tree where

data Tree a = Node a (Tree a) (Tree a)
            | Leaf a
            deriving (Show, Eq)


flatten :: Tree a -> [a]
flatten (Leaf a    ) = [a]
flatten (Node v x y) = flatten x ++ [v] ++ flatten y


tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Leaf a    ) = Leaf (f a)
tmap f (Node v x y) = Node (f v) (tmap f x) (tmap f y)
