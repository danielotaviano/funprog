module Holes where


cS :: (a -> b -> c) -> (a -> b) -> a -> c
cS f g x = f x (g x)
