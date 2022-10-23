module Curry where

import           Prelude                 hiding ( curry
                                                , uncurry
                                                )




curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = f (x, y)


uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (a, b) = f a b
