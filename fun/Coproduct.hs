import           Prelude                 hiding ( Either(..) )

data Either a b = Left a
                | Right b
                deriving (Show, Eq)


h :: Either a b -> d
h (Left x) = f x
 where
  f :: a -> d
  f = error "f"


h (Right x) = g x
 where
  g :: b -> d
  g = error "g"
