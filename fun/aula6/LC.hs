module LC where

data ListInt = Nil | Cons Int ListInt

instance Eq ListInt where
  Nil == Nil = True
  Cons x xs == Cons y ys = x == y && xs == ys
  _ == _ = False

instance Show ListInt where
  show Nil = "[]"
  show (Cons x xs) = "[" ++ show x ++ show' xs ++ "]"
    where
      show' Nil = ""
      show' (Cons x xs) = "," ++ show x ++ show' xs

hd :: ListInt -> Int
hd Nil = error "empty list"
hd (Cons a _) = a

tl :: ListInt -> ListInt
tl Nil = error "empty list"
tl (Cons _ b) = b
