module MyBool where

data Boolean = F | T

lnot :: Boolean -> Boolean
lnot T = F
lnot _ = T

ifthenelse :: Boolean -> a -> a -> a 
ifthenelse T a _ = a
ifthenelse _ _ b = b
