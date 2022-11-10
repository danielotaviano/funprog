module Arith where

import Prelude hiding (iterate)

data Arith = Atom Int | Plus Arith Arith | Times Arith Arith
  deriving (Eq)

makeExpression :: Show a => Char -> a -> a -> String
makeExpression s a b = "(" ++ show a ++ ' ' : s : ' ' : show b ++ ")"

instance Show Arith where
  show (Atom n) = show n
  show (Plus n m) = makeExpression '+' n m
  show (Times n m) = makeExpression '*' n m

val :: Arith -> Int
val (Atom n) = n
val a = val $ step a

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

depth :: Arith -> Int
depth (Atom _) = 0
depth (Plus x y) = 1 + depth x + depth y
depth (Times x y) = 1 + depth x + depth y

step :: Arith -> Arith
step a@(Atom _) = a
step (Plus (Atom n) (Atom m)) = Atom (n + m)
step (Plus n m)
  | depth n >= depth m = Plus (step n) m
  | otherwise = Plus n (step m)
step (Times (Atom n) (Atom m)) = Atom (n * m)
step (Times n m)
  | depth n >= depth m = Times (step n) m
  | otherwise = Times n (step m)
