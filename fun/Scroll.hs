module Scroll where

putTwoChars :: Char -> Char -> IO ()
putTwoChars x y = do
  putChar x
  putChar y

getThreeFL :: IO (Char, Char)
getThreeFL = do
  x <- getChar
  getChar
  z <- getChar
  return (x, z)
