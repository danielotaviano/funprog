module ExEither where

import qualified Data.Either as E

-- Do not alter this import!
import           Prelude     hiding (Either (..), either)

data Either a b
  = Left a
  | Right b
  deriving (Show, Eq)

unboxLeft :: Either a b -> a
unboxLeft (Left x) = x

unboxRight :: Either a b -> b
unboxRight (Right x) = x

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

lefts :: [Either a b] -> [a]
lefts = map unboxLeft . filter isLeft

rights :: [Either a b] -> [b]
rights = map unboxRight . filter isRight

fromLeft :: a -> Either a b -> a
fromLeft _ (Left x) = x
fromLeft d _        = d

fromRight :: b -> Either a b -> b
fromRight _ (Right x) = x
fromRight d _         = d

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers xs = (lefts xs, rights xs)

either :: (a -> c) -> (b -> c) -> Either a b -> c
either lf rf x
  | isLeft x = lf $ unboxLeft x
  | otherwise = rf $ unboxRight x
