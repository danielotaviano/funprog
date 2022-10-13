module Person where

import           Prelude hiding (Either (..))

type Name = String
type Age = Int

data Person = Person Name Age deriving (Show)


data Either a b = Left a | Right b deriving (Show, Eq)

data PersonError = NameError | AgeErro deriving (Show, Eq)


mkPerson :: Name -> Age -> Either PersonError Person
mkPerson name age
    | length name <= 1 = Left NameError
    | age > 200 = Left AgeErro
    | otherwise = Right $ Person name age
