module Reords where


type Name = String
data Sex = Male | Female
           deriving (Show, Eq)



type Age = Int

data Person = Person
  { name :: Name
  , sex  :: Sex
  , age  :: Age
  }
  deriving Show



