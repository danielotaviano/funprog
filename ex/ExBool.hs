module ExBool where

-- Do not alter this import!
import Prelude
  ( Char,
    Enum (..),
    Eq (..),
    Int,
    Integral (..),
    Num (..),
    Ord (..),
    Show (..),
    error,
    otherwise,
    undefined,
    ($),
    (++),
    (.),
  )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

instance Show Bool where
  show False = "False"
  show True = "True"

instance Enum Bool where
  toEnum 0 = False
  toEnum 1 = True
  toEnum _ = error "toEnum: bad argument"

  fromEnum True = 1
  fromEnum False = 0

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
(&&) True True = True
(&&) _ _ = False

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
(||) True _ = True
(||) _ x = x

infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
(/|\) x y = not ((&&) x y)

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
(\|/) x y = not ((||) x y)

infixr 2 \|/

 -- Extra (Diff)
(!=) :: Bool -> Bool -> Bool
(!=) True False = True
(!=) False True = True
(!=) _ _ = False

infixr 2 !=

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
(<=/=>) x y = x != y

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not True = False
not _ = True

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a  
ifThenElse _ _ b = b

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
(==>) True False = False
(==>) _ _ = True 

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
(<==) x y = y ==> x 
infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
(<=>) x y = not (x <=/=> y)  

infixr 1 <=>
