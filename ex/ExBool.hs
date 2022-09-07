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
(&&) = undefined

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
(||) = undefined

infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
(/|\) = undefined

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
(\|/) = undefined

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
(<=/=>) = undefined

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not = undefined

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse = undefined

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
(==>) = undefined

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
(<==) = undefined

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
(<=>) = undefined

infixr 1 <=>
