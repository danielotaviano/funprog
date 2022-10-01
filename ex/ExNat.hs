module ExNat where

-- Do not alter this import!
import           Prelude (Bool (..), Eq (..), Integral, Num (..), Ord (..),
                          Show (..), error, not, otherwise, undefined, ($),
                          (&&), (++), (.), (||))

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat = Zero | Succ Nat

instance Show Nat where
  -- zero  should be shown as O
  -- three should be shown as SSSO
  show Zero     = "O"
  show (Succ n) = "S" ++ show n

instance Eq Nat where
  (==) Zero Zero         = True
  (==) (Succ n) (Succ m) = (==) n m
  (==) _ _               = False

instance Ord Nat where
  (<=) Zero _            = True
  (<=) _ Zero            = False
  (<=) (Succ n) (Succ m) = (<=) n m

  -- Ord does not REQUIRE defining min and max.
  -- Howevener, you should define them WITHOUT using (<=).
  -- Both are binary functions: max m n = ..., etc.

  min (Succ m) (Succ n) = Succ (min m n)
  min _ _               = Zero

  max (Succ m) (Succ n) = Succ (max m n)
  max Zero a            = a
  max a Zero            = a

isZero :: Nat -> Bool
isZero Zero = True
isZero _    = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred Zero     = Zero
pred (Succ n) = n

even :: Nat -> Bool
even Zero     = True
even (Succ n) = odd n

odd :: Nat -> Bool
odd = not . even

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) Zero n     = n
(<+>) (Succ m) n = Succ (m <+> n)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
(<->) Zero n            = n
(<->) n Zero            = n
(<->) (Succ m) (Succ n) = m <-> n

-- multiplication
(<*>) :: Nat -> Nat -> Nat
(<*>) Zero _     = Zero
(<*>) (Succ m) n = n <+> (m <*> n)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) n Zero     = Succ Zero
(<^>) n (Succ m) = n <*> (n <^> m)

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) n Zero = error "Division by zero"
(</>) Zero _ = Zero
(</>) n m    = if n < m then Zero else Succ ((n <-> m) </> m)

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) n Zero = error "Division by zero"
(<%>) Zero _ = Zero
(<%>) n m    = max m n <-> min m n

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) n m = n <%> m == Zero

divides :: Nat -> Nat -> Bool
divides = (<|>)

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff n m = if m >= n then m <-> n else n <-> m

(|-|) = absDiff

factorial :: Nat -> Nat
factorial Zero = Succ Zero
factorial n    = n <*> (factorial . pred) n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg Zero = Zero
sg _    = Succ Zero

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined

--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat n
  | n <= 0 = Zero
  | otherwise = Succ (toNat (n - 1))

fromNat :: Integral a => Nat -> a
fromNat Zero = 0
fromNat n    = 1 + fromNat (n <-> Succ Zero)

-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where
  (+) = (<+>)
  (*) = (<*>)
  (-) = (<->)
  abs n = n
  signum = sg
  fromInteger x
    | x < 0 = error "Negative number"
    | x == 0 = Zero
    | otherwise = Succ (fromInteger (x - 1))
