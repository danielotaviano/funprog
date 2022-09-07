module LiveCoding where


data Nat = Zero | Succ Nat

plus :: Nat -> Nat -> Nat
plus n Zero = n
plus n (Succ m) = Succ (plus n m)



instance (Show Nat) where
    show Zero = "O"
    show (Succ x) = "S" ++ show x
