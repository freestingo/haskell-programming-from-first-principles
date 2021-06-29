module SignalingAdversity.Exercises.Natural where

data Nat = Zero | Succ Nat
           deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i | i >= 0 = Just $ toNat i
               | otherwise = Nothing
    where toNat 0 = Zero
          toNat n = Succ (toNat $ n - 1)
