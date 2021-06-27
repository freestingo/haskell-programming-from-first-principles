module Lists.Exercises.FromTo where

myFromTo :: (Ord a, Enum a) => a -> a -> [a]
myFromTo x y
  | x > y     = []
  | x == y    = [x]
  | otherwise = x : myFromTo (succ x) y
