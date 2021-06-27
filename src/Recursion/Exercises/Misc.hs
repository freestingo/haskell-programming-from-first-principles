module Recursion.Exercises.Misc where

recSum :: (Ord a, Num a) => a -> a
recSum 0 = 0
recSum n = n + (recSum $ n - 1)

recMult :: (Integral a) => a -> a -> a
recMult x y
  | y == 1    = x
  | otherwise = x + (recMult x (y - 1))

mc91 :: (Integral a) => a -> a
mc91 x
  | x > 100   = x - 10
  | otherwise = 91
