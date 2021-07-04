module Monad.Practice.MonadExamples where

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x then [x * x, x * x] else [x * x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
    x <- xs
    if even x then [x * x, x * x] else []

twiceWhenEven'' :: [Integer] -> [Integer]
twiceWhenEven'' = (>>= doubleIfEven)

doubleIfEven x | even x = [x * x, x * x]
               | otherwise = []


