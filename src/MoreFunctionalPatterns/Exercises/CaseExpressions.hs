module MoreFunctionalPatterns.Exercises.CaseExpressions where

functionC :: Ord a => a -> a -> a
functionC x y = case x > y of
    True  -> x
    False -> y

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n = case even n of
    True  -> n + 2
    False -> n

nums :: (Ord a, Num a, Num b) => a -> b
nums x = case compare x 0 of
    LT -> -1
    GT -> 1
    _  -> 0
