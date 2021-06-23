module MoreFunctionalPatterns.Exercises.Pointfree where

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d     = xLast `mod` 10

-- pointfree
tensD = (flip mod) 10 . fst . (flip divMod) 10

-- case expression
foldBool :: a -> a -> Bool -> a
foldBool x y bool = case bool of
    False -> x
    True -> y

-- guards
foldBool' :: a -> a -> Bool -> a
foldBool' x y bool
    | bool = y
    | otherwise = x

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show
