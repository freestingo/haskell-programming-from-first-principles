module Functor.Exercises.Fmapping where

{-|
    Add `fmap`, parentheses and function composition to the expression
    as needed for the expression to typecheck and produce the expected result.
    It may not always need to go in the same place, so don't get complacent.
-}
a :: [Int]
a = fmap (+1) (read "[1]" :: [Int])

b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c :: Num a => a -> a
c = fmap (*2) (\x -> x - 2)

d :: Integer -> String
d = fmap ((return '1' ++) . show) (\x -> x : [1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed
