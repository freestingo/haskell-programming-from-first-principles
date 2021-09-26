module Reader.Exercises.WarmUpStretch where

import Data.Monoid
import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

{-|
   zip x and y using 3 as the lookup key
-}
xs :: Maybe Integer
xs = lookup 3 $ liftA2 (,) x y

ys :: Maybe Integer
ys = lookup 6 $ liftA2 (,) y z

zs :: Maybe Integer
zs = lookup 4 $ liftA2 (,) x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ liftA2 (,) x y

{-|
   Have x1 make a tuple of xs and ys, and x2 make a tuple of ys and zs.
   Also write x3 which takes one input and makes a tuple of the results of
   two applications of z' from above.
-}
x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (> 3) (< 8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(> 3), (< 8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

mainoo :: IO ()
mainoo = do
    print $ and (sequA 3)
    print $ sequenceA [Just 3, Just 2, Just 1]
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> zs)
    print $ bolt 7
    print $ fmap bolt z
    print $ sequenceA [(> 3), (< 8), even] 7



