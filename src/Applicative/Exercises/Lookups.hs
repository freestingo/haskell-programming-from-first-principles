module Applicative.Exercises.Lookups where

import Data.List (elemIndex)
import Control.Applicative (liftA2)

{-|
   In the following exercises you will need to use the following
   terms to make the expressions typecheck:

   - `pure`
   - (<$>) or `fmap`
   - (<*>)
-}
-- 1
added :: Maybe Integer
added = (+3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

-- 2
a :: Maybe Integer
a = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

b :: Maybe Integer
b = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> a <*> b

-- 3
c :: Maybe Int
c = elemIndex 3 [1..5]

d :: Maybe Int
d = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> c <*> d

-- 4
xs = [1, 2, 3]
ys = [4, 5, 6]

e :: Maybe Integer
e = lookup 3 $ zip xs ys

f :: Maybe Integer
f = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> liftA2 (,) e f

