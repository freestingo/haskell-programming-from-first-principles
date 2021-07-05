module Foldable.Exercises.LibraryFunctions where

import Data.Monoid

{-|
   Implement the functions in terms of `foldMap` or `foldr` from `Foldable`,
   then try them out with multiple types that have `Foldable` instances.
-}
mySumF :: (Foldable t, Num a) => t a -> a
mySumF = getSum . foldMap Sum

myProductF :: (Foldable t, Num a) => t a -> a
myProductF = getProduct . foldMap Product

myElemF :: (Foldable t, Eq a) => a -> t a -> Bool
myElemF a = getAny . foldMap (Any . (== a))

myMinimumF :: (Foldable t, Ord a) => t a -> Maybe a
myMinimumF = foldr checkValues Nothing
            where checkValues a (Just acc) = Just $ min a acc
                  checkValues a Nothing = Just a

myMaximumF :: (Foldable t, Ord a) => t a -> Maybe a
myMaximumF = foldr checkValues Nothing
            where checkValues a (Just acc) = Just $ max a acc
                  checkValues a Nothing = Just a

myNullF :: (Foldable t) => t a -> Bool
myNullF = not . getAny . foldMap (Any . const True)

myLengthF :: (Foldable t) => t a -> Int
myLengthF = getSum . foldMap (Sum . const 1)

myToListF :: (Foldable t) => t a -> [a]
myToListF = foldMap (: [])

myFoldF :: (Foldable t, Monoid m) => t m -> m
myFoldF = foldMap id

-- Define `foldMap` in terms of `foldr`.
myFoldMapF :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMapF f = foldr (\x acc -> f x <> acc) mempty


