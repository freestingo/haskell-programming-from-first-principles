module Lists.Exercises.StandardFunctions where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x : xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem a = myAny (== a)

myReverse :: [a] -> [a]
myReverse xs = case xs of
    []            -> []
    [x]           -> [x]
    (x : x' : xs) -> myReverse xs ++ [x', x]

squish :: [[a]] -> [a]
squish [] = []
squish (xs : rest) = xs ++ squish rest

-- squishMap :: (a -> [b]) -> [a] -> [b]
-- squishMap f = squish . map f

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x : xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximumBy _ [] = Nothing
myMaximumBy f (x : xs) = Just $ go f xs x
    where go _ [] x = x
          go f (h : t) x = case f h x of
              LT -> go f t x
              EQ -> go f t x
              GT -> go f t h

myMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMinimumBy _ [] = Nothing
myMinimumBy f xs = myMaximumBy (flip f) xs

myMaximum :: (Ord a) => [a] -> Maybe a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> Maybe a
myMinimum = myMinimumBy compare
