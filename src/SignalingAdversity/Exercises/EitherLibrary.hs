module SignalingAdversity.Exercises.EitherLibrary where

{-|
  Try to eventually arrive at a solution that uses `foldr`,
  even if earlier versions don't use `foldr`.
-}
lefts' :: [Either a b] -> [a]
lefts' = foldr getLeft []
  where getLeft (Left a) acc = a : acc
        getLeft _ acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr getRight []
  where getRight (Right b) acc = b : acc
        getRight _ acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' _ _ = Nothing

{-|
  This is a general catamorphism for `Either` values.
-}
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' aToC _ (Left a) = aToC a
either' _ bToC (Right b) = bToC b

{-|
   Same as before, but use the `either'` function you just wrote.
-}
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' bToC = either' (\x -> Nothing) (\x -> Just $ bToC x)
