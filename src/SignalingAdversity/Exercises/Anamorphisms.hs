module SignalingAdversity.Exercises.Anamorphisms where

import AlgebraicDatatypes.Exercises.BinaryTree

{-|
  Write the function myIterate using direct recursion.
  Compare the behavior with the built-in `iterate` to gauge correctness.

  Do not look at the source or any examples of `iterate` so that
  you are forced to do this yourself.
-}
myIterate :: (a -> a) -> a -> [a]
myIterate f x = (++) [x] $ myIterate f (f x)

{-|
  Write the function `myUnfoldr` using direct recursion.
  Compare with the build-in `unfoldr` to check your implementation.

  Again, don't look at implementations of `unfoldr` so that you
  figure it out yourself.
-}
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
    Nothing -> []
    Just (a, b) -> (++) [a] $ myUnfoldr f b

{-|
  Rewrite `myIterate` into `betterIterate` using `myUnfoldr`.
-}
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

{-|
  Write `unfoldTree` for `BinaryTree`.
-}
unfoldTree :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldTree f x = case f x of
    Nothing -> Leaf
    Just (a, b, a') -> Node (unfoldTree f a) b (unfoldTree f a')

{-|
   Make a tree builder using the `unfold` function you've made for `BinaryTree`.
-}
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldTree createBranches 0
    where createBranches x | n > x = Just (x + 1, x, x + 1)
                           | otherwise = Nothing

