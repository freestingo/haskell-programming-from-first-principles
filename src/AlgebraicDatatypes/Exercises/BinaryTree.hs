module AlgebraicDatatypes.Exercises.BinaryTree where

data BinaryTree a
   = Leaf
   | Node (BinaryTree a) a (BinaryTree a)
     deriving (Eq, Ord, Show)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf)
                2
                (Node Leaf 3 Leaf)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
   | b == a = Node left a right
   | b < a = Node (insert' b left) a right
   | b > a = Node left a (insert' b right)

{-|
   Given the definition of `BinaryTree` above,
   write a map function for the data structure.
-}
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

{-|
   Write functions to convert `BinaryTree` values to lists.
-}
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

{-|
   Write a catamorphism for the binary trees.
-}
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node left a right) = foldTree f leftFold right
                  where leftFold = f a (foldTree f z left)









