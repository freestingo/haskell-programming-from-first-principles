module Traversable.Exercises.Instances where

{-|
   Write a `Traversable` instance for the dataypes provided,
   filling in any required superclasses. Use `QuickCheck` to validate your instances.
-}

-- 1
newtype Identity a = Identity a
                     deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)


instance Foldable Identity where
    foldMap f (Identity a) = f a


instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

-- 2
newtype Constant a b = Constant { getConstant :: a }

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
    foldMap f (Constant a) = mempty

instance Traversable (Constant a) where
    traverse _ (Constant a) = pure (Constant a)

-- 3
data Optional a = Nada | Yep a

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a

instance Traversable Optional where
    traverse _ Nada = pure Nada
    traverse f (Yep a) = Yep <$> f a

-- 4
data List a = Nil | Cons a (List a)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

-- 5
data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> f c

-- 6
data Pair a b = Pair a b

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
    foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
    traverse f (Pair a b) = Pair a <$> f b

-- 7
-- When you have more than one value of type `b`, you'll want to use
-- `Monoid` and `Applicative` for the `Foldable` and `Traversable` instances respectively.
data Big a b = Big a b b

instance Functor (Big a) where
    fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
    foldMap f (Big a b b') = f b <> f b'

instance Traversable (Big a) where
    traverse f (Big a b b') = Big a <$> f b <*> f b'

-- 8
data Bigger a b = Bigger a b b b

instance Functor (Bigger a) where
    fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
    foldMap f (Bigger a b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
    traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''

-- 9
-- This may be difficult.
-- To make it easier, we'll give you the constraints and `QuickCheck` instances.
data S n a = S (n a) a
             deriving (Eq, Show)

instance Functor n => Functor (S n) where
    fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
    foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
    traverse f (S na a) = S <$> traverse f na <*> f a

-- 10
-- This may be hard. Write the following instances for `Tree`.
data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
              deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node t a t') = Node (fmap f t) (f a) (fmap f t')

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node t a t') = foldMap f t <> f a <> foldMap f t'

instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node t a t') = Node <$> traverse f t <*> f a <*> traverse f t'

