module Applicative.Exercises.Instances where

{-|
   Write an `Applicative` instance for `Identity`.
-}
newtype Identity a = Identity a
                     deriving (Eq, Ord, Show)

instance Functor Identity where
   fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
   pure a = Identity a
   (<*>) (Identity f) (Identity a) = Identity (f a)

{-|
   Write an `Applicative` instance for `Constant`.
-}
newtype Constant a b = Constant { getConstant :: a }
                       deriving (Eq, Ord, Show)

instance Functor (Constant a) where
   fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
   pure _ = Constant mempty
   (<*>) (Constant x) (Constant y) = Constant (x <> y)

{-|
   Implement the list `Applicative`.
-}
data List' a = Nil' | Cons' a (List' a)
              deriving (Eq, Show)

instance Functor List' where
   fmap _ Nil' = Nil'
   fmap f (Cons' a rest) = Cons' (f a) (fmap f rest)

instance Applicative List' where
   pure x = Cons' x Nil'
   (Cons' f rest) <*> as = fmap f as `myAppend` (rest <*> as)
   _ <*> _ = Nil'

myAppend :: List' a -> List' a -> List' a
myAppend Nil' ys = ys
myAppend (Cons' x xs) ys = Cons' x $ xs `myAppend` ys

{-|
   Implement the ZipList Applicative.
-}

take' :: Int -> List' a -> List' a
take' _ Nil' = Nil'
take' n list@(Cons' x xs)
  | n > 0 = Cons' x (take' (n - 1) xs)
  | otherwise = Nil'

newtype ZipList' a = ZipList' (List' a)
                     deriving (Eq, Show)

instance Functor ZipList' where
   fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
   pure xs = ZipList' (pure xs)
   ZipList' (Cons' f fs) <*> ZipList' (Cons' a as) = ZipList' $ Cons' (f a) (tupleCombine fs as)
      where tupleCombine (Cons' f fs) (Cons' a as) = Cons' (f a) (tupleCombine fs as)
            tupleCombine _ _ = Nil'
   _ <*> _ = ZipList' Nil'

-- test data
zl = ZipList' $ Cons' (+9) $ Cons' (*2) $ Cons' (+8) Nil'
zl' = ZipList' $ Cons' 1 $ Cons' 2 $ Cons' 3 Nil'

{-|
   `Validation` has the same representation as `Either`, but it can be different.
   The `Functor` will behave the same, but the `Applicative` will be different.
-}
data Validation e a = Failure e
                    | Success a
                      deriving (Eq, Show)

instance Functor (Validation e) where
   fmap f (Success a) = Success (f a)
   fmap _ (Failure e) = Failure e

instance Monoid e => Applicative (Validation e) where
   pure = Success
   (Success f) <*> (Success a) = Success (f a)
   (Failure e) <*> (Failure e') = Failure (e <> e')
   (Failure e) <*> _ = Failure e
   _ <*> (Failure e) = Failure e

{-|
   Write instances for the following datatypes.
-}

-- 1
data Pair a = Pair a a
              deriving Show

instance Functor Pair where
   fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
   pure a = Pair a a
   (Pair f f') <*> (Pair a a') = Pair (f a) (f' a')

-- 2
data Two a b = Two a b

instance Functor (Two a) where
   fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
   pure = Two mempty
   (Two a b) <*> (Two a' b') = Two (a <> a') (b b')

-- 3
data Three a b c = Three a b c

instance Functor (Three a b) where
   fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
   pure = Three mempty mempty
   (Three a b c) <*> (Three a' b' c') = Three (a <> a') (b <> b') (c c')

