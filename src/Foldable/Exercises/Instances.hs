module Foldable.Exercises.Instances where

{-|
   Write `Foldable` instances for the following datatypes.
-}
-- 1
newtype Constant a b = Constant b

instance Foldable (Constant a) where
   foldMap f (Constant b) = f b

-- 2
data Two a b = Two a b

instance Foldable (Two a) where
   foldMap f (Two _ b) = f b

-- 3
data Three a b c = Three a b c

instance Foldable (Three a b) where
   foldMap f (Three _ _ c) = f c

-- 4
data Three' a b = Three' a b b

instance Foldable (Three' a) where
   foldMap f (Three' a b b') = f b <> f b'

-- 5
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
   foldMap f (Four' a b b' b'') = mconcat [f b, f b', f b'']

{-|
   Thinking cap time. Write a filter funciton for `Foldable` types using `foldMap`.
-}
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF pred = foldMap (\t -> if pred t then pure t else mempty)


