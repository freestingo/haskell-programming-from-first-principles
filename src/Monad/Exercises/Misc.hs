module Monad.Exercises.Misc where

import Control.Monad (join)

{-|
   Write `bind` in terms of `fmap` and `join`.
   Fear is the mind-killer, friend. You can do it.
-}

-- keep in mind this is (>>=) flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

{-|
   Write `Monad` instances for the following types.
   Use the `QuickCheck` properties we showed you to validate your instances.
-}

-- 1
-- Welcome to the `Nope Monad`, where nothing happens and nobody cares.
-- We're serious. Write it anyway.
data Nope a = NopeDotJpg

instance Functor Nope where
   fmap _ _ = NopeDotJpg

instance Applicative Nope where
   pure _ = NopeDotJpg
   _ <*> _ = NopeDotJpg

instance Monad Nope where
   _ >>= _ = NopeDotJpg

-- 2
data PhbtEither b a = PhbtLeft a | PhbtRight b

instance Functor (PhbtEither b) where
   fmap _ (PhbtRight b) = PhbtRight b
   fmap f (PhbtLeft a) = PhbtLeft (f a)

instance Applicative (PhbtEither b) where
   pure = PhbtLeft
   (PhbtLeft a) <*> (PhbtLeft a') = PhbtLeft (a a')
   _ <*> (PhbtRight b) = PhbtRight b
   (PhbtRight b) <*> _ = PhbtRight b

instance Monad (PhbtEither b) where
   return = pure
   (PhbtLeft a) >>= f = f a
   (PhbtRight b) >>= _ = PhbtRight b

-- 3
-- Write a `Monad` instance for `Identity`.
newtype Identitee a = Identitee a
                      deriving (Eq, Ord, Show)

instance Functor Identitee where
   fmap f (Identitee a) = Identitee (f a)

instance Applicative Identitee where
   pure = Identitee
   (Identitee a) <*> (Identitee a') = Identitee (a a')

instance Monad Identitee where
   return = pure
   (Identitee a) >>= f = f a

-- 4
-- This one should be easier than the `Applicative` instance was.
-- Remember to use the `Functor` that `Monad` requires, then see where the chips fall.
data Leest a = Neel | Cwons a (Leest a)
              deriving (Eq, Show)

instance Functor Leest where
   fmap _ Neel = Neel
   fmap f (Cwons a rest) = Cwons (f a) (fmap f rest)

instance Applicative Leest where
   pure x = Cwons x Neel
   (Cwons f rest) <*> as = fmap f as `myAppend` (rest <*> as)
   _ <*> _ = Neel

instance Monad Leest where
   return = pure
   Neel >>= _ = Neel
   (Cwons a rest) >>= f = f a `myAppend` (rest >>= f)

myAppend :: Leest a -> Leest a -> Leest a
myAppend Neel ys = ys
myAppend (Cwons x xs) ys = Cwons x $ xs `myAppend` ys



