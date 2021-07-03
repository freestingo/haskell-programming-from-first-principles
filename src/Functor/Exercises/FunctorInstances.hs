{-# LANGUAGE FlexibleInstances #-}
module Functor.Exercises.FunctorInstances where

import Test.QuickCheck

{-|
    Implement `Functor` instances for the following datatypes.
    Use the `QuickCheck` properties we showed you to validate them.
-}

-- functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
-- functorIdentity f = fmap id f == f

-- functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
-- functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

-- 1
newtype Identity a = Identity a

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

-- 2
data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

-- 3
data Two a b = Two a b

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

-- 4
data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

-- 5
data Three' a b = Three' a b b

instance Functor (Three' a) where
    fmap f (Three' x y y') = Three' x (f y) (f y')

-- 6
data Four a b c d = Four a b c d

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

-- 7
data Four' a b = Four' a a a b

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

{-|
    Write a `Functor` instance for a datatype identical to `Maybe`.
-}
data Possibly a = LolNope | Yeppers a
                  deriving (Eq, Show)

instance Functor Possibly where
    fmap f (Yeppers a) = Yeppers $ f a
    fmap _ LolNope = LolNope

{-|
    Write a `Functor` instance for a datatype identical to `Either`.
-}
data EitherWay a b = First a | Second b
                     deriving (Eq, Show)

instance Functor (EitherWay a) where
    fmap f (Second b) = Second (f b)
    fmap _ (First a) = First a

{-|
   Write `Functor` instances for the following datatypes.
-}
-- 1
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

-- 2
newtype K a b = K a

instance Functor (K a) where
    fmap _ (K a) = K a

-- 3
newtype Flip f a b = Flip (f b a)
                     deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip $ K $ f a

-- 4
newtype EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

-- 5
newtype LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 6
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

-- 8
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9
data List a = Nil | Cons a (List a)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a list) = Cons (f a) (fmap f list)

-- 10
data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a)
                            (GoatLord a)
                            (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- 11
data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read stringToA) = Read (fmap f stringToA)








