module Monad.Exercises.EitherMonad where

{-|
   Short exercise: implement the `Either` monad.
-}
data Eithur a b = Furst a | Secund b
                  deriving (Eq, Show)

instance Functor (Eithur a) where
   fmap _ (Furst a) = Furst a
   fmap f (Secund b) = Secund (f b)

instance Applicative (Eithur a) where
   pure = Secund
   (Secund b) <*> (Secund b') = Secund (b b')
   _ <*> (Furst a) = Furst a
   (Furst a) <*> _ = Furst a

instance Monad (Eithur a) where
   return = pure
   (Secund b) >>= f = f b
   (Furst a) >>= _ = Furst a
