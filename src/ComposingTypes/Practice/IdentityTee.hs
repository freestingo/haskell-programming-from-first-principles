{-# LANGUAGE InstanceSigs #-}
module ComposingTypes.Practice.IdentityTee where

import Control.Monad

newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)

newtype IdentityT f a =
    IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Functor m => Functor (IdentityT m) where
    fmap aToB (IdentityT ma) = IdentityT $ aToB <$> ma

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)

instance Applicative m => Applicative (IdentityT m) where
    pure x = IdentityT $ pure x
    (<*>) (IdentityT faToB) (IdentityT fa) = IdentityT $ faToB <*> fa

instance Monad Identity where
    return = pure
    (Identity a) >>= f = f a

instance Monad m => Monad (IdentityT m) where
    return = pure

    (>>=) :: IdentityT m a
          -> (a -> IdentityT m b)
          -> IdentityT m b
    (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

