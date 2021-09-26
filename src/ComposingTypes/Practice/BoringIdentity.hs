{-# LANGUAGE InstanceSigs #-}
module ComposingTypes.Practice.BoringIdentity where

import Control.Applicative

newtype Identity a =
    Identity { runIdentity :: a }

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose $ (pure . pure) a

    (<*>) :: Compose f g (a -> b)
          -> Compose f g a
          -> Compose f g b
    (Compose f) <*> (Compose a) = Compose $ fmap (<*>) f <*> a

newtype One f a = One (f a) deriving (Eq, Show)

instance Functor f => Functor (One f) where
    fmap f (One fa) = One $ f <$> fa
