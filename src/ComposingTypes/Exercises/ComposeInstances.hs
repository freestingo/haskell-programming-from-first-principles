{-# LANGUAGE InstanceSigs #-}
module ComposingTypes.Exercises.ComposeInstances where

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

{-|
    Write the Compose Foldable instance.
-}
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap aToB (Compose fga) = (foldMap . foldMap) aToB fga

{-|
    Write the Compose Traversable instance.
-}
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse aToApplB (Compose fga) = Compose <$> (traverse . traverse) aToApplB fga

{-|
   Write Bifunctor instances for the following types.
-}
class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b)
          -> (c -> d)
          -> p a c
          -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

data Deux a b = Deux a b

instance Bifunctor Deux where
    first f (Deux a b) = Deux (f a) b
    second f (Deux a b) = Deux a (f b)

newtype Const a b = Const a

instance Bifunctor Const where
    first f (Const a) = Const (f a)
    second _ (Const a) = Const a

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
    first f (Drei a b c) = Drei a (f b) c
    second f (Drei a b c) = Drei a b (f c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
    first f (SuperDrei a b) = SuperDrei a (f b)
    second f (SuperDrei a b) = SuperDrei a b

newtype SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
    first _ (SemiDrei a) = SemiDrei a
    second _ (SemiDrei a) = SemiDrei a

data OhOh a b = Sinis a | Des b

instance Bifunctor OhOh where
    first f (Sinis a) = Sinis (f a)
    first _ (Des b) = Des b

    second f (Des b) = Des (f b)
    second _ (Sinis a) = Sinis a

