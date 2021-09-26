{-# LANGUAGE InstanceSigs #-}
module Reader.Exercises.Misc where

import Control.Applicative (liftA2)

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader (f . ra)

{-|
    Implement the following function.
    If you get stuck, remember it's less complicated than it looks.
-}
ask :: Reader a a
ask = Reader id

{-|
    Write `liftA2` yourself.
    Think about it in terms of abstracting out the difference
    between `getDogR` and `getDogR'` if that helps.
-}
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

{-|
   Write the following function. Again, it is simpler than it looks.
-}
asks :: (r -> a) -> Reader r a
asks = Reader

{-|
   Implement the `Applicative` for `Reader`.

   To write the `Applicative` instance for `Reader`, we'll use an extension called `InstanceSigs`.
   It's an extension we need in order to assert a type for the typeclass methods.
   You ordinarily cannot assert type signatures in instances. The compiler already knows
   the type of the functions, so it's not usually necessary to assert the types in instances anyway.
   We did this for the sake of clarity, to make the Reader type explicit in our signatures.
-}
instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ const a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

{-|
    Implement the `Reader` `Monad`.
-}
instance Monad (Reader r) where
    return = pure

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

{-|
   Rewrite the monadic getDogRM to use your Reader datatype.
-}
newtype HumanName = HumanName String
                    deriving (Eq, Show)

newtype DogName = DogName String
                  deriving (Eq, Show)

newtype Address = Address String
                  deriving (Eq, Show)

data Person = Person { humanName :: HumanName
                     , dogName :: DogName
                     , address :: Address
                     } deriving (Eq, Show)

data Dog = Dog { dogsName :: DogName
               , dogsAddress :: Address
               } deriving (Eq, Show)

getDogRM :: Person -> Dog
getDogRM = do
    address <- address
    name <- dogName
    return $ Dog name address

getDogRM' :: Person -> Dog
getDogRM' = liftA2 Dog dogName address

getDogRM'' :: Reader Person Dog
getDogRM'' = Reader (liftA2 Dog dogName address)

