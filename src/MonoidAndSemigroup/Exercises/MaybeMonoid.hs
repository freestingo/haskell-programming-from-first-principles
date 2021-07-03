module MonoidAndSemigroup.Exercises.MaybeMonoid where

import MonoidAndSemigroup.Exercises.Monoids
import Test.QuickCheck

{-|
   Write a `Monoid` instance for a `Maybe`-like type which doesn't require a `Monoid` for the contents.
   Reuse the `Monoid` law `QuickCheck` properties and use them to validate the instance.

   Don't forget to write an `Arbitrary` instance for `First'`. We won't always stub that out
   explicitly for you. We suggest elarning how to use the frequency function from
   `QuickCheck` for `First'`'s instance.
-}

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

newtype First' a = First' { getFirst' :: Optional a }
                   deriving (Eq, Show)

instance Monoid (First' a) where
    mempty = First' Nada

instance Semigroup (First' a) where
    x <> (First' Nada) = x
    (First' Nada) <> y = y
    x <> _ = x

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = frequency [ (1, return $ First' Nada)
                          , (3, fmap (First' . Only) arbitrary)
                          ]
{-|
    alternative:

    arbitrary = do
        a <- arbitrary
        oneof [ return $ First' Nada,
              , return $ First' $ Only a
              ]
-}


firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

qcMaybeMonoid :: IO ()
qcMaybeMonoid = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)

