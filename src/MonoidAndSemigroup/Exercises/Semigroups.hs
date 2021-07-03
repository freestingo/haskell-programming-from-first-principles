{-# LANGUAGE TupleSections #-}
module MonoidAndSemigroup.Exercises.Semigroups where

import Test.QuickCheck

{-|
   Given a datatype, implement the `Semigroup` instance.
   Add `Semigroup` constraints to type variables where needed.
   When we use (<>), we mean the infix mappend from the `Semigroup` typeclass.

   Validate all of your instances with QuickCheck.
   Keep in mind that you'll potentially need to import the modules for
   `Monoid` and `Semigroup` and to avoid naming conflicts for the (<>),
   depending on your version of GHC.
-}

{-|
   Given a datatype, implement the Monoid instance.
   Add Monoid constraints to type variables where needed.
   For the datatypes yuo've already implemented Semigroup instance for,
   you need to figure out what the identity value is.
-}

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- 1
data Trivial = Trivial
               deriving (Eq, Show)

instance Semigroup Trivial where
   _ <> _ = Trivial

instance Monoid Trivial where
   mempty = Trivial
   mappend = (<>)

instance Arbitrary Trivial where
   arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a
                     deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
   (Identity a) <> (Identity b) = Identity $ a <> b

instance (Monoid a) => Monoid (Identity a) where
   mempty = Identity mempty
   mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
   arbitrary = fmap Identity arbitrary

type IdAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3
-- Hint: Ask for another `Semigroup` instance.
data Two a b = Two a b
               deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
   (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
   mempty = Two mempty mempty
   mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
   arbitrary = do
      a <- arbitrary
      Two a <$> arbitrary

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

-- 6
newtype BoolConj = BoolConj Bool
                   deriving (Eq, Show)

instance Semigroup BoolConj where
   (BoolConj True) <> (BoolConj True) = BoolConj True
   _ <> _ = BoolConj False

instance Monoid BoolConj where
   mempty = BoolConj True
   mappend = (<>)

instance Arbitrary BoolConj where
   arbitrary = fmap BoolConj arbitrary

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7
newtype BoolDisj = BoolDisj Bool
                   deriving (Eq, Show)

instance Semigroup BoolDisj where
   (BoolDisj False) <> (BoolDisj False) = BoolDisj False
   _ <> _ = BoolDisj True

instance Monoid BoolDisj where
   mempty = BoolDisj False
   mappend = (<>)

instance Arbitrary BoolDisj where
   arbitrary = fmap BoolDisj arbitrary

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8
data Or a b = Fst a | Snd b
              deriving (Eq, Show)

instance Semigroup (Or a b) where
   (Snd x) <> (Snd y) = Snd x
   _ <> (Snd y) = Snd y
   (Snd x) <> _ = Snd x
   _ <> y = y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
   arbitrary = oneof [ fmap Fst arbitrary
                     , fmap Snd arbitrary
                     ]

type OrAssoc = Or String String -> Or String String -> Or String String -> Bool

-- 9
newtype Combine a b = Combine { unCombine :: a -> b }

instance Show (Combine a b) where
   show _ = "Combine value!"

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
   (Combine f) <> (Combine g) = Combine (f <> g)

instance (Monoid a, Monoid b) => Monoid (Combine a b) where
   mempty = Combine mempty
   mappend = (<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
   arbitrary = fmap Combine arbitrary

type CombineAssoc = Combine String String -> Combine String String -> Combine String String -> Bool

-- 10
newtype Comp a = Comp { unComp :: a -> a }

instance Show (Comp a) where
   show _ = "Comp value!"

instance (Semigroup a) => Semigroup (Comp a) where
   (Comp x) <> (Comp y) = Comp (y . x)

instance (Monoid a) => Monoid (Comp a) where
   mempty = Comp id
   mappend = (<>)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
   arbitrary = fmap Comp arbitrary

type CompAssoc = Comp String -> Comp String -> Comp String -> Bool

-- 11
data Validation' a b = Failure' a | Success' b
                      deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation' a b) where
   (Failure' x) <> (Failure' y) = Failure' $ x <> y
   _ <> (Success' y) = Success' y
   success <> _ = success

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation' a b) where
   arbitrary = oneof [ fmap Failure' arbitrary
                     , fmap Success' arbitrary
                     ]

type ValidationAssoc = Validation' String Int -> Validation' String Int -> Validation' String Int -> Bool

{-|
   This next exercise will invoive doing something that will feel
   a bit unnatural still and you may find it difficult. If you get it
   and you haven't done much FP or Haskell before, get yourself a nice beverage.
   We're going to toss you the instance declaration so you don't churn on
   a missing `Monoid` constraint you didn't know you needed.
-}
newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
   (Mem f) <> (Mem g) = Mem (\x -> (fst (f x) <> fst (g x), snd $ g x))

instance Monoid a => Monoid (Mem s a) where
   mempty = Mem (mempty,)
   mappend = (<>)


-- test whole module
doQuickChecks :: IO ()
doQuickChecks = do
   quickCheck (semigroupAssoc :: TrivAssoc)
   quickCheck (monoidLeftIdentity :: Trivial -> Bool)
   quickCheck (monoidRightIdentity :: Trivial -> Bool)
   quickCheck (semigroupAssoc :: IdAssoc)
   quickCheck (monoidLeftIdentity :: Identity String -> Bool)
   quickCheck (monoidRightIdentity :: Identity String -> Bool)
   quickCheck (semigroupAssoc :: TwoAssoc)
   quickCheck (monoidLeftIdentity :: Two String String -> Bool)
   quickCheck (monoidRightIdentity :: Two String String -> Bool)
   quickCheck (semigroupAssoc :: BoolConjAssoc)
   quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
   quickCheck (monoidRightIdentity :: BoolConj -> Bool)
   quickCheck (semigroupAssoc :: BoolDisjAssoc)
   quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
   quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
   quickCheck (semigroupAssoc :: OrAssoc)
   quickCheck (semigroupAssoc :: ValidationAssoc)

