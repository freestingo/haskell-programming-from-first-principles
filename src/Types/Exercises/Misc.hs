{-# LANGUAGE NoMonomorphismRestriction #-}

module Types.Exercises.Misc where

{-|
    Write both possible versions of `a -> a -> a`.
-}
parametricity :: a -> a -> a
parametricity x y = x

parametricity' :: a -> a -> a
parametricity' x y = y

{-|
    Implement `a -> b -> b`. How many implementations can it have?
    Does the behavior change when the types of a and b change?
-}
aToB :: a -> b -> b
aToB a b = b

{-|
    All functions applications return a value.
    Determine the value returned by these, and the type of that value.
-}
a :: Num a => a
a = (*9) 6

b :: Num a => (a, [Char])
b = head [(0, "doge"), (1, "kitty")]

c :: (Integer, [Char])
c = head [(0 :: Integer, "doge"), (1, "kitty")]

d :: Bool
d = if False then True else False

e :: Int
e = length [1, 2, 3, 4, 5]

f :: Bool
f = (length [1, 2, 3, 4]) > (length "TACOCAT")

{-|
    Write the implementation given the type signature.
-}
c' :: a -> b -> a
c' a b = a

c'' :: a -> b -> b
c'' a b = b

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a

a' :: (a -> c) -> a -> a
a' _ a = a

a'' :: (a -> b) -> a -> b
a'' aToB a = aToB a
