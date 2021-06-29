{-# LANGUAGE FlexibleInstances #-}

module AlgebraicDatatypes.Exercises.Cardinality where

import AlgebraicDatatypes.Practice.Cardinality

{-|
   Reusing the TooMany typeclass, write an instance of
   the typeclass for the type (Int, String). This will require
   adding a language pragma named FlexibleInstances if you do not use a newtype.

   https://ghc.haskell.org/trac/haskell-prime/wiki/FlexibleInstances
-}
instance TooMany (Int, String) where
    tooMany (int, string) = tooMany int

{-|
   Make another TooMany instance for (Int, Int).
   Sum the values together under the assumption
   this is a count of goats from two fields.
-}
instance TooMany (Int, Int) where
    tooMany (a, b) = tooMany $ a + b

{-|
   Make another TooMany instance, this time for (Num a, TooMany a) => (a, a).
   This can mean whatever you want, such as summing the two numbers together.
-}

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, y) = tooMany x || tooMany y

---------------------------------------------

{-|
   What is the sum of products normal form of `Garden`?
-}
data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType
            deriving Show

data Garden' = Gardenia' Gardener
             | Daisy' Gardener
             | Rose' Gardener
             | Lilac' Gardener
             deriving Show

---------------------------------------------

{-|
   Write a function that generates all possible values of `Programmer`.
   Use the provided lists of inhabitants of `OperatingSystem` and `ProgLang`.
-}

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux
                      , OpenBSDPlusNevermindJustBSDStill
                      , Mac
                      , Windows
                      ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer os lang
                 | os <- allOperatingSystems
                 , lang <- allLanguages
                 ]


