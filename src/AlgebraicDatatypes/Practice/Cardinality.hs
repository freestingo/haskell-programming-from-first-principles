{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AlgebraicDatatypes.Practice.Cardinality where

data Example = MakeExample deriving Show

data Example' = MakeExample' Int
    deriving Show

------------------------------

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

------------------------------

-- Record syntax

data Person = Person { name :: String
                     , age :: Int
                     }
                       deriving Show

nicolo :: Person
nicolo = Person "Nicolò Traini" 10

------------------------------

data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill
                     | Mac
                     | Windows
                       deriving (Eq, Show)

data ProgLang = Haskell
              | Agda
              | Idris
              | PureScript
                deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem
                            , lang :: ProgLang
                            }
                 deriving (Eq, Show)

