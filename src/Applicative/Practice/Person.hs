module Applicative.Practice.Person where

import Control.Applicative

newtype Name = Name String
               deriving (Eq, Show)

newtype Address = Address String
                  deriving (Eq, Show)

newtype Age = Age Int
              deriving (Eq, Show)

data Person = Person Name Age Address
              deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength maxLength s
  | length s > maxLength = Nothing
  | otherwise = Just s

validateAge :: (Int, Int) -> Int -> Maybe Int
validateAge range age
  | inRange range age = Just age
  | otherwise = Nothing
    where inRange (min, max) = liftA2 (&&) (>= min) (<= max)

mkName :: String -> Maybe Name
mkName = fmap Name . validateLength 15

mkAddress :: String -> Maybe Address
mkAddress = fmap Address . validateLength 100

mkAge :: Int -> Maybe Age
mkAge = fmap Age . validateAge (0, 120)

mkPerson :: String -> Int -> String -> Maybe Person
mkPerson name age address = Person <$> mkName name
                                   <*> mkAge age
                                   <*> mkAddress address



