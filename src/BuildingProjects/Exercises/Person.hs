module BuildingProjects.Exercises.Person where

import System.IO

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                     deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age >= 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age >= 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ "; age was: " ++ show age

{-|
   Your job is to write the following function without modifying the code above.

   - it should prompt the user for a name and age input;
   - it should attempt to construct a `Person` value
     using the name and age the user entered (you'll need the
     `read` function for `Age` because it's an `Integer` rather than a `String`);
   - if it constructed a person successfully, it should print "Yay! Successfully got a person:",
     followed by the Person value;
   - if it got an error value, report that an error occurred and print the error.
-}
gimmePerson :: IO ()
gimmePerson = do
    hSetBuffering stdout NoBuffering
    putStr "Name: "
    name <- getLine
    putStr "Age: "
    age <- getLine
    putStrLn $ printPerson $ mkPerson name $ read age
        where printPerson (Right person) = show person
              printPerson (Left (PersonInvalidUnknown err)) = "Unhandled error - " ++ err
              printPerson (Left invalid) = show invalid

