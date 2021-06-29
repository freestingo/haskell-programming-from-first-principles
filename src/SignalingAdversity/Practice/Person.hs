module SignalingAdversity.Practice.Person where

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age
              deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                     deriving (Eq, Show)

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
    True  -> Right age
    False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
    True  -> Right name
    False -> Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = smartMkPerson (nameOkay name) (ageOkay age)

smartMkPerson :: ValidatePerson Name
              -> ValidatePerson Age
              -> ValidatePerson Person
smartMkPerson (Right name) (Right age) = Right (Person name age)
smartMkPerson (Left badName) (Left badAge) = Left (badName ++ badAge)
smartMkPerson (Left badName) _ = Left badName
smartMkPerson _ (Left badAge) = Left badAge

{-|
   Later in the book, we'll be able to replace `mkPerson` and `smartMkPerson` with the following:

    mkPerson :: Name -> Age -> Validation [PersonInvalid] Person
    mkPerson name age =
        liftA2
            Person (nameOkay name) (ageOkay age)
-}

