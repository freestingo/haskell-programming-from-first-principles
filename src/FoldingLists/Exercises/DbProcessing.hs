module FoldingLists.Exercises.DbProcessing where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
              , DbNumber 9001
              , DbNumber 1003
              , DbString "Hello, world!"
              , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
              ]

{-|
   Write a function that filters for DbDate values
   and returns a list of the UTCTime values inside them.
-}
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr filterDates []
    where filterDates (DbDate a) acc = a : acc
          filterDates _ acc = acc

{-|
   Write a function that filters for DbNumber values
   and returns a list of the Integer values inside them.
-}
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr filterNumbers []
    where filterNumbers (DbNumber n) acc = n : acc
          filterNumbers _ acc = acc

{-|
   Write a function that gets the most recent date.
-}
mostRecent :: [DatabaseItem] -> Maybe UTCTime
mostRecent = foldr getMostRecent Nothing
    where getMostRecent (DbDate a) (Just b) = Just $ max a b
          getMostRecent (DbDate a) (Nothing) = Just a
          getMostRecent _ acc = acc

{-|
   Write a function that sums all fo the DbNumber values.
-}
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

{-|
   Write a function that gets the average of the DbNumber values.
-}
avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral . sumDb $ xs) / (fromIntegral . length . filterDbNumber $ xs)

