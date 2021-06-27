module Lists.Exercises.CharMagic where

import Data.Char

filterUppers :: String -> String
filterUppers = filter (\x -> isUpper x)

capitalize :: String -> String
capitalize x = (map toUpper . take 1 $ x) ++ drop 1 x

capitalize' :: String -> String
capitalize' = processTuple . splitAt 1
  where processTuple (h, t) = concat [map toUpper h, t]

capitalizeRec' :: String -> String
capitalizeRec' = foldr (\x acc -> toUpper x : acc) ""

capitalizeRec :: String -> String
capitalizeRec "" = ""
capitalizeRec (x : xs) = toUpper x : capitalizeRec xs

capitalizeFirst :: String -> String
capitalizeFirst = map toUpper . safeHead
  where safeHead [] = []
        safeHead (x : _) = [x]


