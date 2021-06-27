module Strings.Exercises where

excite :: String -> String
excite x = x ++ "!"

fifthChar :: String -> Char
fifthChar x = x !! 4

fromTenthChar :: String -> String
fromTenthChar x = drop 9 x

getNCharFrom :: String -> Int -> Char
getNCharFrom str ind = str !! ind

-- only intended to work with the string "Curry is awesome"
reverseCurry :: String -> String
reverseCurry x = unwords $ [(drop 9 x), (take 2 $ drop 6 x), (take 5 x)]

myStringReverse :: String -> String
myStringReverse = unwords . reverse . words
