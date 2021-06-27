module Lists.Exercises.AdventOfCode where

import Data.Ix
import Data.Char
import Data.Bool
import Data.List (intercalate)


-- TODO cover all patterns! this is buggy as hell

validatePassword :: (Int, Int) -> Char -> String -> Bool
validatePassword minMax c = inRange minMax . length . filter (== c)

validate :: ((Int, Int), Char, String) -> Bool
validate (minMax, c, pwd) = inRange minMax . length . filter (== c) $ pwd

{-|
    format: `<min>-<max> <char>: <password>`
    examples:
        - 1-2 f: caffe (valid)
        - 1-4 c: pippofranco (invalid)
-}
parseLine :: String -> ((Int, Int), Char, String)
parseLine = process . words
    where process [minMaxRange, c, password] = ( ( read . takeWhile (/= '-') $ minMaxRange
                                                 , read . tail . dropWhile (/= '-') $ minMaxRange
                                                 )
                                               , head c
                                               , password
                                               )

parseFile :: String -> String
parseFile = intercalate "\n" . map process . lines
  where process = bool "Please change this password." "This password is valid!" . validate . parseLine

checkPassword = do
    putStrLn "Enter your password:"
    putStrLn "format: `<min>-<max> <char>: <password>`"
    file <- getLine
    putStrLn . parseFile $ file
