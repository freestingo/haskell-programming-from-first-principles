module Lists.Exercises.AdventOfCode where

import Data.Ix
import Data.Char
import Data.Bool
import Data.List (intercalate)

type Min = Int
type Max = Int
type RequiredChar = Char
type Password = String

data CharRange = Range Min Max

data PasswordMetadata = PasswordMetadata CharRange RequiredChar Password

-- TODO cover all patterns! this is buggy as hell

validatePassword :: (Int, Int) -> Char -> String -> Bool
validatePassword minMax c = inRange minMax . length . filter (== c)

validate :: PasswordMetadata -> Bool
validate (PasswordMetadata (Range min max) c pwd) = inRange (min, max) . length . filter (== c) $ pwd
validate _ = False

{-|
    format: `<min>-<max> <char>: <password>`
    examples:
        - 1-2 f: caffe (valid)
        - 1-4 c: pippofranco (invalid)
-}
parseLine :: String -> PasswordMetadata
parseLine = process . words
    where process [minMaxRange, c, password] = PasswordMetadata (Range (read . takeWhile (/= '-') $ minMaxRange) (read . tail . dropWhile (/= '-') $ minMaxRange))
                                                                (head c)
                                                                (password)


parseFile :: String -> String
parseFile = intercalate "\n" . map process . lines
  where process = bool "Please change this password." "This password is valid!" . validate . parseLine

checkPassword = do
    putStrLn "Enter your password:"
    putStrLn "format: `<min>-<max> <char>: <password>`"
    file <- getLine
    putStrLn . parseFile $ file
