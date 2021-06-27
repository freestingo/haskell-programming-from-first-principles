module Recursion.Exercises.NumbersIntoWords where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> "unknown"

digits :: Int -> [Int]
digits n = go n []
  where go num digits
          | unit > num  = num : digits
          | otherwise   = go next (digit : digits)
            where unit  = 10
                  res   = divMod num unit
                  next  = fst res
                  digit = snd res

numberToWord :: Int -> String
numberToWord = intercalate "-" . map digitToWord . digits
