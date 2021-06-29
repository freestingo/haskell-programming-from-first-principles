module AlgebraicDatatypes.Exercises.AsPatterns where

import Data.Char

{-|
   This should return `True` if (and only if) all the values in the
   first list appear in the second list, though they need not be contiguous.
-}
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf (x : xs) list@(y : ys) = elem x list && isSubseqOf xs ys

{-|
   Split a sentence into words, then tuple each word with
   the capitalized form of each.
-}

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x : xs) = (toUpper x) : xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords str = zip (wordz) (map capitalizeWord wordz)
   where wordz = words str

{-|
   Write a function that capitalizes sentences in a paragraph.
   Recognize when a new sentence has begun by checking for periods.
   Reuse the capitalizeWord function.
-}
capitalizeParagraph :: String -> String
capitalizeParagraph = unwords . capitalize . words
   where capitalize [] = []
         capitalize str@(x : _) = capitalizeWord x : capitalify str
         capitalify (h : w : t) | (last h) == '.' = capitalizeWord w : capitalify (w : t)
                                | otherwise       = w : capitalify (w : t)
         capitalify _ = []
