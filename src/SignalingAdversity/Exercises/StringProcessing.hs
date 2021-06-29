module SignalingAdversity.Exercises.StringProcessing where

import Data.List

{-|
    Write a recursive funcitonn named `replaceThe` which takes a text/string,
    breaks it into words and replaces each instance of "the" with "a".
    It's intended only to replace exactly the word "the".

    `notThe` is a suggested helper funciton for accomplishing this.
-}
notThe :: String -> Maybe String
notThe str | str == "the" = Nothing
           | otherwise = Just str

replaceThe :: String -> String
replaceThe = unwords . map replaceArticle . map notThe . words
    where replaceArticle Nothing = "a"
          replaceArticle (Just x) = x

{-|
    Write a recursive funciton that takes a text/string, breaks it into words,
    and counts the number of instances of "the" followed by a vowel-initial word.
-}
vowels :: String
vowels = "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go (words str) 0
    where go [] count = count
          go [x] count = count
          go (w : ws) count | w == "the" && (nextStartsWithVocal ws) = go ws (count + 1)
                            | otherwise = go ws count
          nextStartsWithVocal = flip elem vowels . head . head

{-|
    Return the number of letters that are vowels in a word.
    Hint: it's helpful to break this into steps. Add any helper
          functions necessary to achieve your objectives.
-}
countVowels :: String -> Int
countVowels = length . filter (== True) . map isVowel
    where isVowel = flip elem vowels

{-|
    Use the `Maybe` type to write a function that counts the number
    of vowels in a string and the number of consonants. If the number
    of vowels exceeds the number of consonants, the function returns `Nothing`.
-}
newtype Word' = Word' String
                deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str | countVowels str > countConsonants str = Nothing
           | otherwise = Just $ Word' str
    where countConsonants = length . filter (== True) . map isConsonant
          isConsonant = flip notElem vowels
