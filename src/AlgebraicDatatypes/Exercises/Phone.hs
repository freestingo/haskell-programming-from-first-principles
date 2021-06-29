module AlgebraicDatatypes.Exercises.Phone where

import Data.List
import Data.Maybe
import Data.Char

type Digit = Char

type Presses = Int

type Cost = Int

class T9 a where
   getPresses :: a -> Char -> Presses

data Button = Button { digit :: Digit
                     , chars :: String
                     }
              deriving Show

instance T9 Button where
   getPresses (Button _ chars) char = (+) 1 . fromMaybe (-1) . elemIndex char $ chars

newtype Phone = Phone [Button]
                deriving Show

oldNokia :: Phone
oldNokia = Phone [ Button '1' "1"
                 , Button '2' "abc2"
                 , Button '3' "def3"
                 , Button '4' "ghi4"
                 , Button '5' "jkl5"
                 , Button '6' "mno6"
                 , Button '7' "pqrs7"
                 , Button '8' "tuv8"
                 , Button '9' "wxyz9"
                 , Button '*' "^"
                 , Button '0' " 0"
                 , Button '#' ".,"
                 ]

convo :: [String]
convo = [ "We"
        , "we"
        , "Allora"
        , "cosa"
        , "allora io vado..."
        , "ok"
        , "ciaociao"
        , "Buona"
        ]

charTaps :: Phone -> Char -> [(Digit, Presses)]
charTaps (Phone buttons) char = concatMap toPresses . filter charIn $ buttons
   where lowerChar = toLower char
         charIn = elem lowerChar . chars
         toPresses button | isUpper char = [upperPresses, lowerPresses]
                          | otherwise = [lowerPresses]
                            where lowerPresses = (digit button, getPresses button lowerChar)
                                  upperPresses = ('*', 1)

stringTaps :: Phone -> String -> [(Digit, Presses)]
stringTaps phone = concatMap $ charTaps phone

{-|
   How many times do digits need to be pressed for each message?
-}
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

{-|
   What was the most popular letter for each message? What was its cost?
-}
mostPopularLetter :: String -> (Char, Cost)
mostPopularLetter str = getCost $ getMostFrequent $ countLetters
  where count x xs = length . filter (== x) $ xs
        occurrences = map (\x -> count x str) str
        countLetters = nub $ zip str occurrences
        getMostFrequent = maximumBy (\x y -> compare (snd x) (snd y))
        getCost (char, occs) = (char, occs * (fingerTaps $ charTaps oldNokia char))

